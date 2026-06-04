#' Decompose social change from aggregated data
#'
#' Decomposes aggregate-level change into intraindividual change and population turnover
#' components using microsimulation on stacked cross-sectional data. Requires a prediction
#' function that models the outcome as a function of age, period, and covariates.
#'
#' @param stacked_data Stacked data.table with columns \code{age}, \code{period}, \code{n}, \code{y}, and optional cell identifiers
#' @param fun_y Prediction function taking \code{(newdata)} and returning predicted outcome values
#' @param cells Character vector of additional cell identifier columns beyond age (e.g., "gender", "smoking")
#' @param migration Logical; if TRUE, decompose migration components (not yet implemented)
#'
#' @return S3 object of class \code{social_change_decomp} with components:
#'   \itemize{
#'     \item \code{summary}: data.table with decomposition components by period
#'     \item \code{record}: list of detailed event records for each period transition
#'     \item \code{migration}: logical indicating whether migration was decomposed
#'   }
#'
#' @details
#' The function estimates mortality and coming-of-age from period-to-period population
#' differences within cells, then uses microsimulation to randomly order demographic
#' events and track their contribution to aggregate change.
#'
#' \strong{Limitation}: Does not properly handle within-cell state transitions. Transition
#' effects are absorbed into the intraindividual change component. See CLAUDE.md for details.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' # Simulate data first
#' data <- data.table(age = 20:39, n = rep(100, 20))
#' simresult <- sim_social_change(periods = 5, data = data, ...)
#'
#' # Stack snapshots and fit model
#' stacked <- rbindlist(simresult$snapshot)
#' model <- lm(y ~ age, data = stacked)
#' predict_y <- function(newdata) predict(model, newdata = newdata)
#'
#' # Decompose
#' result <- decompose_aggregated(stacked, predict_y)
#' print(result)
#' }
#'
#' @import data.table
#' @export
decompose_aggregated <- function(stacked_data, fun_y, cells = c(), migration = FALSE) {
    checkmate::assert_data_frame(stacked_data)
    checkmate::assert_subset(c("age", "n", cells), names(stacked_data))
    checkmate::assert_function(fun_y, nargs = 1)
    checkmate::assert_vector(cells, any.missing = FALSE, null.ok = TRUE)
    checkmate::assert_logical(migration)

    stacked_data <- copy(stacked_data)
    periods <- stacked_data[, unique(period)]
    stacked_data[, y_pred := fun_y(.SD)]
    cells <- c(cells, "age")

    record <- vector("list", length(periods))
    summary <- data.table(
        period = periods,
        observed_mean = NA_real_,
        modeled_mean = NA_real_,
        intraindividual = NA_real_,
        coming_of_age = NA_real_,
        mortality = NA_real_
    )
    if (migration) {
        stop("Migration not supported")

        summary[, inmigration := NA_real_]
        summary[, outmigration := NA_real_]
    }
    means <- stacked_data[, .(observed = stats::weighted.mean(y, n), modeled = stats::weighted.mean(y_pred, n)), by = .(period)]
    if (means[, max(observed / modeled)] > 1.05) {
        print(means)
        stop("Modeled means deviate strongly from observed mean, reconsider model.")
    }
    summary[means, `:=`(observed_mean = observed, modeled_mean = modeled), on = "period"]

    for (i_period in 1:(length(periods) - 1)) {
        vars <- c("n", cells)
        # aggregate so that join doesn't fan out
        data1 <- stacked_data[period == periods[i_period], .(n = sum(n)), by = cells]
        data2 <- stacked_data[period == periods[i_period + 1], .(n = sum(n)), by = cells]
        data2[, age := age - 1]
        for (var in vars) {
            if (!(var %in% cells)) {
                setnames(data1, var, paste0(var, "1"))
                setnames(data2, var, paste0(var, "2"))
            }
        }
        data <- merge(data1, data2, all = TRUE, by = cells)
        data[, n1 := nafill(n1, fill = 0)]
        data[, n2 := nafill(n2, fill = 0)]
        data[, n := n1]
        data[, period := periods[i_period]]
        data[, y := fun_y(data)]
        data[, cell_id := seq_len(.N)]

        min_age <- data[, min(age)]
        # LIMITATION: pmax() prevents negative probabilities but doesn't account for
        # transitions between cells (e.g., smoker -> non-smoker). This is a known limitation
        # of the aggregated decomposition approach for data with within-cell transitions.
        data[, coming_of_age := ifelse(age == min_age, pmax(0, n2 - n1), 0)]
        data[, mortality := ifelse(age == min_age, 0, pmax(0, n1 - n2))]

        event_counts <- c(
            "coming_of_age" = data[, sum(coming_of_age)],
            "mortality" = data[, sum(mortality)]
        )

        n_ev <- sum(event_counts)
        n_c <- nrow(data)
        n_records <- 2L * n_ev + 1L

        # Plain vectors for the change record; assembled into a data.table after the loop
        cr_component <- character(n_records)
        cr_time <- numeric(n_records)
        cr_delta <- numeric(n_records)
        cr_idx <- 1L

        events_tick <- sort(stats::runif(n_ev))
        tick <- 0

        # Local copies keep R vector semantics inside the loop (no data.table overhead)
        n_vec <- data$n
        mortality_vec <- data$mortality
        coming_of_age_vec <- data$coming_of_age

        # Evaluate fun_y once on a stacked copy of data at every event time, yielding a
        # n_c × n_ev matrix. Only age and period are updated per event-time slice; all
        # other covariates (e.g. gender, smoking) are carried through from data as-is.
        # This requires fun_y to not depend on n, which holds for any externally-fitted
        # statistical model.
        if (n_ev > 0L) {
            idx_rep <- rep.int(seq_len(n_c), n_ev)
            data_stack <- data[idx_rep]
            set(
                data_stack, NULL, "age",
                rep.int(data$age, n_ev) + rep(events_tick, each = n_c)
            )
            set(
                data_stack, NULL, "period",
                rep.int(data$period, n_ev) + rep(events_tick, each = n_c)
            )
            y_mat <- matrix(fun_y(data_stack), nrow = n_c, ncol = n_ev)
        }

        # Maintain running weighted-sum scalars; updated in O(1) after each demographic event
        sum_n <- sum(n_vec)
        sum_yn <- sum(data$y * n_vec)
        post_event_mean <- sum_yn / sum_n

        for (i_ev in seq_len(n_ev)) {
            event_tick <- events_tick[i_ev]
            tick <- event_tick
            time <- i_period - 1 + tick

            pre_mean <- post_event_mean # reuses last iteration's post_event_mean
            y_cur <- y_mat[, i_ev]
            sum_yn <- sum(y_cur * n_vec)
            post_ic_mean <- sum_yn / sum_n

            cr_component[cr_idx] <- "intraindividual"
            cr_time[cr_idx] <- time
            cr_delta[cr_idx] <- post_ic_mean - pre_mean
            cr_idx <- cr_idx + 1L

            event <- sample(names(event_counts), 1L, prob = event_counts)

            if (event == "mortality") {
                pick_idx <- sample.int(n_c, 1L, prob = mortality_vec)
                y_pick <- y_cur[pick_idx]
                mortality_vec[pick_idx] <- mortality_vec[pick_idx] - 1
                n_vec[pick_idx] <- n_vec[pick_idx] - 1
                sum_yn <- sum_yn - y_pick
                sum_n <- sum_n - 1
            } else if (event == "coming_of_age") {
                pick_idx <- sample.int(n_c, 1L, prob = coming_of_age_vec)
                y_pick <- y_cur[pick_idx]
                coming_of_age_vec[pick_idx] <- coming_of_age_vec[pick_idx] - 1
                n_vec[pick_idx] <- n_vec[pick_idx] + 1
                sum_yn <- sum_yn + y_pick
                sum_n <- sum_n + 1
            }
            event_counts[event] <- event_counts[event] - 1
            post_event_mean <- sum_yn / sum_n

            cr_component[cr_idx] <- event
            cr_time[cr_idx] <- time
            cr_delta[cr_idx] <- post_event_mean - post_ic_mean
            cr_idx <- cr_idx + 1L
        }

        # Write local vectors back and advance to next period.
        # age was not updated inside the loop (y was pre-computed instead), so add
        # the full period (1) here rather than just the remaining fraction (1 - tick).
        data[, `:=`(
            n = n_vec, mortality = mortality_vec, coming_of_age = coming_of_age_vec,
            age = age + 1, period = period + 1
        )]
        data[, y := fun_y(data)]
        sum_yn <- sum(data$y * n_vec)
        post_ic_mean <- sum_yn / sum_n

        cr_component[cr_idx] <- "intraindividual"
        cr_time[cr_idx] <- i_period
        cr_delta[cr_idx] <- post_ic_mean - post_event_mean

        # Build change_record data.table once after the loop
        change_record <- data.table(
            i = seq_len(n_records),
            component = cr_component,
            time = cr_time,
            delta = cr_delta
        )

        # summarize period change
        by_component <- change_record[, .(delta = sum(delta)), by = .(component)]
        record[[i_period]] <- change_record
        for (comp in c("intraindividual", "mortality", "outmigration", "coming_of_age", "inmigration")) {
            delta <- by_component[component == comp, "delta"][[1]]
            if (length(delta) == 1) {
                summary[i_period + 1, (comp) := delta]
            } else {
                summary[i_period + 1, (comp) := 0]
            }
        }
    } # i_period loop

    ret <- list(summary = summary, record = record, migration = migration)
    class(ret) <- c("list", "social_change_decomp")
    ret
}

#' @import data.table
#' @export
print.social_change_decomp <- function(x, detailed = TRUE, ...) {
    options(digits = 4, scipen = 999)
    if (detailed) {
        cat("Overview by period:\n")
        print(x$summary, row.names = FALSE, class = FALSE, na.print = "")
    }
    intraindividual <- x$summary[2:.N, round(sum(intraindividual), 6)]
    mortality <- x$summary[2:.N, round(sum(mortality), 6)]
    outmigration <- x$summary[2:.N, round(sum(outmigration), 6)]
    coming_of_age <- x$summary[2:.N, round(sum(coming_of_age), 6)]
    inmigration <- x$summary[2:.N, round(sum(inmigration), 6)]

    pt <- mortality + outmigration + coming_of_age + inmigration

    mean0 <- round(x$summary[1][["modeled_mean"]], 6)
    meanN <- round(x$summary[.N][["modeled_mean"]], 6)

    if (detailed) {
        cat("\nDecomposition of total change:\n")
    }
    decomp <- data.table(
        Component = c(
            "At initial (modeled)",
            "At end (modeled)",
            "Total change",
            "- Intraindividual change",
            "- Population turnover",
            "  - Mortality",
            "  - Out-migration",
            "  - Coming-of-age",
            "  - In-migration"
        ),
        Value = c(
            mean0,
            meanN,
            meanN - mean0,
            intraindividual,
            pt,
            mortality,
            outmigration,
            coming_of_age,
            inmigration
        )
    )
    if (!x$migration) {
        decomp <- decomp[!grepl("migration", Component)]
    }

    print(decomp, row.names = FALSE, class = FALSE, justify = "left")
    if (!x$migration) {
        cat("Assumes no in- or out-migration.\n")
    }
    options(digits = 7, scipen = 0) # reset to default
}
