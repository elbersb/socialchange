#' Decompose social change from aggregated data
#'
#' Decomposes aggregate-level change into intraindividual change and population turnover
#' components using microsimulation on stacked cross-sectional data. Requires a prediction
#' function that models the outcome as a function of age, period, and covariates.
#'
#' @param stacked_data Data frame with columns \code{age}, \code{period}, and \code{y}, plus optional cell identifiers.
#'   If a column \code{n} is present the data is treated as already aggregated to cells; otherwise individual-level
#'   rows are aggregated internally using \code{weight}.
#' @param fun_y Prediction function taking \code{(newdata)} and returning predicted outcome values
#' @param cells Character vector of additional cell identifier columns beyond age (e.g., "gender", "smoking")
#' @param migration Logical; if TRUE, decompose migration components (not yet implemented)
#' @param tol Maximum tolerated relative deviation between observed and modeled period means (default 0.05 = 5\%).
#'   Emits a warning rather than stopping when exceeded.
#' @param weight Name of the weight column used when aggregating individual-level data (ignored if \code{n} is present).
#'   Weights are normalized within each period to sum to the period sample size before aggregation, so that cell
#'   counts \code{n} (rounded sums of normalized weights) reflect the relative population structure rather than
#'   raw sample sizes. This preserves simulation tractability but is an approximation: the ideal approach would
#'   use true population counts, which are generally unavailable from survey data alone.
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
#' events and track their contribution to aggregate change. Unequal and multi-year gaps
#' between periods are supported: when the gap exceeds one year, each entering cohort is
#' assigned to the specific calendar year within the gap when it crosses the minimum age,
#' so that post-entry aging is correctly attributed to intraindividual change rather than
#' coming-of-age.
#'
#' \strong{Limitation}: Does not properly handle within-cell state transitions. Transition
#' effects are absorbed into the intraindividual change component.
#'
#' @examples
#' \donttest{
#' library(data.table)
#' data("gss_homosex", package = "socialchange")
#' stacked <- as.data.table(gss_homosex)[, .(age, period = year, y = homosex)]
#' model <- stats::lm(y ~ age + period, data = stacked)
#' result <- decompose_aggregated(stacked, function(d) predict(model, newdata = d))
#' print(result)
#' }
#'
#' @seealso [decompose_events()] for event-driven decomposition,
#'   [sim_social_change()] for forward simulation with fully specified demographic functions.
#'   Vignette: \code{vignette("decompose_aggregated", package = "socialchange")}.
#' @import data.table
#' @export
decompose_aggregated <- function(stacked_data, fun_y, cells = c(), migration = FALSE, tol = 0.05, weight = NULL) {
    checkmate::assert_data_frame(stacked_data)
    checkmate::assert_subset(c("age", "period", "y", cells), names(stacked_data))
    checkmate::assert_function(fun_y, nargs = 1)
    checkmate::assert_vector(cells, any.missing = FALSE, null.ok = TRUE)
    checkmate::assert_logical(migration)
    checkmate::assert_number(tol, lower = 0)
    checkmate::assert_string(weight, null.ok = TRUE)

    stacked_data <- copy(as.data.table(stacked_data))

    if (!"n" %in% names(stacked_data)) {
        group_cols <- c("age", "period", cells)
        if (!is.null(weight)) {
            checkmate::assert_subset(weight, names(stacked_data))
            setnames(stacked_data, weight, ".wt")
            stacked_data[, .wt := .wt / sum(.wt) * .N, by = period]
            stacked_data <- stacked_data[, .(n = round(sum(.wt)), y = stats::weighted.mean(y, .wt)), by = group_cols]
        } else {
            stacked_data <- stacked_data[, .(n = .N, y = mean(y)), by = group_cols]
        }
    }
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
    max_dev <- means[, max(abs(observed / modeled - 1))]
    if (max_dev > tol) {
        print(means)
        warning(sprintf(
            "Modeled means deviate from observed by up to %.1f%% (tol = %.1f%%). Consider a more flexible model or increase tol.",
            max_dev * 100, tol * 100
        ))
    }
    summary[means, `:=`(observed_mean = observed, modeled_mean = modeled), on = "period"]

    for (i_period in 1:(length(periods) - 1)) {
        gap <- as.numeric(periods[i_period + 1] - periods[i_period])
        vars <- c("n", cells)
        # aggregate so that join doesn't fan out
        data1 <- stacked_data[period == periods[i_period], .(n = sum(n)), by = cells]
        data2 <- stacked_data[period == periods[i_period + 1], .(n = sum(n)), by = cells]
        min_age_data1 <- data1[, min(age)]
        data2[, age := age - gap]
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

        # Ages below min_age_data1 are cohorts that entered during the gap; all others
        # are survivors from the prior period.
        # LIMITATION: pmax() prevents negative probabilities but doesn't account for
        # transitions between cells (e.g., smoker -> non-smoker). This is a known limitation
        # of the aggregated decomposition approach for data with within-cell transitions.
        data[, coming_of_age := ifelse(age < min_age_data1, pmax(0, n2 - n1), 0)]
        data[, mortality := ifelse(age < min_age_data1, 0, pmax(0, n1 - n2))]

        n_ev <- data[, sum(coming_of_age) + sum(mortality)]
        n_c <- nrow(data)
        n_records <- 2L * n_ev + 1L

        # Plain vectors for the change record; assembled into a data.table after the loop
        cr_component <- character(n_records)
        cr_time <- numeric(n_records)
        cr_delta <- numeric(n_records)
        cr_idx <- 1L

        # Pre-assign every event to a (time, type, cell) triple and sort by time.
        #
        # Coming-of-age timing:
        #   data2 ages are shifted back by `gap`, so a row with aligned age `a` represents
        #   people who were `a` years old at period 1 and will be `a + gap` at period 2.
        #   For coming-of-age cells (a < min_age_data1), these people crossed the minimum
        #   age threshold during the gap.  A person with aligned age `a` reaches
        #   min_age_data1 exactly (min_age_data1 - a) years into the gap.  Because birth
        #   dates are spread across a calendar year, entries are uniform over the year
        #   ending at that point:
        #
        #     lo = (min_age_data1 - a - 1) / gap     hi = (min_age_data1 - a) / gap
        #
        #   Example — gap = 2, min_age_data1 = 20:
        #     aligned age 19 (age 21 in period 2): lo = 0,   hi = 0.5  (enter in year 1)
        #     aligned age 18 (age 20 in period 2): lo = 0.5, hi = 1.0  (enter in year 2)
        #
        # Mortality is uniformly distributed across the full gap.
        ev_type <- character(n_ev)
        ev_cell <- integer(n_ev)
        ev_time <- numeric(n_ev)
        ev_idx <- 1L
        coa_vec <- data$coming_of_age
        mort_vec <- data$mortality
        for (cidx in seq_len(n_c)) {
            n_coa <- coa_vec[cidx]
            if (n_coa > 0L) {
                a <- data$age[cidx]
                lo <- max(0, (min_age_data1 - a - 1) / gap)
                hi <- (min_age_data1 - a) / gap
                end_idx <- ev_idx + n_coa - 1L
                ev_type[ev_idx:end_idx] <- "coming_of_age"
                ev_cell[ev_idx:end_idx] <- cidx
                ev_time[ev_idx:end_idx] <- stats::runif(n_coa, lo, hi)
                ev_idx <- end_idx + 1L
            }
            n_mort <- mort_vec[cidx]
            if (n_mort > 0L) {
                end_idx <- ev_idx + n_mort - 1L
                ev_type[ev_idx:end_idx] <- "mortality"
                ev_cell[ev_idx:end_idx] <- cidx
                ev_time[ev_idx:end_idx] <- stats::runif(n_mort)
                ev_idx <- end_idx + 1L
            }
        }
        ord <- order(ev_time)
        events_tick <- ev_time[ord]
        ev_type <- ev_type[ord]
        ev_cell <- ev_cell[ord]

        # n_vec tracks the current population; updated as events fire
        n_vec <- data$n

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
                rep.int(data$age, n_ev) + rep(events_tick * gap, each = n_c)
            )
            set(
                data_stack, NULL, "period",
                rep.int(data$period, n_ev) + rep(events_tick * gap, each = n_c)
            )
            y_mat <- matrix(fun_y(data_stack), nrow = n_c, ncol = n_ev)
        }

        # Maintain running weighted-sum scalars; updated in O(1) after each demographic event
        sum_n <- sum(n_vec)
        sum_yn <- sum(data$y * n_vec)
        post_event_mean <- sum_yn / sum_n

        for (i_ev in seq_len(n_ev)) {
            event_tick <- events_tick[i_ev]
            time <- i_period - 1 + event_tick

            pre_mean <- post_event_mean # reuses last iteration's post_event_mean
            y_cur <- y_mat[, i_ev]
            sum_yn <- sum(y_cur * n_vec)
            post_ic_mean <- sum_yn / sum_n

            cr_component[cr_idx] <- "intraindividual"
            cr_time[cr_idx] <- time
            cr_delta[cr_idx] <- post_ic_mean - pre_mean
            cr_idx <- cr_idx + 1L

            event <- ev_type[i_ev]
            pick_idx <- ev_cell[i_ev]
            y_pick <- y_cur[pick_idx]

            if (event == "mortality") {
                n_vec[pick_idx] <- n_vec[pick_idx] - 1
                sum_yn <- sum_yn - y_pick
                sum_n <- sum_n - 1
            } else if (event == "coming_of_age") {
                n_vec[pick_idx] <- n_vec[pick_idx] + 1
                sum_yn <- sum_yn + y_pick
                sum_n <- sum_n + 1
            }
            post_event_mean <- sum_yn / sum_n

            cr_component[cr_idx] <- event
            cr_time[cr_idx] <- time
            cr_delta[cr_idx] <- post_event_mean - post_ic_mean
            cr_idx <- cr_idx + 1L
        }

        # Write local vectors back and advance to next period.
        # age was not updated inside the loop (y was pre-computed instead), so add
        # the full gap here rather than just the remaining fraction.
        data[, `:=`(
            n = n_vec,
            age = age + gap, period = period + gap
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
    class(ret) <- c("social_change_decomp", "list")
    ret
}

#' Print a social_change_decomp object
#'
#' @param x A `social_change_decomp` object returned by [decompose_aggregated()].
#' @param detailed Logical; if `TRUE` (default) prints period-by-period overview before the summary.
#' @param ... Not used.
#' @return `x`, invisibly.
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
    total_change <- meanN - mean0
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
            total_change,
            intraindividual,
            pt,
            mortality,
            outmigration,
            coming_of_age,
            inmigration
        ),
        Percent = c(
            "", "", "100.0",
            sprintf("%.1f", round(100 * c(intraindividual, pt, mortality,
                outmigration, coming_of_age, inmigration) / total_change, 1))
        )
    )
    if (!x$migration) {
        decomp <- decomp[!grepl("migration", Component)]
    }

    print(decomp, row.names = FALSE, class = FALSE, justify = "left", na.print = "")
    if (!x$migration) {
        cat("Assumes no in- or out-migration.\n")
    }
    options(digits = 7, scipen = 0) # reset to default
    invisible(x)
}

#' @rdname decompose_aggregated
#' @param x A `social_change_decomp` object returned by [decompose_aggregated()].
#' @param ... Not used.
#' @import data.table
#' @import ggplot2
#' @export
plot.social_change_decomp <- function(x, ...) {
    summary <- data.table::copy(x$summary)

    means_long <- data.table::melt(summary,
        id.vars = "period",
        measure.vars = c("observed_mean", "modeled_mean"),
        variable.name = "type", value.name = "value"
    )
    means_long[, type := factor(
        type,
        c("observed_mean", "modeled_mean"),
        c("Observed", "Modeled")
    )]
    means_long[, panel := "Mean outcome"]

    components <- c("intraindividual", "mortality", "coming_of_age")
    if (x$migration) components <- c(components, "outmigration", "inmigration")
    comp_labels <- c(
        "intraindividual" = "Intraindividual change",
        "mortality"       = "Mortality",
        "coming_of_age"   = "Coming-of-age",
        "outmigration"    = "Out-migration",
        "inmigration"     = "In-migration"
    )

    decomp_long <- data.table::melt(summary,
        id.vars = "period",
        measure.vars = components,
        variable.name = "type", value.name = "value"
    )
    decomp_long[is.na(value), value := 0]
    decomp_long[, value := cumsum(value), by = "type"]
    decomp_long[, type := factor(type, components, comp_labels[components])]
    decomp_long[, panel := "Cumulative change"]

    combine <- rbindlist(list(
        means_long[, .(period, type, value, panel)],
        decomp_long[, .(period, type, value, panel)]
    ), use.names = TRUE)
    combine[, panel := factor(panel, c("Mean outcome", "Cumulative change"))]

    color_map <- c(
        "Observed" = "black",
        "Modeled" = "#888888",
        "Intraindividual change" = "#450C54",
        "Mortality" = "#22908C",
        "Coming-of-age" = "#FDE724",
        "Out-migration" = "#3B518B",
        "In-migration" = "#5DC963"
    )

    ggplot(combine, aes(x = period, y = value, color = type)) +
        facet_wrap("panel", nrow = 1, scales = "free_y") +
        geom_hline(yintercept = 0, color = "gray") +
        geom_line() +
        scale_color_manual(values = color_map) +
        labs(color = NULL, y = NULL, x = "Period") +
        theme_light() +
        theme(
            legend.position = "bottom",
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank()
        )
}
