#' Simulate social change with demographic processes
#'
#' Forward simulation of social change dynamics, decomposing aggregate change into
#' components driven by intraindividual change and population turnover (mortality,
#' coming-of-age, migration, and state transitions).
#'
#' @param periods Integer number of time periods to simulate
#' @param data Initial population data.table with columns \code{age}, \code{n}, and optional covariates
#' @param fun_y Outcome function taking \code{(data, time)} and returning outcome values
#' @param fun_coming_of_age Optional function taking \code{(data, period)} returning new entrants data.table
#' @param fun_mortality Optional function taking \code{(data, period)} returning mortality counts per cell
#' @param fun_inmigration Optional function taking \code{(data, period)} returning immigrants data.table
#' @param fun_outmigration Optional function taking \code{(data, period)} returning out-migration counts per cell
#' @param fun_transitions Optional function taking \code{(data, period)} returning state transitions data.table
#'
#' @return S3 object of class \code{social_change_sim} with components:
#'   \itemize{
#'     \item \code{summary}: data.table summarizing change components by period
#'     \item \code{snapshot}: list of population snapshots at each period
#'     \item \code{record}: list of event-by-event change records
#'   }
#'
#' @examples
#' \donttest{
#' library(data.table)
#' data <- data.table(age = 20:39, n = rep(100, 20))
#' fun_y <- function(data, time) data[, age / 40]
#' fun_mortality <- function(data, period) data[, ifelse(age >= 30, 10, 0)]
#' fun_coming_of_age <- function(data, period) data.table(age = 20, n = 100)
#'
#' result <- sim_social_change(
#'     periods = 5,
#'     data = data,
#'     fun_y = fun_y,
#'     fun_mortality = fun_mortality,
#'     fun_coming_of_age = fun_coming_of_age
#' )
#' print(result)
#' }
#'
#' @seealso [decompose_aggregated()] for decomposing change in stacked cross-sectional data,
#'   [decompose_events()] for event-driven decomposition.
#'   Vignette: \code{vignette("simulate", package = "socialchange")}.
#' @import data.table
#' @export
sim_social_change <- function(periods, data, fun_y,
                              fun_coming_of_age = NULL, fun_mortality = NULL,
                              fun_inmigration = NULL, fun_outmigration = NULL, fun_transitions = NULL) {
    checkmate::assert_data_frame(data)
    checkmate::assert_subset(c("age"), names(data))
    checkmate::assert_function(fun_y, nargs = 2)
    checkmate::assert_function(fun_coming_of_age, nargs = 2, null.ok = TRUE)
    checkmate::assert_function(fun_mortality, nargs = 2, null.ok = TRUE)
    checkmate::assert_function(fun_inmigration, nargs = 2, null.ok = TRUE)
    checkmate::assert_function(fun_outmigration, nargs = 2, null.ok = TRUE)
    checkmate::assert_function(fun_transitions, nargs = 2, null.ok = TRUE)

    data <- data[n > 0]
    data[, age := as.double(age)]
    data[, y := fun_y(data, 0)]
    cols <- setdiff(names(data), "n")
    data <- data[, .(n = sum(n)), by = cols]
    data[, cell_id := seq_len(.N)]

    record <- vector("list", periods)
    snapshot <- vector("list", periods + 1)
    snap <- data[, -"cell_id"]
    snap[, period := 0]
    snapshot[[1]] <- snap
    summary <- data.table(
        period = 0:periods,
        mean = 0,
        N = 0,
        intraindividual = NA_real_,
        coming_of_age = NA_real_,
        mortality = NA_real_,
        inmigration = NA_real_,
        outmigration = NA_real_
    )
    summary[1, N := sum(data$n)]
    summary[1, mean := sum(data$y * data$n) / sum(data$n)]

    for (i_period in 1:periods) {
        if (is.null(fun_mortality)) {
            data[, n_mortality := 0]
        } else {
            data[, n_mortality := fun_mortality(data, i_period)]
        }
        if (is.null(fun_outmigration)) {
            data[, n_outmigration := 0]
        } else {
            data[, n_outmigration := fun_outmigration(data, i_period)]
        }
        if (is.null(fun_coming_of_age)) {
            coming_of_age <- data.table(n = 0)
        } else {
            coming_of_age <- fun_coming_of_age(data, i_period)
        }
        if (is.null(fun_inmigration)) {
            inmigration <- data.table(n = 0)
        } else {
            inmigration <- fun_inmigration(data, i_period)
        }
        if (is.null(fun_transitions)) {
            transitions <- data.table(n = 0)
        } else {
            transitions <- fun_transitions(data, i_period)
            transitions[, cell_id := seq_len(.N)]
            # TODO: check whether format is correct
        }

        event_counts <- c(
            "coming_of_age" = coming_of_age[, sum(n)],
            "mortality" = data[, sum(n_mortality)],
            "inmigration" = inmigration[, sum(n)],
            "outmigration" = data[, sum(n_outmigration)],
            "transitions" = transitions[, sum(n)]
        )

        n_events <- sum(event_counts)
        n_records <- 2L * n_events + 1L
        cr_component <- character(n_records)
        cr_time <- numeric(n_records)
        cr_delta <- numeric(n_records)
        change_record_index <- 1L

        events_tick <- sort(stats::runif(n_events))
        tick <- 0

        # Running weighted-sum scalars; updated O(1) after each demographic event
        sum_n <- sum(data$n)
        sum_yn <- sum(data$y * data$n)
        post_event_mean <- sum_yn / sum_n

        next_cell_id <- data[, max(cell_id)] + 1L

        for (event_tick in events_tick) {
            delta <- event_tick - tick
            tick <- event_tick
            time <- i_period - 1 + tick

            pre_mean <- post_event_mean # reuses last iteration's post_event_mean
            set(data, NULL, "age", data$age + delta)
            set(data, NULL, "y", fun_y(data, time))
            sum_yn <- sum(data$y * data$n) # recompute after all y values change
            post_ic_mean <- sum_yn / sum_n

            cr_component[change_record_index] <- "intraindividual"
            cr_time[change_record_index] <- time
            cr_delta[change_record_index] <- post_ic_mean - pre_mean
            change_record_index <- change_record_index + 1L

            # process single event
            event <- sample(names(event_counts), 1, prob = event_counts)

            if (event == "mortality") {
                pick_idx <- sample.int(nrow(data), 1L, prob = data$n_mortality)
                y_pick <- data$y[pick_idx]
                set(data, pick_idx, "n", data$n[pick_idx] - 1)
                set(data, pick_idx, "n_mortality", data$n_mortality[pick_idx] - 1)
                sum_yn <- sum_yn - y_pick
                sum_n <- sum_n - 1
            } else if (event == "outmigration") {
                pick_idx <- sample.int(nrow(data), 1L, prob = data$n_outmigration)
                y_pick <- data$y[pick_idx]
                set(data, pick_idx, "n", data$n[pick_idx] - 1)
                set(data, pick_idx, "n_outmigration", data$n_outmigration[pick_idx] - 1)
                sum_yn <- sum_yn - y_pick
                sum_n <- sum_n - 1
            } else if (event == "transitions") {
                # pick row from transitions table
                pick_transition_id <- transitions[sample.int(nrow(transitions), 1L, prob = transitions$n), ][["cell_id"]]
                transitions[cell_id == pick_transition_id, n := n - 1]
                pick_transition <- transitions[cell_id == pick_transition_id][, -c("n", "cell_id")]
                # union necessary so that result doesn't have reordered columns
                pick <- merge(data[n > 0], pick_transition)[sample(.N, 1, prob = n)][, union(names(data), names(pick_transition)), with = FALSE]
                y_source <- pick[["y"]]
                data[cell_id == pick[["cell_id"]], n := n - 1]
                to_cols <- names(pick)[grepl("^to_", names(pick))]
                for (col in to_cols) {
                    stripped_col <- sub("^to_", "", col)
                    pick[[stripped_col]] <- pick[[col]]
                    pick[, (col) := NULL]
                }
                pick[, `:=`(
                    n = 1,
                    cell_id = next_cell_id,
                    n_mortality = 0,
                    n_outmigration = 0,
                    y = fun_y(pick, i_period - 1 + tick)
                )]
                next_cell_id <- next_cell_id + 1L
                y_dest <- pick[["y"]]
                data <- rbindlist(list(data, pick))
                sum_yn <- sum_yn - y_source + y_dest
                # sum_n unchanged: one person leaves a cell, one enters a new cell
            } else {
                if (event == "coming_of_age") {
                    pick_id <- sample.int(nrow(coming_of_age), 1L, prob = coming_of_age$n)
                    set(coming_of_age, pick_id, "n", coming_of_age$n[pick_id] - 1)
                    pick <- coming_of_age[pick_id, -"n"]
                } else if (event == "inmigration") {
                    pick_id <- sample.int(nrow(inmigration), 1L, prob = inmigration$n)
                    set(inmigration, pick_id, "n", inmigration$n[pick_id] - 1)
                    pick <- inmigration[pick_id, -"n"]
                }
                # TODO: check that pick has the correct columns, including covariates

                pick[, `:=`(
                    # age needs to be adjusted such that this individual is <age> old at end of period
                    age = pick[["age"]] - (1 - tick),
                    y = 0, # temp placeholder
                    n = 1,
                    cell_id = next_cell_id,
                    n_mortality = 0,
                    n_outmigration = 0
                )]
                next_cell_id <- next_cell_id + 1L
                pick[, y := fun_y(pick, i_period - 1 + tick)]
                new_y <- pick[["y"]]
                data <- rbindlist(list(data, pick))
                sum_yn <- sum_yn + new_y
                sum_n <- sum_n + 1
            }
            event_counts[event] <- event_counts[event] - 1
            post_event_mean <- sum_yn / sum_n
            # if transitions introduce a change in mean, that is subsumed under "intraindividual" change
            recorded_event <- if (event == "transitions") "intraindividual" else event
            cr_component[change_record_index] <- recorded_event
            cr_time[change_record_index] <- time
            cr_delta[change_record_index] <- post_event_mean - post_ic_mean
            change_record_index <- change_record_index + 1L
        }
        # bring up to next period
        data[, age := age + (1 - tick)]
        data[, y := fun_y(data, i_period)]
        sum_yn <- sum(data$y * data$n)
        post_ic_mean <- sum_yn / sum_n
        cr_component[change_record_index] <- "intraindividual"
        cr_time[change_record_index] <- i_period
        cr_delta[change_record_index] <- post_ic_mean - post_event_mean

        change_record <- data.table(component = cr_component, time = cr_time, delta = cr_delta)

        # summarize period change
        by_component <- change_record[, .(delta = sum(delta)), by = .(component)]
        record[[i_period]] <- change_record
        summary[i_period + 1, mean := sum_yn / sum_n]
        summary[i_period + 1, N := sum_n]
        for (comp in c("intraindividual", "mortality", "outmigration", "coming_of_age", "inmigration")) {
            delta <- by_component[component == comp, "delta"][[1]]
            if (length(delta) == 1) {
                summary[i_period + 1, (comp) := delta]
            } else {
                summary[i_period + 1, (comp) := 0]
            }
        }

        # clean up
        data <- data[n > 0, -c("cell_id", "n_mortality", "n_outmigration")]
        cols <- setdiff(names(data), "n")
        data <- data[, .(n = sum(n)), by = cols]
        data[, cell_id := seq_len(.N)]

        # store data snapshot
        snap <- data[, -"cell_id"]
        snap[, period := i_period]
        snapshot[[i_period + 1]] <- snap
    }

    ret <- list(summary = summary, snapshot = snapshot, record = record)
    class(ret) <- c("list", "social_change_sim")
    ret
}

#' @import data.table
#' @export
print.social_change_sim <- function(x, detailed = TRUE, ...) {
    old <- options(digits = 4, scipen = 999)
    on.exit(options(old))
    if (detailed) {
        cat("Overview by period:\n")
        print(x$summary, row.names = FALSE, class = FALSE, na.print = "")
    }
    intraindividual <- x$summary[period > 0, round(sum(intraindividual), 6)]
    mortality <- x$summary[period > 0, round(sum(mortality), 6)]
    outmigration <- x$summary[period > 0, round(sum(outmigration), 6)]
    coming_of_age <- x$summary[period > 0, round(sum(coming_of_age), 6)]
    inmigration <- x$summary[period > 0, round(sum(inmigration), 6)]
    pt <- mortality + outmigration + coming_of_age + inmigration

    mean0 <- round(x$summary[1][["mean"]], 6)
    meanN <- round(x$summary[.N][["mean"]], 6)

    if (detailed) {
        cat("\nDecomposition of total change:\n")
    }
    decomp <- data.table(
        Component = c(
            "At initial",
            "At end",
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

    print(decomp, row.names = FALSE, class = FALSE, justify = "left")
    invisible(x)
}
