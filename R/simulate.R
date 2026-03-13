#' @import data.table
#' @export
sim_social_change <- function(periods, data, fun_y, fun_mortality, fun_outmigration, fun_coming_of_age, fun_inmigration) {
    data <- copy(data)
    data[, age := as.double(age)]
    data[, y := fun_y(data, 0)]
    data[, cell_id := seq_len(.N)]
    period <- 0

    record <- vector("list", periods)
    summary <- data.table(
        period = c("initial", 1:periods),
        mean = 0,
        N = 0,
        intraindividual = NA_real_,
        mortality = NA_real_,
        outmigration = NA_real_,
        coming_of_age = NA_real_,
        inmigration = NA_real_
    )
    summary[1, N := data[, sum(n)]]
    summary[1, mean := data[, sum(n / sum(n) * y)]]

    for (i_period in 1:periods) {
        data[, n_mortality := fun_mortality(data, i_period)]
        data[, n_outmigration := fun_outmigration(data, i_period)]
        coming_of_age <- fun_coming_of_age(data, i_period)
        inmigration <- fun_inmigration(data, i_period)

        event_counts <- c(
            "mortality" = data[, sum(n_mortality)],
            "outmigration" = data[, sum(n_outmigration)],
            "coming_of_age" = coming_of_age[, sum(n)],
            "inmigration" = inmigration[, sum(n)]
        )

        change_record <- vector("list", 2 * sum(event_counts) + 1)
        events_tick <- sort(runif(sum(event_counts)))
        tick <- 0
        for (event_tick in events_tick) {
            data[, age := age + event_tick - tick]
            tick <- event_tick

            # process y updates
            pre_mean <- data[, sum(n / sum(n) * y)]
            data[, y := fun_y(data, i_period - 1 + tick)]
            post_ic_mean <- data[, sum(n / sum(n) * y)]
            change_record[[length(change_record) + 1]] <- list("intraindividual", post_ic_mean - pre_mean)

            # process single event
            event <- sample(names(event_counts), 1, prob = event_counts)

            if (event == "mortality") {
                data[
                    sample(.N, 1, prob = n_mortality),
                    `:=`(n = n - 1, n_mortality = n_mortality - 1)
                ]
            } else if (event == "outmigration") {
                data[
                    sample(.N, 1, prob = n_outmigration),
                    `:=`(n = n - 1, n_outmigration = n_outmigration - 1)
                ]
            } else {
                if (event == "coming_of_age") {
                    pick_id <- coming_of_age[, sample(.N, 1, prob = n)]
                    coming_of_age[pick_id, n := n - 1]
                    pick <- coming_of_age[pick_id, -"n"]
                } else if (event == "inmigration") {
                    pick_id <- inmigration[, sample(.N, 1, prob = n)]
                    inmigration[pick_id, n := n - 1]
                    pick <- inmigration[pick_id, -"n"]
                }
                # TODO: check that pick has the correct columns, including covariates

                new_cell_id <- data[, max(cell_id) + 1]
                # columns need to be in order of <data>
                pick[, `:=`(
                    # age needs to be adjusted such that this individual is <age> old at end of period
                    age = pick[["age"]] - (1 - tick),
                    n = 1,
                    y = 0, # temp placeholder
                    cell_id = new_cell_id,
                    n_mortality = 0,
                    n_outmigration = 0
                )]
                pick[, y := fun_y(pick, i_period - 1 + tick)]
                data <- rbindlist(list(data, pick))
            }
            event_counts[event] <- event_counts[event] - 1
            post_event_mean <- data[, sum(n / sum(n) * y)]
            change_record[[length(change_record) + 1]] <- list(event, post_event_mean - post_ic_mean)
        }
        # bring up to next period
        data[, age := age + 1 - tick]
        data[, y := fun_y(data, year)]
        post_ic_mean <- data[, sum(n / sum(n) * y)]
        change_record[[length(change_record) + 1]] <- list("intraindividual", post_ic_mean - post_event_mean)

        # clean up
        data <- data[n > 0]
        data[, cell_id := seq_len(.N)]

        # summarize period change
        change_record_dt <- rbindlist(change_record)
        names(change_record_dt) <- c("component", "delta")
        by_component <- change_record_dt[, .(delta = sum(delta)), by = .(component)]
        record[[i_period]] <- change_record_dt
        summary[i_period + 1, mean := data[, sum(n / sum(n) * y)]]
        summary[i_period + 1, N := data[, sum(n)]]
        for (comp in c("intraindividual", "mortality", "outmigration", "coming_of_age", "inmigration")) {
            delta <- by_component[component == comp, "delta"][[1]]
            if (length(delta) == 1) {
                summary[i_period + 1, (comp) := delta]
            } else {
                summary[i_period + 1, (comp) := 0]
            }
        }
    }

    ret <- list(summary = summary, record = record)
    class(ret) <- c("list", "social_change_sim")
    ret
}

#' @import data.table
#' @export
print.social_change_sim <- function(x, ...) {
    options(digits = 4, scipen = 999)
    print(x$summary, row.names = FALSE, class = FALSE, na.print = "")
    intraindividual <- x$summary[period != "initial", round(sum(intraindividual), 6)]
    mortality <- x$summary[period != "initial", round(sum(mortality), 6)]
    outmigration <- x$summary[period != "initial", round(sum(outmigration), 6)]
    coming_of_age <- x$summary[period != "initial", round(sum(coming_of_age), 6)]
    inmigration <- x$summary[period != "initial", round(sum(inmigration), 6)]
    pt <- mortality + outmigration + coming_of_age + inmigration

    mean0 <- round(x$summary[1][["mean"]], 6)
    meanN <- round(x$summary[.N][["mean"]], 6)

    cat("\nDecomposition of total change:\n")
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
    options(digits = 7, scipen = 0) # reset to default
}
