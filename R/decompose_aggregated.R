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
    means <- stacked_data[, .(observed = weighted.mean(y, n), modeled = weighted.mean(y_pred, n)), by = .(period)]
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
        data[, coming_of_age := ifelse(age == min_age, n2 - n1, 0)]
        data[, mortality := ifelse(age == min_age, 0, n1 - n2)]

        event_counts <- c(
            "coming_of_age" = data[, sum(coming_of_age)],
            "mortality" = data[, sum(mortality)]
        )

        change_record <- data.table(
            i = 1:(2 * sum(event_counts) + 1),
            component = NA_character_,
            time = NA_real_,
            delta = NA_real_
        )
        change_record_index <- 1

        events_tick <- sort(runif(sum(event_counts)))
        tick <- 0
        event_tick <- events_tick[[1]]
        for (event_tick in events_tick) {
            data[, age := age + event_tick - tick]
            data[, period := period + event_tick - tick]
            tick <- event_tick
            time <- i_period - 1 + tick

            # process y updates
            pre_mean <- data[, weighted.mean(y, n)]
            data[, y := fun_y(data)]
            post_ic_mean <- data[, weighted.mean(y, n)]

            change_record[change_record_index, `:=`(component = "intraindividual", time = time, delta = post_ic_mean - pre_mean)]
            change_record_index <- change_record_index + 1

            # process single event
            event <- sample(names(event_counts), 1, prob = event_counts)

            if (event == "mortality") {
                pick_id <- data[, sample(.N, 1, prob = mortality)]
                data[cell_id == pick_id, `:=`(
                    mortality = mortality - 1,
                    n = n - 1
                )]
            } else if (event == "coming_of_age") {
                pick_id <- data[, sample(.N, 1, prob = coming_of_age)]
                data[cell_id == pick_id, `:=`(
                    coming_of_age = coming_of_age - 1,
                    n = n + 1
                )]
            }
            event_counts[event] <- event_counts[event] - 1
            post_event_mean <- data[, weighted.mean(y, n)]
            change_record[change_record_index, `:=`(component = event, time = time, delta = post_event_mean - post_ic_mean)]
            change_record_index <- change_record_index + 1
        }
        # bring up to next period
        data[, age := age + 1 - tick]
        data[, period := period + 1 - tick]
        data[, y := fun_y(data)]
        post_ic_mean <- data[, weighted.mean(y, n)]
        change_record[change_record_index, `:=`(component = "intraindividual", time = i_period, delta = post_ic_mean - post_event_mean)]

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
