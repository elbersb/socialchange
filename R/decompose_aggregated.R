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
#' @param tol Maximum tolerated absolute deviation between observed and modeled period means, in the
#'   outcome's own units (default 0.05). Checks that \code{fun_y} reproduces the observed period means;
#'   if the largest deviation exceeds \code{tol}, the function errors. The default suits outcomes on a
#'   roughly unit scale (e.g. proportions in [0, 1]); set \code{tol} to match outcomes on another scale.
#' @param weight Name of the weight column used when aggregating individual-level data (ignored if \code{n} is present).
#'   Weights are normalized within each period to sum to the period sample size before aggregation, so that cell
#'   counts \code{n} (rounded sums of normalized weights) reflect the relative population structure rather than
#'   raw sample sizes. This preserves simulation tractability but is an approximation: the ideal approach would
#'   use true population counts, which are generally unavailable from survey data alone.
#' @param population Optional data frame of true cell counts \code{n} per cell and period (columns \code{period},
#'   \code{age}, the \code{cells} identifiers, and \code{n}). When supplied, these counts replace the survey-derived
#'   cell counts as the population frame: they drive event derivation and weight the modeled mean, while \code{fun_y}
#'   continues to supply every cell's outcome and \code{stacked_data} is used only for the observed-mean / model-fit
#'   diagnostic. This is the preferred input when true population counts (e.g. from a census or official statistics)
#'   are available alongside survey data, as it sidesteps survey age-structure noise. \code{n} is rounded to whole
#'   counts for the microsimulation, so rescale large frames (e.g. raw population counts in the millions) to a
#'   tractable per-period total first -- only the relative cell structure matters. The frame must match the survey's
#'   minimum age and the level set of each \code{cells} column, and cover every survey period (extra periods are
#'   dropped); these are compared over rows with \code{n > 0}.
#'
#' @return S3 object of class \code{social_change_decomp} with components:
#'   \itemize{
#'     \item \code{summary}: data.table with decomposition components by period (including the
#'       \code{inmigration} and \code{outmigration} columns; print/plot show a migration component
#'       only for whichever of these is non-zero)
#'     \item \code{record}: list of detailed event records for each period transition
#'   }
#'
#' @details
#' The function estimates mortality, coming-of-age, and net in-migration from period-to-period
#' population differences within cells, then uses microsimulation to randomly order demographic
#' events and track their contribution to aggregate change. Unequal and multi-year gaps
#' between periods are supported: when the gap exceeds one year, each entering cohort is
#' assigned to the specific calendar year within the gap when it crosses the minimum age,
#' so that post-entry aging is correctly attributed to intraindividual change rather than
#' coming-of-age. All waves must share a common minimum age (the youngest age observed with a
#' non-zero count); this single threshold separates entering cohorts from survivors, and a
#' mismatch across periods is an error.
#'
#' By default the survey itself supplies both the cell counts and the outcomes. Supplying
#' \code{population} decouples these: the population frame supplies the cell counts \code{n}
#' (and hence the inferred demographic events), while \code{fun_y} supplies the outcomes. The
#' reported \code{modeled_mean} is then weighted by the population frame, whereas
#' \code{observed_mean} remains the survey's own observed mean, so the two lines may diverge
#' when the survey and population age structures differ.
#'
#' Within a cell, a survivor cohort that shrinks between periods loses people to mortality,
#' while one that grows gains people through net in-migration. Only \emph{net} migration is
#' recovered: gross out-migration is not separable from deaths (a survivor loss could be either),
#' so it is folded into mortality and the reported out-migration is always zero. Each cell's net
#' change is attributed to a single event type by sign: a shrinking cell records only mortality
#' (any concurrent in-migration is invisible) and a growing cell records only net in-migration
#' (any concurrent deaths are folded in), so offsetting flows within a cell cannot be seen. New
#' cohorts (below the minimum age) attribute all their growth to coming-of-age;
#' migration among entering cohorts is not modeled. On noisy survey cells this strategy relabels
#' sampling fluctuation as in-migration and mortality, so the inferred in-migration is most
#' meaningful when \code{population} supplies a true population frame, where growing cohorts reflect
#' genuine net immigration rather than survey noise.
#'
#' \strong{Limitation}: Does not properly handle within-cell state transitions. Transition
#' effects are absorbed into the intraindividual change component.
#'
#' @examples
#' \donttest{
#' library(data.table)
#' data("gss_homosex", package = "socialchange")
#' # restrict to age >= 21 so every wave shares a common minimum age
#' stacked <- as.data.table(gss_homosex)[age >= 21, .(age, period = year, y = homosex)]
#' model <- stats::lm(y ~ age + period, data = stacked)
#' result <- decompose_aggregated(stacked, function(d) predict(model, newdata = d), tol = 0.1)
#' print(result)
#' }
#'
#' @seealso [decompose_events()] for event-driven decomposition,
#'   [sim_social_change()] for forward simulation with fully specified demographic functions.
#'   Vignette: \code{vignette("decompose_aggregated", package = "socialchange")}.
#' @import data.table
#' @export
decompose_aggregated <- function(stacked_data, fun_y, cells = c(), tol = 0.05, weight = NULL, population = NULL) {
    checkmate::assert_data_frame(stacked_data)
    checkmate::assert_subset(c("age", "period", "y", cells), names(stacked_data))
    # NAs here would otherwise crash deep in the simulation or silently drop a wave.
    checkmate::assert_numeric(stacked_data$age, any.missing = FALSE, .var.name = "age")
    checkmate::assert_numeric(stacked_data$y, any.missing = FALSE, .var.name = "y")
    checkmate::assert_atomic_vector(stacked_data$period, any.missing = FALSE, .var.name = "period")
    checkmate::assert_function(fun_y, nargs = 1)
    checkmate::assert_character(cells, any.missing = FALSE, null.ok = TRUE)
    checkmate::assert_number(tol, lower = 0)
    checkmate::assert_string(weight, null.ok = TRUE)
    checkmate::assert_data_frame(population, null.ok = TRUE)
    # cells/weight name columns; reserved names collide with internal ones.
    reserved <- c("age", "period", "y", "n", "y_pred", "cell_id")
    if (length(intersect(cells, reserved)) > 0) {
        stop("`cells` must not name reserved columns: ", paste(intersect(cells, reserved), collapse = ", "), ".")
    }
    if (!is.null(weight) && weight %in% reserved) {
        stop("`weight` must not name a reserved column: ", weight, ".")
    }

    stacked_data <- copy(as.data.table(stacked_data))

    if (!"n" %in% names(stacked_data)) {
        stacked_data <- aggregate_to_cells(stacked_data, cells, weight)
    } else {
        # n must be whole: the microsimulation is integer-based, so fractional
        # counts would be silently truncated by runif()/indexing.
        checkmate::assert_integerish(stacked_data$n, lower = 0, any.missing = FALSE, .var.name = "n")
    }
    stacked_data[, y_pred := fun_y(.SD)]

    # The population frame supplies the cell counts n that drive event derivation
    # and weight the modeled mean. By default it is the survey itself. When a
    # `population` table is supplied it replaces the survey counts: the survey
    # then only supplies fun_y, while n comes from the external frame.
    if (is.null(population)) {
        frame <- stacked_data
    } else {
        population <- copy(as.data.table(population))
        checkmate::assert_subset(c("period", "age", "n", cells), names(population))
        checkmate::assert_numeric(population$n, lower = 0, any.missing = FALSE, .var.name = "population$n")
        checkmate::assert_numeric(population$age, any.missing = FALSE, .var.name = "population$age")
        checkmate::assert_atomic_vector(population$period, any.missing = FALSE, .var.name = "population$period")

        # Frame and survey must share structure
        survey_nz <- stacked_data[n > 0]
        pop_nz <- population[n > 0]

        if (min(survey_nz$age) != min(pop_nz$age)) {
            stop(sprintf(
                "Survey and population must share a minimum age, but it is %g for the survey and %g for the population.",
                min(survey_nz$age), min(pop_nz$age)
            ))
        }
        # Per-column level sets, not unique combinations (the survey may be sparser).
        for (col in cells) {
            survey_levels <- sort(unique(as.character(survey_nz[[col]])))
            pop_levels <- sort(unique(as.character(pop_nz[[col]])))
            if (!identical(survey_levels, pop_levels)) {
                stop(sprintf(
                    "Survey and population must share the same levels of cell column '%s'.\n  survey: %s\n  population: %s",
                    col, paste(survey_levels, collapse = ", "), paste(pop_levels, collapse = ", ")
                ))
            }
        }
        # The frame may cover extra periods; subset to the survey's, but require all.
        survey_periods <- unique(survey_nz$period)
        missing_periods <- setdiff(survey_periods, unique(pop_nz$period))
        if (length(missing_periods) > 0) {
            stop(sprintf(
                "Population frame is missing survey period(s): %s.",
                paste(sort(missing_periods), collapse = ", ")
            ))
        }
        population <- population[period %in% survey_periods]

        # counts must be whole numbers for the integer-based microsimulation
        population[, n := round(n)]
        population[, y_pred := fun_y(.SD)]
        frame <- population
    }

    periods <- sort(frame[, unique(period)])
    if (length(periods) < 2) {
        stop("decompose_aggregated() requires at least 2 distinct periods, but found ", length(periods), ".")
    }
    cells <- c(cells, "age")

    # Collapse to one row per cell per period. y_pred is constant within a
    # cell (fun_y depends only on the grouping columns)
    frame <- frame[, .(n = sum(n), y_pred = y_pred[1L]), by = c("period", cells)]
    frame <- frame[n > 0]

    # One minimum age, shared by every wave: the threshold splitting entering
    # cohorts (age < min_age) from survivors.
    min_ages <- frame[, .(min_age = min(age)), by = period]
    if (uniqueN(min_ages$min_age) > 1L) {
        setorder(min_ages, period)
        stop(sprintf(
            paste0(
                "All waves must share a common minimum age, but it varies across periods:\n%s\n",
                "Restrict each wave to a common minimum age before decomposing."
            ),
            paste(sprintf("  period %s: minimum age %g", min_ages$period, min_ages$min_age), collapse = "\n")
        ))
    }
    min_age <- min_ages$min_age[1L]

    record <- vector("list", length(periods))
    summary <- data.table(
        period = periods,
        observed_mean = NA_real_,
        modeled_mean = NA_real_,
        intraindividual = NA_real_,
        coming_of_age = NA_real_,
        mortality = NA_real_,
        outmigration = NA_real_,
        inmigration = NA_real_
    )
    # Model-fit diagnostic, always computed on the survey's own structure, so it stays
    # a meaningful check even when an external frame is used.
    survey_means <- stacked_data[, .(observed = stats::weighted.mean(y, n), modeled = stats::weighted.mean(y_pred, n)), by = .(period)]
    # Does fun_y reproduce each period mean to within `tol`, measured as
    # an absolute deviation in the outcome's own units?
    survey_means[, deviation := abs(observed - modeled)]
    max_dev <- survey_means[, max(deviation)]
    if (max_dev > tol) {
        print(survey_means)
        stop(sprintf(
            paste0(
                "Modeled means deviate from observed by up to %.3f (tol = %.3f), in the ",
                "outcome's own units, evaluated on the survey's own age structure. Consider ",
                "a more flexible model or increase tol."
            ),
            max_dev, tol
        ))
    }
    if (is.null(population)) {
        means <- survey_means
    } else {
        # observed_mean stays the survey's observed mean (a reference line);
        # modeled_mean is fun_y weighted by the population frame, which is what
        # the decomposition components below sum to.
        means <- merge(
            survey_means[, .(period, observed)],
            frame[, .(modeled = stats::weighted.mean(y_pred, n)), by = .(period)],
            by = "period", all = TRUE
        )
    }
    summary[means, `:=`(observed_mean = observed, modeled_mean = modeled), on = "period"]

    # decompose - main loop
    for (i_period in seq_len(length(periods) - 1)) {
        gap <- as.numeric(periods[i_period + 1] - periods[i_period])

        # Align the two waves into one per-cell table with start/end counts n1/n2.
        data <- align_periods(frame, periods, i_period, gap, cells, fun_y)

        # Derive the per-cell, four-type event table. Ages below min_age are
        # cohorts that entered during the gap; all others are survivors from the prior
        # period. Emits coming-of-age, mortality, and net in-migration; out-migration
        # is always 0 (see derive_events()).
        data <- derive_events(data, min_age)

        # Draw a random event schedule (the only stochastic step), then replay it
        # deterministically to get the period-to-period change record.
        sched <- schedule_events(data, min_age, gap)
        change_record <- simulate_schedule(data, fun_y, gap, sched)

        # shift time onto the global continuous index so transition i_period spans [i_period - 1, i_period].
        change_record[, time := time + (i_period - 1)]

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

    ret <- list(summary = summary, record = record)
    class(ret) <- c("social_change_decomp", "list")
    ret
}

# Aggregate individual-level rows to age x period x cells, producing the cell
# count n and cell mean y. With a weight column, weights are first normalized
# within each period to sum to the period sample size, so n (rounded weight
# sums) reflects relative population structure rather than raw sample sizes.
aggregate_to_cells <- function(stacked_data, cells, weight) {
    group_cols <- c("age", "period", cells)
    if (!is.null(weight)) {
        checkmate::assert_subset(weight, names(stacked_data))
        checkmate::assert_numeric(stacked_data[[weight]], any.missing = FALSE, .var.name = weight)
        setnames(stacked_data, weight, ".wt")
        stacked_data[, .wt := .wt / sum(.wt) * .N, by = period]
        stacked_data[, .(n = round(sum(.wt)), y = stats::weighted.mean(y, .wt)), by = group_cols]
    } else {
        stacked_data[, .(n = .N, y = mean(y)), by = group_cols]
    }
}

# Align the two waves of transition i_period into one per-cell table. data2 ages
# are shifted back by `gap` so survivors line up on the cell key; the outer merge
# pairs each cell's start count n1 with its end count n2 (0-filled where a cell is
# absent from one wave). Carries the period-1 age/period and the predicted outcome
# y, ready for derive_events() and the simulation loop.
align_periods <- function(frame, periods, i_period, gap, cells, fun_y) {
    # frame is already one row per cell per period, so a plain subset
    # (no re-aggregation) gives the period's cell counts.
    data1 <- frame[period == periods[i_period], c(cells, "n"), with = FALSE]
    data2 <- frame[period == periods[i_period + 1], c(cells, "n"), with = FALSE]
    data2[, age := age - gap]
    setnames(data1, "n", "n1")
    setnames(data2, "n", "n2")
    data <- merge(data1, data2, all = TRUE, by = cells)
    data[, n1 := nafill(n1, fill = 0)]
    data[, n2 := nafill(n2, fill = 0)]
    data[, n := n1]
    data[, period := periods[i_period]]
    data[, y := fun_y(data)]
    data
}

# Add the four per-cell event-count columns to a period-aligned cell table
# (columns age, n1, n2). New cohorts (age < min_age) enter via coming-of-age;
# survivor cells reconcile n1 -> n2 via mortality (shrink) or net in-migration
# (grow). Out-migration is folded into mortality and stays 0 (see @details).
derive_events <- function(data, min_age) {
    is_new <- data$age < min_age
    data[, coming_of_age := ifelse(is_new, pmax(0, n2 - n1), 0)]
    data[, mortality := ifelse(is_new, 0, pmax(0, n1 - n2))]
    data[, inmigration := ifelse(is_new, 0, pmax(0, n2 - n1))]
    data[, outmigration := 0]
    data
}

# Expand the per-cell event-count table into a flat, time-sorted event list.
# Returns parallel vectors (ev_type, ev_cell, events_tick), each of length n_ev,
# assigning every demographic event a (type, cell, time-in-[0,1]) triple.
#
# Coming-of-age timing:
#   data2 ages are shifted back by `gap`, so a row with aligned age `a` represents
#   people who were `a` years old at period 1 and will be `a + gap` at period 2.
#   For coming-of-age cells (a < min_age), these people crossed the minimum age
#   threshold during the gap.  A person with aligned age `a` reaches min_age
#   exactly (min_age - a) years into the gap.  Because birth dates are spread
#   across a calendar year, entries are uniform over the year ending at that point:
#
#     lo = (min_age - a - 1) / gap     hi = (min_age - a) / gap
#
#   Example — gap = 2, min_age = 20:
#     aligned age 19 (age 21 in period 2): lo = 0,   hi = 0.5  (enter in year 1)
#     aligned age 18 (age 20 in period 2): lo = 0.5, hi = 1.0  (enter in year 2)
#
# Mortality and migration are uniformly distributed across the full gap.
schedule_events <- function(data, min_age, gap) {
    n_ev <- data[, sum(coming_of_age) + sum(mortality) + sum(inmigration) + sum(outmigration)]
    n_c <- nrow(data)
    ev_type <- character(n_ev)
    ev_cell <- integer(n_ev)
    ev_time <- numeric(n_ev)
    ev_idx <- 1L
    coa_vec <- data$coming_of_age
    # Uniform-over-gap event types, in fixed order. outmigration is always 0, so
    # its guarded branch never runs.
    unif_vecs <- list(
        mortality = data$mortality,
        inmigration = data$inmigration,
        outmigration = data$outmigration
    )
    for (cidx in seq_len(n_c)) {
        n_coa <- coa_vec[cidx]
        if (n_coa > 0L) {
            a <- data$age[cidx]
            lo <- max(0, (min_age - a - 1) / gap)
            hi <- (min_age - a) / gap
            end_idx <- ev_idx + n_coa - 1L
            ev_type[ev_idx:end_idx] <- "coming_of_age"
            ev_cell[ev_idx:end_idx] <- cidx
            ev_time[ev_idx:end_idx] <- stats::runif(n_coa, lo, hi)
            ev_idx <- end_idx + 1L
        }
        for (et in names(unif_vecs)) {
            n_et <- unif_vecs[[et]][cidx]
            if (n_et > 0L) {
                end_idx <- ev_idx + n_et - 1L
                ev_type[ev_idx:end_idx] <- et
                ev_cell[ev_idx:end_idx] <- cidx
                ev_time[ev_idx:end_idx] <- stats::runif(n_et)
                ev_idx <- end_idx + 1L
            }
        }
    }
    ord <- order(ev_time)
    list(events_tick = ev_time[ord], ev_type = ev_type[ord], ev_cell = ev_cell[ord])
}

# Microsimulate one period transition by replaying a fixed event schedule from
# schedule_events(). Deterministic: all randomness lives in the schedule, so the
# same (data, fun_y, gap, sched) always yields the same record. Fires the events
# in time order, attributing each step of the weighted-mean change to a component
# (intraindividual drift between events, then the event itself). Returns the change
# record: one row per delta, with local event times in [0, 1]. Does not mutate
# `data`; the caller discards it and keeps the return value.
simulate_schedule <- function(data, fun_y, gap, sched) {
    events_tick <- sched$events_tick
    ev_type <- sched$ev_type
    ev_cell <- sched$ev_cell

    n_ev <- length(ev_type)
    n_cells <- nrow(data)
    n_records <- 2L * n_ev + 1L

    # Plain vectors for the change record; assembled into a data.table after the loop
    cr_component <- character(n_records)
    cr_time <- numeric(n_records)
    cr_delta <- numeric(n_records)
    cr_idx <- 1L

    # n_vec tracks the current population; updated as events fire
    n_vec <- data$n

    # Evaluate fun_y once on a stacked copy of data at every event time plus the
    # end of the period (tick = 1), yielding an n_cells × (n_ev + 1) matrix whose
    # final column is the end-of-period outcome. Only age and period are updated per
    # slice; all other covariates (e.g. gender, smoking) are carried through from
    # data as-is. This requires fun_y to not depend on n, which holds for any
    # externally-fitted statistical model.
    eval_ticks <- c(events_tick, 1)
    n_eval <- n_ev + 1L
    idx_rep <- rep.int(seq_len(n_cells), n_eval)
    data_stack <- data[idx_rep]
    set(
        data_stack, NULL, "age",
        rep.int(data$age, n_eval) + rep(eval_ticks * gap, each = n_cells)
    )
    set(
        data_stack, NULL, "period",
        rep.int(data$period, n_eval) + rep(eval_ticks * gap, each = n_cells)
    )
    y_mat <- matrix(fun_y(data_stack), nrow = n_cells, ncol = n_eval)

    # Running weighted-sum scalars. The demographic-event update is O(1) (one cell
    # changes by one person), but the intraindividual step re-sums y_cur * n_vec each
    # iteration because every cell's y drifts with aging, so the loop is O(n_ev * n_cells).
    sum_n <- sum(n_vec)
    sum_yn <- sum(data$y * n_vec)
    post_event_mean <- sum_yn / sum_n

    for (i_ev in seq_len(n_ev)) {
        event_tick <- events_tick[i_ev]

        pre_mean <- post_event_mean # reuses last iteration's post_event_mean
        y_cur <- y_mat[, i_ev]
        sum_yn <- sum(y_cur * n_vec)
        post_ic_mean <- sum_yn / sum_n

        cr_component[cr_idx] <- "intraindividual"
        cr_time[cr_idx] <- event_tick
        cr_delta[cr_idx] <- post_ic_mean - pre_mean
        cr_idx <- cr_idx + 1L

        event <- ev_type[i_ev]
        pick_idx <- ev_cell[i_ev]
        y_pick <- y_cur[pick_idx]

        # Exits (mortality, out-migration) remove an individual from the cell;
        # entries (coming-of-age, in-migration) add one at the cell's current y.
        if (event == "mortality" || event == "outmigration") {
            n_vec[pick_idx] <- n_vec[pick_idx] - 1
            sum_yn <- sum_yn - y_pick
            sum_n <- sum_n - 1
        } else if (event == "coming_of_age" || event == "inmigration") {
            n_vec[pick_idx] <- n_vec[pick_idx] + 1
            sum_yn <- sum_yn + y_pick
            sum_n <- sum_n + 1
        }
        post_event_mean <- sum_yn / sum_n

        cr_component[cr_idx] <- event
        cr_time[cr_idx] <- event_tick
        cr_delta[cr_idx] <- post_event_mean - post_ic_mean
        cr_idx <- cr_idx + 1L
    }

    # End-of-period outcome is the final (tick = 1) column of y_mat.
    y_end <- y_mat[, n_eval]
    sum_yn <- sum(y_end * n_vec)
    post_ic_mean <- sum_yn / sum_n

    cr_component[cr_idx] <- "intraindividual"
    cr_time[cr_idx] <- 1
    cr_delta[cr_idx] <- post_ic_mean - post_event_mean

    data.table(
        i = seq_len(n_records),
        component = cr_component,
        time = cr_time,
        delta = cr_delta
    )
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
    old <- options(digits = 4, scipen = 999)
    on.exit(options(old))
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
            "", "", if (total_change == 0) "" else "100.0",
            # Percentages are undefined when total change is zero (components may
            # still net to zero from offsetting flows), so leave them blank.
            if (total_change == 0) {
                rep("", 6)
            } else {
                sprintf("%.1f", round(100 * c(
                    intraindividual, pt, mortality,
                    outmigration, coming_of_age, inmigration
                ) / total_change, 1))
            }
        )
    )
    # Show a migration row only for whichever migration type was actually inferred.
    # In-migration is derived as a residual from cell growth; out-migration is always
    # zero under the current strategy, so its row is dropped here.
    if (inmigration == 0) decomp <- decomp[Component != "  - In-migration"]
    if (outmigration == 0) decomp <- decomp[Component != "  - Out-migration"]

    print(decomp, row.names = FALSE, class = FALSE, justify = "left", na.print = "")
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
    # Add a migration component only for whichever type was actually inferred.
    if (summary[, sum(outmigration, na.rm = TRUE)] != 0) components <- c(components, "outmigration")
    if (summary[, sum(inmigration, na.rm = TRUE)] != 0) components <- c(components, "inmigration")
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
