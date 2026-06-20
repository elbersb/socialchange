#' Decompose social change from aggregated data
#'
#' Decomposes aggregate-level change into intraindividual change and population turnover
#' components using microsimulation on stacked cross-sectional data. Requires a fitted model
#' that predicts the outcome as a function of age, period, and covariates.
#'
#' @param stacked_data Data frame with columns \code{age}, \code{period}, and \code{y}, plus optional cell identifiers.
#'   If a column \code{n} is present the data is treated as already aggregated to cells; otherwise individual-level
#'   rows are aggregated internally using \code{weight}.
#' @param model A fitted model object (\code{lm}, \code{glm}, or \code{gam}) predicting the outcome
#'   from \code{age}, \code{period}, and any \code{cells}. Predictions are taken on the response scale
#'   via \code{predict()}.
#' @param cells Character vector of additional cell identifier columns beyond age (e.g., "gender", "smoking")
#' @param R Number of Dirichlet-bootstrap replicates used to attach standard errors capturing
#'   \code{model} uncertainty (default 0, point estimate only). When \code{R > 0}, \code{model} is
#'   refit \code{R} times on Dirichlet-reweighted copies of its training data; the spread of the
#'   resulting decompositions (under common random numbers, so event-ordering noise is differenced
#'   out) gives per-component standard errors and cumulative confidence bands. This captures model
#'   uncertainty only -- the dominant source -- not demographic uncertainty in the cell counts. For
#'   \code{gam} models each replicate is a full refit plus prediction, so large \code{R} can be slow.
#' @param seed Optional integer seed for reproducible bootstrap replicates (default \code{NULL}).
#'   The Dirichlet draw is always isolated from the global RNG stream, so passing \code{R > 0} never
#'   changes the point estimate relative to \code{R = 0} under the same outer seed.
#' @param tol Maximum tolerated absolute deviation between observed and modeled period means, in the
#'   outcome's own units (default 0.05). Checks that \code{model} reproduces the observed period means;
#'   if the largest deviation exceeds \code{tol}, the function errors. The default suits outcomes on a
#'   roughly unit scale (e.g. proportions in [0, 1]); set \code{tol} to match outcomes on another scale.
#' @param weight Name of the weight column used when aggregating individual-level data (ignored if \code{n} is present).
#'   Weights are normalized within each period to sum to the period sample size before aggregation, so that cell
#'   counts \code{n} (rounded sums of normalized weights) reflect the relative population structure rather than
#'   raw sample sizes. This preserves simulation tractability but is an approximation: the ideal approach would
#'   use true population counts, which are generally unavailable from survey data alone.
#' @param population Optional data frame of true cell counts \code{n} per cell and period (columns \code{period},
#'   \code{age}, the \code{cells} identifiers, and \code{n}). When supplied, these counts replace the survey-derived
#'   cell counts as the population frame: they drive event derivation and weight the modeled mean, while \code{model}
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
#'       only for whichever of these is non-zero).
#'     \item \code{record}: list of per-transition change tables (one per period transition). Each
#'       table is tidy, with columns \code{component}, the cell covariates (\code{age} and any
#'       \code{cells}), and \code{delta} -- one row per component per cell, holding that cell's total
#'       contribution to the change for that component over the transition. Summed over cells it
#'       reproduces the per-component totals in \code{summary}.
#'     \item \code{draws}: when \code{R > 0}, a long data.table of per-(draw, period, cell)
#'       component deltas (columns \code{draw}, \code{period}, \code{component}, \code{delta}, and
#'       the cell covariates) from which any aggregate's confidence band can be computed; \code{NULL}
#'       when \code{R = 0}.
#'   }
#'
#' @details
#' The function estimates mortality, coming-of-age, and net in-migration from period-to-period
#' population differences within cells, then uses microsimulation to randomly order demographic
#' events -- placed at evenly spaced times within each inter-period gap -- and track their
#' contribution to aggregate change. Unequal and multi-year gaps
#' between periods are supported: when the gap exceeds one year, each entering cohort is
#' assigned to the specific calendar year within the gap when it crosses the minimum age,
#' so that post-entry aging is correctly attributed to intraindividual change rather than
#' coming-of-age. All waves must share a common minimum age (the youngest age observed with a
#' non-zero count); this single threshold separates entering cohorts from survivors, and a
#' mismatch across periods is an error.
#'
#' By default the survey itself supplies both the cell counts and the outcomes. Supplying
#' \code{population} decouples these: the population frame supplies the cell counts \code{n}
#' (and hence the inferred demographic events), while \code{model} supplies the outcomes. The
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
#' result <- decompose_aggregated(stacked, model, tol = 0.1)
#' print(result)
#' }
#'
#' @seealso [decompose_events()] for event-driven decomposition,
#'   [sim_social_change()] for forward simulation with fully specified demographic functions.
#'   Vignette: \code{vignette("decompose_aggregated", package = "socialchange")}.
#' @import data.table
#' @export
decompose_aggregated <- function(stacked_data, model, cells = c(), R = 0,
                                 tol = 0.05, weight = NULL, population = NULL, seed = NULL) {
    checkmate::assert_data_frame(stacked_data)
    checkmate::assert_int(R, lower = 0)
    checkmate::assert_int(seed, null.ok = TRUE)
    checkmate::assert_subset(c("age", "period", "y", cells), names(stacked_data))
    # NAs here would otherwise crash deep in the simulation or silently drop a wave.
    checkmate::assert_numeric(stacked_data$age, any.missing = FALSE, .var.name = "age")
    checkmate::assert_numeric(stacked_data$y, any.missing = FALSE, .var.name = "y")
    checkmate::assert_atomic_vector(stacked_data$period, any.missing = FALSE, .var.name = "period")
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
    stacked_data[, y_pred := predict_y(model, .SD)]

    # The population frame supplies the cell counts n that drive event derivation
    # and weight the modeled mean. By default it is the survey itself. When a
    # `population` table is supplied it replaces the survey counts: the survey
    # then only supplies the model, while n comes from the external frame.
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
        population[, y_pred := predict_y(model, .SD)]
        frame <- population
    }

    periods <- sort(frame[, unique(period)])
    if (length(periods) < 2) {
        stop("decompose_aggregated() requires at least 2 distinct periods, but found ", length(periods), ".")
    }
    cells <- c(cells, "age")

    # Collapse to one row per cell per period. y_pred is constant within a
    # cell (the model depends only on the grouping columns)
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

    # Bootstrap replicates for model-uncertainty SEs.
    if (R > 0L) message("Computing ", R, " bootstrap replicate(s); this can take a while for gam models.")
    reps <- if (R > 0L) y_replicates(model, R, seed) else NULL
    draws_record <- if (R > 0L) vector("list", length(periods) - 1L) else NULL

    record <- vector("list", length(periods) - 1)
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
    # Does the model reproduce each period mean to within `tol`, measured as
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
        # modeled_mean is the model weighted by the population frame, which is what
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
        # coming-of-age banding (coa_band = min_age - age) is exact only for whole-number
        # gaps; a fractional gap would silently drop entrants.
        checkmate::assert_integerish(gap, lower = 1, .var.name = "period gap")

        # Align the two waves into one per-cell table with start/end counts n1/n2.
        data <- align_periods(frame, periods, i_period, gap, cells, model)

        # Derive the per-cell, four-type event table. Ages below min_age are
        # cohorts that entered during the gap; all others are survivors from the prior
        # period. Emits coming-of-age, mortality, and net in-migration; out-migration
        # is always 0 (see derive_events()).
        data <- derive_events(data, min_age)

        # The schedule is the only stochastic step; replicates replay the same one
        # (common random numbers), so cross-draw spread is model uncertainty alone.
        sched <- schedule_events(data, min_age, gap)
        sim <- simulate_schedule(data, model, reps, gap, sched)

        change_record <- tag_cells(sim$point, data, cells)
        if (!is.null(sim$draws)) {
            draws_record[[i_period]] <- tag_cells(sim$draws, data, cells)
        }

        # summarize period change
        by_component <- change_record[, .(delta = sum(delta)), by = .(component)]
        record[[i_period]] <- change_record
        for (comp in names(component_labels)) {
            delta <- by_component[component == comp, "delta"][[1]]
            if (length(delta) == 1) {
                summary[i_period + 1, (comp) := delta]
            } else {
                summary[i_period + 1, (comp) := 0]
            }
        }
    } # i_period loop

    draws_long <- if (!is.null(draws_record)) build_draws_long(draws_record, periods) else NULL

    ret <- list(summary = summary, record = record, draws = draws_long, cells = cells)
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
align_periods <- function(frame, periods, i_period, gap, cells, model) {
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
    data[, y := predict_y(model, data)]
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

# Expand the per-cell event-count table into a flat, time-ordered event list.
# Returns parallel vectors (events_tick, ev_type, ev_cell), each of length K (the
# total number of events), giving every demographic event a (time, type, cell) triple.
#
# The gap splits into `gap` crossing-year bands of width 1/gap. A coming-of-age cell
# (aligned age a < min_age) crosses min_age exactly (min_age - a) years in, so its
# entrants belong to band b = min_age - a; the band's interval ((b-1)/gap, b/gap] is
# their entry window. Mortality and in-migration span the whole gap, so they are split
# as evenly as possible across the bands (which event lands in which band is random).
#
#   Example — gap = 2, min_age = 20:
#     aligned age 19 (age 21 in period 2): band 1, window (0,   0.5]  (crosses in year 1)
#     aligned age 18 (age 20 in period 2): band 2, window (0.5, 1.0]  (crosses in year 2)
#
# Within a band the events sit on a fixed even grid -- unique slots at the midpoints of
# n equal subintervals -- while their *order* (which event lands in which slot) is drawn
# at random. This keeps the random interleaving of births/deaths/migration (the
# demographically meaningful part) and drops the Monte-Carlo noise in the exact sub-gap
# timing. Banding (rather than one global grid) lets entrants concentrated in a single
# year fill that year's slots without colliding with the rest of the gap. outmigration is
# always 0, so it never enters the pool.
schedule_events <- function(data, min_age, gap) {
    coa <- data$coming_of_age
    n_c <- nrow(data)
    nb <- max(1L, as.integer(round(gap))) # one band per crossing-year of the gap

    # Whole-gap events (out-migration is always 0), each a (type, cell) pair, assigned to
    # bands in round-robin over a random permutation: even counts per band, random membership.
    free_type <- c(rep.int("mortality", sum(data$mortality)),
                   rep.int("inmigration", sum(data$inmigration)))
    free_cell <- c(rep.int(seq_len(n_c), data$mortality),
                   rep.int(seq_len(n_c), data$inmigration))
    M <- length(free_type)
    free_band <- integer(M)
    if (M > 0L) free_band[sample.int(M)] <- ((seq_len(M) - 1L) %% nb) + 1L

    n_ev <- sum(coa) + M
    if (n_ev == 0L) {
        return(list(events_tick = numeric(0), ev_type = character(0), ev_cell = integer(0)))
    }

    coa_band <- min_age - data$age # band index for coming-of-age cells (1..nb)
    ev_time <- numeric(n_ev)
    ev_type <- character(n_ev)
    ev_cell <- integer(n_ev)
    pos <- 1L
    for (b in seq_len(nb)) {
        coa_cells <- which(coa > 0L & coa_band == b)
        fidx <- which(free_band == b)
        b_type <- c(rep.int("coming_of_age", sum(coa[coa_cells])), free_type[fidx])
        b_cell <- c(rep.int(coa_cells, coa[coa_cells]), free_cell[fidx])
        nbk <- length(b_type)
        if (nbk == 0L) next

        # Even unique slots inside the band interval ((b-1)/nb, b/nb], events in random order.
        slots <- ((b - 1L) + (seq_len(nbk) - 0.5) / nbk) / nb
        o <- if (nbk > 1L) sample.int(nbk) else 1L
        rng <- pos:(pos + nbk - 1L)
        ev_time[rng] <- slots
        ev_type[rng] <- b_type[o]
        ev_cell[rng] <- b_cell[o]
        pos <- pos + nbk
    }
    # Bands are emitted in order and slots ascend within each, so ev_time is already sorted.
    list(events_tick = ev_time, ev_type = ev_type, ev_cell = ev_cell)
}

# One prediction slice per evaluation tick: stack `data`, advancing only age and period
# by tick * gap (other covariates carried through). n_cells * length(eval_ticks) rows.
build_event_stack <- function(data, gap, eval_ticks) {
    n_cells <- nrow(data)
    n_eval <- length(eval_ticks)
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
    data_stack
}

# Deterministic replay of a fixed schedule against precomputed outcome surfaces, vectorized
# across R outcome draws (R = 1 for the point estimate, R = #replicates for the bootstrap). No
# model call, no RNG, so the same (schedule, surface) always yields the same record. The
# demographic trajectory (n_vec, sum_n) is fixed by the schedule alone, so it is shared across
# draws; only the y-dependent quantities carry the draw axis as a matrix column -- one R-level
# pass replaces the per-replicate loop. Fires events in time order, attributing the
# weighted-mean change to a component: intraindividual drift between events, then the event.
# Inputs:
#   n_vec   - per-cell period-start counts (data$n)
#   y_start - n_cells x R per-cell tick-0 outcomes (one column per draw)
#   surface - (n_cells * n_eval) x R per-cell outcomes stacked by eval tick then draw; tick
#             slice i is rows ((i-1)*n_cells + 1):(i*n_cells), with n_eval = n_ev + 1 and the
#             final tick (1) last.
# Returns one row per (draw, component, cell) the component touched: every cell for intra-
# individual change, the firing cells for turnover.
replay_schedule <- function(n_vec, y_start, surface, sched) {
    ev_type <- sched$ev_type
    ev_cell <- sched$ev_cell

    n_ev <- length(ev_type)
    n_cells <- length(n_vec)
    R <- ncol(surface)

    # Per-event turnover deltas (n_ev x R) and per-cell intraindividual deltas (n_cells x R),
    # accumulated then aggregated to (draw, component, cell). cr_cell records which cell each
    # event fired in (a row index into `data`); the caller uses it to attach covariates. The
    # per-cell IC delta at each tick is sum_c (y_cur[c] - y_prev[c]) * n_vec[c] / sum_n, an
    # exact split across cells (its column sum is the scalar IC delta).
    cr_component <- character(n_ev)
    cr_cell <- integer(n_ev)
    cr_delta <- matrix(0, n_ev, R)
    ic_by_cell <- matrix(0, n_cells, R)

    # Running weighted sums. n_vec / sum_n are shared scalars (the schedule fixes the demography
    # across draws); sum_yn is a length-R vector. The demographic-event update is O(1) (one cell
    # changes by one person), but the intraindividual step re-sums y_cur * n_vec each iteration
    # because every cell's y drifts with aging, so the loop is O(n_ev * n_cells * R).
    sum_n <- sum(n_vec)
    y_prev <- y_start
    sum_yn <- colSums(y_prev * n_vec)

    for (i_ev in seq_len(n_ev)) {
        # Intraindividual drift since the previous tick, accumulated per cell (n_vec recycles
        # down the matrix columns). Its column sum is the scalar IC delta per draw.
        rows <- ((i_ev - 1L) * n_cells + 1L):(i_ev * n_cells)
        y_cur <- surface[rows, , drop = FALSE]
        ic_by_cell <- ic_by_cell + (y_cur - y_prev) * n_vec / sum_n
        y_prev <- y_cur
        sum_yn <- colSums(y_cur * n_vec)
        post_ic_mean <- sum_yn / sum_n

        event <- ev_type[i_ev]
        pick_idx <- ev_cell[i_ev]
        y_pick <- y_cur[pick_idx, ] # length-R draw vector

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

        cr_component[i_ev] <- event
        cr_cell[i_ev] <- pick_idx
        cr_delta[i_ev, ] <- sum_yn / sum_n - post_ic_mean
    }

    # Final intraindividual drift to the end of the period (tick = 1, the last surface slice).
    rows <- (n_ev * n_cells + 1L):((n_ev + 1L) * n_cells)
    y_cur <- surface[rows, , drop = FALSE]
    ic_by_cell <- ic_by_cell + (y_cur - y_prev) * n_vec / sum_n

    # Combine the per-cell intraindividual deltas with the per-event turnover deltas and
    # aggregate to one row per (draw, component, cell). as.vector() unrolls each matrix
    # column-major (cell/event fastest, draw outer), matching draw = rep(seq_len(R), each = .).
    ev_dt <- data.table(
        draw = rep(seq_len(R), each = n_ev),
        component = rep(cr_component, R),
        cell = rep(cr_cell, R),
        delta = as.vector(cr_delta)
    )
    ic_dt <- data.table(
        draw = rep(seq_len(R), each = n_cells),
        component = "intraindividual",
        cell = rep(seq_len(n_cells), R),
        delta = as.vector(ic_by_cell)
    )
    out <- rbind(ev_dt, ic_dt)[, .(delta = sum(delta)), by = .(draw, component, cell)]
    setorder(out, draw, component, cell)
    out[]
}

# Attach each cell's covariates by its `cell` row-index into `data`, then drop the index.
tag_cells <- function(dt, data, cells) {
    for (col in cells) set(dt, j = col, value = data[[col]][dt$cell])
    dt[, cell := NULL][]
}

# Build the outcome surface once, replay the shared schedule for the point estimate and
# every replicate. Returns list(point, draws) (draws NULL when reps is NULL, i.e. R = 0).
#
# align_periods() set data$y = predict_y(model, data) at tick 0, and nothing since has
# touched age/period/y (derive_events only adds event-count columns the model ignores), so
# data$y is an *exact* cache of the point model's tick-0 surface -- the point path reuses it
# rather than re-predicting. Replicates have no such cache, so they predict tick 0 (y_start).
# The replicate prediction (replicate_predict) is reached only when reps is non-NULL, so the
# R = 0 path makes exactly one model call (the point surface) and allocates no draw machinery.
simulate_schedule <- function(data, model, reps, gap, sched) {
    eval_ticks <- c(sched$events_tick, 1)
    stack <- build_event_stack(data, gap, eval_ticks)

    # The point estimate is the R = 1 case: tick-0 outcomes from data$y, the surface from the
    # model's own predict(). Its single draw column is dropped after replay.
    point <- replay_schedule(
        data$n, matrix(data$y, ncol = 1L),
        matrix(predict_y(model, stack), ncol = 1L), sched
    )
    point[, draw := NULL]

    draws <- NULL
    if (!is.null(reps)) {
        surface <- replicate_predict(reps, stack) # (n_cells * n_eval) x R
        y_start <- replicate_predict(reps, data) # n_cells x R, replicate tick-0 outcomes
        draws <- replay_schedule(data$n, y_start, surface, sched)
    }
    list(point = point, draws = draws)
}

# Stack the per-transition draw records into one long per-(draw, period, cell) delta table
# (draws_record[[i]] is the transition into period[i + 1]). All bootstrap summaries -- the
# cumulative CI band in print/plot -- are derived from this table on demand by
# cumulative_series(); nothing is precomputed onto `summary`.
build_draws_long <- function(draws_record, periods) {
    draws_long <- rbindlist(draws_record, idcol = "i")
    draws_long[, period := periods[i + 1L]]
    draws_long[, i := NULL]
    setcolorder(draws_long, c("draw", "period", "component", "delta"))
    draws_long[]
}

#' Print a social_change_decomp object
#'
#' @param x A `social_change_decomp` object returned by [decompose_aggregated()].
#' @param detailed Logical; if `TRUE` (default) prints period-by-period overview before the summary.
#' @param covariate Optional name of a single cell covariate (one of the `cells`, or `"age"`) by
#'   which to additionally break down the components. When supplied, one column per covariate level
#'   is appended to the decomposition table. For more elaborate breakdowns, aggregate
#'   `x$record` directly, which carries the cell covariates.
#' @param ... Not used.
#' @return `x`, invisibly.
#' @import data.table
#' @export
print.social_change_decomp <- function(x, detailed = TRUE, covariate = NULL, ...) {
    checkmate::assert_string(covariate, null.ok = TRUE)
    old <- options(digits = 4, scipen = 999)
    on.exit(options(old))
    if (detailed) {
        cat("Overview by period:\n")
        print(x$summary[], row.names = FALSE, class = FALSE, na.print = "")
    }

    ov <- cumulative_series(x)[period == max(period)]
    val <- function(ty) {
        v <- ov[type == ty, value]
        round(if (length(v)) v else 0, 6)
    }
    mean0 <- round(x$summary[1][["modeled_mean"]], 6)
    meanN <- round(x$summary[.N][["modeled_mean"]], 6)
    total_change <- meanN - mean0

    # `type` keys each row to its cumulative_series() component (NA for header rows); all
    # value/percent/CI/per-level columns join on it, never row position. Dropped before print.
    decomp <- data.table(
        Component = c(
            "At initial (modeled)", "At end (modeled)", "Total change",
            "- Intraindividual change", "- Population turnover",
            "  - Mortality", "  - Out-migration", "  - Coming-of-age", "  - In-migration"
        ),
        type = c(
            NA, NA, "Total change", "Intraindividual change", "Population turnover",
            "Mortality", "Out-migration", "Coming-of-age", "In-migration"
        )
    )
    component <- !is.na(decomp$type) & decomp$type != "Total change"
    decomp[, Value := fcase(
        Component == "At initial (modeled)", mean0,
        Component == "At end (modeled)", meanN,
        Component == "Total change", total_change,
        default = vapply(type, val, numeric(1), USE.NAMES = FALSE)
    )]
    # Percentages are undefined when total change is zero, so leave them blank.
    pct <- if (total_change == 0) rep("", nrow(decomp)) else sprintf("%.1f", round(100 * decomp$Value / total_change, 1))
    decomp[, Percent := fifelse(
        component, pct,
        fifelse(Component == "Total change" & total_change != 0, "100.0", "")
    )]

    if (detailed) cat("\nDecomposition of total change:\n")

    # One character column per covariate level, holding that level's cumulative value.
    if (!is.null(covariate)) {
        lv <- cumulative_series(x, covariate)[period == max(period)]
        fmtcol <- function(v) {
            out <- format(round(v, 6), trim = TRUE, scientific = FALSE)
            out[is.na(v)] <- ""
            out
        }
        for (L in sort(unique(lv$level))) {
            decomp[, (L) := fmtcol(lv[level == L][decomp, on = "type", x.value])]
        }
    }

    if (!is.null(x$draws)) {
        ci_col <- function(band) {
            band[decomp, on = "type"][, fifelse(is.na(lci), "", sprintf("[%.4f, %.4f]", lci, uci))]
        }
        if (is.null(covariate)) {
            decomp[, `95% CI` := ci_col(ov)]
        } else {
            for (L in sort(unique(lv$level))) {
                decomp[, (paste(L, "95% CI")) := ci_col(lv[level == L])]
            }
        }
    }

    # Show a migration row only for whichever migration type was actually inferred.
    # In-migration is a residual from cell growth; out-migration is always zero here.
    if (val("In-migration") == 0) decomp <- decomp[Component != "  - In-migration"]
    if (val("Out-migration") == 0) decomp <- decomp[Component != "  - Out-migration"]

    decomp[, type := NULL]
    print(decomp[], row.names = FALSE, class = FALSE, justify = "left", na.print = "")
    invisible(x)
}

#' @rdname decompose_aggregated
#' @param x A `social_change_decomp` object returned by [decompose_aggregated()].
#' @param covariate Optional name of a single cell covariate (one of the `cells`, or `"age"`) by
#'   which to split the cumulative change lines. For more elaborate breakdowns, aggregate `x$record` directly.
#' @param ... Not used.
#' @import data.table
#' @import ggplot2
#' @export
plot.social_change_decomp <- function(x, covariate = NULL, ...) {
    checkmate::assert_string(covariate, null.ok = TRUE)
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
    # `level` (the covariate value) is the line-type aesthetic; it is NA for the
    # means, which then draw as a single solid line.
    means_part <- means_long[, .(period,
        type = as.character(type),
        level = NA_character_, value, panel = "Mean outcome"
    )]

    # Drop the aggregate pseudo-types cumulative_series() carries; keep only leaves. Always
    # draw IC/mortality/coming-of-age, plus a migration series only where it actually moves.
    series <- cumulative_series(x, covariate)
    leaves <- unname(component_labels)
    base <- c("Intraindividual change", "Mortality", "Coming-of-age")
    series[, keep := type %in% leaves & (type %in% base | any(value != 0)), by = .(type, level)]
    decomp_part <- series[keep == TRUE, .(period, type, level,
        value,
        panel = "Cumulative change"
    )]

    # CI ribbon for exactly the drawn lines (NA-band series drop out via the lci filter).
    ribbon_part <- NULL
    if (!is.null(x$draws)) {
        drawn <- unique(decomp_part[, .(type, level)])
        ribbon_part <- series[drawn, on = .(type, level), nomatch = 0L][
            !is.na(lci), .(period, type, level, lci, uci, panel = "Cumulative change")
        ]
    }

    # Component colours are fixed across both paths; with a covariate, levels are
    # instead distinguished by line type (see scale_linetype_manual below).
    color_map <- c(
        "Observed" = "black",
        "Modeled" = "#888888",
        "Intraindividual change" = "#450C54",
        "Mortality" = "#22908C",
        "Coming-of-age" = "#FDE724",
        "Out-migration" = "#3B518B",
        "In-migration" = "#5DC963"
    )

    combine <- rbindlist(list(means_part, decomp_part), use.names = TRUE)
    combine[, type := factor(type, names(color_map))]
    combine[, panel := factor(panel, c("Mean outcome", "Cumulative change"))]
    if (!is.null(ribbon_part)) {
        ribbon_part[, type := factor(type, names(color_map))]
        ribbon_part[, panel := factor(panel, c("Mean outcome", "Cumulative change"))]
    }

    if (is.null(covariate)) {
        p <- ggplot(combine, aes(x = period, y = value, color = type))
    } else {
        p <- ggplot(combine, aes(x = period, y = value, color = type, linetype = level))
    }
    # Ribbon first so the component lines draw on top of their bands.
    if (!is.null(ribbon_part)) {
        p <- p + geom_ribbon(
            data = ribbon_part,
            # paste() not interaction(): interaction(type, NA) is NA, merging all ribbons.
            aes(x = period, ymin = lci, ymax = uci, fill = type, group = paste(type, level)),
            inherit.aes = FALSE, alpha = 0.2
        ) +
            scale_fill_manual(values = color_map, guide = "none")
    }
    p <- p +
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

    if (!is.null(covariate)) {
        # Distinct line type per covariate level. The mean lines (level = NA) draw
        # solid via na.value and are kept out of the line-type legend
        lv <- sort(unique(stats::na.omit(combine$level)))
        lt_vals <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
        lt <- stats::setNames(rep(lt_vals, length.out = length(lv)), lv)
        p <- p + scale_linetype_manual(
            values = lt, breaks = lv, na.value = "solid", name = covariate
        )
    }
    p
}

# Maps the internal component names to display labels, in print/plot order.
component_labels <- c(
    intraindividual = "Intraindividual change",
    mortality = "Mortality", outmigration = "Out-migration",
    coming_of_age = "Coming-of-age", inmigration = "In-migration"
)

# Tag long component rows with display `type` (factor in print/plot order) and the
# optional covariate `level` (NA for the unsplit, one-series-per-component case).
# Shared by the point and draw delta builders below.
tag_components <- function(dt, covariate) {
    dt[, type := factor(component_labels[component], levels = component_labels)]
    dt[, level := if (is.null(covariate)) NA_character_ else as.character(get(covariate))][]
}

# Per-transition component deltas from x$record, long with columns period, type
# (display label, factor in print/plot order), level, and delta. record[[i]] holds
# the change into period[i + 1]. `covariate` (one of x$cells) splits each component
# by that cell covariate's levels; without it every row has level = NA, i.e. one
# series per component.
component_deltas <- function(x, covariate = NULL) {
    if (!is.null(covariate)) checkmate::assert_choice(covariate, x$cells)
    periods <- x$summary$period
    dt <- rbindlist(x$record, idcol = "i")
    dt[, period := periods[i + 1L]]
    dt <- tag_components(dt, covariate)
    dt[, .(delta = sum(delta)), by = .(type, level, period)]
}

# Per-(draw, period) component deltas, long. Draws analogue of component_deltas().
draw_component_deltas <- function(x, covariate = NULL) {
    if (!is.null(covariate)) checkmate::assert_choice(covariate, x$cells)
    dt <- tag_components(copy(x$draws), covariate)
    dt[, .(delta = sum(delta)), by = .(draw, type, level, period)]
}

# Worker for cumulative_series: append the "Population turnover"/"Total change" aggregate
# pseudo-types, complete every series across all `periods` (missing transition = 0), and
# return per-series running totals in `cum`. `extra` carries extra grouping keys ("draw"
# for the per-draw path, none for the point estimate).
#
# Aggregates must be summed across components BEFORE cumulating: neither cumulation nor the
# downstream quantiles are additive across components, so an aggregate band can't be summed
# from the per-component bands.
cumulate_with_aggregates <- function(dt, periods, extra = character()) {
    turnover_types <- setdiff(component_labels, "Intraindividual change")
    by_pl <- c(extra, "level", "period")
    agg <- function(types, label) {
        dt[type %in% types, .(type = label, delta = sum(delta)), by = by_pl]
    }
    leaf_cols <- c(extra, "type", "level", "period", "delta")
    long <- rbindlist(list(
        dt[, ..leaf_cols][, type := as.character(type)],
        agg(turnover_types, "Population turnover"),
        agg(component_labels, "Total change")
    ), use.names = TRUE)
    by_tl <- c(extra, "type", "level")
    grid <- unique(long[, ..by_tl])[, .(period = periods), by = by_tl]
    long <- long[grid, on = c(by_tl, "period")]
    long[is.na(delta), delta := 0]
    setorderv(long, c(by_tl, "period"))
    long[, cum := cumsum(delta), by = by_tl]
    long[]
}

# Cumulative change per (type, level, period): point estimate in `value`, bootstrap bounds
# in `lci`/`uci` (NA when x has no draws). `type` spans the leaves plus the aggregate
# pseudo-types. Single source for both print's final-period totals/CI and plot's
# lines/ribbons; point and CI paths cumulate identically, differing only in the quantile.
cumulative_series <- function(x, covariate = NULL, probs = c(0.025, 0.975)) {
    periods <- x$summary$period
    pt <- cumulate_with_aggregates(component_deltas(x, covariate), periods)
    out <- pt[, .(type, level, period, value = cum)]
    if (is.null(x$draws)) {
        out[, c("lci", "uci") := NA_real_]
        return(out[])
    }
    dd <- cumulate_with_aggregates(draw_component_deltas(x, covariate), periods, extra = "draw")
    band <- dd[, .(lci = stats::quantile(cum, probs[1]), uci = stats::quantile(cum, probs[2])),
        by = .(type, level, period)
    ]
    out[band, on = .(type, level, period)]
}
