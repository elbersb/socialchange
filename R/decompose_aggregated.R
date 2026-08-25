#' Decompose social change from aggregated data
#'
#' Decomposes aggregate-level change into intraindividual change and population turnover
#' components using microsimulation on stacked cross-sectional data. Requires a fitted model
#' that predicts the outcome as a function of age, period, and covariates.
#'
#' @param stacked_data Data frame with columns \code{age}, \code{period} (numeric), and \code{y}, plus optional cell identifiers.
#'   If a column \code{n} is present the data is treated as already aggregated to cells; otherwise individual-level
#'   rows are aggregated internally using \code{weight}.
#' @param model A fitted model object (\code{lm}, \code{glm}, or \code{gam}) predicting the outcome
#'   from \code{age}, \code{period}, and any \code{cells}; it must use no predictors beyond these.
#'   Predictions are taken on the response scale via \code{predict()}.
#' @param cells Character vector of additional cell identifier columns beyond age (e.g., "gender", "smoking")
#' @param R Number of paired (event-ordering, model-draw) replicates used to attach standard
#'   errors (default 0, point estimate only). When \code{R > 0}, each replicate draws its own
#'   random event ordering and pairs it with a Dirichlet-reweighted refit of \code{model}; the
#'   spread of the resulting decompositions gives per-component standard errors and cumulative
#'   confidence bands. The band is the \emph{combined} event-ordering and model uncertainty, not
#'   demographic uncertainty: the cell counts and any supplied \code{population} or
#'   \code{mortality} inputs are held fixed across replicates. For \code{gam} models
#'   each replicate is a full refit plus prediction, so large \code{R} can be slow.
#' @param seed Optional integer seed for reproducible bootstrap replicates (default \code{NULL}).
#'   The Dirichlet refit draw is isolated from the global RNG stream, so the replicate refits are
#'   reproducible via \code{seed} while the event orderings follow the outer \code{set.seed()}.
#'   Only \code{R = 0} reproduces the legacy single-ordering point estimate; with \code{R > 0} the
#'   point is the mean over the \code{R} orderings (see Details).
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
#'   tractable per-period total first -- only the relative cell structure matters. The frame must share the survey's
#'   minimum age, reach at least its maximum age, share the level set of each \code{cells} column, and cover every
#'   survey period (extra periods are dropped); these are compared over rows with \code{n > 0}. Population ages above
#'   the survey maximum remain separate for demographic calculations, but their outcome predictions and reported
#'   contributions are pooled into the survey's open maximum-age cell. Ages from the survey maximum through the
#'   frame's terminal age must be gap-free in every period. The final age must be common across periods and is
#'   treated as the terminal open age.
#' @param mortality Optional data frame of annual death probabilities; supplying it switches the
#'   attribution of survivor-cell change from sign attribution to residual migration (see Details).
#'   Columns \code{period}, \code{age}, \code{prob}, plus optionally any subset of the \code{cells}
#'   columns: probabilities are joined on the columns present and broadcast over the rest (e.g.
#'   rates by age and sex apply to all age x sex x education cells). \code{prob} is the annual
#'   death probability q(x) in [0, 1]; convert central death rates m(x) (e.g.
#'   \code{mortality_us$death_rate}) via \code{prob = 1 - exp(-death_rate)}. The table must cover
#'   every calendar year from the first survey period up to (but not including) the last and every
#'   age from the shared minimum through the terminal age in \code{population}, or in
#'   \code{stacked_data} when \code{population} is omitted, without gaps. The mortality table
#'   must have the same terminal age; both final rows are interpreted as the same open group. Works with or without
#'   \code{population}, but is most meaningful with it: on raw survey counts the expected deaths
#'   round to zero for most small cells and the migration residual mostly reflects sampling noise.
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
#'     \item \code{strategy}: how survivor-cell change was attributed -- \code{"sign attribution"}
#'       (default) or \code{"residual migration"} (when \code{mortality} is supplied).
#'   }
#'
#' @details
#' The function estimates mortality, coming-of-age, and net in-migration from period-to-period
#' population differences within cells, then uses microsimulation to randomly order demographic
#' events -- placed at evenly spaced times within each inter-period gap -- and track their
#' contribution to aggregate change. The ordering is itself an uncertainty source (it stands in
#' for the unobserved true event sequence): the point estimate is the mean decomposition over
#' \code{max(R, 1)} random orderings, and when \code{R > 0} each replicate carries its own
#' ordering (paired with its own model refit), so the reported band folds ordering and model
#' uncertainty together. Unequal and multi-year gaps
#' between periods are supported: when the gap exceeds one year, each entering cohort is
#' assigned to the specific calendar year within the gap when it crosses the minimum age,
#' so that post-entry aging is correctly attributed to intraindividual change rather than
#' coming-of-age. All waves must share a common minimum age (the youngest age observed with a
#' non-zero count); this single threshold separates entering cohorts from survivors, and a
#' mismatch across periods is an error.
#'
#' Survey waves must also share a common maximum outcome age T, interpreted as an open
#' cell T+. If raw maxima are ragged, create a genuine common open group first with
#' \code{age = pmin(age, T)}; a hard eligibility cutoff is not an open group and is unsupported.
#' A supplied population frame can extend above T. Its single-age constituents remain separate
#' for aging and mortality, while the outcome model receives \code{pmin(age, T)} and all reported
#' contributions at ages T and above appear at T. Between waves, all constituents reaching T are
#' matched to the period-2 T+ pool, so they do not age out of the observed range. The population
#' frame's final age is itself treated as an open demographic group and must be common across
#' periods. When mortality is supplied, its final age must represent the same open group.
#'
#' By default the survey itself supplies both the cell counts and the outcomes. Supplying
#' \code{population} decouples these: the population frame supplies the cell counts \code{n}
#' (and hence the inferred demographic events), while \code{model} supplies the outcomes. The
#' reported \code{modeled_mean} is then weighted by the population frame, whereas
#' \code{observed_mean} remains the survey's own observed mean, so the two lines may diverge
#' when the survey and population age structures differ.
#'
#' Within each survivor cell the balancing identity is \code{n2 = n1 - deaths + net migration}:
#' one equation, two unknowns. The supplied inputs determine the strategy recorded on the result:
#' \itemize{
#'   \item \strong{Sign attribution} (default; no mortality input): each survivor cell's net
#'     change is routed by sign. A shrinking cell records only mortality (any concurrent
#'     in-migration is invisible) and a growing cell records only net in-migration (any
#'     concurrent deaths are folded in), so offsetting flows within a cell cannot be seen and
#'     the reported out-migration is always zero. On noisy survey cells this relabels sampling
#'     fluctuation as in-migration and mortality, so the inferred in-migration is most
#'     meaningful when \code{population} supplies a true population frame, where growing cohorts
#'     reflect genuine net immigration rather than survey noise.
#'   \item \strong{Residual migration} (\code{mortality} supplied): deaths come from the supplied
#'     probabilities. A survivor cell aged \code{a} in year \code{t} compounds annual survival
#'     over the gap years, \code{qtilde = 1 - prod_j (1 - q(a + j, t + j))} for
#'     \code{j = 0..gap-1} (lookup ages clamped at the mortality table's maximum age), and books
#'     \code{deaths = round(n1 * qtilde)}; annual death events are assigned to the year implied
#'     by that survival path. When \code{population} extends above the survey maximum, the
#'     survey's open outcome group sums expected deaths over those separate constituent ages,
#'     each compounding its own path; only the terminal constituent uses the terminal open-group
#'     probability. Migration is then the
#'     \emph{signed} residual \code{n2 - (n1 - deaths)}, so out-migration appears and a cell can
#'     carry both deaths and in-migration. Approximations: deaths are computed on the
#'     period-start count \code{n1} (within-gap mortality of migrants and entrants is ignored),
#'     migrants take the receiving cell's mean outcome, and migration remains net per cell.
#'     Bootstrap replicates (\code{R > 0}) hold the mortality input fixed.
#' }
#' Under either strategy, new cohorts (below the minimum age) attribute all their growth to
#' coming-of-age; migration among entering cohorts is not modeled.
#'
#' \strong{Limitation}: Does not properly handle within-cell state transitions. Transition
#' effects are absorbed into the intraindividual change component.
#'
#' @examples
#' \donttest{
#' library(data.table)
#' data("gss_homosex", package = "socialchange")
#' # restrict to age >= 21 and top-code at 81 ("81+") so every wave shares a
#' # common minimum and maximum age
#' stacked <- as.data.table(gss_homosex)[
#'     age >= 21, .(age = pmin(age, 81), period = year, y = homosex)]
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
                                 tol = 0.05, weight = NULL, population = NULL,
                                 mortality = NULL, seed = NULL) {
    checkmate::assert_data_frame(stacked_data)
    checkmate::assert_data_frame(mortality, null.ok = TRUE)
    checkmate::assert_int(R, lower = 0)
    checkmate::assert_int(seed, null.ok = TRUE)
    checkmate::assert_subset(c("age", "period", "y", cells), names(stacked_data))
    # NAs here would otherwise crash deep in the simulation or silently drop a wave.
    checkmate::assert_numeric(stacked_data$age, any.missing = FALSE, .var.name = "age")
    checkmate::assert_numeric(stacked_data$y, any.missing = FALSE, .var.name = "y")
    checkmate::assert_numeric(stacked_data$period, any.missing = FALSE, .var.name = "period")
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
    # The frame collapse and align_periods() predict on tables carrying only the
    # grouping columns, so the model must not use predictors beyond them.
    model_vars <- all.vars(stats::delete.response(stats::terms(model)))
    unknown <- setdiff(model_vars, c("age", "period", cells))
    if (length(unknown) > 0) {
        stop(
            "`model` must only use `age`, `period`, and `cells` columns as predictors, ",
            "but also uses: ", paste(unknown, collapse = ", "),
            ". Add the column(s) to `cells` or refit the model without them."
        )
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

    # The survey defines the outcome age support. Its shared maximum is an open
    # outcome cell T+: population ages above T can inform demography, but their
    # model predictions are clamped to T.
    survey_nz <- stacked_data[n > 0]
    survey_min_ages <- survey_nz[, .(min_age = min(age)), by = period]
    if (uniqueN(survey_min_ages$min_age) > 1L) {
        setorder(survey_min_ages, period)
        stop(sprintf(
            paste0(
                "All survey waves must share a common minimum age, but it varies across periods:\n%s\n",
                "Restrict each wave to a common minimum age before decomposing."
            ),
            paste(sprintf(
                "  period %s: minimum age %g", survey_min_ages$period, survey_min_ages$min_age
            ), collapse = "\n")
        ))
    }
    survey_max_ages <- survey_nz[, .(max_age = max(age)), by = period]
    if (uniqueN(survey_max_ages$max_age) > 1L) {
        setorder(survey_max_ages, period)
        stop(sprintf(
            paste0(
                "All survey waves must share a common maximum age, but it varies across periods:\n%s\n",
                "Top-code each wave to a common maximum age before decomposing."
            ),
            paste(sprintf(
                "  period %s: maximum age %g", survey_max_ages$period, survey_max_ages$max_age
            ), collapse = "\n")
        ))
    }
    outcome_top_age <- survey_max_ages$max_age[1L]

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
        checkmate::assert_numeric(population$period, any.missing = FALSE, .var.name = "population$period")

        # Frame and survey must share structure. The population can extend above
        # the survey's open outcome age, but cannot end below it.
        pop_nz <- population[n > 0]

        if (min(survey_nz$age) != min(pop_nz$age)) {
            stop(sprintf(
                "Survey and population must share a minimum age, but it is %g for the survey and %g for the population.",
                min(survey_nz$age), min(pop_nz$age)
            ))
        }
        if (max(pop_nz$age) < outcome_top_age) {
            stop(sprintf(
                paste0(
                    "Population must reach at least the survey's open maximum age %g, ",
                    "but only reaches age %g."
                ),
                outcome_top_age, max(pop_nz$age)
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

        # Above the survey top-code, the population must provide a continuous
        # single-age sequence through one terminal open age in every period.
        pop_nz <- population[n > 0]
        need_pop_ages <- seq(outcome_top_age, max(pop_nz$age))
        required_ages <- CJ(period = survey_periods, age = need_pop_ages)
        missing_ages <- required_ages[
            !unique(pop_nz[, .(period, age)]), on = .(period, age)
        ]
        if (nrow(missing_ages) > 0L) {
            show <- missing_ages[seq_len(min(.N, 5L))]
            stop(sprintf(
                paste0(
                    "Population ages from the survey maximum through the terminal open age ",
                    "must be gap-free in every period; missing %d period-age combination(s), e.g.:\n%s"
                ),
                nrow(missing_ages),
                paste(sprintf("  period %g, age %g", show$period, show$age), collapse = "\n")
            ))
        }

        # Counts must be whole numbers for the integer-based microsimulation.
        # Predict on the survey outcome support: true ages above its top-code all
        # receive the model prediction for the open outcome cell.
        population[, n := round(n)]
        prediction_data <- copy(population)
        prediction_data[age > outcome_top_age, age := outcome_top_age]
        population[, y_pred := predict_y(model, prediction_data)]
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

    # The population frame's final age is its demographic open interval. It can
    # exceed the survey's open outcome age, but must be stable across periods.
    max_ages <- frame[, .(max_age = max(age)), by = period]
    if (uniqueN(max_ages$max_age) > 1L) {
        setorder(max_ages, period)
        stop(sprintf(
            paste0(
                "The population frame must share a common terminal age, but it varies across periods:\n%s\n",
                "Pool each period to one common terminal open age before decomposing."
            ),
            paste(sprintf("  period %s: maximum age %g", max_ages$period, max_ages$max_age), collapse = "\n")
        ))
    }
    demographic_top_age <- max_ages$max_age[1L]

    if (!is.null(mortality)) {
        mortality <- prepare_mortality(
            mortality, setdiff(cells, "age"), frame, periods, min_age,
            demographic_top_age
        )
    }
    strategy <- if (is.null(mortality)) "sign attribution" else "residual migration"

    # Model-fit diagnostic, always computed on the survey's own structure, so it stays
    # a meaningful check even when an external frame is used.
    survey_means <- stacked_data[, .(observed = stats::weighted.mean(y, n), modeled = stats::weighted.mean(y_pred, n)), by = .(period)]
    # Does the model reproduce each period mean to within `tol`, measured as
    # an absolute deviation in the outcome's own units?
    survey_means[, deviation := abs(observed - modeled)]
    max_dev <- survey_means[, max(deviation)]
    if (max_dev > tol) {
        worst <- survey_means[order(-deviation)][deviation > tol][seq_len(min(.N, 5L))]
        stop(sprintf(
            paste0(
                "Modeled means deviate from observed by up to %.3f (tol = %.3f), in the ",
                "outcome's own units, evaluated on the survey's own age structure. ",
                "Worst periods:\n%s\nConsider a more flexible model or increase tol."
            ),
            max_dev, tol,
            paste(sprintf(
                "  period %s: observed %.3f, modeled %.3f (deviation %.3f)",
                worst$period, worst$observed, worst$modeled, worst$deviation
            ), collapse = "\n")
        ))
    }

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
        data <- align_periods(frame, periods, i_period, gap, cells, model, outcome_top_age)

        # Derive the per-cell, four-type event table. Ages below min_age are
        # cohorts that entered during the gap; all others are survivors from the prior
        # period, reconciled to n2 by sign attribution or, with `mortality`, by
        # residual migration (see derive_events()).
        data <- derive_events(data, min_age, gap, mortality)

        # Event ordering is the stochastic step. Each replicate draws its own ordering
        # (paired with its own model refit), so cross-draw spread is combined
        # ordering + model uncertainty; the point estimate is the mean over orderings.
        sim <- simulate_schedule(data, model, reps, gap, min_age, cells, outcome_top_age)

        change_record <- tag_cells(sim$point, data, cells)
        if (!is.null(sim$draws)) {
            draws_record[[i_period]] <- tag_cells(sim$draws, data, cells)
        }

        # summarize period change
        by_component <- change_record[, .(delta = sum(delta)), by = .(component)]
        record[[i_period]] <- change_record
        comps <- names(component_labels)
        deltas <- by_component$delta[match(comps, by_component$component)]
        summary[i_period + 1, (comps) := as.list(nafill(deltas, fill = 0))]
    } # i_period loop

    draws_long <- if (!is.null(draws_record)) build_draws_long(draws_record, periods) else NULL

    ret <- list(
        summary = summary, record = record, draws = draws_long, cells = cells,
        strategy = strategy
    )
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
#
# The survey maximum age T is an open outcome interval "T+". A population frame
# can retain true ages above T; the period-2 pool combines all ages T and above,
# while period-1 constituents from T-gap upward map to it. Those rows collapse
# into one open-group cell labeled T (n1
# summed) whose n2 is the period-2 pool; below T-gap the map is one-to-one and the
# shift-back alignment applies. The open cell's tick-0 y is the n-weighted mean of
# its constituents' predictions -- not the prediction at T -- so the transition's
# start mean equals the period's modeled mean exactly (the accounting identity);
# the constituents' remaining climb to y(T) then lands in intraindividual change,
# which is what their within-gap aging is. The pre-collapse constituent rows are
# attached as attribute "open_constituents" (with `cell` = merged row index) so
# the bootstrap can apply the same weighting per replicate.
align_periods <- function(frame, periods, i_period, gap, cells, model, top_age) {
    # frame is already one row per cell per period, so a plain subset
    # (no re-aggregation) gives the period's cell counts.
    data1 <- frame[period == periods[i_period], c(cells, "n", "y_pred"), with = FALSE]
    data2 <- frame[period == periods[i_period + 1], c(cells, "n"), with = FALSE]
    open <- copy(data1[age >= top_age - gap])
    data1[age >= top_age - gap, age := top_age]
    data1 <- data1[, .(n1 = sum(n), y = stats::weighted.mean(y_pred, n)), by = cells]
    data2[age >= top_age, age := top_age]
    data2[age < top_age, age := age - gap] # all population ages at T or above pool at T
    data2 <- data2[, .(n = sum(n)), by = cells]
    setnames(data2, "n", "n2")
    data <- merge(data1, data2, all = TRUE, by = cells)
    data[, n1 := nafill(n1, fill = 0)]
    data[, n2 := nafill(n2, fill = 0)]
    data[, n := n1]
    data[, period := periods[i_period]]
    # cells absent from period 1 (entering cohorts) have no constituent
    # prediction; their tick-0 y is never n-weighted (n = 0) but must be finite
    idx <- which(is.na(data$y))
    if (length(idx) > 0L) set(data, idx, "y", predict_y(model, data[idx]))
    open[, period := periods[i_period]]
    open_key <- copy(open)[, age := top_age]
    open[, cell := data[open_key, on = cells, which = TRUE]]
    setattr(data, "open_constituents", open[, c(cells, "period", "n", "cell"), with = FALSE])
    data
}

# Validate the `mortality` input against the data and return the prepared lookup
# table: columns period, age, any cell columns (coerced to character), prob;
# keyed for the joins in gap_death_prob(). `cells` here excludes "age".
prepare_mortality <- function(mortality, cells, frame, periods, min_age, top_age) {
    mortality <- as.data.table(mortality)
    checkmate::assert_subset(c("period", "age", "prob"), names(mortality))
    mort_cells <- setdiff(names(mortality), c("period", "age", "prob"))
    stray <- setdiff(mort_cells, cells)
    if (length(stray) > 0) {
        stop("`mortality` has column(s) not in `cells`: ", paste(stray, collapse = ", "), ".")
    }
    checkmate::assert_numeric(mortality$prob, lower = 0, upper = 1, any.missing = FALSE, .var.name = "mortality$prob")
    checkmate::assert_integerish(mortality$age, any.missing = FALSE, .var.name = "mortality$age")
    checkmate::assert_integerish(mortality$period, any.missing = FALSE, .var.name = "mortality$period")

    mortality <- mortality[, c("period", "age", mort_cells, "prob"), with = FALSE]
    # character cell columns on both join sides (the frame's may be factor or character)
    for (col in mort_cells) set(mortality, NULL, col, as.character(mortality[[col]]))

    key_cols <- c("period", "age", mort_cells)
    if (anyDuplicated(mortality, by = key_cols) > 0) {
        stop("`mortality` has duplicate (", paste(key_cols, collapse = ", "), ") rows.")
    }

    # Broadcast direction: every level observed in the data must have a rate; the
    # mortality frame may carry extra levels.
    for (col in mort_cells) {
        missing_levels <- setdiff(unique(as.character(frame[[col]])), unique(mortality[[col]]))
        if (length(missing_levels) > 0) {
            stop(sprintf(
                "`mortality` is missing level(s) of cell column '%s': %s.",
                col, paste(sort(missing_levels), collapse = ", ")
            ))
        }
    }

    # Coverage: the compounding steps through every calendar year in
    # [min(periods), max(periods)) -- not just survey years -- at ages min_age..top_age.
    # The mortality and population frames must end at the same terminal open age.
    max_table_age <- max(mortality$age)
    if (max_table_age != top_age) {
        stop(sprintf(
            paste0(
                "`mortality` and the data supplying cell counts must share one terminal open age, ",
                "but they end at %g and %g, respectively."
            ),
            max_table_age, top_age
        ))
    }
    need_years <- seq(min(periods), max(periods) - 1)
    need_ages <- seq(min_age, max_table_age)
    if (length(mort_cells) > 0) {
        # Require every joint cell combination in the population frame. Checking
        # marginal levels alone misses absent combinations such as one sex x education pair.
        combos <- unique(frame[, mort_cells, with = FALSE])
        for (col in mort_cells) set(combos, NULL, col, as.character(combos[[col]]))
        required <- combos[, CJ(period = need_years, age = need_ages), by = mort_cells]
    } else {
        required <- CJ(period = need_years, age = need_ages)
    }
    missing <- required[!mortality, on = key_cols]
    if (nrow(missing) > 0) {
        show <- missing[seq_len(min(.N, 5L))]
        stop(sprintf(
            paste0(
                "`mortality` must cover every calendar year %g-%g for every age %g-%g, ",
                "but %d combination(s) are missing, e.g.:\n%s"
            ),
            min(need_years), max(need_years), min_age, max_table_age, nrow(missing),
            paste(sprintf("  period %g, age %g", show$period, show$age), collapse = "\n")
        ))
    }

    setkeyv(mortality, key_cols)
    mortality
}

# Per-year unconditional death probabilities over the gap for each row of `rows`.
# A person aged a in year t is aged a + j in year t + j. Column j is survival
# through prior years times q(a + j, t + j). Lookup ages are clamped at the
# mortality table's maximum age.
gap_death_profile <- function(rows, gap, mortality) {
    mort_cells <- setdiff(names(mortality), c("period", "age", "prob"))
    max_table_age <- max(mortality$age)
    n_rows <- nrow(rows)
    idx <- rep(seq_len(n_rows), gap) # evaluated outside `[` so the n column can't capture it
    lk <- rows[idx, c(mort_cells, "age", "period"), with = FALSE]
    for (col in mort_cells) set(lk, NULL, col, as.character(lk[[col]]))
    offset <- rep(seq_len(gap) - 1L, each = n_rows)
    set(lk, NULL, "age", pmin(lk$age + offset, max_table_age))
    set(lk, NULL, "period", lk$period + offset)
    prob <- mortality[lk, on = c("period", "age", mort_cells), prob]
    if (anyNA(prob)) {
        miss <- unique(lk[is.na(prob)])
        stop(sprintf(
            "No mortality probability for %d looked-up combination(s), e.g. period %g, age %g.",
            nrow(miss), miss$period[1L], miss$age[1L]
        ))
    }

    out <- matrix(0, nrow = n_rows, ncol = gap)
    surv <- rep(1, n_rows)
    for (j in seq_len(gap)) {
        qj <- prob[((j - 1L) * n_rows + 1L):(j * n_rows)]
        out[, j] <- surv * qj
        surv <- surv * (1 - qj)
    }
    out
}

# Death probability compounded over the complete gap.
gap_death_prob <- function(rows, gap, mortality) {
    rowSums(gap_death_profile(rows, gap, mortality))
}

# Convert expected deaths by year to integer event counts. Rounding cumulative
# deaths keeps all annual counts nonnegative and preserves round(total expected deaths).
integerize_death_profile <- function(expected) {
    cumulative <- expected
    if (ncol(cumulative) > 1L) {
        for (j in 2:ncol(cumulative)) cumulative[, j] <- cumulative[, j] + cumulative[, j - 1L]
    }
    cumulative <- round(cumulative)
    previous <- cbind(0, cumulative[, -ncol(cumulative), drop = FALSE])
    cumulative - previous
}

# Add the four per-cell event-count columns to a period-aligned cell table
# (columns age, n1, n2). New cohorts (age < min_age) enter via coming-of-age.
# Survivor cells reconcile n1 -> n2 by one of two strategies:
#   - sign attribution (mortality = NULL): shrinkage is mortality, growth is net
#     in-migration; out-migration is folded into mortality and stays 0.
#   - residual migration (mortality = prepared probability table): deaths are
#     round(n1 * qtilde) with qtilde compounded over the gap (gap_death_prob);
#     the open-group cell at the top age instead sums expected deaths over its
#     pre-merge constituents, each compounding its own survival path. Migration
#     is the signed residual n2 - (n1 - deaths), so a cell can carry both deaths
#     and in-migration, and n2 = n1 - deaths + in - out holds exactly.
derive_events <- function(data, min_age, gap, mortality = NULL) {
    is_new <- data$age < min_age
    data[, coming_of_age := ifelse(is_new, pmax(0, n2 - n1), 0)]
    if (is.null(mortality)) {
        data[, mortality := ifelse(is_new, 0, pmax(0, n1 - n2))]
        data[, inmigration := ifelse(is_new, 0, pmax(0, n2 - n1))]
        data[, outmigration := 0]
        return(data)
    }
    expected <- matrix(0, nrow = nrow(data), ncol = gap)
    surv <- which(!is_new)
    if (length(surv) > 0L) {
        expected[surv, ] <- gap_death_profile(data[surv], gap, mortality) * data$n1[surv]
    }
    oc <- attr(data, "open_constituents")
    if (nrow(oc) > 0L) {
        oc_expected <- gap_death_profile(oc, gap, mortality) * oc$n
        oc_expected <- rowsum(oc_expected, oc$cell, reorder = FALSE)
        expected[as.integer(rownames(oc_expected)), ] <- oc_expected
    }
    mortality_by_band <- integerize_death_profile(expected)
    deaths <- rowSums(mortality_by_band)
    net <- data$n2 - (data$n1 - deaths)
    set(data, NULL, "mortality", deaths)
    data[, inmigration := ifelse(is_new, 0, pmax(0, net))]
    data[, outmigration := ifelse(is_new, 0, pmax(0, -net))]
    setattr(data, "mortality_by_band", mortality_by_band)
    data
}

# Expand the per-cell event-count table into a flat, time-ordered event list.
# Returns parallel vectors (events_tick, ev_type, ev_cell), each of length K (the
# total number of events), giving every demographic event a (time, type, cell) triple.
#
# The gap splits into `gap` crossing-year bands of width 1/gap. A coming-of-age cell
# (aligned age a < min_age) crosses min_age exactly (min_age - a) years in, so its
# entrants belong to band b = min_age - a; the band's interval ((b-1)/gap, b/gap] is
# their entry window. Supplied annual probabilities place mortality in its year;
# sign-attributed mortality and migration span the full gap and are split across bands.
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
# year fill that year's slots without colliding with the rest of the gap. Out-migration
# (zero under sign attribution) joins the same whole-gap pool.
schedule_events <- function(data, min_age, gap) {
    coa <- data$coming_of_age
    n_c <- nrow(data)
    nb <- max(1L, as.integer(round(gap))) # one band per crossing-year of the gap

    # Under residual migration, annual probabilities assign mortality to its
    # calendar-year band. Under sign attribution its timing remains unknown and
    # joins the whole-gap pool. Migration timing is always unknown.
    mortality_by_band <- attr(data, "mortality_by_band")
    fixed_mortality <- !is.null(mortality_by_band)
    free_type <- c(
        if (!fixed_mortality) rep.int("mortality", sum(data$mortality)),
        rep.int("inmigration", sum(data$inmigration)),
        rep.int("outmigration", sum(data$outmigration))
    )
    free_cell <- c(
        if (!fixed_mortality) rep.int(seq_len(n_c), data$mortality),
        rep.int(seq_len(n_c), data$inmigration),
        rep.int(seq_len(n_c), data$outmigration)
    )
    M <- length(free_type)
    free_band <- integer(M)
    if (M > 0L) free_band[sample.int(M)] <- ((seq_len(M) - 1L) %% nb) + 1L

    n_ev <- sum(coa) + M + if (fixed_mortality) sum(mortality_by_band) else 0L
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
        mort_cells <- if (fixed_mortality) which(mortality_by_band[, b] > 0L) else integer(0)
        b_type <- c(
            rep.int("coming_of_age", sum(coa[coa_cells])),
            rep.int("mortality", if (fixed_mortality) sum(mortality_by_band[, b]) else 0L),
            free_type[fidx]
        )
        b_cell <- c(
            rep.int(coa_cells, coa[coa_cells]),
            if (fixed_mortality) rep.int(mort_cells, mortality_by_band[mort_cells, b]),
            free_cell[fidx]
        )
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

# One prediction slice per evaluation tick: stack the prediction columns of `data`
# (cells + period, the only predictors the model may use), advancing age and period
# by tick * gap. n_cells * length(eval_ticks) rows. Ages are clamped at top_age:
# the open-interval cell stays at "T+" as it ages, and the model is never asked to
# extrapolate past the data's age support. period advances unclamped.
build_event_stack <- function(data, gap, eval_ticks, cells, top_age) {
    n_cells <- nrow(data)
    n_eval <- length(eval_ticks)
    idx_rep <- rep.int(seq_len(n_cells), n_eval)
    data_stack <- data[idx_rep, c(cells, "period"), with = FALSE]
    set(
        data_stack, NULL, "age",
        pmin(rep.int(data$age, n_eval) + rep(eval_ticks * gap, each = n_cells), top_age)
    )
    set(
        data_stack, NULL, "period",
        rep.int(data$period, n_eval) + rep(eval_ticks * gap, each = n_cells)
    )
    data_stack
}

# Deterministic replay of K event orderings against precomputed outcome surfaces, vectorized
# across the K columns (each column = one (ordering, outcome-draw) replicate). No model call, no
# RNG, so the same (orderings, surface) always yields the same record. Every ordering shares the
# deterministic tick grid (fixed slot positions and event counts; only which event lands in which
# slot varies), so all K schedules have identical length n_ev and share the surface's tick-slice
# row blocks. Unlike the prior shared-schedule form, each column now carries its OWN demographic
# trajectory (n_mat, sum_n per column): the orderings diverge, so the running counts become
# per-column state rather than shared scalars. Fires events in time order, attributing the
# weighted-mean change to a component: intraindividual drift between events, then the event.
# Inputs:
#   n_vec       - per-cell period-start counts (data$n); shared start point for every column
#   y_start     - n_cells x K per-cell tick-0 outcomes (one column per replicate)
#   surface     - (n_cells * n_eval) x K per-cell outcomes stacked by eval tick then column; tick
#                 slice i is rows ((i-1)*n_cells + 1):(i*n_cells), with n_eval = n_ev + 1 and the
#                 final tick (1) last.
#   ev_type_mat - n_ev x K event types; column k is ordering k (point: K identical orderings)
#   ev_cell_mat - n_ev x K firing cells (row indices into `data`); column k is ordering k
# Returns one row per (draw, component, cell) the component touched: every cell for intra-
# individual change, the firing cells for turnover. draw indexes the K columns.
replay_schedule <- function(n_vec, y_start, surface, ev_type_mat, ev_cell_mat) {
    n_ev <- nrow(ev_type_mat)
    n_cells <- length(n_vec)
    K <- ncol(surface)

    # Per-event turnover deltas (n_ev x K) and per-cell intraindividual deltas (n_cells x K),
    # accumulated then aggregated to (draw, component, cell). The per-cell IC delta at each tick
    # is sum_c (y_cur[c] - y_prev[c]) * n[c] / sum_n, an exact split across cells (its column sum
    # is the scalar IC delta).
    cr_delta <- matrix(0, n_ev, K)
    ic_by_cell <- matrix(0, n_cells, K)

    # Per-column running state: each ordering evolves on its own demographic trajectory, so n_mat
    # (n_cells x K) and sum_n / sum_yn (length K) carry the column axis. The demographic-event
    # update is O(K) (one cell per column changes by one person), but the intraindividual step
    # re-sums y_cur * n_mat each iteration because every cell's y drifts with aging, so the loop
    # is O(n_ev * n_cells * K).
    n_mat <- matrix(n_vec, n_cells, K)
    sum_n <- rep.int(sum(n_vec), K)
    y_prev <- y_start

    for (i_ev in seq_len(n_ev)) {
        # Intraindividual drift since the previous tick, accumulated per cell. Divide each column
        # by its own sum_n (rep'd down the n_cells rows). Its column sum is the scalar IC delta.
        rows <- ((i_ev - 1L) * n_cells + 1L):(i_ev * n_cells)
        y_cur <- surface[rows, , drop = FALSE]
        ic_by_cell <- ic_by_cell + (y_cur - y_prev) * n_mat / rep(sum_n, each = n_cells)
        y_prev <- y_cur
        sum_yn <- colSums(y_cur * n_mat)
        post_ic_mean <- sum_yn / sum_n

        # The firing cell/type/sign are length-K vectors (one per column). A 2-D index gathers
        # and scatters the per-column firing cell out of the n_cells x K matrices.
        cell_k <- ev_cell_mat[i_ev, ]
        type_k <- ev_type_mat[i_ev, ]
        idx <- cbind(cell_k, seq_len(K))
        y_pick <- y_cur[idx] # length-K vector

        # Exits (mortality, out-migration) remove an individual from the cell (sgn -1); entries
        # (coming-of-age, in-migration) add one at the cell's current y (sgn +1).
        sgn <- ifelse(type_k == "mortality" | type_k == "outmigration", -1, 1)
        n_mat[idx] <- n_mat[idx] + sgn
        sum_yn <- sum_yn + sgn * y_pick
        sum_n <- sum_n + sgn

        cr_delta[i_ev, ] <- sum_yn / sum_n - post_ic_mean
    }

    # Final intraindividual drift to the end of the period (tick = 1, the last surface slice).
    rows <- (n_ev * n_cells + 1L):((n_ev + 1L) * n_cells)
    y_cur <- surface[rows, , drop = FALSE]
    ic_by_cell <- ic_by_cell + (y_cur - y_prev) * n_mat / rep(sum_n, each = n_cells)

    # Combine the per-cell intraindividual deltas with the per-event turnover deltas and
    # aggregate to one row per (draw, component, cell). as.vector() unrolls each matrix
    # column-major (cell/event fastest, column outer), matching draw = rep(seq_len(K), each = .).
    # The per-event component/cell are the ordering matrices themselves.
    ev_dt <- data.table(
        draw = rep(seq_len(K), each = n_ev),
        component = as.vector(ev_type_mat),
        cell = as.vector(ev_cell_mat),
        delta = as.vector(cr_delta)
    )
    ic_dt <- data.table(
        draw = rep(seq_len(K), each = n_cells),
        component = "intraindividual",
        cell = rep(seq_len(n_cells), K),
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

# Draw K = max(R, 1) event orderings, build the outcome surface once, and replay every ordering
# against it for the point estimate and the replicates. Returns list(point, draws) (draws NULL
# when reps is NULL, i.e. R = 0). The K orderings share one deterministic tick grid, so the
# surface (the dominant cost, ~75% of wall time) is built once and replayed against cheap
# arithmetic orderings -- ordering uncertainty is nearly free.
#
# Point estimate: the fitted-model surface, replayed across the K orderings and averaged (the
# expected decomposition at the fitted model). align_periods() set data$y = predict_y(model, data)
# at tick 0, and nothing since has touched age/period/y (derive_events only adds event-count
# columns the model ignores), so data$y is an *exact* cache of the point model's tick-0 surface --
# the point path reuses it rather than re-predicting. Replicates have no such cache, so they
# predict tick 0 (y_start). The replicate prediction (replicate_predict) is reached only when
# reps is non-NULL, so the R = 0 path makes exactly one model call (the point surface), draws a
# single ordering, and allocates no draw machinery -- byte-identical to the legacy single-ordering
# point.
#
# Each replicate pairs ordering k with model refit k by column index: the ordering stream (global
# RNG) and the Dirichlet refit stream (isolated in y_replicates) are independent, so any index
# pairing yields valid joint (ordering, model) samples. The reported band is the combined
# ordering + model uncertainty.
simulate_schedule <- function(data, model, reps, gap, min_age, cells, top_age) {
    has_draws <- !is.null(reps)
    R <- if (has_draws) ncol(reps$beta) else 0L
    n_ord <- max(1L, R) # R orderings; 1 when R = 0 (legacy single-ordering fast path)

    # K orderings sharing ONE deterministic tick grid -> one surface build. events_tick and n_ev
    # are deterministic across orderings; only ev_type/ev_cell (the interleaving) are randomized.
    scheds <- lapply(seq_len(n_ord), function(k) schedule_events(data, min_age, gap))
    n_ev <- length(scheds[[1L]]$ev_type)
    ev_type_mat <- matrix(vapply(scheds, `[[`, character(n_ev), "ev_type"), nrow = n_ev, ncol = n_ord)
    ev_cell_mat <- matrix(vapply(scheds, `[[`, integer(n_ev), "ev_cell"), nrow = n_ev, ncol = n_ord)
    eval_ticks <- c(scheds[[1L]]$events_tick, 1)
    stack <- build_event_stack(data, gap, eval_ticks, cells, top_age)

    # Point: fitted-model surface replicated across the orderings, then averaged. Computed and
    # freed before the draw surface, so peak memory holds one n_ord-column surface. mean(delta)
    # is exact: every ordering fires the identical (component, cell) key set (event counts fixed,
    # only order varies), so each group has n_ord rows.
    pt_surface <- matrix(predict_y(model, stack), ncol = 1L)[, rep(1L, n_ord), drop = FALSE]
    pt_y <- matrix(data$y, ncol = 1L)[, rep(1L, n_ord), drop = FALSE]
    point <- replay_schedule(data$n, pt_y, pt_surface, ev_type_mat, ev_cell_mat)[
        , .(delta = mean(delta)),
        by = .(component, cell)
    ]
    setorder(point, component, cell)

    draws <- NULL
    if (has_draws) { # ordering k paired with refit k
        surface <- replicate_predict(reps, stack) # (n_cells * n_eval) x R
        y_start <- replicate_predict(reps, data) # n_cells x R, replicate tick-0 outcomes
        # The open-group cells' tick-0 outcome is the n-weighted mean over their
        # pre-merge constituents (see align_periods()); apply the same weighting
        # per replicate so the draws start from each refit's own weighted mean.
        oc <- attr(data, "open_constituents")
        if (nrow(oc) > 0L) {
            oc_predict <- copy(oc)
            oc_predict[age > top_age, age := top_age]
            y_const <- replicate_predict(reps, oc_predict) # n_const x R
            y_start[sort(unique(oc$cell)), ] <-
                rowsum(y_const * oc$n, oc$cell) / as.vector(rowsum(oc$n, oc$cell))
        }
        draws <- replay_schedule(data$n, y_start, surface, ev_type_mat, ev_cell_mat)
    }
    list(point = point[], draws = draws)
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
