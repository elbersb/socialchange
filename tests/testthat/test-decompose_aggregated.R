test_that("decompose_aggregated recovers Scenario 1 (perfect recovery)", {
  # ARRANGE - Simulate data
  set.seed(12345)
  data <- build_simple_population()
  fun_y <- make_age_only_outcome()
  fun_mortality <- make_stable_mortality_simple()
  fun_coming_of_age <- make_stable_coming_of_age_simple()

  simresult <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  # Stack the data and fit model
  stacked_data <- rbindlist(simresult$snapshot)
  model <- lm(y ~ age, data = stacked_data)

  # ACT
  decomp <- decompose_aggregated(stacked_data, model)

  # ASSERT - Structure
  expect_s3_class(decomp, "social_change_decomp")
  expect_true(is.list(decomp))
  expect_named(decomp, c("summary", "record", "draws", "cells"))
  expect_null(decomp$draws) # NULL without bootstrap (R = 0)
  expect_s3_class(decomp$summary, "data.table")
  expect_true(is.list(decomp$record))

  # ASSERT - Exact recovery of simulation results
  sim_ic <- simresult$summary[period > 0, sum(intraindividual)]
  decomp_ic <- decomp$summary[period > 0, sum(intraindividual)]
  expect_equal(decomp_ic, sim_ic, tolerance = 1e-4)

  sim_pt <- simresult$summary[period > 0, sum(mortality + coming_of_age)]
  decomp_pt <- decomp$summary[period > 0, sum(mortality + coming_of_age)]
  expect_equal(decomp_pt, sim_pt, tolerance = 1e-4)

  # Verify decomposition is internally consistent
  total_change <- decomp$summary[.N, modeled_mean] - decomp$summary[1, modeled_mean]
  decomp_total <- decomp$summary[period > 0, sum(intraindividual + mortality + coming_of_age)]
  expect_equal(decomp_total, total_change, tolerance = 1e-4)

  # ASSERT - Check absolute values from vignette
  expect_equal(decomp$summary[1, modeled_mean], 0.3758, tolerance = 1e-3)
  expect_equal(decomp$summary[.N, modeled_mean], 0.3758, tolerance = 1e-3)
  expect_equal(decomp$summary[period > 0, sum(intraindividual)], 0.25, tolerance = 1e-3)
  expect_equal(decomp$summary[period > 0, sum(mortality)], -0.121, tolerance = 1e-2)
  expect_equal(decomp$summary[period > 0, sum(coming_of_age)], -0.129, tolerance = 1e-2)
})

test_that("decompose_aggregated handles Scenario 2 (gender, misspecified model)", {
  # ARRANGE - Simulate data
  set.seed(12345)
  data <- build_gendered_population()
  fun_y <- make_gender_age_outcome()
  fun_mortality <- make_stable_mortality_gendered()
  fun_coming_of_age <- make_stable_coming_of_age_gendered()

  simresult <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  # Stack the data and fit MISSPECIFIED model (ignores gender)
  stacked_data <- rbindlist(simresult$snapshot)
  model_no_gender <- lm(y ~ age + period, data = stacked_data)

  # ACT
  decomp <- decompose_aggregated(stacked_data, model_no_gender)

  # ASSERT - Should still approximate the true decomposition
  sim_ic <- simresult$summary[period > 0, sum(intraindividual)]
  decomp_ic <- decomp$summary[period > 0, sum(intraindividual)]
  expect_equal(decomp_ic, sim_ic, tolerance = 1e-2)  # More lenient tolerance

  sim_pt <- simresult$summary[period > 0, sum(mortality + coming_of_age)]
  decomp_pt <- decomp$summary[period > 0, sum(mortality + coming_of_age)]
  expect_equal(decomp_pt, sim_pt, tolerance = 1e-2)
})

test_that("decompose_aggregated handles Scenario 2 (gender, correct model)", {
  # ARRANGE - Simulate data
  set.seed(12345)
  data <- build_gendered_population()
  fun_y <- make_gender_age_outcome()
  fun_mortality <- make_stable_mortality_gendered()
  fun_coming_of_age <- make_stable_coming_of_age_gendered()

  simresult <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  # Stack the data and fit CORRECT model (includes gender interaction)
  stacked_data <- rbindlist(simresult$snapshot)
  model_gender <- lm(y ~ age * gender, data = stacked_data)

  # ACT
  decomp <- decompose_aggregated(stacked_data, model_gender, "gender")

  # ASSERT - Should recover the simulation (more lenient due to stochasticity)
  sim_ic <- simresult$summary[period > 0, sum(intraindividual)]
  decomp_ic <- decomp$summary[period > 0, sum(intraindividual)]
  expect_equal(decomp_ic, sim_ic, tolerance = 0.01)

  sim_pt <- simresult$summary[period > 0, sum(mortality + coming_of_age)]
  decomp_pt <- decomp$summary[period > 0, sum(mortality + coming_of_age)]
  expect_equal(decomp_pt, sim_pt, tolerance = 0.01)

  # ASSERT - Check absolute values from vignette (half of Scenario 1)
  expect_equal(decomp$summary[1, modeled_mean], 0.3758, tolerance = 1e-3)
  expect_equal(decomp$summary[.N, modeled_mean], 0.3758, tolerance = 1e-3)
  expect_equal(decomp_ic, 0.125, tolerance = 0.01)
  expect_equal(decomp_pt, -0.125, tolerance = 0.01)
})

test_that("decompose_aggregated handles Scenario 3 (pure IC, misspecified)", {
  # ARRANGE - Simulate data
  set.seed(12345)
  data <- build_gendered_population()
  fun_y <- make_time_only_outcome()
  fun_mortality <- make_stable_mortality_gendered()
  fun_coming_of_age <- make_stable_coming_of_age_gendered()

  simresult <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  # Stack the data and fit MISSPECIFIED model
  stacked_data <- rbindlist(simresult$snapshot)
  model_no_gender <- lm(y ~ age + period, data = stacked_data)

  # ACT
  decomp <- decompose_aggregated(stacked_data, model_no_gender)

  # ASSERT - PT should be ~0 (pure IC scenario)
  decomp_pt <- decomp$summary[period > 0, sum(mortality + coming_of_age)]
  expect_equal(decomp_pt, 0, tolerance = 1e-2)

  # IC should account for all change
  total_change <- decomp$summary[.N, modeled_mean] - decomp$summary[1, modeled_mean]
  decomp_ic <- decomp$summary[period > 0, sum(intraindividual)]
  expect_equal(decomp_ic, total_change, tolerance = 1e-2)
})

test_that("decompose_aggregated handles Scenario 3 (pure IC, correct model)", {
  # ARRANGE - Simulate data
  set.seed(12345)
  data <- build_gendered_population()
  fun_y <- make_time_only_outcome()
  fun_mortality <- make_stable_mortality_gendered()
  fun_coming_of_age <- make_stable_coming_of_age_gendered()

  simresult <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  # Stack the data and fit CORRECT model
  stacked_data <- rbindlist(simresult$snapshot)
  model_gender <- lm(y ~ age + period + gender, data = stacked_data)

  # ACT
  decomp <- decompose_aggregated(stacked_data, model_gender, "gender")

  # ASSERT - Should exactly recover simulation
  sim_ic <- simresult$summary[period > 0, sum(intraindividual)]
  decomp_ic <- decomp$summary[period > 0, sum(intraindividual)]
  expect_equal(decomp_ic, sim_ic, tolerance = 1e-4)

  sim_pt <- simresult$summary[period > 0, sum(mortality + coming_of_age)]
  decomp_pt <- decomp$summary[period > 0, sum(mortality + coming_of_age)]
  expect_equal(decomp_pt, sim_pt, tolerance = 1e-4)

  # ASSERT - Check absolute values from vignette (pure IC)
  expect_equal(decomp$summary[1, modeled_mean], 0.3, tolerance = 1e-3)
  expect_equal(decomp$summary[.N, modeled_mean], 0.8, tolerance = 1e-3)
  expect_equal(decomp_ic, 0.5, tolerance = 1e-3)
  expect_equal(decomp_pt, 0, tolerance = 1e-3)
})

test_that("decompose_aggregated handles Scenario 4 (pure PT, misspecified)", {
  # ARRANGE - Simulate data
  set.seed(12345)
  data <- build_gendered_population()
  fun_y <- make_cohort_only_outcome()
  fun_mortality <- make_stable_mortality_gendered()
  fun_coming_of_age <- make_stable_coming_of_age_gendered()

  simresult <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  # Stack the data and fit MISSPECIFIED model
  stacked_data <- rbindlist(simresult$snapshot)
  model_no_gender <- lm(y ~ age + period, data = stacked_data)

  # ACT
  decomp <- decompose_aggregated(stacked_data, model_no_gender)

  # ASSERT - IC should be ~0 (pure PT scenario)
  decomp_ic <- decomp$summary[period > 0, sum(intraindividual)]
  expect_equal(decomp_ic, 0, tolerance = 1e-2)

  # PT should account for all change
  total_change <- decomp$summary[.N, modeled_mean] - decomp$summary[1, modeled_mean]
  decomp_pt <- decomp$summary[period > 0, sum(mortality + coming_of_age)]
  expect_equal(decomp_pt, total_change, tolerance = 1e-2)
})

test_that("decompose_aggregated handles Scenario 4 (pure PT, correct model)", {
  # ARRANGE - Simulate data
  set.seed(12345)
  data <- build_gendered_population()
  fun_y <- make_cohort_only_outcome()
  fun_mortality <- make_stable_mortality_gendered()
  fun_coming_of_age <- make_stable_coming_of_age_gendered()

  simresult <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  # Stack the data and fit CORRECT model
  stacked_data <- rbindlist(simresult$snapshot)
  model_gender <- lm(y ~ age + period + gender, data = stacked_data)

  # ACT
  decomp <- decompose_aggregated(stacked_data, model_gender, "gender")

  # ASSERT - Should exactly recover simulation
  sim_ic <- simresult$summary[period > 0, sum(intraindividual)]
  decomp_ic <- decomp$summary[period > 0, sum(intraindividual)]
  expect_equal(decomp_ic, sim_ic, tolerance = 1e-4)

  sim_pt <- simresult$summary[period > 0, sum(mortality + coming_of_age)]
  decomp_pt <- decomp$summary[period > 0, sum(mortality + coming_of_age)]
  expect_equal(decomp_pt, sim_pt, tolerance = 1e-4)

  # ASSERT - Check absolute values from vignette (pure PT)
  expect_equal(decomp$summary[1, modeled_mean], 0.3, tolerance = 1e-3)
  expect_equal(decomp$summary[.N, modeled_mean], 0.8, tolerance = 1e-3)
  expect_equal(decomp_ic, 0, tolerance = 1e-3)
  expect_equal(decomp_pt, 0.5, tolerance = 1e-3)
})

test_that("decompose_aggregated handles Scenario 5a (smoking, static)", {
  # ARRANGE - Simulate data
  set.seed(12345)
  data <- build_smoking_population()
  fun_y <- make_smoking_static_outcome()
  fun_mortality <- make_smoking_mortality()
  fun_coming_of_age <- make_smoking_coming_of_age_static()

  simresult <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  # Stack the data and fit model
  stacked_data <- rbindlist(simresult$snapshot)
  model <- lm(y ~ age + period + smoking, data = stacked_data)

  # ACT
  decomp <- decompose_aggregated(stacked_data, model, "smoking")

  # ASSERT - Should recover simulation
  sim_ic <- simresult$summary[period > 0, sum(intraindividual)]
  decomp_ic <- decomp$summary[period > 0, sum(intraindividual)]
  expect_equal(decomp_ic, sim_ic, tolerance = 1e-3)

  sim_pt <- simresult$summary[period > 0, sum(mortality + coming_of_age)]
  decomp_pt <- decomp$summary[period > 0, sum(mortality + coming_of_age)]
  expect_equal(decomp_pt, sim_pt, tolerance = 1e-3)

  # Both should be ~0
  expect_equal(decomp_ic, 0, tolerance = 1e-2)
  expect_equal(decomp_pt, 0, tolerance = 1e-2)
})

test_that("decompose_aggregated handles Scenario 5b (smoking, IC)", {
  # ARRANGE - Simulate data
  set.seed(12345)
  data <- build_smoking_population()
  fun_y <- make_smoking_time_outcome()
  fun_mortality <- make_smoking_mortality()
  fun_coming_of_age <- make_smoking_coming_of_age_static()

  simresult <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  # Stack the data and fit model (with interaction for time effect)
  stacked_data <- rbindlist(simresult$snapshot)
  model <- lm(y ~ age + period * smoking, data = stacked_data)

  # ACT
  decomp <- decompose_aggregated(stacked_data, model, "smoking")

  # ASSERT - Should recover simulation
  sim_ic <- simresult$summary[period > 0, sum(intraindividual)]
  decomp_ic <- decomp$summary[period > 0, sum(intraindividual)]
  expect_equal(decomp_ic, sim_ic, tolerance = 1e-3)

  sim_pt <- simresult$summary[period > 0, sum(mortality + coming_of_age)]
  decomp_pt <- decomp$summary[period > 0, sum(mortality + coming_of_age)]
  expect_equal(decomp_pt, sim_pt, tolerance = 1e-3)

  # PT should be ~0, IC should be negative
  expect_equal(decomp_pt, 0, tolerance = 1e-2)
  expect_true(decomp_ic < 0)

  # ASSERT - Check absolute values from vignette
  expect_equal(decomp$summary[1, modeled_mean], 0.87, tolerance = 0.02)
  expect_equal(decomp$summary[.N, modeled_mean], 0.59, tolerance = 0.02)
  total_change <- decomp$summary[.N, modeled_mean] - decomp$summary[1, modeled_mean]
  expect_equal(decomp_ic, total_change, tolerance = 0.02)
})

test_that("decompose_aggregated handles Scenario 5c (smoking, varying coming-of-age)", {
  # ARRANGE - Simulate data
  set.seed(12345)
  data <- build_smoking_population()
  fun_y <- make_smoking_time_outcome()
  fun_mortality <- make_smoking_mortality()
  fun_coming_of_age <- make_smoking_coming_of_age_varying()

  simresult <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  # Stack the data and fit model
  stacked_data <- rbindlist(simresult$snapshot)
  model <- lm(y ~ age + period * smoking, data = stacked_data)

  # ACT
  decomp <- decompose_aggregated(stacked_data, model, "smoking")

  # ASSERT - Should recover simulation
  sim_ic <- simresult$summary[period > 0, sum(intraindividual)]
  decomp_ic <- decomp$summary[period > 0, sum(intraindividual)]
  expect_equal(decomp_ic, sim_ic, tolerance = 1e-3)

  sim_pt <- simresult$summary[period > 0, sum(mortality + coming_of_age)]
  decomp_pt <- decomp$summary[period > 0, sum(mortality + coming_of_age)]
  # Turnover is small and ordering-sensitive here; the random-ordering estimate carries
  # ~3e-3 relative Monte Carlo noise, so recover it to 1e-2 (as in the other such scenarios).
  expect_equal(decomp_pt, sim_pt, tolerance = 1e-2)

  # Both IC and PT should be negative
  expect_true(decomp_ic < 0)
  expect_true(decomp_pt < 0)
})

test_that("decompose_aggregated print method works", {
  # ARRANGE
  set.seed(12345)
  data <- build_simple_population()
  fun_y <- make_age_only_outcome()
  fun_mortality <- make_stable_mortality_simple()
  fun_coming_of_age <- make_stable_coming_of_age_simple()

  simresult <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  stacked_data <- rbindlist(simresult$snapshot)
  model <- lm(y ~ age, data = stacked_data)
  decomp <- decompose_aggregated(stacked_data, model)

  # ACT & ASSERT
  expect_output(print(decomp), "Overview by period")
  expect_output(print(decomp, detailed = FALSE), "Component")
})

test_that("components fully account for total change when a survivor cohort grows", {
  # The decomposition is an exact accounting identity: every component must sum
  # to ybar_N - ybar_1, with no residual. A survivor cohort that *grows* between
  # periods (n2 > n1) is net in-migration; the older clip-to-zero strategy
  # dropped that growth and left part of the change unaccounted for. The earlier
  # consistency tests all come from sim_social_change (cells only shrink or come
  # of age), so none exercised this case -- this fixture does, with a survivor
  # cell that grows in both transitions.
  stacked <- data.table(
    period = rep(1:3, each = 3),
    age    = rep(c(20, 21, 22), times = 3),
    n      = c(
      100, 100, 100, # period 1
      100, 130,  90, # period 2: age-21 cohort grew 100 -> 130 (in-migration)
      100, 150, 110  # period 3: age-21 cohort grew 130 -> 150 (in-migration)
    ),
    y = rep(c(0, 0.05, 0.1), times = 3) # outcome rises with age; matches model
  )
  # y already equals (age - 20) / 20 exactly for ages 20-22, so this lm reproduces it
  model <- lm(y ~ age, data = stacked)

  set.seed(1)
  decomp <- decompose_aggregated(stacked, model)

  total_change <- decomp$summary[.N, modeled_mean] - decomp$summary[1, modeled_mean]
  all_components <- decomp$summary[period > min(period), sum(
    intraindividual + coming_of_age + mortality + inmigration + outmigration
  )]
  expect_equal(all_components, total_change, tolerance = 1e-10)

  # the growth must register as in-migration, not vanish into a residual: at least
  # one cell carries an in-migration row in the tidy record
  n_inmig <- sum(vapply(decomp$record, function(r) sum(r$component == "inmigration"), integer(1)))
  expect_gt(n_inmig, 0)
  # in-migration contributes (non-zero), so print/plot display the migration component
  expect_true(decomp$summary[, sum(inmigration, na.rm = TRUE)] != 0)
})

test_that("decompose_aggregated validates input", {
  # Missing required arguments should error
  expect_error(
    decompose_aggregated(),
    "missing"
  )

  # Invalid data should error (missing required columns). The stacked_data subset
  # assert fires before any prediction, so a placeholder model is fine here.
  placeholder <- lm(y ~ age, data = data.frame(y = 1:5, age = 1:5))
  expect_error(
    decompose_aggregated(data.frame(x = 1:5), placeholder),
    "subset"
  )
})

test_that("decompose_aggregated errors on fractional directly-supplied n", {
  # The microsimulation is integer-based; fractional n would be silently
  # truncated by runif()/indexing and leave an empty trailing record row.
  stacked_data <- rbind(
    data.table(age = 20:24, period = 0, y = 20:24, n = 100.4),
    data.table(age = 20:24, period = 1, y = 21:25, n = 100)
  )
  # the integerish n assert fires before prediction
  model <- lm(y ~ age, data = stacked_data)
  expect_error(
    decompose_aggregated(stacked_data, model),
    "integerish|n"
  )
})

test_that("decompose_aggregated handles non-annual data (gap = 2)", {
  # ARRANGE - Simulate 4 annual periods, then use only every other snapshot (gap = 2)
  set.seed(42)
  simresult <- sim_social_change(
    periods = 4, data = build_simple_population(),
    fun_y = make_age_only_outcome(),
    fun_mortality = make_stable_mortality_simple(),
    fun_coming_of_age = make_stable_coming_of_age_simple()
  )

  stacked_gap2 <- rbindlist(simresult$snapshot[c(1, 3, 5)])  # periods 0, 2, 4
  model <- lm(y ~ age, data = stacked_gap2)

  # ACT
  decomp <- decompose_aggregated(stacked_gap2, model)

  # ASSERT - Total IC and PT should match the full 4-year simulation
  expect_equal(
    decomp$summary[period > 0, sum(intraindividual)],
    simresult$summary[period > 0, sum(intraindividual)],
    tolerance = 1e-3
  )
  expect_equal(
    decomp$summary[period > 0, sum(mortality + coming_of_age)],
    simresult$summary[period > 0, sum(mortality + coming_of_age)],
    tolerance = 1e-3
  )

  # Internal consistency: components sum to total modeled change
  total_change <- decomp$summary[.N, modeled_mean] - decomp$summary[1, modeled_mean]
  expect_equal(
    decomp$summary[period > 0, sum(intraindividual + mortality + coming_of_age)],
    total_change, tolerance = 1e-4
  )
})

test_that("coming-of-age events respect their entry-year window (gap > 1)", {
  # With gap = 2 and min_age = 20, aligned age 19 (actual age 21 in period 2)
  # crossed the threshold in year 1 of the gap → tick in [0, 0.5]; aligned age 18
  # (actual age 20 in period 2) crossed in year 2 → tick in [0.5, 1.0].
  # runif(n, lo, hi) is a hard constraint, so these assertions are deterministic.
  # Tested via schedule_events() directly: isolating one cohort per decomposition
  # would need unequal minimum ages, which decompose_aggregated() now rejects.
  make_cell <- function(age) {
    data.table(age = age, coming_of_age = 100, mortality = 0, inmigration = 0, outmigration = 0)
  }

  # Aligned age 19: year-1 entrants, all ticks in [0, 0.5].
  sched_yr1 <- socialchange:::schedule_events(make_cell(19), min_age = 20, gap = 2)
  expect_true(all(sched_yr1$ev_type == "coming_of_age"))
  expect_true(all(sched_yr1$events_tick <= 0.5))

  # Aligned age 18: year-2 entrants, all ticks in [0.5, 1.0].
  sched_yr2 <- socialchange:::schedule_events(make_cell(18), min_age = 20, gap = 2)
  expect_true(all(sched_yr2$events_tick >= 0.5))
})

test_that("decompose_aggregated captures all entering cohorts for gap > 1", {
  # ARRANGE - 100 entrants/yr; gap=2 must count 2 cohorts, not 1
  set.seed(42)
  simresult <- sim_social_change(
    periods = 2, data = build_simple_population(),
    fun_y = make_age_only_outcome(),
    fun_mortality = make_stable_mortality_simple(),
    fun_coming_of_age = make_stable_coming_of_age_simple()
  )

  stacked <- rbindlist(simresult$snapshot)
  model <- lm(y ~ age, data = stacked)

  # ACT
  decomp_gap1 <- decompose_aggregated(rbindlist(simresult$snapshot[1:2]), model)
  decomp_gap2 <- decompose_aggregated(rbindlist(simresult$snapshot[c(1, 3)]), model)

  coa_gap1 <- decomp_gap1$summary[period > 0, sum(coming_of_age)]
  coa_gap2 <- decomp_gap2$summary[period > 0, sum(coming_of_age)]

  # ASSERT - gap=2 picks up two cohorts, so effect is ~double gap=1
  expect_equal(coa_gap2, 2 * coa_gap1, tolerance = 0.05)
})

test_that("decompose_aggregated handles unequal gaps between periods", {
  # ARRANGE - Periods 0, 1, 3: first gap=1, second gap=2
  set.seed(42)
  simresult <- sim_social_change(
    periods = 3, data = build_simple_population(),
    fun_y = make_age_only_outcome(),
    fun_mortality = make_stable_mortality_simple(),
    fun_coming_of_age = make_stable_coming_of_age_simple()
  )

  stacked_mixed <- rbindlist(simresult$snapshot[c(1, 2, 4)])  # periods 0, 1, 3
  model <- lm(y ~ age, data = stacked_mixed)

  # ACT
  decomp <- decompose_aggregated(stacked_mixed, model)

  # ASSERT - Total IC and PT should match the full 3-year simulation
  expect_equal(
    decomp$summary[period > 0, sum(intraindividual)],
    simresult$summary[period > 0, sum(intraindividual)],
    tolerance = 1e-3
  )
  expect_equal(
    decomp$summary[period > 0, sum(mortality + coming_of_age)],
    simresult$summary[period > 0, sum(mortality + coming_of_age)],
    tolerance = 1e-3
  )
})

test_that("decompose_aggregated is invariant to input row order (period sorting)", {
  # Regression test: periods were previously taken in appearance order, so
  # rows ordered e.g. 2002, 2000, 2001 silently decomposed a backwards
  # transition (negative gap -> garbage components) with no error. The fix
  # sorts unique(period), so shuffling input rows must not change the result.
  set.seed(12345)
  data <- build_simple_population()
  simresult <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = make_age_only_outcome(),
    fun_mortality = make_stable_mortality_simple(),
    fun_coming_of_age = make_stable_coming_of_age_simple()
  )
  stacked_data <- rbindlist(simresult$snapshot)
  model <- lm(y ~ age, data = stacked_data)

  set.seed(999)
  decomp_ordered <- decompose_aggregated(stacked_data, model)

  # shuffle the period blocks (and rows within) into a non-chronological order
  set.seed(7)
  shuffled <- stacked_data[sample(.N)]
  set.seed(999)
  decomp_shuffled <- decompose_aggregated(shuffled, model)

  expect_equal(decomp_shuffled$summary, decomp_ordered$summary)
  expect_equal(decomp_shuffled$record, decomp_ordered$record)
})

test_that("decompose_aggregated errors on single-period input", {
  stacked_data <- data.table(
    age = c(20, 21, 22, 23),
    period = 2000,
    y = c(0.1, 0.2, 0.3, 0.4)
  )
  model <- lm(y ~ age, data = stacked_data)

  expect_error(
    decompose_aggregated(stacked_data, model),
    "at least 2 distinct periods"
  )
})

test_that("decompose_aggregated requires a common minimum age across waves", {
  # The minimum age (among cells with people) must be identical in every wave: it
  # is the single threshold separating coming-of-age cohorts from survivors and
  # setting coming-of-age entry timing. In real data every wave shares it, so a
  # mismatch signals a malformed/inconsistent age range and is rejected.

  # Later wave dips below the earlier minimum (17 vs 20).
  below <- rbindlist(list(
    data.table(age = 20:30, period = 2000),
    data.table(age = 17:30, period = 2004)
  ))
  below[, y := (age - 20) / 20]
  model_below <- lm(y ~ age, data = below)
  expect_error(
    decompose_aggregated(below, model_below),
    "common minimum age"
  )

  # Later wave sits above the earlier minimum (22 vs 20) — also rejected.
  above <- rbindlist(list(
    data.table(age = 20:30, period = 2000),
    data.table(age = 22:32, period = 2004)
  ))
  above[, y := (age - 20) / 20]
  model_above <- lm(y ~ age, data = above)
  expect_error(
    decompose_aggregated(above, model_above),
    "common minimum age"
  )
})

test_that("decompose_aggregated handles a zero-centered outcome via the absolute fit check", {
  # The fit diagnostic is a plain absolute deviation between observed and modeled
  # period means (exceeding `tol` errors). An earlier relative check
  # (abs(observed/modeled - 1)) blew up for a zero-centered outcome whose period
  # means sit at (or near) 0: it either crashed (0/0 = NaN in the `max_dev > tol`
  # comparison) or flagged a spurious ~100% deviation even for a perfect fit. The
  # absolute check stays finite and passes a perfect fit here.
  stacked_data <- rbindlist(list(
    data.table(age = c(20, 21), period = 2000),
    data.table(age = c(20, 21), period = 2004)
  ))
  # symmetric +/-1 with equal cell counts => weighted period mean is exactly 0
  stacked_data[, y := fifelse(age == 20, -1, 1)]
  # two ages => saturated lm predicts -1/+1 exactly
  model <- lm(y ~ age, data = stacked_data)

  expect_no_error(decomp <- decompose_aggregated(stacked_data, model))
  expect_equal(decomp$summary[, modeled_mean], c(0, 0), tolerance = 1e-12)

  # Near-zero (not exactly zero) period means: an lm fit leaves tiny float drift,
  # which a relative check would have flagged as a huge deviation. A perfect-fit
  # model should pass the absolute check.
  stacked2 <- rbindlist(list(
    data.table(age = 20:39, period = 2000),
    data.table(age = 20:39, period = 2004)
  ))
  stacked2[, y := age - 29.5]  # zero-centered within each wave
  model <- lm(y ~ age, data = stacked2)
  expect_no_error(decompose_aggregated(stacked2, model))
})

# cumulative_series() backs both the print table and the plot. The -se.R file already
# covers its CI-path structure (pseudo-types, level split, band non-additivity); these
# two cover the point-path arithmetic it does not: the components are additive, and a
# covariate split partitions them. The modeled-mean identity is asserted on `summary`
# in the scenario tests above, so it is not repeated here.

test_that("cumulative_series is additive on the point path (overall and split)", {
  set.seed(202)
  n <- 6000
  d <- data.table(
    age = sample(25:50, n, TRUE),
    period = sample(c(2000, 2004, 2008), n, TRUE),
    gender = sample(c("f", "m"), n, TRUE)
  )
  d[, y := 0.01 * age + 0.004 * (period - 2000) + 0.1 * (gender == "m") +
    stats::rnorm(n, 0, 0.3)]
  model <- stats::lm(y ~ age + period + gender, data = d)
  set.seed(7)
  decomp <- decompose_aggregated(d, model, cells = "gender", tol = 0.2)

  ci <- cumulative_series(decomp)
  last <- max(ci$period)
  val <- function(ty) {
    v <- ci[type == ty & period == last, value]
    if (length(v)) v else 0
  }
  # Total change = intraindividual + population turnover = the sum of the leaf turnover
  # components -- the builder's aggregate pseudo-types reconcile with their parts.
  turnover_leaves <- c("Mortality", "Out-migration", "Coming-of-age", "In-migration")
  expect_equal(
    val("Total change"),
    val("Intraindividual change") + val("Population turnover"),
    tolerance = 1e-9
  )
  expect_equal(
    val("Population turnover"),
    ci[type %in% turnover_leaves & period == last, sum(value)],
    tolerance = 1e-9
  )

  # A covariate split partitions the cells, so per-level cumulative values sum back to
  # the overall (level-NA) series for every type and period.
  split <- cumulative_series(decomp, "gender")
  expect_setequal(unique(split$level), c("f", "m"))
  summed <- split[, .(value = sum(value)), by = .(type, period)]
  m <- merge(summed, ci[, .(type, period, value)], by = c("type", "period"))
  expect_equal(nrow(m), nrow(ci)) # split covers exactly the overall (type, period) set
  expect_equal(m$value.x, m$value.y, tolerance = 1e-9)
})

test_that("cumulative_series completes every series across all periods (zero-fill)", {
  # In-migration fires only in the second transition here, never the first, so its
  # delta is absent from record[[1]]. Grid completion must still emit an In-migration
  # row at every period, cumulating from 0 -- otherwise the plot/CI would have a
  # ragged series starting mid-range. Age 20 is present in every wave (common minimum
  # age); the age-21 survivor cohort grows only in the second transition (80 -> 120).
  stacked <- data.table(
    period = c(rep(1, 3), rep(2, 4), rep(3, 5)),
    age = c(20, 21, 22, 20, 21, 22, 23, 20, 21, 22, 23, 24),
    n = c(100, 100, 100, 100, 80, 80, 80, 100, 80, 120, 80, 80)
  )
  stacked[, y := (age - 20) / 20] # linear in age, so lm(y ~ age) reproduces it exactly
  model <- lm(y ~ age, data = stacked)
  set.seed(1)
  decomp <- decompose_aggregated(stacked, model)

  # In-migration appears only in the second transition's record...
  expect_equal(sum(decomp$record[[1]]$component == "inmigration"), 0)
  expect_gt(sum(decomp$record[[2]]$component == "inmigration"), 0)

  # ...yet the completed series spans all three periods and starts cumulating at 0.
  periods <- decomp$summary$period
  inmig <- cumulative_series(decomp)[type == "In-migration"]
  expect_setequal(inmig$period, periods)
  expect_equal(inmig[period == min(period), value], 0)
})

test_that("decompose_aggregated errors when the model badly misfits the period means", {
  # A model that cannot reproduce the observed period means should halt rather
  # than silently decompose a divergent modeled trajectory. bad_model here is fit
  # on zeroed data, so it predicts ~0 while the observed mean is 0.5.
  stacked_data <- rbindlist(list(
    data.table(age = 20:39, period = 2000),
    data.table(age = 20:39, period = 2004)
  ))
  stacked_data[, y := 0.5]
  bad_model <- lm(y ~ age, data = data.frame(y = 0, age = 20:39))  # predicts ~0, off by 0.5

  expect_error(decompose_aggregated(stacked_data, bad_model), "deviate from observed")
  # ...unless the caller deliberately loosens tol
  expect_no_error(decompose_aggregated(stacked_data, bad_model, tol = 1))
})
