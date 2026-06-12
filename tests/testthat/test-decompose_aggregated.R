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
  predict_y <- function(newdata) { predict(model, newdata = newdata) }

  # ACT
  decomp <- decompose_aggregated(stacked_data, predict_y)

  # ASSERT - Structure
  expect_s3_class(decomp, "social_change_decomp")
  expect_true(is.list(decomp))
  expect_named(decomp, c("summary", "record"))
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
  predict_y <- function(newdata) { predict(model_no_gender, newdata = newdata) }

  # ACT
  decomp <- decompose_aggregated(stacked_data, predict_y)

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
  predict_y <- function(newdata) { predict(model_gender, newdata = newdata) }

  # ACT
  decomp <- decompose_aggregated(stacked_data, predict_y, "gender")

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
  predict_y <- function(newdata) { predict(model_no_gender, newdata = newdata) }

  # ACT
  decomp <- decompose_aggregated(stacked_data, predict_y)

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
  predict_y <- function(newdata) { predict(model_gender, newdata = newdata) }

  # ACT
  decomp <- decompose_aggregated(stacked_data, predict_y, "gender")

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
  predict_y <- function(newdata) { predict(model_no_gender, newdata = newdata) }

  # ACT
  decomp <- decompose_aggregated(stacked_data, predict_y)

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
  predict_y <- function(newdata) { predict(model_gender, newdata = newdata) }

  # ACT
  decomp <- decompose_aggregated(stacked_data, predict_y, "gender")

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
  predict_y <- function(newdata) { predict(model, newdata = newdata) }

  # ACT
  decomp <- decompose_aggregated(stacked_data, predict_y, "smoking")

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
  predict_y <- function(newdata) { predict(model, newdata = newdata) }

  # ACT
  decomp <- decompose_aggregated(stacked_data, predict_y, "smoking")

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
  predict_y <- function(newdata) { predict(model, newdata = newdata) }

  # ACT
  decomp <- decompose_aggregated(stacked_data, predict_y, "smoking")

  # ASSERT - Should recover simulation
  sim_ic <- simresult$summary[period > 0, sum(intraindividual)]
  decomp_ic <- decomp$summary[period > 0, sum(intraindividual)]
  expect_equal(decomp_ic, sim_ic, tolerance = 1e-3)

  sim_pt <- simresult$summary[period > 0, sum(mortality + coming_of_age)]
  decomp_pt <- decomp$summary[period > 0, sum(mortality + coming_of_age)]
  expect_equal(decomp_pt, sim_pt, tolerance = 1e-3)

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
  predict_y <- function(newdata) { predict(model, newdata = newdata) }
  decomp <- decompose_aggregated(stacked_data, predict_y)

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
    y = rep(c(0, 0.05, 0.1), times = 3) # outcome rises with age; matches fun_y
  )
  fun_y <- function(nd) (nd$age - 20) / 20

  set.seed(1)
  decomp <- decompose_aggregated(stacked, fun_y)

  total_change <- decomp$summary[.N, modeled_mean] - decomp$summary[1, modeled_mean]
  all_components <- decomp$summary[period > min(period), sum(
    intraindividual + coming_of_age + mortality + inmigration + outmigration
  )]
  expect_equal(all_components, total_change, tolerance = 1e-10)

  # the growth must register as in-migration events, not vanish into a residual
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

  # Invalid data should error (missing required columns)
  expect_error(
    decompose_aggregated(data.frame(x = 1:5), function(x) x),
    "subset"
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
  predict_y <- function(newdata) predict(model, newdata = newdata)

  # ACT
  decomp <- decompose_aggregated(stacked_gap2, predict_y)

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

  # Only year-1 entrants: period 2 has ages 21–26, no age-20.
  # Aligned age 19 is the sole CoA cell; all CoA event times must be in [0, 0.5].
  stacked_yr1 <- rbind(
    data.table(age = 20:24, period = 0, y = 20:24, n = 100),
    data.table(age = 21:26, period = 2, y = 21:26, n = 100)
  )
  model_yr1 <- lm(y ~ age, data = stacked_yr1)
  decomp_yr1 <- decompose_aggregated(stacked_yr1, function(nd) predict(model_yr1, newdata = nd))
  coa_yr1 <- decomp_yr1$record[[1]][component == "coming_of_age"]
  expect_true(all(coa_yr1$time <= 0.5))

  # Only year-2 entrants: period 2 has ages 20 and 22–27, no age-21.
  # Aligned age 18 is the sole CoA cell; all CoA event times must be in [0.5, 1.0].
  stacked_yr2 <- rbind(
    data.table(age = 20:24,        period = 0, y = 20:24,        n = 100),
    data.table(age = c(20, 22:27), period = 2, y = c(20, 22:27), n = 100)
  )
  model_yr2 <- lm(y ~ age, data = stacked_yr2)
  decomp_yr2 <- decompose_aggregated(stacked_yr2, function(nd) predict(model_yr2, newdata = nd))
  coa_yr2 <- decomp_yr2$record[[1]][component == "coming_of_age"]
  expect_true(all(coa_yr2$time >= 0.5))
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
  predict_y <- function(newdata) predict(model, newdata = newdata)

  # ACT
  decomp_gap1 <- decompose_aggregated(rbindlist(simresult$snapshot[1:2]), predict_y)
  decomp_gap2 <- decompose_aggregated(rbindlist(simresult$snapshot[c(1, 3)]), predict_y)

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
  predict_y <- function(newdata) predict(model, newdata = newdata)

  # ACT
  decomp <- decompose_aggregated(stacked_mixed, predict_y)

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
  predict_y <- function(newdata) { predict(model, newdata = newdata) }

  set.seed(999)
  decomp_ordered <- decompose_aggregated(stacked_data, predict_y)

  # shuffle the period blocks (and rows within) into a non-chronological order
  set.seed(7)
  shuffled <- stacked_data[sample(.N)]
  set.seed(999)
  decomp_shuffled <- decompose_aggregated(shuffled, predict_y)

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
  predict_y <- function(newdata) { predict(model, newdata = newdata) }

  expect_error(
    decompose_aggregated(stacked_data, predict_y),
    "at least 2 distinct periods"
  )
})

test_that("decompose_aggregated errors when a later wave dips below the earlier minimum age", {
  # Period 2004 contains a 17-year-old, below period 2000's minimum of 20. Such a
  # respondent has not crossed min_age by the later wave, so classifying them as
  # coming-of-age would schedule their entry past the period boundary and
  # extrapolate fun_y. The function should error instead.
  stacked_data <- rbindlist(list(
    data.table(age = 20:30, period = 2000),
    data.table(age = 17:30, period = 2004)
  ))
  stacked_data[, y := (age - 20) / 20]
  model <- lm(y ~ age, data = stacked_data)
  predict_y <- function(newdata) predict(model, newdata = newdata)

  expect_error(
    decompose_aggregated(stacked_data, predict_y),
    "below period 2000's minimum age"
  )
})

test_that("decompose_aggregated allows a later wave whose minimum age is above the earlier one", {
  # The guard is one-sided: a wave that simply did not sample the youngest age
  # (here period 2004 starts at 22, above period 2000's 20) is harmless.
  stacked_data <- rbindlist(list(
    data.table(age = 20:30, period = 2000),
    data.table(age = 22:32, period = 2004)
  ))
  stacked_data[, y := (age - 20) / 20]
  model <- lm(y ~ age, data = stacked_data)
  predict_y <- function(newdata) predict(model, newdata = newdata)

  expect_no_error(decompose_aggregated(stacked_data, predict_y))
})
