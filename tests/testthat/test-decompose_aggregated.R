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
  expect_named(decomp, c("summary", "record", "migration"))
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
