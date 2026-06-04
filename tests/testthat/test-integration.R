test_that("Full pipeline: simulate → decompose (Scenario 1)", {
  # ARRANGE - Run simulation
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

  # Extract snapshots, fit model, decompose
  stacked_data <- rbindlist(simresult$snapshot)
  model <- lm(y ~ age, data = stacked_data)
  predict_y <- function(newdata) { predict(model, newdata = newdata) }

  # ACT
  decomp <- decompose_aggregated(stacked_data, predict_y)

  # ASSERT - Decomposition recovers simulation within tight tolerance
  # IC component
  sim_ic <- simresult$summary[period > 0, sum(intraindividual)]
  decomp_ic <- decomp$summary[period > 0, sum(intraindividual)]
  expect_equal(decomp_ic, sim_ic, tolerance = 1e-3)

  # Mortality component
  sim_mort <- simresult$summary[period > 0, sum(mortality)]
  decomp_mort <- decomp$summary[period > 0, sum(mortality)]
  expect_equal(decomp_mort, sim_mort, tolerance = 1e-3)

  # Coming-of-age component
  sim_coa <- simresult$summary[period > 0, sum(coming_of_age)]
  decomp_coa <- decomp$summary[period > 0, sum(coming_of_age)]
  expect_equal(decomp_coa, sim_coa, tolerance = 1e-3)

  # Total change
  sim_total <- simresult$summary[.N, mean] - simresult$summary[1, mean]
  decomp_total <- decomp$summary[.N, modeled_mean] - decomp$summary[1, modeled_mean]
  expect_equal(decomp_total, sim_total, tolerance = 1e-4)
})

test_that("Full pipeline: simulate → decompose (Scenario 3, pure IC)", {
  # ARRANGE - Run simulation
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

  # Extract snapshots, fit model, decompose
  stacked_data <- rbindlist(simresult$snapshot)
  model <- lm(y ~ age + period + gender, data = stacked_data)
  predict_y <- function(newdata) { predict(model, newdata = newdata) }

  # ACT
  decomp <- decompose_aggregated(stacked_data, predict_y, "gender")

  # ASSERT - Verify pure IC is detected
  decomp_ic <- decomp$summary[period > 0, sum(intraindividual)]
  decomp_pt <- decomp$summary[period > 0, sum(mortality + coming_of_age)]

  # All change should be IC
  total_change <- decomp$summary[.N, modeled_mean] - decomp$summary[1, modeled_mean]
  expect_equal(decomp_ic, total_change, tolerance = 1e-4)
  expect_equal(decomp_pt, 0, tolerance = 1e-4)

  # Verify matches simulation
  sim_ic <- simresult$summary[period > 0, sum(intraindividual)]
  expect_equal(decomp_ic, sim_ic, tolerance = 1e-4)
})

test_that("Full pipeline: simulate → decompose (Scenario 4, pure PT)", {
  # ARRANGE - Run simulation
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

  # Extract snapshots, fit model, decompose
  stacked_data <- rbindlist(simresult$snapshot)
  model <- lm(y ~ age + period + gender, data = stacked_data)
  predict_y <- function(newdata) { predict(model, newdata = newdata) }

  # ACT
  decomp <- decompose_aggregated(stacked_data, predict_y, "gender")

  # ASSERT - Verify pure PT is detected
  decomp_ic <- decomp$summary[period > 0, sum(intraindividual)]
  decomp_pt <- decomp$summary[period > 0, sum(mortality + coming_of_age)]

  # All change should be PT
  total_change <- decomp$summary[.N, modeled_mean] - decomp$summary[1, modeled_mean]
  expect_equal(decomp_pt, total_change, tolerance = 1e-4)
  expect_equal(decomp_ic, 0, tolerance = 1e-4)

  # Verify matches simulation
  sim_pt <- simresult$summary[period > 0, sum(mortality + coming_of_age)]
  expect_equal(decomp_pt, sim_pt, tolerance = 1e-4)

  # Mortality and coming-of-age should be roughly balanced
  decomp_mort <- decomp$summary[period > 0, sum(mortality)]
  decomp_coa <- decomp$summary[period > 0, sum(coming_of_age)]
  expect_equal(decomp_mort, decomp_coa, tolerance = 0.08)  # More lenient due to stochasticity
})

test_that("Full pipeline: smoking scenario with transitions", {
  # ARRANGE - Run simulation with transitions
  set.seed(12345)
  data <- build_smoking_population()
  fun_y <- make_smoking_time_outcome()
  fun_mortality <- make_smoking_mortality()
  fun_coming_of_age <- make_smoking_coming_of_age_varying()
  fun_transitions <- make_smoking_transitions()

  simresult <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age,
    fun_transitions = fun_transitions
  )

  # Extract snapshots, fit model, decompose
  stacked_data <- rbindlist(simresult$snapshot)
  model <- lm(y ~ age + period * smoking, data = stacked_data)
  predict_y <- function(newdata) { predict(model, newdata = newdata) }

  # ACT
  decomp <- decompose_aggregated(stacked_data, predict_y, "smoking")

  # ASSERT - Verify decomposition is internally consistent
  # NOTE: Looser tolerance because transitions aren't separately identified
  # (see Known Limitations in CLAUDE.md)
  decomp_total_components <- decomp$summary[period > 0,
    sum(intraindividual + mortality + coming_of_age)]
  total_change <- decomp$summary[.N, modeled_mean] - decomp$summary[1, modeled_mean]
  expect_equal(decomp_total_components, total_change, tolerance = 0.20)

  # IC should be negative (attitudes changing)
  decomp_ic <- decomp$summary[period > 0, sum(intraindividual)]
  expect_true(decomp_ic < 0)

  # Population should grow (check from stacked_data, not decomp summary)
  final_n <- stacked_data[period == max(period), sum(n)]
  initial_n <- stacked_data[period == min(period), sum(n)]
  expect_true(final_n > initial_n)
})

test_that("Print methods work for all S3 classes", {
  # ARRANGE - Create objects of each class
  set.seed(12345)
  data <- build_simple_population()
  fun_y <- make_age_only_outcome()
  fun_mortality <- make_stable_mortality_simple()
  fun_coming_of_age <- make_stable_coming_of_age_simple()

  simresult <- sim_social_change(
    periods = 2,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  stacked_data <- rbindlist(simresult$snapshot)
  model <- lm(y ~ age, data = stacked_data)
  predict_y <- function(newdata) { predict(model, newdata = newdata) }
  decomp <- decompose_aggregated(stacked_data, predict_y)

  # ACT & ASSERT - No errors when printing
  expect_silent(capture.output(print(simresult)))
  expect_silent(capture.output(print(simresult, detailed = FALSE)))
  expect_silent(capture.output(print(decomp)))
  expect_silent(capture.output(print(decomp, detailed = FALSE)))

  # Verify output contains expected content
  sim_output <- capture.output(print(simresult))
  expect_true(any(grepl("Overview by period", sim_output)))

  decomp_output <- capture.output(print(decomp))
  expect_true(any(grepl("Decomposition of total change", decomp_output)))
})

test_that("Decomposition is additive across components", {
  # ARRANGE - Run multiple scenarios and verify additivity
  scenarios <- list(
    list(
      name = "Scenario 1",
      data = build_simple_population(),
      fun_y = make_age_only_outcome(),
      fun_mortality = make_stable_mortality_simple(),
      fun_coming_of_age = make_stable_coming_of_age_simple()
    ),
    list(
      name = "Scenario 2",
      data = build_gendered_population(),
      fun_y = make_gender_age_outcome(),
      fun_mortality = make_stable_mortality_gendered(),
      fun_coming_of_age = make_stable_coming_of_age_gendered()
    )
  )

  for (scenario in scenarios) {
    set.seed(12345)

    # Run simulation
    simresult <- sim_social_change(
      periods = 3,
      data = scenario$data,
      fun_y = scenario$fun_y,
      fun_mortality = scenario$fun_mortality,
      fun_coming_of_age = scenario$fun_coming_of_age
    )

    # Decompose
    stacked_data <- rbindlist(simresult$snapshot)
    if ("gender" %in% names(stacked_data)) {
      model <- lm(y ~ age * gender, data = stacked_data)
      predict_y <- function(newdata) { predict(model, newdata = newdata) }
      decomp <- decompose_aggregated(stacked_data, predict_y, "gender")
    } else {
      model <- lm(y ~ age, data = stacked_data)
      predict_y <- function(newdata) { predict(model, newdata = newdata) }
      decomp <- decompose_aggregated(stacked_data, predict_y)
    }

    # ASSERT - Components sum to total change
    total_change <- decomp$summary[.N, modeled_mean] - decomp$summary[1, modeled_mean]
    component_sum <- decomp$summary[period > 0,
      sum(intraindividual + mortality + coming_of_age)]

    expect_equal(
      component_sum,
      total_change,
      tolerance = 1e-4,
      label = paste("Additivity in", scenario$name)
    )
  }
})

test_that("Robustness to model specification", {
  # ARRANGE - Test that misspecified models still provide reasonable results
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

  stacked_data <- rbindlist(simresult$snapshot)

  # Correct model
  model_correct <- lm(y ~ age * gender, data = stacked_data)
  predict_correct <- function(newdata) { predict(model_correct, newdata = newdata) }
  decomp_correct <- decompose_aggregated(stacked_data, predict_correct, "gender")

  # Misspecified model (missing gender)
  model_misspec <- lm(y ~ age + period, data = stacked_data)
  predict_misspec <- function(newdata) { predict(model_misspec, newdata = newdata) }
  decomp_misspec <- decompose_aggregated(stacked_data, predict_misspec)

  # ACT - Extract components
  correct_ic <- decomp_correct$summary[-1, sum(intraindividual)]
  misspec_ic <- decomp_misspec$summary[-1, sum(intraindividual)]

  correct_pt <- decomp_correct$summary[-1, sum(mortality + coming_of_age)]
  misspec_pt <- decomp_misspec$summary[-1, sum(mortality + coming_of_age)]

  # ASSERT - Misspecified model should approximate correct model
  expect_equal(misspec_ic, correct_ic, tolerance = 1e-2)
  expect_equal(misspec_pt, correct_pt, tolerance = 1e-2)
})

test_that("Multiple periods produce consistent results", {
  # ARRANGE - Test that results are consistent across different numbers of periods
  set.seed(12345)
  data <- build_simple_population()
  fun_y <- make_age_only_outcome()
  fun_mortality <- make_stable_mortality_simple()
  fun_coming_of_age <- make_stable_coming_of_age_simple()

  # Run with 3 periods
  sim_3 <- sim_social_change(
    periods = 3,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  # ACT - Run with 5 periods (same seed)
  set.seed(12345)
  sim_5 <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  # ASSERT - First 3 periods should be identical
  expect_equal(
    sim_3$summary[1:4, ],  # initial + 3 periods
    sim_5$summary[1:4, ],
    tolerance = 1e-10
  )

  # Per-period IC should be similar
  ic_per_period_3 <- sim_3$summary[period > 0, mean(intraindividual)]
  ic_per_period_5 <- sim_5$summary[period > 0, mean(intraindividual)]
  expect_equal(ic_per_period_3, ic_per_period_5, tolerance = 1e-2)
})
