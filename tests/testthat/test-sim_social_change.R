test_that("sim_social_change handles Scenario 1 (simple age-based)", {
  # ARRANGE
  set.seed(12345)
  data <- build_simple_population()
  fun_y <- make_age_only_outcome()
  fun_mortality <- make_stable_mortality_simple()
  fun_coming_of_age <- make_stable_coming_of_age_simple()

  # ACT
  result <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  # ASSERT - Structure
  expect_s3_class(result, "social_change_sim")
  expect_true(is.list(result))
  expect_named(result, c("summary", "snapshot", "record"))
  expect_s3_class(result$summary, "data.table")
  expect_true(is.list(result$snapshot))
  expect_true(is.list(result$record))

  # ASSERT - Dimensions
  expect_equal(nrow(result$summary), 6)  # initial + 5 periods
  expect_equal(length(result$snapshot), 6)

  # ASSERT - Values from vignette
  # Population is stable at 1550
  expect_equal(result$summary[.N, N], 1550)
  # Mean is stable at ~0.38 (within 2%)
  expect_equal(result$summary[.N, mean], 0.38, tolerance = 0.02)

  # Total IC and PT (from vignette: IC ≈ +0.25, PT ≈ -0.25)
  ic_total <- result$summary[period > 0, sum(intraindividual)]
  pt_total <- result$summary[period > 0, sum(mortality + coming_of_age)]
  expect_equal(ic_total, 0.25, tolerance = 1e-2)
  expect_equal(pt_total, -0.25, tolerance = 1e-2)

  # IC and PT should roughly cancel out
  expect_equal(ic_total + pt_total, 0, tolerance = 1e-2)
})

test_that("sim_social_change handles Scenario 2 (gender covariate)", {
  # ARRANGE
  set.seed(12345)
  data <- build_gendered_population()
  fun_y <- make_gender_age_outcome()
  fun_mortality <- make_stable_mortality_gendered()
  fun_coming_of_age <- make_stable_coming_of_age_gendered()

  # ACT
  result <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  # ASSERT - Structure
  expect_s3_class(result, "social_change_sim")
  expect_equal(nrow(result$summary), 6)

  # ASSERT - Values from vignette
  # Population is stable at 1550
  expect_equal(result$summary[.N, N], 1550)
  # Mean is stable at ~0.38 (within 2%)
  expect_equal(result$summary[.N, mean], 0.38, tolerance = 0.02)

  # IC and PT are half of Scenario 1 (because only men contribute)
  ic_total <- result$summary[period > 0, sum(intraindividual)]
  pt_total <- result$summary[period > 0, sum(mortality + coming_of_age)]
  expect_equal(ic_total, 0.125, tolerance = 1e-2)
  expect_equal(pt_total, -0.125, tolerance = 1e-2)
})

test_that("sim_social_change handles Scenario 3 (pure intraindividual change)", {
  # ARRANGE
  set.seed(12345)
  data <- build_gendered_population()
  fun_y <- make_time_only_outcome()
  fun_mortality <- make_stable_mortality_gendered()
  fun_coming_of_age <- make_stable_coming_of_age_gendered()

  # ACT
  result <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  # ASSERT - Structure
  expect_s3_class(result, "social_change_sim")
  expect_equal(nrow(result$summary), 6)

  # ASSERT - Values from vignette
  # Mean increases 0.1 per period
  initial_mean <- result$summary[period == 0, mean]
  final_mean <- result$summary[period == 5, mean]
  expect_equal(final_mean - initial_mean, 0.5, tolerance = 1e-2)

  # All change is IC, PT ≈ 0
  ic_total <- result$summary[period > 0, sum(intraindividual)]
  pt_total <- result$summary[period > 0, sum(mortality + coming_of_age)]
  expect_equal(ic_total, 0.5, tolerance = 1e-2)
  expect_equal(pt_total, 0, tolerance = 1e-2)
})

test_that("sim_social_change handles Scenario 4 (pure population turnover)", {
  # ARRANGE
  set.seed(12345)
  data <- build_gendered_population()
  fun_y <- make_cohort_only_outcome()
  fun_mortality <- make_stable_mortality_gendered()
  fun_coming_of_age <- make_stable_coming_of_age_gendered()

  # ACT
  result <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  # ASSERT - Structure
  expect_s3_class(result, "social_change_sim")
  expect_equal(nrow(result$summary), 6)

  # ASSERT - Values from vignette
  # Mean increases 0.1 per period (same as Scenario 3, but different mechanism)
  initial_mean <- result$summary[period == 0, mean]
  final_mean <- result$summary[period == 5, mean]
  expect_equal(final_mean - initial_mean, 0.5, tolerance = 1e-2)

  # All change is PT, IC ≈ 0
  ic_total <- result$summary[period > 0, sum(intraindividual)]
  pt_total <- result$summary[period > 0, sum(mortality + coming_of_age)]
  expect_equal(ic_total, 0, tolerance = 1e-2)
  expect_equal(pt_total, 0.5, tolerance = 1e-2)

  # Mortality and coming-of-age contribute roughly equally
  mort_total <- result$summary[period > 0, sum(mortality)]
  coa_total <- result$summary[period > 0, sum(coming_of_age)]
  expect_equal(mort_total, coa_total, tolerance = 0.08)
})

test_that("sim_social_change handles Scenario 5a (smoking, static)", {
  # ARRANGE
  set.seed(12345)
  data <- build_smoking_population()
  fun_y <- make_smoking_static_outcome()
  fun_mortality <- make_smoking_mortality()
  fun_coming_of_age <- make_smoking_coming_of_age_static()

  # ACT
  result <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  # ASSERT - Structure
  expect_s3_class(result, "social_change_sim")
  expect_equal(nrow(result$summary), 6)

  # ASSERT - Values from vignette
  # Population is stable, no net change
  # IC ≈ 0, PT ≈ 0 (but mortality and coming-of-age individually non-zero, canceling out)
  ic_total <- result$summary[period > 0, sum(intraindividual)]
  pt_total <- result$summary[period > 0, sum(mortality + coming_of_age)]
  expect_equal(ic_total, 0, tolerance = 1e-2)
  expect_equal(pt_total, 0, tolerance = 1e-2)

  # Mortality and coming-of-age should roughly cancel
  mort_total <- result$summary[period > 0, sum(mortality)]
  coa_total <- result$summary[period > 0, sum(coming_of_age)]
  expect_equal(mort_total + coa_total, 0, tolerance = 1e-2)
})

test_that("sim_social_change handles Scenario 5b (smoking, IC)", {
  # ARRANGE
  set.seed(12345)
  data <- build_smoking_population()
  fun_y <- make_smoking_time_outcome()
  fun_mortality <- make_smoking_mortality()
  fun_coming_of_age <- make_smoking_coming_of_age_static()

  # ACT
  result <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  # ASSERT - Structure
  expect_s3_class(result, "social_change_sim")
  expect_equal(nrow(result$summary), 6)

  # ASSERT - Values from vignette
  # Mean decreases from ~0.87 to ~0.59
  initial_mean <- result$summary[period == 0, mean]
  final_mean <- result$summary[period == 5, mean]
  expect_equal(initial_mean, 0.87, tolerance = 1e-2)
  expect_equal(final_mean, 0.59, tolerance = 1e-2)

  # All change is IC, PT ≈ 0
  ic_total <- result$summary[period > 0, sum(intraindividual)]
  pt_total <- result$summary[period > 0, sum(mortality + coming_of_age)]
  total_change <- final_mean - initial_mean
  expect_equal(ic_total, total_change, tolerance = 1e-2)
  expect_equal(pt_total, 0, tolerance = 1e-2)
})

test_that("sim_social_change handles Scenario 5c (smoking, varying coming-of-age)", {
  # ARRANGE
  set.seed(12345)
  data <- build_smoking_population()
  fun_y <- make_smoking_time_outcome()
  fun_mortality <- make_smoking_mortality()
  fun_coming_of_age <- make_smoking_coming_of_age_varying()

  # ACT
  result <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  # ASSERT - Structure
  expect_s3_class(result, "social_change_sim")
  expect_equal(nrow(result$summary), 6)

  # ASSERT - Values from vignette
  # PT component should now be larger than in 5b
  ic_total <- result$summary[period > 0, sum(intraindividual)]
  pt_total <- result$summary[period > 0, sum(mortality + coming_of_age)]

  # PT should be negative (more non-smokers coming of age decreases mean)
  expect_true(pt_total < 0)
  # IC should still be negative (time effect)
  expect_true(ic_total < 0)
  # PT magnitude should be larger than in 5b (where it was ~0)
  expect_true(abs(pt_total) > 0.05)
})

test_that("sim_social_change handles Scenario 5d (smoking, with transitions)", {
  # ARRANGE
  set.seed(12345)
  data <- build_smoking_population()
  fun_y <- make_smoking_time_outcome()
  fun_mortality <- make_smoking_mortality()
  fun_coming_of_age <- make_smoking_coming_of_age_varying()
  fun_transitions <- make_smoking_transitions()

  # ACT
  result <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age,
    fun_transitions = fun_transitions
  )

  # ASSERT - Structure
  expect_s3_class(result, "social_change_sim")
  expect_equal(nrow(result$summary), 6)

  # ASSERT - Values from vignette
  # IC should be larger than 5c due to smokers becoming non-smokers
  ic_total <- result$summary[period > 0, sum(intraindividual)]
  expect_true(ic_total < 0)  # Still negative (time effect)

  # Population should grow (fewer smokers → less mortality)
  initial_n <- result$summary[period == 0, N]
  final_n <- result$summary[period == 5, N]
  expect_true(final_n > initial_n)
})

test_that("sim_social_change print method works", {
  # ARRANGE
  set.seed(12345)
  data <- build_simple_population()
  fun_y <- make_age_only_outcome()
  fun_mortality <- make_stable_mortality_simple()
  fun_coming_of_age <- make_stable_coming_of_age_simple()

  result <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
  )

  # ACT & ASSERT
  expect_output(print(result), "Overview by period")
  expect_output(print(result, detailed = FALSE), "Component")
})

test_that("sim_social_change validates input", {
  # Missing required arguments should error
  expect_error(
    sim_social_change(periods = 5),
    "missing"
  )

  expect_error(
    sim_social_change(periods = 5, data = build_simple_population()),
    "missing"
  )
})

test_that("sim_social_change handles missing optional dynamics", {
  # Should work with only mortality
  set.seed(12345)
  data <- build_simple_population()
  fun_y <- make_age_only_outcome()
  fun_mortality <- make_stable_mortality_simple()

  result <- sim_social_change(
    periods = 2,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality
  )

  expect_s3_class(result, "social_change_sim")
  expect_equal(nrow(result$summary), 3)  # initial + 2 periods
})
