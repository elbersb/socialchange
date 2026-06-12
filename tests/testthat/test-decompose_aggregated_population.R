# Tests for the `population` argument of decompose_aggregated(): an external
# cell x period count table that replaces the survey-derived cell counts as the
# population frame, while fun_y still supplies the outcomes.

# Build a survey-like stacked data set + a fitted model once for reuse.
build_population_fixture <- function() {
  set.seed(12345)
  sim <- sim_social_change(periods = 5, data = build_simple_population(),
    fun_y = make_age_only_outcome(),
    fun_mortality = make_stable_mortality_simple(),
    fun_coming_of_age = make_stable_coming_of_age_simple())
  stacked <- rbindlist(sim$snapshot)
  model <- lm(y ~ age, data = stacked)
  list(stacked = stacked, fun_y = function(nd) predict(model, newdata = nd))
}

test_that("supplying the survey's own cell counts as population reproduces the default", {
  fx <- build_population_fixture()

  # population frame == the survey's own cell counts (age x period)
  pop <- fx$stacked[, .(n = sum(n)), by = .(age, period)]

  set.seed(999)
  default <- decompose_aggregated(fx$stacked, fx$fun_y)
  set.seed(999)
  with_pop <- decompose_aggregated(fx$stacked, fx$fun_y, population = pop)

  # frame counts are identical, so events, RNG draws and means all match
  expect_equal(with_pop$summary, default$summary)
  expect_equal(with_pop$record, default$record)
})

test_that("an external population frame drives the modeled mean and event counts", {
  fx <- build_population_fixture()

  # double the youngest cohort each period -> different age structure than survey
  pop <- fx$stacked[, .(n = sum(n)), by = .(age, period)]
  pop[age == min(age), n := n * 2]

  set.seed(999)
  default <- decompose_aggregated(fx$stacked, fx$fun_y)
  set.seed(999)
  with_pop <- decompose_aggregated(fx$stacked, fx$fun_y, population = pop)

  # modeled mean is now weighted by the population frame, not the survey
  expect_true(any(with_pop$summary$modeled_mean != default$summary$modeled_mean))

  # and it matches a direct population-weighted prediction
  pop[, y_pred := fx$fun_y(.SD)]
  expected_modeled <- pop[, .(m = weighted.mean(y_pred, n)), by = period][order(period)]
  got <- with_pop$summary[order(period)]
  expect_equal(got$modeled_mean, expected_modeled$m)

  # observed_mean stays the survey's own observed mean (a reference line)
  survey_obs <- fx$stacked[, .(m = weighted.mean(y, n)), by = period][order(period)]
  expect_equal(got$observed_mean, survey_obs$m)
})

test_that("decomposition components sum to the total modeled change with a population frame", {
  fx <- build_population_fixture()
  pop <- fx$stacked[, .(n = sum(n)), by = .(age, period)]
  pop[age == min(age), n := n * 2]

  set.seed(999)
  res <- decompose_aggregated(fx$stacked, fx$fun_y, population = pop)

  total <- res$summary[.N, modeled_mean] - res$summary[1, modeled_mean]
  components <- res$summary[2:.N, sum(
    intraindividual + coming_of_age + mortality + inmigration + outmigration
  )]
  expect_equal(components, total)
})

test_that("population accepts a plain data.frame and validates required columns", {
  fx <- build_population_fixture()
  pop <- fx$stacked[, .(n = sum(n)), by = .(age, period)]

  # plain data.frame is coerced internally
  set.seed(999)
  expect_no_error(decompose_aggregated(fx$stacked, fx$fun_y,
    population = as.data.frame(pop)))

  # missing required column (n) is rejected
  set.seed(999)
  expect_error(
    decompose_aggregated(fx$stacked, fx$fun_y, population = pop[, .(age, period)]),
    "n"
  )
})
