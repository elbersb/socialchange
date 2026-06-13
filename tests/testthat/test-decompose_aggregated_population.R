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

test_that("zero-count young cells in the population frame do not affect classification", {
  # A WPP-style frame lists young ages with n = 0. These rows must not drag
  # min_age down to 0: otherwise no cohort is classified as "new" and the
  # entering cohort's growth is misrouted from coming-of-age to in-migration.
  fx <- build_population_fixture()
  pop <- fx$stacked[, .(n = sum(n)), by = .(age, period)]

  # pad every period with zero-count rows below the minimum populated age (20)
  padding <- CJ(age = 0:19, period = pop[, unique(period)])[, n := 0]
  pop_padded <- rbind(pop, padding)

  set.seed(999)
  unpadded <- decompose_aggregated(fx$stacked, fx$fun_y, population = pop)
  set.seed(999)
  padded <- decompose_aggregated(fx$stacked, fx$fun_y, population = pop_padded)

  # zero-count rows are inert: events, RNG draws and means are bit-for-bit equal
  expect_equal(padded$summary, unpadded$summary)
  expect_equal(padded$record, unpadded$record)

  # and the entering cohort is credited to coming-of-age, not in-migration
  expect_true(padded$summary[2:.N, sum(coming_of_age)] != 0)
  expect_equal(padded$summary[2:.N, sum(inmigration)], 0)
})

test_that("population frame must share the survey's minimum age", {
  fx <- build_population_fixture()
  pop <- fx$stacked[, .(n = sum(n)), by = .(age, period)]

  # drop the youngest survivors from the survey so its min age rises above pop's
  survey_high <- fx$stacked[age > min(age) + 1]
  expect_error(
    decompose_aggregated(survey_high, fx$fun_y, population = pop),
    "minimum age"
  )
})

test_that("population frame must share each cell column's level set", {
  set.seed(1)
  sim <- sim_social_change(periods = 5, data = build_gendered_population(),
    fun_y = make_age_only_outcome(),
    fun_mortality = make_stable_mortality_gendered(),
    fun_coming_of_age = make_stable_coming_of_age_gendered())
  stacked <- rbindlist(sim$snapshot)
  model <- lm(y ~ age, data = stacked)
  fun_y <- function(nd) predict(model, newdata = nd)

  pop <- stacked[, .(n = sum(n)), by = .(age, period, gender)]
  pop_one_gender <- pop[gender == pop$gender[1]]
  expect_error(
    decompose_aggregated(stacked, fun_y, cells = "gender", population = pop_one_gender),
    "levels of cell column 'gender'"
  )
})

test_that("population frame must cover every survey period, but may have more", {
  fx <- build_population_fixture()
  pop <- fx$stacked[, .(n = sum(n)), by = .(age, period)]
  survey_periods <- pop[, unique(period)]

  # missing a survey period is an error
  pop_missing <- pop[period != max(survey_periods)]
  expect_error(
    decompose_aggregated(fx$stacked, fx$fun_y, population = pop_missing),
    "missing survey period"
  )

  # extra population periods are dropped, giving the same result
  extra <- copy(pop[period == max(survey_periods)])[, period := max(survey_periods) + 4]
  pop_extra <- rbind(pop, extra)

  set.seed(999)
  base <- decompose_aggregated(fx$stacked, fx$fun_y, population = pop)
  set.seed(999)
  with_extra <- decompose_aggregated(fx$stacked, fx$fun_y, population = pop_extra)
  expect_equal(with_extra$summary, base$summary)
  expect_equal(with_extra$record, base$record)
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
