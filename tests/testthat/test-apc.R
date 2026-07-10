# Balanced age x period grid with cohort = period - age
make_apc_data <- function(n_periods) {
  grid <- CJ(age = 20:24, period = 2000 + seq_len(n_periods) - 1)
  grid[, cohort := period - age]
  grid[, y := 0.1 * age + 0.05 * (period - 2000) + rnorm(.N, sd = 0.01)]
  grid
}

test_that("apc_nonlinearities errors when a dimension has fewer than 3 levels", {
  set.seed(1)
  m <- apc(make_apc_data(2), y ~ age + period + cohort)
  expect_error(apc_nonlinearities(m), "period has only 2 levels")
})

test_that("apc_total validates the assumption argument", {
  set.seed(1)
  m <- apc(make_apc_data(3), y ~ age + period + cohort)
  expect_error(apc_total(m, 0.01), "named numeric")
  expect_error(apc_total(m, c(foo = 0.01)), "named numeric")
  expect_error(apc_total(m, "age_linear"), "numeric")

  res <- apc_total(m, c(age_linear = 0.01))
  expect_named(res, c("age", "period", "cohort"))
})
