test_that("cr_ic errors clearly on periods/cohorts that don't parse as integers", {
  # labels would otherwise NA-coerce silently and surface later as "0 (non-NA) cases"
  d <- data.table(
    y = rnorm(12),
    year = rep(c("wave 1", "wave 2"), each = 6),
    cohort = rep(1950:1955, 2)
  )
  expect_error(cr_ic(d, y ~ year + cohort), "Column 'year'")

  d2 <- data.table(
    y = rnorm(12),
    year = rep(c(2000, 2001), each = 6),
    cohort = rep(seq(1950, 1952.5, by = 0.5), 2)
  )
  expect_error(cr_ic(d2, y ~ year + cohort), "Column 'cohort'")
})

test_that("cr_ic accepts factor periods/cohorts with integer labels", {
  d <- data.table(
    y = rnorm(12),
    year = factor(rep(c(2000, 2001), each = 6)),
    cohort = factor(rep(1950:1955, 2))
  )
  res <- cr_ic(d, y ~ year + cohort)
  expect_identical(res$periods, c(2000L, 2001L))
})
