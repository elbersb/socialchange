test_that("GSS data cover the full series and retain the open age group", {
  d <- as.data.table(gss_homosex)

  expect_equal(range(d$year), c(1973, 2024))
  expect_length(intersect(d$year, c(1972, 1975, 1978, 1983, 1986)), 0L)
  expect_true("wtssps" %in% names(d))
  expect_false(any(c("wtssall", "sample") %in% names(d)))
  expect_false(anyNA(d$wtssps))
  expect_false(anyNA(d$sex))
  expect_true(anyNA(d$educ))
  expect_true(anyNA(d$marital))
  expect_true(anyNA(d$relig16))
  expect_gt(d[year == 1973 & age > 81, .N], 0L)
  expect_true(all(d[, max(age), by = year]$V1 == 89))
  expect_lt(min(d$cohort), 1892)
})

test_that("WPP data cover all GSS years and adult ages", {
  d <- as.data.table(wpp_us)

  expect_equal(range(d$period), c(1973, 2024))
  expect_equal(range(d$age), c(18, 89))
  expect_false(anyNA(d$n))
})
