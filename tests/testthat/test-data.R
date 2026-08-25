test_that("GSS data retain respondents above the analytical age-81 top-code", {
  d <- as.data.table(gss_homosex)

  # The source GSS uses 89+. Older 1973 respondents must remain available so
  # pmin(age, 81) creates a genuine 81+ group instead of a hard cohort cutoff.
  expect_gt(d[year == 1973 & age > 81, .N], 0L)
  expect_true(all(d[, max(age), by = year]$V1 == 89))
  expect_lt(min(d$cohort), 1892)
})
