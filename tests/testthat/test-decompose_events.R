test_that("decompose_events works when all events share one time point", {
  events <- data.table(
    country = c("A", "B"),
    year = c(1995, 1995),
    event_type = c("entry", "entry")
  )
  outcomes <- CJ(country = c("A", "B"), year = 1995:2000)
  outcomes[, gdp := year - 1990 + (country == "B")]

  result <- decompose_events(events, outcomes, gdp ~ country + year, end_period = 2000)

  expect_s3_class(result, "decompose_events")
  # no replacement events, so all change is within-unit
  expect_equal(result$decomp[event_type == "change", pct], 100)
})

test_that("decompose_events excludes first-step exits from the initial population", {
  events <- data.table(
    country = c("A", "B", "C"),
    year = c(1995, 1995, 1995),
    event_type = c("entry", "entry", "exit")
  )
  outcomes <- CJ(country = c("A", "B", "C"), year = 1995:2000)
  outcomes[, gdp := fifelse(country == "A", 5, fifelse(country == "B", 7, 15))]

  result <- decompose_events(events, outcomes, gdp ~ country + year, end_period = 2000)

  # initial population is A/B only (mean 6), not A/B/C (mean 9)
  expect_equal(result$long[order == 1 & type == "pre", outcome], 6)
})

test_that("decompose_events counts non-entry founding events (eu_membership uses 'initial')", {
  events <- data.table(
    country = c("A", "B", "C"),
    year = c(1995, 1995, 2000),
    event_type = c("initial", "initial", "entry")
  )
  outcomes <- CJ(country = c("A", "B", "C"), year = 1995:2005)
  outcomes[, gdp := fifelse(country == "A", 5, fifelse(country == "B", 7, 15))]

  result <- decompose_events(events, outcomes, gdp ~ country + year, end_period = 2005)

  expect_equal(result$long[order == 1 & type == "pre", outcome], 6)
  # C joining in 2000 is the replacement component: mean jumps from 6 to 9
  expect_equal(result$decomp[event_type == "replacement", term], 3)
})
