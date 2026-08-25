# Tests for the open age interval in decompose_aggregated(): the shared maximum
# age T is always treated as "T+" (the demographic top-code). The period-2 cell
# recorded at T pools the survivors of period-1 ages T-gap..T, so align_periods()
# collapses those period-1 cells into one open-group cell at T instead of
# orphaning them (phantom mortality at the top, phantom in-migration at T-gap),
# and the model is never asked to predict past T.

test_that("align_periods pools the top ages into one open-group cell, merge width following the gap", {
  # A top-coded frame: recorded ages 20..30 per sex; the 30-cell is the "30+" pool.
  frame <- CJ(period = c(2000, 2001, 2003, 2006), age = 20:30, sex = c("f", "m"))
  frame[, n := 100L + 10L * (age == 30)] # pool bigger than the single-age cells
  frame[, y := (age - 20) / 20 + 0.1 * (sex == "m")]
  model <- lm(y ~ age + sex, data = frame)
  frame[, y_pred := predict(model, frame)]
  frame[, y := NULL]

  periods <- c(2000, 2001, 2003, 2006)
  cells <- c("sex", "age")

  # gap = 1: period-1 ages 29..30 merge into the open cell.
  a1 <- socialchange:::align_periods(frame, periods, 1L, 1, cells, model, 30)
  open1 <- a1[age == 30]
  expect_equal(nrow(open1), 2L) # one open cell per sex
  expect_equal(open1$n1, rep(100 + 110, 2)) # ages 29 + 30 summed
  expect_equal(open1$n2, rep(110, 2)) # the period-2 pool recorded at 30
  # 29 merged away; entering cohorts align below 20; no orphans above the pool
  expect_setequal(a1$age, c(19:28, 30))
  expect_false(any(a1[age >= 25, n2 == 0])) # every top cell has survivors to match
  expect_equal(a1[age == 25 & sex == "f", n2], 100) # one-to-one below the pool

  # gap = 2 and gap = 3: the merge width follows the gap.
  a2 <- socialchange:::align_periods(frame, periods, 2L, 2, cells, model, 30)
  expect_equal(a2[age == 30 & sex == "f", n1], 100 + 100 + 110) # ages 28..30
  expect_setequal(a2[n1 > 0]$age, c(20:27, 30))
  a3 <- socialchange:::align_periods(frame, periods, 3L, 3, cells, model, 30)
  expect_equal(a3[age == 30 & sex == "f", n1], 3 * 100 + 110) # ages 27..30
  expect_setequal(a3[n1 > 0]$age, c(20:26, 30))

  # The open cell's tick-0 y is the n-weighted mean of its constituents'
  # predictions, so the transition's start mean equals the period's modeled mean.
  wexp <- frame[period == 2001 & sex == "f" & age >= 28, weighted.mean(y_pred, n)]
  expect_equal(a2[age == 30 & sex == "f", y], wexp)
})

test_that("a top-coded stationary population books honest deaths at the pool, no phantom events", {
  # True world: ages 20..35, 100 per age, stationary (100 enter at 20, the 100
  # aged 35 die each year). Recording top-codes at 30, so the 30-cell pools true
  # ages 30..35 (n = 600) and every wave is identical. The one-to-one aligner
  # used to orphan the pool into +500 phantom in-migration at 29 and 600 phantom
  # deaths at 30; the open-interval merge books exactly the 100 true deaths.
  stacked <- CJ(period = 2000:2002, age = 20:30)
  stacked[, n := fifelse(age == 30, 600L, 100L)]
  stacked[, y := (age - 20) / 20]
  model <- lm(y ~ age, data = stacked)

  frame <- copy(stacked)[, y_pred := predict(model, .SD)]
  aligned <- socialchange:::align_periods(frame, 2000:2002, 1L, 1, "age", model, 30)
  ev <- socialchange:::derive_events(aligned, 20, 1)
  expect_equal(ev[, sum(inmigration)], 0)
  expect_equal(ev[, sum(mortality)], 100)
  expect_equal(ev[age == 30, mortality], 100)
  expect_equal(ev[age < 20, sum(coming_of_age)], 100)

  set.seed(1)
  decomp <- decompose_aggregated(stacked, model)
  expect_equal(decomp$summary[period > 2000, sum(inmigration)], 0)
  expect_equal(decomp$summary[period > 2000, sum(outmigration)], 0)

  # Components sum exactly to the total modeled change, with the pool active.
  total <- decomp$summary[.N, modeled_mean] - decomp$summary[1, modeled_mean]
  comps <- decomp$summary[period > 2000, sum(
    intraindividual + coming_of_age + mortality + inmigration + outmigration
  )]
  expect_equal(comps, total, tolerance = 1e-10)

  # The record carries one pooled contribution at T: mortality appears only
  # there, and no cell sits above T.
  rec <- decomp$record[[1]]
  expect_lte(max(rec$age), 30)
  expect_equal(rec[component == "mortality", unique(age)], 30)
})

test_that("the model is never asked to predict past the maximum age", {
  # A wrapper model that errors on any prediction request beyond the top age:
  # the age clamp in build_event_stack() must keep every mid-gap evaluation --
  # including the open group's -- within the data's age support.
  stacked <- CJ(period = c(2000, 2003), age = 20:30)
  stacked[, n := 100L]
  stacked[, y := (age - 20) / 20]
  model <- lm(y ~ age, data = stacked)
  class(model) <- c("agecapped", class(model))
  registerS3method("predict", "agecapped", function(object, newdata = NULL, ...) {
    if (!is.null(newdata) && any(newdata$age > 30)) {
      stop("prediction requested past the maximum age")
    }
    class(object) <- setdiff(class(object), "agecapped")
    predict(object, newdata = newdata, ...)
  })

  set.seed(1)
  expect_no_error(decompose_aggregated(stacked, model))
})

test_that("waves must share a common maximum age", {
  ragged <- rbindlist(list(
    data.table(age = 20:30, period = 2000),
    data.table(age = 20:32, period = 2004)
  ))
  ragged[, y := (age - 20) / 20]
  model <- lm(y ~ age, data = ragged)
  expect_error(
    decompose_aggregated(ragged, model),
    "common maximum age"
  )
})

test_that("population can extend above the survey's open outcome age", {
  survey <- CJ(period = c(2000, 2004), age = 20:30)
  survey[, n := 100L]
  survey[, y := (age - 20) / 20]
  model <- lm(y ~ age, data = survey)

  # Ages 31:35 are demographic constituents of the survey's 30+ outcome cell.
  # Their predictions must be clamped to 30, not extrapolated.
  pop <- CJ(period = c(2000, 2004), age = 20:35)[, n := 100]
  set.seed(1)
  res <- decompose_aggregated(survey, model, population = pop)
  expected <- weighted.mean((pmin(20:35, 30) - 20) / 20, rep(100, 16))
  expect_equal(res$summary$modeled_mean, rep(expected, 2))
  expect_lte(max(res$record[[1]]$age), 30)

  # Bootstrap constituent predictions use the same clamp. With identical period
  # frames and an age-only model, every draw must telescope to zero total change.
  set.seed(1)
  boot <- suppressMessages(decompose_aggregated(
    survey, model, population = pop, R = 3, seed = 2
  ))
  expect_equal(boot$draws[, .(total = sum(delta)), by = draw]$total, rep(0, 3), tolerance = 1e-10)

  short <- CJ(period = c(2000, 2004), age = 20:29)[, n := 100]
  expect_error(
    decompose_aggregated(survey, model, population = short),
    "must reach at least"
  )

  gap <- pop[age != 33]
  expect_error(
    decompose_aggregated(survey, model, population = gap),
    "must be gap-free"
  )
})

test_that("per-draw deltas sum to that replicate's modeled change (open-group start weighting)", {
  # The bootstrap start outcomes must weight the open group's constituents per
  # replicate, or each draw's deltas would not telescope to that replicate's own
  # start-to-end change in the modeled mean.
  set.seed(3)
  d <- CJ(period = c(2000, 2004), age = 20:40)
  d[, n := sample(80:120, .N, TRUE)]
  d[, y := 0.02 * (age - 20) + 0.01 * (period == 2004) + stats::rnorm(.N, 0, 0.01)]
  m <- lm(y ~ age + period, data = d)

  set.seed(9)
  res <- suppressMessages(decompose_aggregated(d, m, R = 5, seed = 11, tol = 0.1))

  # Same seed -> the same replicate coefficients as inside the decomposition.
  reps <- socialchange:::y_replicates(m, 5, seed = 11)
  pred <- socialchange:::replicate_predict(reps, d) # nrow(d) x 5
  w <- d$n
  p1 <- d$period == 2000
  expected <- apply(pred, 2, function(yk) {
    weighted.mean(yk[!p1], w[!p1]) - weighted.mean(yk[p1], w[p1])
  })
  got <- res$draws[, .(total = sum(delta)), keyby = draw]$total
  expect_equal(got, expected, tolerance = 1e-9, ignore_attr = TRUE)
})
