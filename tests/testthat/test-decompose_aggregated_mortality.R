# Tests for the `mortality` argument of decompose_aggregated(): external annual
# death probabilities close the survivor-cell balancing identity
# n2 = n1 - deaths + net migration directly ("residual migration"), instead of
# routing each cell's net change by sign ("sign attribution"). Deaths are
# round(n1 * qtilde) with qtilde compounded over the gap years; migration is the
# signed residual, so out-migration appears for the first time.

# A two-wave frame with y_pred, ready for align_periods(); ages 20..25, gap 1,
# top age 25 (the open cell pools period-1 ages 24-25).
build_aligned_fixture <- function() {
  frame <- CJ(period = c(2000, 2001), age = 20:25)
  frame[, n := c(
    100, 100, 100, 100, 50, 100, # period 1
    90, 95, 80, 105, 100, 140 # period 2
  )]
  frame[, y := (age - 20) / 20]
  model <- lm(y ~ age, data = frame)
  frame[, y_pred := predict(model, frame)]
  frame[, y := NULL]
  list(frame = frame, model = model)
}

test_that("derive_events with mortality books hand-computed deaths and a signed residual", {
  fx <- build_aligned_fixture()
  aligned <- socialchange:::align_periods(fx$frame, c(2000, 2001), 1L, 1, "age", fx$model, 25)

  mort <- data.table(period = 2000, age = 20:25, prob = c(0.05, 0.1, 0.02, 0.05, 0.2, 0.5))
  prepared <- socialchange:::prepare_mortality(mort, character(0), fx$frame, c(2000, 2001), 20, 25)
  ev <- socialchange:::derive_events(aligned, 20, 1, prepared)
  setorder(ev, age)

  # entering cohort (aligned 19, recorded 20 in period 2): all growth is coming-of-age
  expect_equal(ev[age == 19, .(coming_of_age, mortality, inmigration, outmigration)],
    data.table(coming_of_age = 90, mortality = 0, inmigration = 0, outmigration = 0),
    ignore_attr = TRUE)

  # survivors: deaths = round(n1 * q), migration the signed residual n2 - (n1 - deaths)
  expect_equal(ev[age == 20, mortality], 5) # 100 * 0.05; net 95 - 95 = 0
  expect_equal(ev[age == 20, c(inmigration, outmigration)], c(0, 0))
  expect_equal(ev[age == 21, mortality], 10) # net 80 - 90 = -10 -> out-migration
  expect_equal(ev[age == 21, c(inmigration, outmigration)], c(0, 10))
  # a cell can now carry BOTH deaths and in-migration
  expect_equal(ev[age == 22, mortality], 2) # net 105 - 98 = +7 -> in-migration
  expect_equal(ev[age == 22, c(inmigration, outmigration)], c(7, 0))
  expect_equal(ev[age == 23, mortality], 5) # net 100 - 95 = +5
  expect_equal(ev[age == 23, c(inmigration, outmigration)], c(5, 0))

  # open group at 25 (pools period-1 ages 24-25): per-constituent expected deaths
  expect_equal(ev[age == 25, mortality], round(50 * 0.2 + 100 * 0.5)) # 60
  expect_equal(ev[age == 25, c(inmigration, outmigration)], c(50, 0)) # 140 - (150 - 60)

  # the integer identity holds exactly in every cell (entering cohorts
  # reconcile via coming-of-age)
  expect_equal(ev[, n2], ev[, n1 + coming_of_age - mortality + inmigration - outmigration])
})

test_that("multi-year gaps compound annual survival with age and year advancing", {
  frame <- CJ(period = c(2000, 2002), age = 20:24)
  frame[, n := 100L]
  frame[, y := (age - 20) / 20]
  model <- lm(y ~ age, data = frame)
  frame[, y_pred := predict(model, frame)]
  frame[, y := NULL]
  aligned <- socialchange:::align_periods(frame, c(2000, 2002), 1L, 2, "age", model, 24)

  # q(a, t) distinct in both age and year: 0.01 * (a - 19) + 0.1 * (t - 2000)
  mort <- CJ(period = 2000:2001, age = 20:24)
  mort[, prob := 0.01 * (age - 19) + 0.1 * (period - 2000)]
  q <- function(a, t) mort[age == a & period == t, prob]
  prepared <- socialchange:::prepare_mortality(mort, character(0), frame, c(2000, 2002), 20, 24)
  ev <- socialchange:::derive_events(aligned, 20, 2, prepared)

  # a survivor aged a in 2000 is aged a + 1 in 2001: qtilde = 1 - (1 - q1)(1 - q2)
  expect_equal(ev[age == 20, mortality], round(100 * (1 - (1 - q(20, 2000)) * (1 - q(21, 2001)))))
  expect_equal(ev[age == 21, mortality], round(100 * (1 - (1 - q(21, 2000)) * (1 - q(22, 2001)))))

  # open group (period-1 ages 22..24): each constituent compounds its own path,
  # with lookup ages clamped at the table's maximum (age 24 looks up 25 -> 24)
  qt <- function(a) 1 - (1 - q(a, 2000)) * (1 - q(min(a + 1, 24), 2001))
  expect_equal(ev[age == 24, mortality], round(100 * (qt(22) + qt(23) + qt(24))))

  # A population extending past the survey's outcome top retains true ages for
  # mortality while align_periods() pools their outcomes at age 24.
  frame_ext <- CJ(period = c(2000, 2002), age = 20:26)
  frame_ext[, `:=`(n = 100L, y_pred = (pmin(age, 24) - 20) / 20)]
  mort_ext <- CJ(period = 2000:2001, age = 20:26)
  mort_ext[, prob := 0.01 * (age - 19) + 0.1 * (period - 2000)]
  prepared_ext <- socialchange:::prepare_mortality(
    mort_ext, character(0), frame_ext, c(2000, 2002), 20, 26
  )
  aligned2 <- socialchange:::align_periods(frame_ext, c(2000, 2002), 1L, 2, "age", model, 24)
  ev_ext <- socialchange:::derive_events(aligned2, 20, 2, prepared_ext)
  qe <- function(a) {
    q1 <- mort_ext[age == a & period == 2000, prob]
    q2 <- mort_ext[age == min(a + 1, 26) & period == 2001, prob]
    1 - (1 - q1) * (1 - q2)
  }
  expect_equal(ev_ext[age == 24, mortality], round(100 * sum(vapply(22:26, qe, numeric(1)))))
  expect_gt(ev_ext[age == 24, mortality], ev[age == 24, mortality])
})

test_that("annual mortality events stay in their calendar-year bands", {
  frame <- CJ(period = c(2000, 2002), age = 20:24)
  frame[, `:=`(n = 100L, y = age / 100)]
  model <- lm(y ~ age, data = frame)
  frame[, y_pred := predict(model, frame)]
  frame[, y := NULL]
  aligned <- socialchange:::align_periods(frame, c(2000, 2002), 1L, 2, "age", model, 24)

  first <- CJ(period = 2000:2001, age = 20:24)
  first[, prob := fifelse(period == 2000, 0.5, 0)]
  last <- copy(first)[, prob := fifelse(period == 2001, 0.5, 0)]
  first <- socialchange:::prepare_mortality(first, character(0), frame, c(2000, 2002), 20, 24)
  last <- socialchange:::prepare_mortality(last, character(0), frame, c(2000, 2002), 20, 24)

  ev_first <- socialchange:::derive_events(copy(aligned), 20, 2, first)
  ev_last <- socialchange:::derive_events(copy(aligned), 20, 2, last)
  by_first <- attr(ev_first, "mortality_by_band")
  by_last <- attr(ev_last, "mortality_by_band")

  expect_gt(sum(by_first[, 1]), 0)
  expect_equal(sum(by_first[, 2]), 0)
  expect_equal(sum(by_last[, 1]), 0)
  expect_gt(sum(by_last[, 2]), 0)
  expect_equal(rowSums(by_first), ev_first$mortality)
  expect_equal(rowSums(by_last), ev_last$mortality)

  set.seed(1)
  schedule_first <- socialchange:::schedule_events(ev_first, 20, 2)
  set.seed(1)
  schedule_last <- socialchange:::schedule_events(ev_last, 20, 2)
  expect_true(all(schedule_first$events_tick[schedule_first$ev_type == "mortality"] < 0.5))
  expect_true(all(schedule_last$events_tick[schedule_last$ev_type == "mortality"] > 0.5))
})

test_that("mortality by period x age broadcasts over cell columns not present", {
  frame <- CJ(period = c(2000, 2001), age = 20:25, sex = c("f", "m"))
  frame[, n := 100L]
  frame[, y := (age - 20) / 20 + 0.1 * (sex == "m")]
  model <- lm(y ~ age + sex, data = frame)
  frame[, y_pred := predict(model, frame)]
  frame[, y := NULL]
  cells <- c("sex", "age")
  aligned <- socialchange:::align_periods(frame, c(2000, 2001), 1L, 1, cells, model, 25)

  mort <- data.table(period = 2000, age = 20:25, prob = 0.1)
  prepared <- socialchange:::prepare_mortality(mort, "sex", frame, c(2000, 2001), 20, 25)
  ev <- socialchange:::derive_events(aligned, 20, 1, prepared)

  # both sexes get the same qtilde, hence identical deaths on identical counts
  setorder(ev, sex, age)
  expect_equal(ev[sex == "f", mortality], ev[sex == "m", mortality])
  expect_equal(ev[sex == "f" & age == 22, mortality], 10) # round(100 * 0.1)
  expect_equal(ev[sex == "f" & age == 25, mortality], 20) # open group: 100 * 0.1 twice
})

# End-to-end fixture: three waves with gap 2 then 1, a sex cell, and counts that
# exercise both residual signs. y is linear in age and sex, so the lm is exact.
build_e2e_fixture <- function() {
  stacked <- CJ(period = c(2000, 2002, 2003), age = 20:24, sex = c("f", "m"))
  stacked[, n := 100L]
  stacked[period == 2002 & age == 23, n := 60L] # shrinks past deaths -> out-migration
  stacked[period == 2003 & age == 22, n := 130L] # grows -> in-migration
  stacked[, y := (age - 20) / 20 + 0.1 * (sex == "m")]
  model <- lm(y ~ age + sex, data = stacked)
  mort <- CJ(period = 2000:2002, age = 20:24, sex = c("f", "m"))
  mort[, prob := 0.05]
  pop <- stacked[, .(period, age, sex, n)]
  list(stacked = stacked, model = model, mort = mort, pop = pop)
}

test_that("mortality input is validated", {
  fx <- build_e2e_fixture()
  run <- function(mort) {
    decompose_aggregated(fx$stacked, fx$model, cells = "sex", population = fx$pop, mortality = mort)
  }

  # the full table passes
  set.seed(1)
  expect_no_error(run(fx$mort))

  # stray column not in `cells`
  expect_error(run(copy(fx$mort)[, educ := "hs"]), "not in `cells`: educ")

  # missing between-waves calendar year (2001 is not a survey year)
  expect_error(run(fx$mort[period != 2001]), "every calendar year")

  # mortality and population must share one terminal open age
  expect_error(run(fx$mort[age <= 22]), "share one terminal open age")
  expect_error(
    run(rbind(fx$mort, copy(fx$mort[age == 24])[, age := 25L])),
    "share one terminal open age"
  )

  # a single missing age within the required range
  expect_error(run(fx$mort[!(period == 2001 & age == 23)]), "period 2001, age 23")

  # marginal levels are complete, but one joint cell combination is absent
  stacked2 <- CJ(period = c(2000, 2001), age = 20:24,
    sex = c("f", "m"), group = c("a", "b"))
  stacked2[, `:=`(n = 100L, y = age / 100)]
  model2 <- lm(y ~ age, data = stacked2)
  mort2 <- CJ(period = 2000, age = 20:24,
    sex = c("f", "m"), group = c("a", "b"))[
      !(sex == "m" & group == "b")
    ][, prob := 0.05]
  expect_error(
    decompose_aggregated(stacked2, model2, cells = c("sex", "group"), mortality = mort2),
    "combination\\(s\\) are missing"
  )

  # data has a cell level the mortality frame lacks
  expect_error(run(fx$mort[sex == "f"]), "missing level\\(s\\) of cell column 'sex': m")

  # prob out of range
  expect_error(run(copy(fx$mort)[1, prob := 1.5]), "prob")
  expect_error(run(copy(fx$mort)[1, prob := NA_real_]), "prob")

  # duplicate (period, age, cells) keys
  expect_error(run(rbind(fx$mort, fx$mort[1])), "duplicate")
})

test_that("residual migration end-to-end: exact accounting, out-migration in record and draws", {
  fx <- build_e2e_fixture()
  set.seed(42)
  res <- suppressMessages(decompose_aggregated(
    fx$stacked, fx$model, cells = "sex", population = fx$pop, mortality = fx$mort,
    R = 5, seed = 7
  ))

  expect_equal(res$strategy, "residual migration")

  # components (including out-migration) sum exactly to the total modeled change
  total <- res$summary[.N, modeled_mean] - res$summary[1, modeled_mean]
  comps <- res$summary[2:.N, sum(
    intraindividual + coming_of_age + mortality + inmigration + outmigration
  )]
  expect_equal(comps, total, tolerance = 1e-10)

  # the engineered flows appear: out-migration in transition 1, in-migration in transition 2
  expect_gt(sum(res$record[[1]]$component == "outmigration"), 0)
  expect_gt(sum(res$record[[2]]$component == "inmigration"), 0)

  # bootstrap draws carry out-migration rows too
  expect_gt(res$draws[component == "outmigration", .N], 0)
})

test_that("population and mortality combine: frame counts drive the derived deaths", {
  fx <- build_e2e_fixture()
  # a frame that doubles every count relative to the survey
  pop2 <- copy(fx$pop)[, n := n * 2L]

  set.seed(42)
  res <- decompose_aggregated(fx$stacked, fx$model, cells = "sex",
    population = pop2, mortality = fx$mort)
  expect_equal(res$strategy, "residual migration")

  # the modeled mean is weighted by the frame, and the accounting stays exact
  total <- res$summary[.N, modeled_mean] - res$summary[1, modeled_mean]
  comps <- res$summary[2:.N, sum(
    intraindividual + coming_of_age + mortality + inmigration + outmigration
  )]
  expect_equal(comps, total, tolerance = 1e-10)

  # deaths derive from the frame's counts: derive_events on the doubled frame
  # books twice the expected deaths of the survey-count frame (q = 0.05, exact)
  frame <- pop2[, .(n = sum(n), y_pred = 0), by = .(period, sex, age)]
  aligned <- socialchange:::align_periods(
    frame, c(2000, 2002, 2003), 2L, 1, c("sex", "age"), fx$model, 24
  )
  prepared <- socialchange:::prepare_mortality(fx$mort, "sex", frame, c(2000, 2002, 2003), 20, 24)
  ev <- socialchange:::derive_events(aligned, 20, 1, prepared)
  expect_equal(ev[age == 21 & sex == "f", mortality], round(2 * 100 * 0.05))
})

test_that("strategy is recorded and printed in both modes", {
  fx <- build_e2e_fixture()
  set.seed(1)
  res_default <- decompose_aggregated(fx$stacked, fx$model, cells = "sex")
  set.seed(1)
  res_mort <- decompose_aggregated(
    fx$stacked, fx$model, cells = "sex", mortality = fx$mort
  )

  expect_equal(res_default$strategy, "sign attribution")
  expect_equal(res_mort$strategy, "residual migration")
  expect_output(print(res_default), "Strategy: sign attribution")
  expect_output(print(res_mort), "Strategy: residual migration")

  # the default path is unchanged: no out-migration is ever inferred
  expect_equal(res_default$summary[2:.N, sum(outmigration)], 0)
})
