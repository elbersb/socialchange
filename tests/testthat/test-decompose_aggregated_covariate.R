# Tests for the per-covariate breakdown of decompose_aggregated(): the record is a
# tidy per-(component, cell) table carrying each cell's covariates, and print()/plot()
# split all components -- including intraindividual change -- by a single covariate.

make_gendered_decomp <- function() {
  set.seed(12345)
  sim <- sim_social_change(
    periods = 5, data = build_gendered_population(),
    fun_y = make_gender_age_outcome(),
    fun_mortality = make_stable_mortality_gendered(),
    fun_coming_of_age = make_stable_coming_of_age_gendered()
  )
  stacked <- rbindlist(sim$snapshot)
  model <- lm(y ~ age * gender, data = stacked)
  set.seed(2024)
  decompose_aggregated(stacked, function(nd) predict(model, newdata = nd), "gender")
}

test_that("record is a tidy per-(component, cell) table carrying covariates", {
  d <- make_gendered_decomp()
  rec <- d$record[[1]]

  # Tidy shape: component, the covariates, delta -- and no leftover per-event columns.
  expect_setequal(names(rec), c("component", "age", "gender", "delta"))
  expect_false(anyNA(rec$gender))
  expect_false(anyNA(rec$age))
  # Intraindividual change is present per cell, and (component, cell) is unique.
  expect_gt(rec[component == "intraindividual", .N], 0)
  expect_equal(nrow(rec), nrow(unique(rec[, .(component, age, gender)])))
})

test_that("component_deltas per-covariate totals reconcile with the summary components", {
  d <- make_gendered_decomp()
  # Per-component totals summed across levels match the summary's component totals.
  byc <- component_deltas(d, "gender")[, .(total = sum(delta)), by = type]
  get <- function(ty) byc[type == ty, total]
  expect_equal(get("Mortality"), d$summary[2:.N, sum(mortality)], tolerance = 1e-9)
  expect_equal(get("Coming-of-age"), d$summary[2:.N, sum(coming_of_age)], tolerance = 1e-9)
  expect_equal(get("Intraindividual change"), d$summary[2:.N, sum(intraindividual)], tolerance = 1e-9)
})

test_that("component_deltas splits every component by covariate and reconciles with summary", {
  d <- make_gendered_decomp()
  cc <- component_deltas(d, "gender")

  # Intraindividual change is split by gender too, like the turnover components, and
  # only the components that fired in this (migration-free) sim are present.
  expect_setequal(cc[type == "Intraindividual change", unique(level)], c("f", "m"))
  expect_setequal(
    as.character(unique(cc$type)),
    c("Intraindividual change", "Mortality", "Coming-of-age")
  )

  # Summed over levels, IC equals the summary's IC total and the turnover series sum
  # to the summary's turnover total.
  ic <- cc[type == "Intraindividual change", sum(delta)]
  turn <- cc[type != "Intraindividual change", sum(delta)]
  expect_equal(ic, d$summary[2:.N, sum(intraindividual)], tolerance = 1e-9)
  expect_equal(
    turn,
    d$summary[2:.N, sum(mortality + coming_of_age + inmigration + outmigration)],
    tolerance = 1e-9
  )
})

test_that("per-cell intraindividual deltas sum to the scalar IC per transition", {
  d <- make_gendered_decomp()
  # The per-cell IC rows of each transition must sum to that transition's scalar IC.
  for (i in seq_along(d$record)) {
    ic_sum <- d$record[[i]][component == "intraindividual", sum(delta)]
    expect_equal(ic_sum, d$summary[i + 1, intraindividual], tolerance = 1e-12)
  }
})

test_that("print and plot accept covariate; invalid covariate errors", {
  d <- make_gendered_decomp()

  out <- capture.output(print(d, detailed = FALSE, covariate = "gender"))
  # Per-level columns (f, m) are appended to the decomposition table header.
  header <- grep("Component", out, value = TRUE)
  expect_match(header, "\\bf\\b.*\\bm\\b|\\bm\\b.*\\bf\\b")
  expect_true(any(grepl("Intraindividual change", out)))

  expect_s3_class(plot(d, covariate = "gender"), "ggplot")
  expect_s3_class(plot(d), "ggplot")

  # The covariate plot must not drop the mean lines (level = NA): building it emits
  # no "Removed ... rows" warning, and both facets carry drawn lines.
  b <- expect_no_warning(ggplot2::ggplot_build(plot(d, covariate = "gender")))
  line_layer <- which(vapply(b$plot$layers, function(L) inherits(L$geom, "GeomLine"), logical(1)))
  expect_setequal(unique(as.integer(b$data[[line_layer]]$PANEL)), c(1L, 2L))

  expect_error(print(d, covariate = "nope"), "Must be element of set")
  expect_error(plot(d, covariate = "nope"), "Must be element of set")
})
