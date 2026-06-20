# Tests for the Dirichlet-bootstrap standard errors of decompose_aggregated()
# (Feature (a): model uncertainty). The model is refit R times on Dirichlet-reweighted
# microdata; under common random numbers the point estimate is unchanged and the spread
# across draws (the `draws` table) gives the cumulative CI band print/plot render.

# A small stacked individual-level dataset with a near-linear age/period signal, a common
# minimum age, and a binary covariate, so an lm reproduces the period means within tol.
make_se_data <- function() {
  set.seed(123)
  n <- 6000
  d <- data.table(
    age = sample(20:60, n, TRUE),
    period = sample(c(2000, 2004, 2008), n, TRUE),
    gender = sample(c("f", "m"), n, TRUE)
  )
  d[, y := 0.01 * age + 0.004 * (period - 2000) + 0.1 * (gender == "m") +
    stats::rnorm(n, 0, 0.3)]
  d[age >= 20]
}

make_se_model <- function(d) stats::lm(y ~ age + period + gender, data = d)

test_that("y_replicates returns an R-column predictor, reproducible, and supports lm/glm/gam", {
  d <- make_se_data()
  m <- make_se_model(d)

  # The refits are folded into a predictor (make_X / beta / linkinv); only the R coefficient
  # columns survive, not the fitted model objects.
  reps <- y_replicates(m, 5, seed = 1)
  expect_named(reps, c("make_X", "beta", "linkinv"))
  expect_equal(ncol(reps$beta), 5L)
  expect_equal(nrow(reps$beta), length(coef(m)))

  # Reproducible with seed.
  reps2 <- y_replicates(m, 5, seed = 1)
  expect_equal(reps$beta, reps2$beta)

  # The fast matmul path reproduces predict_y() for each refit; check against an honest refit.
  set.seed(1)
  W <- rudirichlet(5, nrow(d))
  refit3 <- refit_weighted(m, W[3, ])
  expect_equal(replicate_predict(reps, d)[, 3], predict_y(refit3, d), ignore_attr = TRUE)

  # Unsupported class errors with the supported-list message.
  expect_error(y_replicates(structure(list(), class = "foo"), 2),
    "supported: lm, glm, gam")

  # glm (binomial) dispatches through the lm method, carrying its (non-identity) link. Dirichlet
  # weights are non-integer, so binomial glm warns about non-integer successes -- suppress it.
  d[, pos := as.integer(y > stats::median(y))]
  mg <- stats::glm(pos ~ age + period, data = d, family = stats::binomial())
  rg <- suppressWarnings(y_replicates(mg, 3, seed = 1))
  expect_equal(ncol(rg$beta), 3L)
  expect_equal(rg$linkinv, stats::binomial()$linkinv)

  # gam: mgcv resets the formula environment to the global env, so the refit's by-name
  # data lookup needs the data there (the documented "data must be in scope" contract,
  # which for gam means global). Fit against a globally-scoped copy and clean up.
  skip_if_not_installed("mgcv")
  assign(".d_se_gam", d, envir = globalenv())
  ma <- mgcv::gam(y ~ s(age) + period, data = .d_se_gam)
  ra <- y_replicates(ma, 2, seed = 1) # reads .d_se_gam via the gam's (global) formula env
  rm(".d_se_gam", envir = globalenv())
  expect_equal(ncol(ra$beta), 2L)
})

test_that("y_replicates always restores the caller's RNG stream (seeded and unseeded)", {
  d <- make_se_data()
  m <- make_se_model(d)

  # Unseeded branch: the Dirichlet draw must not advance the global stream.
  set.seed(99)
  y_replicates(m, 5)
  a <- stats::runif(1)
  set.seed(99)
  b <- stats::runif(1)
  expect_identical(a, b)

  # Seeded branch: same guarantee, despite the internal set.seed(seed).
  set.seed(99)
  y_replicates(m, 5, seed = 4242)
  a2 <- stats::runif(1)
  expect_identical(a2, b)
})

test_that("point estimate is preserved when R > 0 (CRN); summary carries no SE columns", {
  d <- make_se_data()
  m <- make_se_model(d)

  set.seed(7)
  r0 <- decompose_aggregated(d, m, cells = "gender", tol = 0.2)
  set.seed(7)
  r1 <- suppressMessages(
    decompose_aggregated(d, m, cells = "gender", R = 25, tol = 0.2, seed = 11)
  )

  comps <- c("intraindividual", "coming_of_age", "mortality", "inmigration", "outmigration")
  for (cc in comps) {
    expect_equal(r0$summary[[cc]], r1$summary[[cc]], tolerance = 1e-12, info = cc)
  }

  # R = 0: no draws. All uncertainty lives in the on-demand cumulative band, so neither
  # path writes per-period SE columns onto summary.
  expect_null(r0$draws)
  expect_false(any(grepl("_se", names(r0$summary))))
  expect_false(any(grepl("_se", names(r1$summary))))
})

test_that("draws table is balanced under CRN and carries the cell covariates", {
  d <- make_se_data()
  m <- make_se_model(d)
  set.seed(7)
  r1 <- suppressMessages(
    decompose_aggregated(d, m, cells = "gender", R = 25, tol = 0.2, seed = 11)
  )

  dr <- r1$draws
  expect_s3_class(dr, "data.table")
  expect_setequal(dr$draw, 1:25)
  expect_true(all(c("draw", "period", "component", "delta", "age", "gender") %in% names(dr)))

  # Balanced: every draw fires the identical (period, component, cell) row set, only
  # the delta differs. Equal row counts per draw and an identical key set per draw.
  per_draw <- dr[, .N, by = draw]
  expect_equal(uniqueN(per_draw$N), 1L)
  key_of <- function(g) {
    setorder(unique(g[, .(period, component, age, gender)]), period, component, age, gender)
  }
  k1 <- key_of(dr[draw == 1L])
  k2 <- key_of(dr[draw == 25L])
  expect_identical(k1, k2)
})

test_that("draws are reproducible across runs (same outer + bootstrap seed)", {
  d <- make_se_data()
  m <- make_se_model(d)

  set.seed(7)
  a <- suppressMessages(
    decompose_aggregated(d, m, cells = "gender", R = 20, tol = 0.2, seed = 11)
  )
  set.seed(7)
  b <- suppressMessages(
    decompose_aggregated(d, m, cells = "gender", R = 20, tol = 0.2, seed = 11)
  )
  expect_equal(a$draws$delta, b$draws$delta)
})

test_that("print shows the 95% CI and plot returns a ggplot, with and without covariate", {
  d <- make_se_data()
  m <- make_se_model(d)
  set.seed(7)
  r1 <- suppressMessages(
    decompose_aggregated(d, m, cells = "gender", R = 20, tol = 0.2, seed = 11)
  )

  out <- capture.output(print(r1, detailed = FALSE))
  expect_true(any(grepl("95% CI", out, fixed = TRUE)))
  out_cov <- capture.output(print(r1, detailed = FALSE, covariate = "gender"))
  expect_true(any(grepl("95% CI", out_cov, fixed = TRUE)))

  expect_s3_class(plot(r1), "ggplot")
  expect_s3_class(plot(r1, covariate = "gender"), "ggplot")
})

test_that("cumulative_series: ordered band, pseudo-types, level split, NA band for point-only", {
  d <- make_se_data()
  m <- make_se_model(d)
  set.seed(7)
  r1 <- suppressMessages(
    decompose_aggregated(d, m, cells = "gender", R = 30, tol = 0.2, seed = 11)
  )

  ci <- cumulative_series(r1)
  expect_true(all(ci$lci <= ci$uci))
  expect_true(all(is.na(ci$level)))
  # Leaf components plus the two aggregate pseudo-types.
  expect_true(all(c("Total change", "Population turnover") %in% ci$type))
  expect_true(all(c("Intraindividual change", "Mortality", "Coming-of-age") %in% ci$type))

  # Split by gender carries both levels.
  cig <- cumulative_series(r1, "gender")
  expect_setequal(unique(cig$level), c("f", "m"))

  # Point-only object: point estimate present, band columns all NA.
  set.seed(7)
  r0 <- decompose_aggregated(d, m, cells = "gender", tol = 0.2)
  s0 <- cumulative_series(r0)
  expect_false(anyNA(s0$value))
  expect_true(all(is.na(s0$lci)) && all(is.na(s0$uci)))

  # The Total change band is formed per draw, NOT by summing the leaf component bands
  # (quantiles are not additive).
  last <- max(ci$period)
  tc <- ci[type == "Total change" & period == last]
  leaves <- c("Intraindividual change", "Mortality", "Coming-of-age",
    "Out-migration", "In-migration")
  leaf_sum_lci <- ci[type %in% leaves & period == last, sum(lci)]
  expect_false(isTRUE(all.equal(tc$lci, leaf_sum_lci)))
})

test_that("rudirichlet weights have mean 1 (sum to n), not sum to 1", {
  # The Dirichlet draw is used as case weights in a weighted-likelihood refit. They
  # must average 1 so the refit's effective sample size matches the original fit; a
  # penalized gam's smoothing-parameter selection depends on the absolute weight scale,
  # so sum-to-1 weights would make every replicate oversmooth.
  set.seed(1)
  W <- rudirichlet(4, 500)
  expect_equal(dim(W), c(4L, 500L))
  expect_equal(rowSums(W), rep(500, 4), tolerance = 1e-8)
  expect_true(all(W > 0))
})

test_that("gam bootstrap refits preserve effective df (no oversmoothing bias)", {
  # Root-cause regression test for the sum-to-1 vs sum-to-n Dirichlet weight bug. The
  # draws are used as case weights in a weighted-likelihood refit; a penalized gam selects
  # its smoothing parameters from the absolute weight scale. Weights summing to 1 (total
  # information ~1 spread over all rows) made every replicate drastically oversmooth --
  # total edf collapsing to about half the original -- which biased the whole bootstrap
  # band off the point estimate, so the plotted ribbons sat outside the point lines. lm is
  # scale-invariant in weights and never showed this, so the test must use a gam. The
  # collapse only bites at realistic data scale (large n, multiple smooths), so it runs on
  # the shipped GSS data rather than a small synthetic frame.
  skip_if_not_installed("mgcv")
  data("gss_homosex", package = "socialchange")
  # mgcv resets the formula environment to the global env, so the by-name refit lookup
  # needs the training data there (the documented gam contract). Fit globally, clean up.
  gss_edf_dat <- gss_homosex[age >= 21, .(age, period = year, y = homosex, wtssall)]
  assign(".gss_edf_dat", gss_edf_dat, envir = globalenv())
  on.exit(rm(".gss_edf_dat", envir = globalenv()), add = TRUE)
  m <- mgcv::gam(y ~ s(age) + s(period), data = .gss_edf_dat, weights = wtssall)

  edf <- function(mod) sum(summary(mod)$edf)
  orig_edf <- edf(m)

  # y_replicates() discards the refit objects (keeping only coefficients), so probe the edf at
  # the refit layer that produced the bug: the same mean-1 Dirichlet draw + weighted refit.
  set.seed(1)
  W <- rudirichlet(10, nrow(gss_edf_dat))
  mean_refit_edf <- mean(vapply(seq_len(10), function(r) edf(refit_weighted(m, W[r, ])), numeric(1)))

  # Refits must keep roughly the original flexibility. Pre-fix this collapsed to about
  # half the original edf; the corrected mean-1 weighting holds it close.
  expect_gt(mean_refit_edf, 0.8 * orig_edf)
})

test_that("refit_weighted folds the model's prior weights into the Dirichlet draw", {
  d <- make_se_data()
  set.seed(5)
  d[, wt := stats::runif(.N, 0.5, 2)]
  mw <- stats::lm(y ~ age + period, data = d, weights = wt)

  w <- stats::runif(nrow(d))
  rw <- refit_weighted(mw, w)
  expected <- stats::lm(y ~ age + period, data = transform(d, sw = w * d$wt), weights = sw)
  expect_equal(coef(rw), coef(expected))
})
