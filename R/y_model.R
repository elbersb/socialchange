# Response-scale predictions for a stacked newdata frame. The single seam between
# decompose_aggregated() and the user's model, alongside y_replicates() below.
predict_y <- function(object, newdata) UseMethod("predict_y")

# Covers lm, glm, gam (the latter two inherit through their class vectors). as.vector
# drops the names/array shape mgcv::predict.gam can return, so callers always get a
# plain numeric of length nrow(newdata).
#' @exportS3Method
predict_y.lm <- function(object, newdata) {
    as.vector(stats::predict(object, newdata = newdata, type = "response"))
}

#' @exportS3Method
predict_y.default <- function(object, newdata) {
    stop(
        "decompose_aggregated() needs a fitted lm, glm, or gam model; ",
        "got an object of class '", class(object)[1L], "'.",
        call. = FALSE
    )
}

# R replicate predictors capturing model uncertainty; each is itself a predict_y() object.
# The seam model types vary at: lm/glm/gam -> Dirichlet refit; a future brmsfit -> draws.
y_replicates <- function(object, R, seed = NULL) UseMethod("y_replicates")

# Dirichlet(1,...,1) weights (gamma(1) == exp(1)), scaled to mean 1 -- GAM smoothing-param
# selection depends on absolute weight scale, so ESS~1 would oversmooth every replicate.
rudirichlet <- function(R, n) {
    e <- matrix(stats::rexp(R * n), nrow = R, ncol = n)
    e / rowSums(e) * n
}

# One Dirichlet-reweighted refit on the model's original data via its stored call (robust
# to NA-dropping and transformed predictors), folding draws into any prior survey weights.
refit_weighted <- function(object, w) {
    cl <- stats::getCall(object)
    if (is.null(cl)) stop("model has no recoverable call(); cannot refit for bootstrap")
    fenv <- environment(stats::formula(object))
    data <- eval(cl$data, fenv)
    prior <- if (is.null(cl$weights)) rep(1, nrow(data)) else eval(cl$weights, data, fenv)
    data[[".scw"]] <- w * prior
    cl$weights <- quote(.scw)
    cl$data <- quote(.d)
    eval(cl, list2env(list(.d = data), parent = fenv))
}

#' @exportS3Method
y_replicates.default <- function(object, R, seed = NULL) {
    stop("model class '", class(object)[1L],
        "' has no y_replicates() method; supported: lm, glm, gam")
}

# Covers lm, glm, gam (glm/gam inherit through their class vector).
#
# The Dirichlet draw always save/restores .Random.seed, keeping the refit stream independent of
# the global stream that schedule_events() draws orderings from: so under one outer set.seed()
# the R > 0 orderings begin from the same global state as R = 0 (R = 0 takes the first ordering),
# and the refits don't perturb them. `seed` (optional) only sets cross-run reproducibility of the
# replicate refits.
#' @exportS3Method
y_replicates.lm <- function(object, R, seed = NULL) {
    if (exists(".Random.seed", envir = .GlobalEnv)) {
        old <- get(".Random.seed", envir = .GlobalEnv)
        on.exit(assign(".Random.seed", old, envir = .GlobalEnv), add = TRUE)
    } else {
        on.exit(rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
    }
    if (!is.null(seed)) set.seed(seed)
    n <- nrow(eval(stats::getCall(object)$data, environment(stats::formula(object))))
    W <- rudirichlet(R, n)
    reps <- lapply(seq_len(R), function(r) refit_weighted(object, W[r, ]))
    make_predictor(object, reps)
}

# newdata -> a model's design matrix via its own stored basis (gam: lpmatrix; lm/glm:
# model.matrix from the fitted terms/xlevels/contrasts). Built for both the original (to
# predict) and a refit (the reuse probe), so the two are compared like-for-like.
design_builder <- function(model) {
    if (inherits(model, "gam")) {
        function(newdata) stats::predict(model, newdata = newdata, type = "lpmatrix")
    } else {
        tt <- stats::delete.response(stats::terms(model))
        function(newdata) {
            mf <- stats::model.frame(tt, newdata, xlev = model$xlevels)
            stats::model.matrix(tt, mf, contrasts.arg = model$contrasts)
        }
    }
}

# NA coefficients mark aliased (rank-deficient) columns; predict() drops them, so zero them
# to keep the coefficient vector aligned with the design matrix's columns.
coef0 <- function(fit) {
    b <- stats::coef(fit)
    b[is.na(b)] <- 0
    b
}

# Package R honest Dirichlet refits into a predictor for replicate_predict(). Prediction
# collapses to one matmul linkinv(X %*% coef_r): the design matrix X is identical across refits
# because the Dirichlet bootstrap reweights fixed rows, so any basis built from the data columns
# rather than the weights (linear, gam smooths, poly/bs/ns/scale) is shared. Only the
# coefficients are kept; the refits are discarded.
#
# This is the fast path; predict_y() is the canonical one (point estimate, setup). They are not
# unified -- the headline estimate should go through the model's own predict() -- so the probe
# enforces that they coincide, on probe rows from the training data:
#   (1) the matmul reproduces predict_y() for the original model (linkinv(X %*% coef) is the
#       model's prediction), and
#   (2) the basis is invariant to the reweighting (the original's X stands in for every refit's).
# Either failing would silently corrupt the SEs, so it errors rather than guesses -- no stock
# lm/glm/gam term trips it.
make_predictor <- function(object, reps) {
    make_X <- design_builder(object)
    data <- eval(stats::getCall(object)$data, environment(stats::formula(object)))
    probe <- data[seq_len(min(100L, nrow(data))), , drop = FALSE]
    X_orig <- make_X(probe)
    linkinv <- if (inherits(object, "glm")) stats::family(object)$linkinv else identity

    beta_orig <- coef0(object)
    matmul_matches_predict <- length(beta_orig) == ncol(X_orig) &&
        isTRUE(all.equal(
            as.vector(linkinv(X_orig %*% beta_orig)),
            predict_y(object, probe),
            check.attributes = FALSE
        ))
    basis_invariant <- tryCatch(
        isTRUE(all.equal(X_orig, design_builder(reps[[1L]])(probe), check.attributes = FALSE)),
        error = function(e) FALSE
    )
    if (!matmul_matches_predict || !basis_invariant) {
        stop("Cannot attach bootstrap SEs (R > 0): the model's predictions are not a fixed ",
            "linkinv(X %*% coef) (a weight- or data-dependent basis?).", call. = FALSE)
    }

    beta <- vapply(reps, coef0, numeric(ncol(X_orig)))
    list(make_X = make_X, beta = beta, linkinv = linkinv)
}

# Per-replicate predictions -> nrow(newdata) x R matrix: build the shared design matrix X once
# and matmul all replicates at once (the basis-reuse optimization).
replicate_predict <- function(reps, newdata) {
    reps$linkinv(reps$make_X(newdata) %*% reps$beta)
}
