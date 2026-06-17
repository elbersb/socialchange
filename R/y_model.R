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
# The Dirichlet draw always save/restores .Random.seed: otherwise rexp() would advance the
# global stream before schedule_events(), so R > 0 and R = 0 under one outer set.seed()
# would draw different schedules and different point estimates. `seed` (optional) only sets
# cross-run reproducibility of the replicates.
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
    lapply(seq_len(R), function(r) refit_weighted(object, W[r, ]))
}

# Per-replicate predictions -> n x R matrix. predict_y is wrapped in a closure, not passed
# straight to vapply, so S3 dispatch finds the unexported generic's methods under both an
# installed package and devtools::load_all().
replicate_predict <- function(reps, newdata) {
    vapply(reps, function(rep) predict_y(rep, newdata), numeric(nrow(newdata)))
}
