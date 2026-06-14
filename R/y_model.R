# Response-scale predictions for a stacked newdata frame. The single seam between
# decompose_aggregated() and the user's model; se_plan.md adds y_replicates() here too.
predict_y <- function(object, newdata) UseMethod("predict_y")

# Covers lm, glm, gam (the latter two inherit through their class vectors). as.vector
# drops the names/array shape mgcv::predict.gam can return, so callers always get a
# plain numeric of length nrow(newdata).
predict_y.lm <- function(object, newdata) {
    as.vector(stats::predict(object, newdata = newdata, type = "response"))
}

predict_y.default <- function(object, newdata) {
    stop(
        "decompose_aggregated() needs a fitted lm, glm, or gam model; ",
        "got an object of class '", class(object)[1L], "'.",
        call. = FALSE
    )
}
