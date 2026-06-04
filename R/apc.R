#' Age-Period-Cohort model estimation
#'
#' Fits an Age-Period-Cohort (APC) model using orthogonal polynomial contrasts to handle
#' the linear identification problem. The period linear effect is constrained to zero,
#' allowing estimation of age and cohort linear trends.
#'
#' @param data data.frame or data.table with APC variables
#' @param formula Formula specifying \code{outcome ~ age + period + cohort} structure
#'
#' @return S3 object of class \code{apc_model} with components:
#'   \itemize{
#'     \item \code{model_period_zero}: fitted lm object with period linear effect set to zero
#'     \item \code{thetas}: named vector of theta parameters (age and cohort linear slopes)
#'     \item \code{contrasts}: list of orthogonal polynomial contrast matrices
#'     \item \code{values}: unique values for age, period, and cohort
#'   }
#'
#' @export
#' @import data.table
apc <- function(data, formula) {
    data <- data.table::as.data.table(data)
    vars <- all.vars(formula)

    data <- data[, list(
        y = get(vars[1]),
        a = get(vars[2]), p = get(vars[3]), c = get(vars[4])
    )]

    values <- list(
        age = data[, sort(unique(a))],
        period = data[, sort(unique(p))],
        cohort = data[, sort(unique(c))]
    )

    # set orthogonal contrasts
    data[, `:=`(a = as.factor(a), p = as.factor(p), c = as.factor(c))]
    contrasts <- list(
        age = weightedcontrasts::contr.poly.weighted(data[["a"]], width = 1),
        period = weightedcontrasts::contr.poly.weighted(data[["p"]], width = 1),
        cohort = weightedcontrasts::contr.poly.weighted(data[["c"]], width = 1)
    )
    contrasts(data[["a"]]) <- contrasts[["age"]]
    contrasts(data[["p"]]) <- contrasts[["period"]]
    contrasts(data[["c"]]) <- contrasts[["cohort"]]
    # zero out the period linear effect
    contrasts(data[["p"]])[, 1] <- 0

    # estimate model
    mod <- stats::lm(y ~ a + p + c, data = data)
    theta1 <- unname(stats::coef(mod)["a.L"]) # theta1 <- ageL + periodL
    theta2 <- unname(stats::coef(mod)["c.L"]) # theta2 <- cohortL + periodL

    ret <- list(
        model_period_zero = mod,
        thetas = c("theta1" = theta1, "theta2" = theta2),
        contrasts = contrasts,
        values = values
    )
    class(ret) <- c("list", "apc_model")
    ret
}

#' @export
print.apc_model <- function(x, ...) {
    cat("APC Model", sep = "\n")
    print(x$thetas)
}

#' Plot APC two-dimensional visualization
#'
#' @param apc APC model object from \code{apc()}
#' @return ggplot2 object
#' @export
#' @import ggplot2
apc_plot_two2d <- function(apc) {
    theta1 <- apc$thetas[1]
    theta2 <- apc$thetas[2]
    sum_thetas <- abs(theta2 - theta1)
    limits <- c(-sum_thetas, sum_thetas)

    ggplot() +
        geom_abline(aes(intercept = theta1, slope = -1),
            size = 1, color = "#1080BA"
        ) +
        geom_hline(yintercept = 0, linetype = 2, color = "#3C4650") +
        geom_hline(yintercept = -(theta2 - theta1), linetype = 2, color = "#3C4650") +
        geom_vline(xintercept = 0, linetype = 2, color = "#3C4650") +
        scale_y_continuous(expression(alpha),
            limits = limits * 1.2,
            sec.axis = sec_axis(~ . + (theta2 - theta1), name = expression(gamma))
        ) +
        scale_x_continuous(expression(pi),
            limits = limits * 1.1,
            sec.axis = dup_axis()
        ) +
        theme_bw() +
        theme(
            text = element_text(size = 10),
            axis.title.y.left = element_text(angle = 0, vjust = 0.5),
            axis.title.y.right = element_text(angle = 0, vjust = 0.5)
        )
}

list_to_df <- function(list) {
    df <- data.table(
        effect = unname(unlist(list)),
        year = c(names(list$age), names(list$period), names(list$cohort)),
        apc = c(
            rep("Age", length(list$age)),
            rep("Period", length(list$period)),
            rep("Cohort", length(list$cohort))
        )
    )
    df[, year := as.numeric(year)]
    df
}

extract_nl <- function(model, set, contrasts, values, intercept) {
    extract_coefs <- stats::coef(model)[grepl(set, names(stats::coef(model)))]
    # remove linear effect
    extract_coefs <- extract_coefs[2:length(extract_coefs)]
    extract_coefs[is.na(extract_coefs)] <- 0 # why is this line here?
    deviations <- contrasts[, 2:(1 + length(extract_coefs))] %*% extract_coefs
    if (intercept == TRUE) {
        coefs <- stats::coef(model)["(Intercept)"] + deviations[, 1]
    } else {
        coefs <- deviations[, 1]
    }
    names(coefs) <- values
    coefs
}

#' Extract non-linear APC effects
#'
#' @param apc APC model object from \code{apc()}
#' @param intercept Logical; include intercept in output
#' @return List of non-linear effect estimates
#' @export
apc_nonlinearities <- function(apc, intercept = FALSE) {
    m <- apc[["model_period_zero"]]
    list(
        age = extract_nl(m, "^a", apc[["contrasts"]]$age, apc[["values"]]$age, intercept),
        period = extract_nl(m, "^p", apc[["contrasts"]]$period, apc[["values"]]$period, intercept),
        cohort = extract_nl(m, "^c", apc[["contrasts"]]$cohort, apc[["values"]]$cohort, intercept)
    )
}

#' Plot non-linear APC effects
#'
#' @param model APC model object from \code{apc()}
#' @return ggplot2 object showing non-linear effects
#' @export
apc_plot_nonlinearities <- function(model) {
    df <- list_to_df(apc_nonlinearities(model))
    ggplot(df, aes(x = year, y = effect)) +
        facet_wrap("apc", scales = "free_x", nrow = 3) +
        geom_point(size = 0.2) +
        geom_line(alpha = 0.5) +
        labs(y = "Non-linear effect", group = "", linetype = "") +
        theme_bw() +
        theme(legend.position = "bottom", axis.title.x = element_blank())
}

#' Compute total APC effects under assumption
#'
#' @param model APC model object from \code{apc()}
#' @param assumption Character specifying linear trend assumption
#' @return List of total effect estimates
#' @export
apc_total <- function(model, assumption) {
    age_l <- NULL
    period_l <- NULL
    cohort_l <- NULL
    if (length(assumption) == 1 && names(assumption) == "age_linear") {
        age_l <- assumption[["age_linear"]]
        period_l <- model[["thetas"]][1] - age_l
        cohort_l <- model[["thetas"]][2] - period_l
    } else if (length(assumption) == 1 && names(assumption) == "period_linear") {
        period_l <- assumption[["period_linear"]]
        age_l <- model[["thetas"]][1] - period_l
        cohort_l <- model[["thetas"]][2] - period_l
    } else if (length(assumption) == 1 && names(assumption) == "cohort_linear") {
        cohort_l <- assumption[["cohort_linear"]]
        period_l <- model[["thetas"]][2] - cohort_l
        age_l <- model[["thetas"]][1] - period_l
    } else {
        stop("assumption must be list of length 1")
    }

    nl <- apc_nonlinearities(model)
    age_total <- nl[["age"]] + model[["contrasts"]]$age[, 1] * age_l
    period_total <- nl[["period"]] + model[["contrasts"]]$period[, 1] * period_l
    cohort_total <- nl[["cohort"]] + model[["contrasts"]]$cohort[, 1] * cohort_l

    list(age = age_total, period = period_total, cohort = cohort_total)
}

#' Plot total APC effects under assumption
#'
#' @param model APC model object from \code{apc()}
#' @param assumption Character specifying linear trend assumption
#' @return ggplot2 object showing total effects
#' @export
apc_plot_total <- function(model, assumption) {
    df <- list_to_df(apc_total(model, assumption))
    ggplot(df, aes(x = year, y = effect)) +
        facet_wrap("apc", scales = "free_x", nrow = 3) +
        geom_point(size = 0.2) +
        geom_line(alpha = 0.5) +
        labs(y = "Total effect (under given assumption)", group = "", linetype = "") +
        theme_bw() +
        theme(legend.position = "bottom", axis.title.x = element_blank())
}
