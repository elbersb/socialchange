map_predict = function(period, cohort, model) {
    predict_data = data.table(period, cohort)
    names(predict_data) = attr(model, "period_cohort")
    stats::predict(model, newdata = predict_data)
}

#' @import data.table
cr_ic_compute = function(data, model = NULL) {
    period1 = min(data$p)
    period2 = max(data$p)

    comp = merge(data[p == period1, !"p"], data[p == period2, !"p"], by = "c",
                  all = TRUE, suffixes = c("1", "2"))
    comp[, cclass := "spanning"]
    comp[, cclass := ifelse(!is.na(y1) & is.na(y2), "replaced", cclass)]
    comp[, cclass := ifelse(is.na(y1) & !is.na(y2), "new", cclass)]
    diff_replaced = comp[cclass == "replaced", -sum(prop1 * y1)]
    diff_new = comp[cclass == "new", sum(prop2 * y2)]
    diff_spanning = comp[cclass == "spanning", sum(prop2 * y2) - sum(prop1 * y1)]

    y1 = comp[, sum(prop1 * y1, na.rm = TRUE)]
    y2 = comp[, sum(prop2 * y2, na.rm = TRUE)]

    # algebraic decomposition
    ic_ad = comp[cclass == "spanning", sum((prop1 + prop2) / 2 * (y2 - y1))]
    cr_ad = comp[cclass == "spanning", sum((y1 + y2) / 2 * (prop2 - prop1))]
    # linear decomposition
    mod = stats::lm(y ~ p + c, weight = prop, data = data)
    ic_ld = unname(stats::coef(mod)[2]) * (period2 - period1)
    diff_c = comp[, sum(prop2 * c, na.rm = TRUE)] - comp[, sum(prop1 * c, na.rm = TRUE)]
    cr_ld = unname(stats::coef(mod)[3]) * diff_c

    # model-based
    if (!is.null(model)) {
        comp[, prop1 := ifelse(is.na(prop1), 0, prop1)]
        comp[, prop2 := ifelse(is.na(prop2), 0, prop2)]

        comp[, pred_y1 := map_predict(period1, c, model)]
        comp[, pred_y2 := map_predict(period2, c, model)]
        comp[, y1 := ifelse(is.na(y1), pred_y1, y1)]
        comp[, y2 := ifelse(is.na(y2), pred_y2, y2)]

        # compute AD+
        ic_adplus = comp[, sum((prop1 + prop2) / 2 * (y2 - y1))]
        cr_adplus = comp[, sum((y1 + y2) / 2 * (prop2 - prop1))]
        # compute Model
        ic_model = comp[, sum((prop1 + prop2) / 2 * (pred_y2 - pred_y1))]
        cr_model = comp[, sum((pred_y1 + pred_y2) / 2 *  (prop2 - prop1))]

        # cohort details
        comp = comp[, `:=`(
            ic = (prop1 + prop2) / 2 * (pred_y2 - pred_y1),
            cr = (pred_y1 + pred_y2) / 2 * (prop2 - prop1),
            y_diff = (pred_y2 - pred_y1))]

        comp = comp[, c("c", "ic", "cr", "y_diff")]
        setnames(comp, "c", "cohort")
    }

    terms = c("total", "IC", "CR", "resid")
    ret = data.table(
        group = c(rep("Outcome", 2), rep("Difference", 4), rep("LD", 4), rep("AD", 4)),
        factor = c(as.character(period1), as.character(period2),
            "total", "replaced", "new", "spanning",
            rep(terms, 2)),
        value = c(y1, y2,
            y2 - y1, diff_replaced, diff_new, diff_spanning,
            ic_ld + cr_ld, ic_ld, cr_ld, ic_ld + cr_ld - (y2 - y1),
            y2 - y1, ic_ad, cr_ad + diff_replaced + diff_new, 0),
        pct_explained = c(rep(NA, 6),
            100, 100 * ic_ld / (ic_ld + cr_ld), 100 * cr_ld / (ic_ld + cr_ld), NA,
            100, 100 * ic_ad / (y2 - y1), 100 * (cr_ad + diff_replaced + diff_new) / (y2 - y1), NA))

    if (is.null(model)) {
        list(ret, NULL)
    } else {
        model_results = data.table(
            group = c(rep("AD+", 4), rep("Model", 4)),
            factor = rep(terms, 2),
            value = c(
                ic_adplus + cr_adplus, ic_adplus, cr_adplus, 0,
                ic_model + cr_model, ic_model, cr_model, y2 - y1 - ic_model - cr_model),
            pct_explained = c(
                100, 100 * ic_adplus / (ic_adplus + cr_adplus),
                100 * cr_adplus / (ic_adplus + cr_adplus), NA,
                100, 100 * ic_model / (ic_model + cr_model),
                100 * cr_model / (ic_model + cr_model), NA))
        list(rbindlist(list(ret, model_results)), comp)
    }
}

#' Desc
#'
#' Desc
#'
#' @param data A data frame.
#' @param formula X
#' @param weight X
#' @param model Can be a model object that supports `predict` or a model formula.
#' @return Returns ...
#' @references
#' X
#' @examples
#' # todo
#' @import data.table
#' @export
cr_ic = function(data, formula, weight = NULL, model = NULL) {
    data = data.table::as.data.table(data)
    vars = all.vars(formula)
    if (is.null(weight)) {
        data = data[, vars, with = FALSE]
        data[, weight := 1]
    } else {
        data = data[, c(vars, weight), with = FALSE]
    }

    # check
    names(data) = c("y", "p", "c", "w")
    periods = data[, sort(unique(p))]
    if (length(periods) < 2) {
        stop("not enough periods")
    }
    nrow = nrow(data)
    data = stats::na.omit(data)
    if (nrow(data) < nrow) {
        warning(paste0("dropped ", nrow - nrow(data),
                       " rows due to NA"))
    }

    # prepare data
    data = data[, list(y = stats::weighted.mean(y, w), w = sum(w)), by = c("p", "c")]
    data[, prop := w / sum(w), by = "p"]
    data[, w := NULL]
    # TODO: make sure year and cohort are numeric (not factors) or warn?
    data[, p := as.integer(as.character(p))]
    data[, c := as.integer(as.character(c))]

    # estimate model
    if (!is.null(model)) {
        # a formula is supplied
        if (inherits(model, "formula")) {
            # replace variables in model (which is a formula here)
            model = stats::update.formula(model, "y ~ .")
            renames = list(quote(p), quote(c))
            names(renames) = all.vars(model)[2:3]
            model = do.call("substitute", list(model, renames))
            # estimate model (overwrite formula)
            model = stats::lm(model, weights = prop, data = data)
            attr(model, "period_cohort") = c("p", "c")
        } else {
            # store the names of the period and cohort variable for prediction
            attr(model, "period_cohort") = all.vars(formula)[2:3]
        }
    }

    # detailed period-over-period comparison
    decomp = list()
    by_cohort = list()
    for (i in 1:(length(periods) - 1)) {
        data_subset = data[p %in% c(periods[i], periods[i+1])]
        res = cr_ic_compute(data_subset, model = model)
        tab = data.table(period1 = periods[i], period2 = periods[i + 1], res[[1]])
        decomp[[i]] = tab
        bc = data.table(period1 = periods[i], period2 = periods[i + 1], res[[2]])
        by_cohort[[i]] = bc
    }
    detailed = rbindlist(decomp)
    by_cohort = rbindlist(by_cohort)

    # summarize complete period
    complete = detailed[, .SD[1:2], by = c("period1", "period2")][,
        .SD[c(1, .N)]][,
        c("group", "factor", "value", "pct_explained")]
    diff = data.table(group = "Difference", factor = "total",
        value = diff(complete[, value]), pct_explained = NA)
    methods = detailed[!(group %in% c("Outcome", "Difference")),
        list(value = sum(value)), by = c("group", "factor")]
    methods[, pct_explained := 100 * value /
        (value[factor == "IC"] + value[factor == "CR"]), by = "group"]
    methods[factor == "resid", pct_explained := NA]
    # summarize by_cohort
    if (is.null(model)) {
        by_cohort = NULL
    } else {
        cohort_type = data[, list(type = fcase(
            !(max(periods) %in% p), "removed",
            !(min(periods) %in% p), "new",
            default = "spanning")), by = "c"]
        setnames(cohort_type, "c", "cohort")
        by_cohort = merge(by_cohort, cohort_type, by = "cohort", all.x = TRUE)
        by_cohort = by_cohort[, list(
            type = first(type),
            ic = sum(ic),
            cr = sum(cr),
            y_diff_mean = mean(y_diff)), by = "cohort"]
    }
    summary = rbindlist(list(complete, diff, methods))

    ret = list(summary = summary,
        detailed = detailed,
        periods = periods,
        cohort = by_cohort,
        model = model)
    class(ret) = c("list", "cr_ic_decomposition")
    ret
}

#' @export
print.cr_ic_decomposition = function(x, digits = getOption("digits"), ...) {
    n_periods = length(x$periods)
    cat(paste0("Cohort decomposition (year-over-year) with ", n_periods, " periods:\n"))
    cat(paste0("   ", paste0(x$periods, collapse = ", "), "\n\n"))
    cat("Summary for entire period:\n")
    tab1 = x[["summary"]][1:3, c("group", "factor", "value")]
    tab1 = dcast(tab1[, -c("group")], . ~ factor)[, 2:4]
    tab1[1, ] = round(tab1, digits = digits)
    names(tab1)[3] = "Difference"
    print(tab1, digits = digits, row.names = FALSE)
    cat("\nDecompositions:\n")
    tab2 = x[["summary"]][4:.N, ]
    tab2[, value := round(value, digits = digits)]
    tab2[, pct_explained := round(pct_explained, digits = digits)]
    names(tab2) = c("method", "factor", "value", "%")
    print(tab2, digits = digits, row.names = FALSE)
}

#' Desc
#'
#' Desc
#'
#' @param x X
#' @param total X
#' @param methods X
#' @param ... Not used.
#' @return Returns ...
#' @references
#' X
#' @examples
#' # todo
#' @import data.table
#' @import ggplot2
#' @export
plot.cr_ic_decomposition = function(x, total = TRUE, methods = NULL, ...) {
    x = data.table::as.data.table(x[["detailed"]])

    if (total == TRUE) {
        total_dt = x[group == "Outcome"][,
            list(period = as.integer(factor), value)][,
            list(value = first(value)), by = "period"]
        total_dt[, `:=`(value_change = value - value[1],
            factor = "Total", method = "Total")]
    }

    methods_dt = x[factor %in% c("CR", "IC", "resid")][,
            list(period = period2, method = group, factor, value)]
    if (!is.null(methods)) {
        methods_dt = methods_dt[method %in% methods]
    }
    methods_dt[, value_change := cumsum(value), by = c("method", "factor")]
    methods_dt = methods_dt[!(factor == "resid" & value_change == 0)]

    if (total == TRUE) {
        combine = rbindlist(list(total_dt, methods_dt), use.names = TRUE)
    } else {
        combine = methods_dt
    }
    combine[, method := factor(method,
        c("Total", "LD", "AD", "AD+", "Model"),
        c("Total", "LD", "AD", "AD+", "Model"))]
    combine[, factor := factor(factor,
        c("Total", "CR", "IC", "resid"),
        c("Total", "Cohort Replacement", "Intracohort Change", "Residual"))]

    ggplot(combine, aes(x = period, y = value_change, color = factor)) +
        facet_wrap("method", nrow = 1) +
        geom_line() +
        geom_hline(yintercept = 0, color = "gray") +
        scale_color_manual(values = c(
            "Total" = "black",
            "Cohort Replacement" = "#22908C",
            "Intracohort Change" = "#450C54",
            "Residual" = "#FDE724")) +
        labs(color = "Factor", y = "Change in Outcome", x = "Period") +
        theme_light() +
        theme(legend.position = "bottom",
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank())
}
