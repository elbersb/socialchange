#' Print a social_change_decomp object
#'
#' @param x A `social_change_decomp` object returned by [decompose_aggregated()].
#' @param detailed Logical; if `TRUE` (default) prints period-by-period overview before the summary.
#' @param covariate Optional name of a single cell covariate (one of the `cells`, or `"age"`) by
#'   which to additionally break down the components. When supplied, one column per covariate level
#'   is appended to the decomposition table. For more elaborate breakdowns, aggregate
#'   `x$record` directly, which carries the cell covariates.
#' @param digits Number of digits to print.
#' @param ... Not used.
#' @return `x`, invisibly.
#' @import data.table
#' @export
print.social_change_decomp <- function(x, detailed = TRUE, covariate = NULL, digits = 3, ...) {
    checkmate::assert_string(covariate, null.ok = TRUE)
    old <- options(digits = digits, scipen = 999)
    on.exit(options(old))
    if (detailed) {
        cat("Overview by period:\n")
        print(x$summary[], row.names = FALSE, class = FALSE, na.print = "")
    }

    ov <- cumulative_series(x)[period == max(period)]
    val <- function(ty) {
        v <- ov[type == ty, value]
        if (length(v)) v else 0
    }
    mean0 <- x$summary[1][["modeled_mean"]]
    meanN <- x$summary[.N][["modeled_mean"]]
    total_change <- meanN - mean0

    # `type` keys each row to its cumulative_series() component (NA for header rows); all
    # value/percent/CI/per-level columns join on it, never row position. Dropped before print.
    decomp <- data.table(
        Component = c(
            "At initial (modeled)", "At end (modeled)", "Total change",
            "- Intraindividual change", "- Population turnover",
            "  - Mortality", "  - Out-migration", "  - Coming-of-age", "  - In-migration"
        ),
        type = c(
            NA, NA, "Total change", "Intraindividual change", "Population turnover",
            "Mortality", "Out-migration", "Coming-of-age", "In-migration"
        )
    )
    component <- !is.na(decomp$type) & decomp$type != "Total change"
    decomp[, Value := fcase(
        Component == "At initial (modeled)", mean0,
        Component == "At end (modeled)", meanN,
        Component == "Total change", total_change,
        default = vapply(type, val, numeric(1), USE.NAMES = FALSE)
    )]
    # Percentages are undefined when total change is zero, so leave them blank.
    pct <- if (total_change == 0) rep("", nrow(decomp)) else formatC(100 * decomp$Value / total_change, format = "f", digits = 1, width = 5)
    decomp[, Percent := fifelse(
        component, pct,
        fifelse(Component == "Total change" & total_change != 0, "100.0", "")
    )]
    decomp[, Value := formatC(Value, format = "f", digits = digits, width = digits + 3)]

    if (detailed) cat("\nDecomposition of total change:\n")

    # One character column per covariate level, holding that level's cumulative value.
    if (!is.null(covariate)) {
        lv <- cumulative_series(x, covariate)[period == max(period)]
        fmtcol <- function(v) {
            out <- formatC(v, format = "f", digits = digits, width = digits + 3)
            out[is.na(v)] <- ""
            out
        }
        for (L in sort(unique(lv$level))) {
            decomp[, (L) := fmtcol(lv[level == L][decomp, on = "type", x.value])]
        }
    }

    if (!is.null(x$draws)) {
        ci_col <- function(band) {
            band[decomp, on = "type"][, fifelse(is.na(lci), "", fmt_ci(lci, uci, digits))]
        }
        if (is.null(covariate)) {
            decomp[, `95% CI` := ci_col(ov)]
        } else {
            for (L in sort(unique(lv$level))) {
                decomp[, (paste(L, "95% CI")) := ci_col(lv[level == L])]
            }
        }
    }

    # Show a migration row only for whichever migration type was actually inferred.
    # In-migration is a residual from cell growth; out-migration is always zero here.
    if (val("In-migration") == 0) decomp <- decomp[Component != "  - In-migration"]
    if (val("Out-migration") == 0) decomp <- decomp[Component != "  - Out-migration"]

    decomp[, type := NULL]
    print(decomp[], row.names = FALSE, class = FALSE, justify = "left", na.print = "")
    invisible(x)
}

#' @rdname decompose_aggregated
#' @param x A `social_change_decomp` object returned by [decompose_aggregated()].
#' @param covariate Optional name of a single cell covariate (one of the `cells`, or `"age"`) by
#'   which to split the cumulative change lines. For more elaborate breakdowns, aggregate `x$record` directly.
#' @param ... Not used.
#' @import data.table
#' @import ggplot2
#' @export
plot.social_change_decomp <- function(x, covariate = NULL, ...) {
    checkmate::assert_string(covariate, null.ok = TRUE)
    summary <- data.table::copy(x$summary)

    means_long <- data.table::melt(summary,
        id.vars = "period",
        measure.vars = c("observed_mean", "modeled_mean"),
        variable.name = "type", value.name = "value"
    )
    means_long[, type := factor(
        type,
        c("observed_mean", "modeled_mean"),
        c("Observed", "Modeled")
    )]
    # `level` (the covariate value) is the line-type aesthetic; it is NA for the
    # means, which then draw as a single solid line.
    means_part <- means_long[, .(period,
        type = as.character(type),
        level = NA_character_, value, panel = "Mean outcome"
    )]

    # Drop the aggregate pseudo-types cumulative_series() carries; keep only leaves. Always
    # draw IC/mortality/coming-of-age, plus a migration series only where it actually moves.
    series <- cumulative_series(x, covariate)
    leaves <- unname(component_labels)
    base <- c("Intraindividual change", "Mortality", "Coming-of-age")
    series[, keep := type %in% leaves & (type %in% base | any(value != 0)), by = .(type, level)]
    decomp_part <- series[keep == TRUE, .(period, type, level,
        value,
        panel = "Cumulative change"
    )]

    # CI ribbon for exactly the drawn lines (NA-band series drop out via the lci filter).
    ribbon_part <- NULL
    if (!is.null(x$draws)) {
        drawn <- unique(decomp_part[, .(type, level)])
        ribbon_part <- series[drawn, on = .(type, level), nomatch = 0L][
            !is.na(lci), .(period, type, level, lci, uci, panel = "Cumulative change")
        ]
    }

    # Component colours are fixed across both paths; with a covariate, levels are
    # instead distinguished by line type (see scale_linetype_manual below).
    color_map <- c(
        "Observed" = "black",
        "Modeled" = "#888888",
        "Intraindividual change" = "#450C54",
        "Mortality" = "#22908C",
        "Coming-of-age" = "#FDE724",
        "Out-migration" = "#3B518B",
        "In-migration" = "#5DC963"
    )

    combine <- rbindlist(list(means_part, decomp_part), use.names = TRUE)
    combine[, type := factor(type, names(color_map))]
    combine[, panel := factor(panel, c("Mean outcome", "Cumulative change"))]
    if (!is.null(ribbon_part)) {
        ribbon_part[, type := factor(type, names(color_map))]
        ribbon_part[, panel := factor(panel, c("Mean outcome", "Cumulative change"))]
    }

    if (is.null(covariate)) {
        p <- ggplot(combine, aes(x = period, y = value, color = type))
    } else {
        p <- ggplot(combine, aes(x = period, y = value, color = type, linetype = level))
    }
    # Ribbon first so the component lines draw on top of their bands.
    if (!is.null(ribbon_part)) {
        p <- p + geom_ribbon(
            data = ribbon_part,
            # paste() not interaction(): interaction(type, NA) is NA, merging all ribbons.
            aes(x = period, ymin = lci, ymax = uci, fill = type, group = paste(type, level)),
            inherit.aes = FALSE, alpha = 0.2
        ) +
            scale_fill_manual(values = color_map, guide = "none")
    }
    p <- p +
        facet_wrap("panel", nrow = 1, scales = "free_y") +
        geom_hline(yintercept = 0, color = "gray") +
        geom_line() +
        scale_color_manual(values = color_map) +
        labs(color = NULL, y = NULL, x = "Period") +
        theme_light() +
        theme(
            legend.position = "bottom",
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank()
        )

    if (!is.null(covariate)) {
        # Distinct line type per covariate level. The mean lines (level = NA) draw
        # solid via na.value and are kept out of the line-type legend
        lv <- sort(unique(stats::na.omit(combine$level)))
        lt_vals <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
        lt <- stats::setNames(rep(lt_vals, length.out = length(lv)), lv)
        p <- p + scale_linetype_manual(
            values = lt, breaks = lv, na.value = "solid", name = covariate
        )
    }
    p
}

# Fixed width CI formatting with central comma
fmt_ci <- function(lower, upper, digits = 3) {
    lo <- sprintf(paste0("%.", digits, "f"), lower)
    hi <- sprintf(paste0("%.", digits, "f"), upper)

    w_lo <- max(nchar(lo))
    w_hi <- max(nchar(hi))

    sprintf(
        paste0("[%", w_lo, "s, %-", w_hi, "s]"),
        lo, hi
    )
}

# Maps the internal component names to display labels, in print/plot order.
component_labels <- c(
    intraindividual = "Intraindividual change",
    mortality = "Mortality", outmigration = "Out-migration",
    coming_of_age = "Coming-of-age", inmigration = "In-migration"
)

# Tag long component rows with display `type` (factor in print/plot order) and the
# optional covariate `level` (NA for the unsplit, one-series-per-component case).
# Shared by the point and draw delta builders below.
tag_components <- function(dt, covariate) {
    dt[, type := factor(component_labels[component], levels = component_labels)]
    dt[, level := if (is.null(covariate)) NA_character_ else as.character(get(covariate))][]
}

# Per-transition component deltas from x$record, long with columns period, type
# (display label, factor in print/plot order), level, and delta. record[[i]] holds
# the change into period[i + 1]. `covariate` (one of x$cells) splits each component
# by that cell covariate's levels; without it every row has level = NA, i.e. one
# series per component.
component_deltas <- function(x, covariate = NULL) {
    if (!is.null(covariate)) checkmate::assert_choice(covariate, x$cells)
    periods <- x$summary$period
    dt <- rbindlist(x$record, idcol = "i")
    dt[, period := periods[i + 1L]]
    dt <- tag_components(dt, covariate)
    dt[, .(delta = sum(delta)), by = .(type, level, period)]
}

# Per-(draw, period) component deltas, long. Draws analogue of component_deltas().
draw_component_deltas <- function(x, covariate = NULL) {
    if (!is.null(covariate)) checkmate::assert_choice(covariate, x$cells)
    dt <- tag_components(copy(x$draws), covariate)
    dt[, .(delta = sum(delta)), by = .(draw, type, level, period)]
}

# Worker for cumulative_series: append the "Population turnover"/"Total change" aggregate
# pseudo-types, complete every series across all `periods` (missing transition = 0), and
# return per-series running totals in `cum`. `extra` carries extra grouping keys ("draw"
# for the per-draw path, none for the point estimate).
#
# Aggregates must be summed across components BEFORE cumulating: neither cumulation nor the
# downstream quantiles are additive across components, so an aggregate band can't be summed
# from the per-component bands.
cumulate_with_aggregates <- function(dt, periods, extra = character()) {
    turnover_types <- setdiff(component_labels, "Intraindividual change")
    by_pl <- c(extra, "level", "period")
    agg <- function(types, label) {
        dt[type %in% types, .(type = label, delta = sum(delta)), by = by_pl]
    }
    leaf_cols <- c(extra, "type", "level", "period", "delta")
    long <- rbindlist(list(
        dt[, ..leaf_cols][, type := as.character(type)],
        agg(turnover_types, "Population turnover"),
        agg(component_labels, "Total change")
    ), use.names = TRUE)
    by_tl <- c(extra, "type", "level")
    grid <- unique(long[, ..by_tl])[, .(period = periods), by = by_tl]
    long <- long[grid, on = c(by_tl, "period")]
    long[is.na(delta), delta := 0]
    setorderv(long, c(by_tl, "period"))
    long[, cum := cumsum(delta), by = by_tl]
    long[]
}

# Cumulative change per (type, level, period): point estimate in `value`, bootstrap bounds
# in `lci`/`uci` (NA when x has no draws). `type` spans the leaves plus the aggregate
# pseudo-types. Single source for both print's final-period totals/CI and plot's
# lines/ribbons; point and CI paths cumulate identically, differing only in the quantile.
cumulative_series <- function(x, covariate = NULL, probs = c(0.025, 0.975)) {
    periods <- x$summary$period
    pt <- cumulate_with_aggregates(component_deltas(x, covariate), periods)
    out <- pt[, .(type, level, period, value = cum)]
    if (is.null(x$draws)) {
        out[, c("lci", "uci") := NA_real_]
        return(out[])
    }
    dd <- cumulate_with_aggregates(draw_component_deltas(x, covariate), periods, extra = "draw")
    band <- dd[, .(lci = stats::quantile(cum, probs[1]), uci = stats::quantile(cum, probs[2])),
        by = .(type, level, period)
    ]
    out[band, on = .(type, level, period)]
}
