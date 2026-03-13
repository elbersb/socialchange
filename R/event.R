#' @import data.table
#' @export
event_decomposition <- function(
    events, outcomes, formula, end_period = NULL,
    event_type = "event_type", fun = mean) {
  vars <- all.vars(formula)
  checkmate::assert_vector(vars, len = 3, any.missing = FALSE)
  checkmate::assert_data_frame(events)
  checkmate::assert_data_frame(outcomes)
  checkmate::assert_names(names(events), must.include = c(vars[2:3], event_type))
  checkmate::assert_names(names(outcomes), must.include = vars)
  events <- as.data.table(events)
  setnames(events, vars[2:3], c("unit", "index"))
  events <- events[, .(unit, index, event_type)]
  outcomes <- as.data.table(outcomes)
  setnames(outcomes, vars, c("outcome", "unit", "index"))
  outcomes <- outcomes[, .(unit, index, outcome)]
  if (is.null(end_period)) {
    end_period <- max(events[["index"]])
  }
  # subset events to match requested period
  setorder(events, index)
  events <- events[index <= end_period]

  steps <- split(events, events[["index"]])
  if (!(end_period %in% names(steps))) {
    steps[[length(steps) + 1]] <- data.table(index = end_period)
    names(steps)[[length(steps)]] <- end_period
  }
  current_pop <- steps[[1]][["unit"]]
  pairwise <- list()
  ix <- 1
  for (i in 1:(length(steps) - 1)) {
    current_index <- steps[[i]][["index"]][1]
    next_index <- steps[[i + 1]][["index"]][1]
    for (fill_index in current_index:(next_index - 1)) {
      pairwise[[ix]] <- rbindlist(list(
        data.table(
          unit = current_pop, index = fill_index, event_type = "change",
          type = "pre", order = ix
        ),
        data.table(
          unit = current_pop, index = fill_index + 1, event_type = "change",
          type = "post", order = ix
        )
      ))
      setorder(pairwise[[ix]], index)
      ix <- ix + 1
    }
    # update population
    if (i < (length(steps) - 1)) {
      entries <- steps[[i + 1]][event_type == "entry", unit]
      exits <- steps[[i + 1]][event_type == "exit", unit]
      next_pop <- setdiff(current_pop, exits)
      next_pop <- c(next_pop, entries)
      pairwise[[ix]] <- rbindlist(list(
        data.table(
          unit = current_pop, index = next_index, event_type = "replacement",
          type = "pre", order = ix
        ),
        data.table(
          unit = next_pop, index = next_index, event_type = "replacement",
          type = "post", order = ix
        )
      ))
      ix <- ix + 1
    }
    current_pop <- next_pop
  }
  # core decomposition
  pairwise <- rbindlist(pairwise)
  pairwise[outcomes, outcome := i.outcome, on = .(unit, index)]
  if (sum(is.na(pairwise[["outcome"]])) > 0) {
    print(pairwise[is.na(outcome), .(index, unit)])
    stop("NA encountered")
  }

  summ <- pairwise[, .(outcome = fun(outcome)), by = .(order, index, event_type, type)]
  wide <- dcast(summ, order + event_type ~ type, value.var = c("index", "outcome"))
  setcolorder(wide, c("order", "index_pre", "index_post", "event_type", "outcome_pre", "outcome_post"))
  wide[, diff := outcome_post - outcome_pre]
  decomp <- wide[, .(term = sum(diff)), by = .(event_type)]
  decomp[, total := sum(term)]
  decomp[, pct := 100 * term / total]

  # compute counterfactuals
  # ctf -- change only
  change_only <- wide[event_type == "change"]
  first <- change_only[1, .(index = index_pre, counterf = "change only (event)", outcome = outcome_pre)]
  change_only <- change_only[, .(index = index_post, counterf = "change only (event)", outcome = first$outcome + cumsum(diff))]

  # ctf -- replacement only
  repl_only <- change_only[, .(index, counterf = "replacement only (event)")]
  repl_only <- merge(repl_only,
    wide[event_type == "replacement"][, .(index_pre, outcome = cumsum(diff))],
    all.x = TRUE, by.x = "index", by.y = "index_pre"
  )
  repl_only[, outcome := nafill(outcome, "locf")]
  repl_only[is.na(outcome), "outcome"] <- 0
  repl_only[, outcome := first$outcome + outcome]

  ctf <- rbindlist(list(
    first, change_only,
    first[, .(index, counterf = "replacement only (event)", outcome)], repl_only
  ))

  # forward -- change only
  pop <- pairwise[order == 1 & type == "pre", unit]
  forward <- CJ(unit = pop, index = min(pairwise$index):max(pairwise$index))
  forward[outcomes, outcome := i.outcome, on = .(unit, index)]
  forward <- forward[, .(counterf = "change only (forward)", outcome = fun(outcome)), by = .(index)]

  # backward -- change only
  pop <- pairwise[order == max(order) & type == "post", unit]
  backward <- CJ(unit = pop, index = min(pairwise$index):max(pairwise$index))
  backward[outcomes, outcome := i.outcome, on = .(unit, index)]
  backward <- backward[, .(counterf = "change only (backward)", outcome = fun(outcome)), by = .(index)]

  ctf <- rbindlist(list(ctf, forward, backward))

  ret <- list(long = summ, wide = wide, decomp = decomp, ctf = ctf)
  class(ret) <- c("list", "event_decomposition")
  ret
}

#' @import data.table
#' @export
print.event_decomposition <- function(x, ...) {
  print(x$decomp)
}

#' @import data.table
#' @import ggplot2
#' @export
plot.event_decomposition <- function(x, ...) {
  ggplot(
    x$wide,
    aes(x = index_pre, xend = index_post, y = outcome_pre, yend = outcome_post)
  ) +
    geom_segment(linewidth = 1, aes(linetype = event_type)) +
    geom_line(data = x$ctf, aes(
      x = index, y = outcome, color = counterf,
      xend = NULL, yend = NULL
    )) +
    labs(linetype = "observed", color = "implied counterfactuals") +
    theme_light()
}
