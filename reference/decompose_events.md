# Decompose social change from discrete events

Event-based decomposition for populations where units enter and exit at
specific time points. Separates aggregate change into within-unit change
and population replacement components.

## Usage

``` r
decompose_events(
  events,
  outcomes,
  formula,
  end_period = NULL,
  event_type = "event_type",
  fun = mean
)
```

## Arguments

- events:

  data.frame with unit entry/exit events, must contain unit ID, time
  index, and event type columns

- outcomes:

  data.frame with outcome values by unit and time period

- formula:

  Formula specifying `Outcome ~ Unit + Time` structure

- end_period:

  Optional numeric end period; defaults to maximum event time

- event_type:

  Character name of column indicating event type (entry/exit)

- fun:

  Aggregation function to compute period-level outcomes (default: mean)

## Value

S3 object of class `decompose_events` containing decomposition results

## See also

\[decompose_aggregated()\] for decomposing change in stacked
cross-sectional data, \[sim_social_change()\] for forward simulation
with fully specified demographic functions. Vignette:
[`vignette("gss_homosexuality", package = "socialchange")`](https://elbersb.github.io/socialchange/articles/gss_homosexuality.md).

## Examples

``` r
# \donttest{
library(data.table)
events <- data.table(
  country = c("A", "B", "A"),
  year = c(1995, 2000, 2010),
  event_type = c("entry", "entry", "exit")
)
outcomes <- data.table(
  gdp = runif(50),
  country = rep(c("A", "B"), each = 25),
  year = rep(1995:2019, 2)
)
result <- decompose_events(events, outcomes, gdp ~ country + year)
print(result)
#>     event_type       term      total      pct
#>         <char>      <num>      <num>    <num>
#> 1:      change -0.3032191 -0.4281657 70.81816
#> 2: replacement -0.1249466 -0.4281657 29.18184
# }
```
