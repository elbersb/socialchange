# Simulate social change with demographic processes

Forward simulation of social change dynamics, decomposing aggregate
change into components driven by intraindividual change and population
turnover (mortality, coming-of-age, migration, and state transitions).

## Usage

``` r
sim_social_change(
  periods,
  data,
  fun_y,
  fun_coming_of_age = NULL,
  fun_mortality = NULL,
  fun_inmigration = NULL,
  fun_outmigration = NULL,
  fun_transitions = NULL
)
```

## Arguments

- periods:

  Integer number of time periods to simulate

- data:

  Initial population data.table with columns `age`, `n`, and optional
  covariates

- fun_y:

  Outcome function taking `(data, time)` and returning outcome values

- fun_coming_of_age:

  Optional function taking `(data, period)` returning new entrants
  data.table

- fun_mortality:

  Optional function taking `(data, period)` returning mortality counts
  per cell

- fun_inmigration:

  Optional function taking `(data, period)` returning immigrants
  data.table

- fun_outmigration:

  Optional function taking `(data, period)` returning out-migration
  counts per cell

- fun_transitions:

  Optional function taking `(data, period)` returning state transitions
  data.table

## Value

S3 object of class `social_change_sim` with components:

- `summary`: data.table summarizing change components by period

- `snapshot`: list of population snapshots at each period

- `record`: list of event-by-event change records

## See also

\[decompose_aggregated()\] for decomposing change in stacked
cross-sectional data, \[decompose_events()\] for event-driven
decomposition. Vignette:
[`vignette("simulate", package = "socialchange")`](https://elbersb.github.io/socialchange/articles/simulate.md).

## Examples

``` r
# \donttest{
library(data.table)
data <- data.table(age = 20:39, n = rep(100, 20))
fun_y <- function(data, time) data[, age / 40]
fun_mortality <- function(data, period) data[, ifelse(age >= 30, 10, 0)]
fun_coming_of_age <- function(data, period) data.table(age = 20, n = 100)

result <- sim_social_change(
    periods = 5,
    data = data,
    fun_y = fun_y,
    fun_mortality = fun_mortality,
    fun_coming_of_age = fun_coming_of_age
)
print(result)
#> Overview by period:
#>  period   mean    N intraindividual coming_of_age mortality inmigration
#>       0 0.7375 2000              NA            NA        NA          NA
#>       1 0.7431 2000           0.025      -0.01263 -0.006748           0
#>       2 0.7474 1990           0.025      -0.01292 -0.007845           0
#>       3 0.7500 1970           0.025      -0.01319 -0.009172           0
#>       4 0.7508 1940           0.025      -0.01344 -0.010786           0
#>       5 0.7493 1900           0.025      -0.01368 -0.012753           0
#>  outmigration
#>            NA
#>             0
#>             0
#>             0
#>             0
#>             0
#> 
#> Decomposition of total change:
#>                 Component    Value
#>  At initial                0.73750
#>  At end                    0.74934
#>  Total change              0.01184
#>  - Intraindividual change  0.12500
#>  - Population turnover    -0.11316
#>    - Mortality            -0.04730
#>    - Out-migration         0.00000
#>    - Coming-of-age        -0.06585
#>    - In-migration          0.00000
# }
```
