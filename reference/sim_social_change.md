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

## Examples

``` r
if (FALSE) { # \dontrun{
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
} # }
```
