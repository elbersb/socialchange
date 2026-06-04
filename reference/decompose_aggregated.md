# Decompose social change from aggregated data

Decomposes aggregate-level change into intraindividual change and
population turnover components using microsimulation on stacked
cross-sectional data. Requires a prediction function that models the
outcome as a function of age, period, and covariates.

## Usage

``` r
decompose_aggregated(stacked_data, fun_y, cells = c(), migration = FALSE)
```

## Arguments

- stacked_data:

  Stacked data.table with columns `age`, `period`, `n`, `y`, and
  optional cell identifiers

- fun_y:

  Prediction function taking `(newdata)` and returning predicted outcome
  values

- cells:

  Character vector of additional cell identifier columns beyond age
  (e.g., "gender", "smoking")

- migration:

  Logical; if TRUE, decompose migration components (not yet implemented)

## Value

S3 object of class `social_change_decomp` with components:

- `summary`: data.table with decomposition components by period

- `record`: list of detailed event records for each period transition

- `migration`: logical indicating whether migration was decomposed

## Details

The function estimates mortality and coming-of-age from period-to-period
population differences within cells, then uses microsimulation to
randomly order demographic events and track their contribution to
aggregate change.

**Limitation**: Does not properly handle within-cell state transitions.
Transition effects are absorbed into the intraindividual change
component. See CLAUDE.md for details.

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)
# Simulate data first
data <- data.table(age = 20:39, n = rep(100, 20))
simresult <- sim_social_change(periods = 5, data = data, ...)

# Stack snapshots and fit model
stacked <- rbindlist(simresult$snapshot)
model <- lm(y ~ age, data = stacked)
predict_y <- function(newdata) predict(model, newdata = newdata)

# Decompose
result <- decompose_aggregated(stacked, predict_y)
print(result)
} # }
```
