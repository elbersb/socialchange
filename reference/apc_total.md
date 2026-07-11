# Compute total APC effects under assumption

Compute total APC effects under assumption

## Usage

``` r
apc_total(model, assumption)
```

## Arguments

- model:

  APC model object from
  [`apc()`](https://elbersb.github.io/socialchange/reference/apc.md)

- assumption:

  Named numeric of length 1 giving the assumed linear trend: one of
  `c(age_linear = x)`, `c(period_linear = x)`, or
  `c(cohort_linear = x)`.

## Value

List of total effect estimates

## See also

\[apc()\] for model estimation, \[apc_nonlinearities()\] for the
non-linear effects that are combined with the linear trend assumption
here.
