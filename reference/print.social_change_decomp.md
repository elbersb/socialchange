# Print a social_change_decomp object

Print a social_change_decomp object

## Usage

``` r
# S3 method for class 'social_change_decomp'
print(x, detailed = TRUE, covariate = NULL, digits = 3, ...)
```

## Arguments

- x:

  A \`social_change_decomp\` object returned by
  \[decompose_aggregated()\].

- detailed:

  Logical; if \`TRUE\` (default) prints period-by-period overview before
  the summary.

- covariate:

  Optional name of a single cell covariate (one of the \`cells\`, or
  \`"age"\`) by which to additionally break down the components. When
  supplied, one column per covariate level is appended to the
  decomposition table. For more elaborate breakdowns, aggregate
  \`x\$record\` directly, which carries the cell covariates.

- digits:

  Number of digits to print.

- ...:

  Not used.

## Value

\`x\`, invisibly.
