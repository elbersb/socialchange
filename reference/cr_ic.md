# Cohort replacement vs. intracohort change decomposition

Decomposes total change in an outcome between two or more periods into
cohort replacement (CR) and intracohort change (IC) components. Four
methods are available:

## Usage

``` r
cr_ic(data, formula, weight = NULL, model = NULL)
```

## Arguments

- data:

  A data frame with one row per respondent (or one row per cohort-period
  cell if \`weight\` is supplied).

- formula:

  A formula of the form \`outcome ~ period + cohort\`.

- weight:

  Name of the weight variable as a string. If \`NULL\`, all observations
  are weighted equally.

- model:

  Either a model formula (e.g., \`~ period \* cohort\`) or a fitted
  model object that supports \`predict()\`. When a formula is supplied
  it is fitted as a weighted \`lm\`. Required for the AD+ and Model
  methods.

## Value

A list of class \`cr_ic_decomposition\` with components:

- \`summary\`:

  \`data.table\` summarising IC and CR over the full period for each
  method.

- \`detailed\`:

  \`data.table\` with year-over-year results for each consecutive pair
  of periods.

- \`periods\`:

  Numeric vector of all periods found in the data.

- \`cohort\`:

  \`data.table\` with cohort-level IC and CR contributions (only when
  \`model\` is supplied, otherwise \`NULL\`).

- \`model\`:

  The fitted or supplied model object, or \`NULL\`.

## Details

\- \*\*AD\*\* (algebraic decomposition): Decomposes spanning cohorts
into IC and CR exactly. Entering and exiting cohorts cannot be split and
are assigned entirely to CR, which Firebaugh notes is "very
problematic". - \*\*LD\*\* (linear decomposition): Fits a linear model
\`y ~ period + cohort\` and uses its coefficients to attribute change.
Handles entering/exiting cohorts but cannot capture non-linearities and
produces a residual. - \*\*AD+\*\*: Extends AD by imputing missing
outcome values for entering/exiting cohorts from the model supplied via
\`model\`, then applies the algebraic decomposition. Produces no
residual; quality depends on model fit. - \*\*Model\*\*: Like AD+ but
replaces \*all\* cohort-specific values (not just missing ones) with
model predictions. Acts as a form of regularization, which can help for
small cohorts with noisy estimates.

AD and LD are always computed. AD+ and Model require a \`model\`
argument.

## References

Firebaugh, G. (1992). Where does social change come from? Estimating the
relative contributions of intra-cohort change and population turnover.
\*Population Research and Policy Review\*, 11(1), 61–74.
