# Decompose social change from aggregated data

Decomposes aggregate-level change into intraindividual change and
population turnover components using microsimulation on stacked
cross-sectional data. Requires a prediction function that models the
outcome as a function of age, period, and covariates.

## Usage

``` r
decompose_aggregated(
  stacked_data,
  fun_y,
  cells = c(),
  migration = FALSE,
  tol = 0.05,
  weight = NULL
)

# S3 method for class 'social_change_decomp'
plot(x, ...)
```

## Arguments

- stacked_data:

  Data frame with columns `age`, `period`, and `y`, plus optional cell
  identifiers. If a column `n` is present the data is treated as already
  aggregated to cells; otherwise individual-level rows are aggregated
  internally using `weight`.

- fun_y:

  Prediction function taking `(newdata)` and returning predicted outcome
  values

- cells:

  Character vector of additional cell identifier columns beyond age
  (e.g., "gender", "smoking")

- migration:

  Logical; if TRUE, decompose migration components (not yet implemented)

- tol:

  Maximum tolerated relative deviation between observed and modeled
  period means (default 0.05 = 5%). Emits a warning rather than stopping
  when exceeded.

- weight:

  Name of the weight column used when aggregating individual-level data
  (ignored if `n` is present). Weights are normalized within each period
  to sum to the period sample size before aggregation, so that cell
  counts `n` (rounded sums of normalized weights) reflect the relative
  population structure rather than raw sample sizes. This preserves
  simulation tractability but is an approximation: the ideal approach
  would use true population counts, which are generally unavailable from
  survey data alone.

- x:

  A \`social_change_decomp\` object returned by
  \[decompose_aggregated()\].

- ...:

  Not used.

## Value

S3 object of class `social_change_decomp` with components:

- `summary`: data.table with decomposition components by period

- `record`: list of detailed event records for each period transition

- `migration`: logical indicating whether migration was decomposed

## Details

The function estimates mortality and coming-of-age from period-to-period
population differences within cells, then uses microsimulation to
randomly order demographic events and track their contribution to
aggregate change. Unequal and multi-year gaps between periods are
supported: when the gap exceeds one year, each entering cohort is
assigned to the specific calendar year within the gap when it crosses
the minimum age, so that post-entry aging is correctly attributed to
intraindividual change rather than coming-of-age.

**Limitation**: Does not properly handle within-cell state transitions.
Transition effects are absorbed into the intraindividual change
component.

## See also

\[decompose_events()\] for event-driven decomposition,
\[sim_social_change()\] for forward simulation with fully specified
demographic functions. Vignette:
[`vignette("decompose_aggregated", package = "socialchange")`](https://elbersb.github.io/socialchange/articles/decompose_aggregated.md).

## Examples

``` r
# \donttest{
library(data.table)
#> 
#> Attaching package: ‘data.table’
#> The following object is masked from ‘package:base’:
#> 
#>     %notin%
data("gss_homosex", package = "socialchange")
stacked <- as.data.table(gss_homosex)[, .(age, period = year, y = homosex)]
model <- stats::lm(y ~ age + period, data = stacked)
result <- decompose_aggregated(stacked, function(d) predict(model, newdata = d))
#>     period  observed   modeled
#>      <num>     <num>     <num>
#>  1:   1973 0.1876190 0.1230814
#>  2:   1974 0.2092385 0.1321867
#>  3:   1976 0.2334528 0.1490655
#>  4:   1977 0.2187208 0.1603541
#>  5:   1980 0.2072464 0.1881045
#>  6:   1982 0.2085096 0.2065993
#>  7:   1984 0.2101606 0.2307257
#>  8:   1985 0.1969697 0.2329092
#>  9:   1987 0.1809661 0.2541966
#> 10:   1988 0.1820128 0.2634544
#> 11:   1989 0.2107793 0.2688312
#> 12:   1990 0.1845033 0.2783597
#> 13:   1991 0.2041260 0.2915271
#> 14:   1993 0.2840985 0.3086763
#> 15:   1994 0.2878949 0.3224485
#> 16:   1996 0.3406532 0.3460172
#> 17:   1998 0.3588710 0.3602138
#> 18:   2000 0.3579073 0.3767569
#> 19:   2002 0.3956670 0.3932927
#> 20:   2004 0.3680556 0.4191036
#> 21:   2006 0.3907865 0.4309749
#> 22:   2008 0.4371497 0.4485022
#> 23:   2010 0.4913722 0.4685791
#> 24:   2012 0.5004068 0.4860782
#> 25:   2014 0.5475752 0.5015160
#> 26:   2016 0.5724020 0.5143308
#> 27:   2018 0.6066282 0.5364883
#>     period  observed   modeled
#>      <num>     <num>     <num>
#> Warning: Modeled means deviate from observed by up to 58.3% (tol = 5.0%). Consider a more flexible model or increase tol.
print(result)
#> Overview by period:
#>  period observed_mean modeled_mean intraindividual coming_of_age mortality
#>    1973        0.1876       0.1231              NA            NA        NA
#>    1974        0.2092       0.1322        0.004929     0.0009909 0.0043280
#>    1976        0.2335       0.1491        0.009858     0.0040308 0.0032304
#>    1977        0.2187       0.1604        0.004929     0.0003133 0.0035926
#>    1980        0.2072       0.1881        0.014788     0.0053492 0.0038481
#>    1982        0.2085       0.2066        0.009858     0.0032022 0.0027370
#>    1984        0.2102       0.2307        0.009858     0.0019597 0.0089227
#>    1985        0.1970       0.2329        0.004929     0.0003938 0.0005407
#>    1987        0.1810       0.2542        0.009858     0.0024195 0.0069269
#>    1988        0.1820       0.2635        0.004929     0.0002371 0.0045217
#>    1989        0.2108       0.2688        0.004929     0.0003048 0.0049986
#>    1990        0.1845       0.2784        0.004929     0.0003099 0.0060997
#>    1991        0.2041       0.2915        0.004929     0.0003496 0.0077983
#>    1993        0.2841       0.3087        0.009858     0.0021751 0.0046758
#>    1994        0.2879       0.3224        0.004929     0.0007090 0.0011852
#>    1996        0.3407       0.3460        0.009858     0.0017552 0.0083482
#>    1998        0.3589       0.3602        0.009858     0.0021034 0.0050295
#>    2000        0.3579       0.3768        0.009858     0.0026601 0.0048520
#>    2002        0.3957       0.3933        0.009858     0.0006312 0.0053834
#>    2004        0.3681       0.4191        0.009858     0.0034469 0.0104974
#>    2006        0.3908       0.4310        0.009858     0.0050627 0.0021275
#>    2008        0.4371       0.4485        0.009858     0.0021029 0.0055807
#>    2010        0.4914       0.4686        0.009858     0.0012607 0.0068483
#>    2012        0.5004       0.4861        0.009858     0.0023436 0.0040091
#>    2014        0.5476       0.5015        0.009858     0.0022068 0.0030314
#>    2016        0.5724       0.5143        0.009858     0.0000000 0.0041546
#>    2018        0.6066       0.5365        0.009858     0.0000000 0.0112625
#>  period observed_mean modeled_mean intraindividual coming_of_age mortality
#>  outmigration inmigration
#>            NA          NA
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>             0           0
#>  outmigration inmigration
#> 
#> Decomposition of total change:
#>                 Component   Value
#>  At initial (modeled)     0.12308
#>  At end (modeled)         0.53649
#>  Total change             0.41341
#>  - Intraindividual change 0.22181
#>  - Population turnover    0.18085
#>    - Mortality            0.13453
#>    - Coming-of-age        0.04632
#> Assumes no in- or out-migration.
# }
```
