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
  tol = 0.05,
  weight = NULL,
  population = NULL
)

# S3 method for class 'social_change_decomp'
plot(x, covariate = NULL, ...)
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

- tol:

  Maximum tolerated absolute deviation between observed and modeled
  period means, in the outcome's own units (default 0.05). Checks that
  `fun_y` reproduces the observed period means; if the largest deviation
  exceeds `tol`, the function errors. The default suits outcomes on a
  roughly unit scale (e.g. proportions in \[0, 1\]); set `tol` to match
  outcomes on another scale.

- weight:

  Name of the weight column used when aggregating individual-level data
  (ignored if `n` is present). Weights are normalized within each period
  to sum to the period sample size before aggregation, so that cell
  counts `n` (rounded sums of normalized weights) reflect the relative
  population structure rather than raw sample sizes. This preserves
  simulation tractability but is an approximation: the ideal approach
  would use true population counts, which are generally unavailable from
  survey data alone.

- population:

  Optional data frame of true cell counts `n` per cell and period
  (columns `period`, `age`, the `cells` identifiers, and `n`). When
  supplied, these counts replace the survey-derived cell counts as the
  population frame: they drive event derivation and weight the modeled
  mean, while `fun_y` continues to supply every cell's outcome and
  `stacked_data` is used only for the observed-mean / model-fit
  diagnostic. This is the preferred input when true population counts
  (e.g. from a census or official statistics) are available alongside
  survey data, as it sidesteps survey age-structure noise. `n` is
  rounded to whole counts for the microsimulation, so rescale large
  frames (e.g. raw population counts in the millions) to a tractable
  per-period total first – only the relative cell structure matters. The
  frame must match the survey's minimum age and the level set of each
  `cells` column, and cover every survey period (extra periods are
  dropped); these are compared over rows with `n > 0`.

- x:

  A \`social_change_decomp\` object returned by
  \[decompose_aggregated()\].

- covariate:

  Optional name of a single cell covariate (one of the \`cells\`, or
  \`"age"\`) by which to split the cumulative change lines. For more
  elaborate breakdowns, aggregate \`x\$record\` directly.

- ...:

  Not used.

## Value

S3 object of class `social_change_decomp` with components:

- `summary`: data.table with decomposition components by period
  (including the `inmigration` and `outmigration` columns; print/plot
  show a migration component only for whichever of these is non-zero)

- `record`: list of per-transition change tables (one per period
  transition). Each table is tidy, with columns `component`, the cell
  covariates (`age` and any `cells`), and `delta` – one row per
  component per cell, holding that cell's total contribution to the
  change for that component over the transition. Summed over cells it
  reproduces the per-component totals in `summary`.

## Details

The function estimates mortality, coming-of-age, and net in-migration
from period-to-period population differences within cells, then uses
microsimulation to randomly order demographic events and track their
contribution to aggregate change. Unequal and multi-year gaps between
periods are supported: when the gap exceeds one year, each entering
cohort is assigned to the specific calendar year within the gap when it
crosses the minimum age, so that post-entry aging is correctly
attributed to intraindividual change rather than coming-of-age. All
waves must share a common minimum age (the youngest age observed with a
non-zero count); this single threshold separates entering cohorts from
survivors, and a mismatch across periods is an error.

By default the survey itself supplies both the cell counts and the
outcomes. Supplying `population` decouples these: the population frame
supplies the cell counts `n` (and hence the inferred demographic
events), while `fun_y` supplies the outcomes. The reported
`modeled_mean` is then weighted by the population frame, whereas
`observed_mean` remains the survey's own observed mean, so the two lines
may diverge when the survey and population age structures differ.

Within a cell, a survivor cohort that shrinks between periods loses
people to mortality, while one that grows gains people through net
in-migration. Only *net* migration is recovered: gross out-migration is
not separable from deaths (a survivor loss could be either), so it is
folded into mortality and the reported out-migration is always zero.
Each cell's net change is attributed to a single event type by sign: a
shrinking cell records only mortality (any concurrent in-migration is
invisible) and a growing cell records only net in-migration (any
concurrent deaths are folded in), so offsetting flows within a cell
cannot be seen. New cohorts (below the minimum age) attribute all their
growth to coming-of-age; migration among entering cohorts is not
modeled. On noisy survey cells this strategy relabels sampling
fluctuation as in-migration and mortality, so the inferred in-migration
is most meaningful when `population` supplies a true population frame,
where growing cohorts reflect genuine net immigration rather than survey
noise.

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
# restrict to age >= 21 so every wave shares a common minimum age
stacked <- as.data.table(gss_homosex)[age >= 21, .(age, period = year, y = homosex)]
model <- stats::lm(y ~ age + period, data = stacked)
result <- decompose_aggregated(stacked, function(d) predict(model, newdata = d), tol = 0.1)
print(result)
#> Overview by period:
#>  period observed_mean modeled_mean intraindividual coming_of_age  mortality
#>    1973        0.1845       0.1257              NA            NA         NA
#>    1974        0.2064       0.1348        0.004378      0.003070  0.0038321
#>    1976        0.2285       0.1490        0.008756      0.005024  0.0024406
#>    1977        0.2142       0.1609        0.004378      0.002362  0.0036549
#>    1980        0.2030       0.1880        0.013134      0.009134  0.0036248
#>    1982        0.2079       0.2060        0.008756      0.005605  0.0021687
#>    1984        0.2082       0.2303        0.008756      0.005809  0.0084059
#>    1985        0.1974       0.2313        0.004378      0.002114 -0.0002418
#>    1987        0.1830       0.2523        0.008756      0.004421  0.0070308
#>    1988        0.1826       0.2599        0.004378      0.001818  0.0032712
#>    1989        0.2112       0.2649        0.004378      0.003036  0.0044193
#>    1990        0.1828       0.2739        0.004378      0.001961  0.0054826
#>    1991        0.2068       0.2885        0.004378      0.004092  0.0070642
#>    1993        0.2821       0.3044        0.008756      0.003684  0.0044503
#>    1994        0.2839       0.3179        0.004378      0.002122  0.0009061
#>    1996        0.3366       0.3408        0.008756      0.003902  0.0080467
#>    1998        0.3566       0.3541        0.008756      0.004041  0.0045272
#>    2000        0.3508       0.3682        0.008756      0.003694  0.0041280
#>    2002        0.3969       0.3861        0.008756      0.003428  0.0058540
#>    2004        0.3667       0.4085        0.008756      0.003658  0.0094180
#>    2006        0.3880       0.4211        0.008756      0.005997  0.0013902
#>    2008        0.4324       0.4361        0.008756      0.002466  0.0046382
#>    2010        0.4885       0.4585        0.008756      0.003184  0.0079399
#>    2012        0.4958       0.4735        0.008756      0.004220  0.0036313
#>    2014        0.5462       0.4882        0.008756      0.003325  0.0027515
#>    2016        0.5724       0.5035        0.008756      0.003638  0.0038334
#>  period observed_mean modeled_mean intraindividual coming_of_age  mortality
#>  outmigration inmigration
#>            NA          NA
#>             0  -0.0021454
#>             0  -0.0020226
#>             0   0.0014728
#>             0   0.0012228
#>             0   0.0014504
#>             0   0.0014016
#>             0  -0.0052631
#>             0   0.0008072
#>             0  -0.0019064
#>             0  -0.0068347
#>             0  -0.0028205
#>             0  -0.0009099
#>             0  -0.0010134
#>             0   0.0061373
#>             0   0.0021765
#>             0  -0.0040684
#>             0  -0.0024392
#>             0  -0.0001393
#>             0   0.0005537
#>             0  -0.0035228
#>             0  -0.0009016
#>             0   0.0025513
#>             0  -0.0016353
#>             0  -0.0001252
#>             0  -0.0008985
#>  outmigration inmigration
#> 
#> Decomposition of total change:
#>                 Component    Value Percent
#>  At initial (modeled)      0.12566        
#>  At end (modeled)          0.50352        
#>  Total change              0.37786   100.0
#>  - Intraindividual change  0.18826   49.8 
#>  - Population turnover     0.18960   50.2 
#>    - Mortality             0.11267   29.8 
#>    - Coming-of-age         0.09581   25.4 
#>    - In-migration         -0.01887   -5.0 
# }
```
