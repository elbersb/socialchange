# Decompose social change from aggregated data

Decomposes aggregate-level change into intraindividual change and
population turnover components using microsimulation on stacked
cross-sectional data. Requires a fitted model that predicts the outcome
as a function of age, period, and covariates.

## Usage

``` r
decompose_aggregated(
  stacked_data,
  model,
  cells = c(),
  R = 0,
  tol = 0.05,
  weight = NULL,
  population = NULL,
  seed = NULL
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

- model:

  A fitted model object (`lm`, `glm`, or `gam`) predicting the outcome
  from `age`, `period`, and any `cells`. Predictions are taken on the
  response scale via
  [`predict()`](https://rdrr.io/r/stats/predict.html).

- cells:

  Character vector of additional cell identifier columns beyond age
  (e.g., "gender", "smoking")

- R:

  Number of paired (event-ordering, model-draw) replicates used to
  attach standard errors (default 0, point estimate only). When `R > 0`,
  each replicate draws its own random event ordering and pairs it with a
  Dirichlet-reweighted refit of `model`; the spread of the resulting
  decompositions gives per-component standard errors and cumulative
  confidence bands. The band is the *combined* event-ordering and model
  uncertainty, not demographic uncertainty in the cell counts. For `gam`
  models each replicate is a full refit plus prediction, so large `R`
  can be slow.

- tol:

  Maximum tolerated absolute deviation between observed and modeled
  period means, in the outcome's own units (default 0.05). Checks that
  `model` reproduces the observed period means; if the largest deviation
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
  mean, while `model` continues to supply every cell's outcome and
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

- seed:

  Optional integer seed for reproducible bootstrap replicates (default
  `NULL`). The Dirichlet refit draw is isolated from the global RNG
  stream, so the replicate refits are reproducible via `seed` while the
  event orderings follow the outer
  [`set.seed()`](https://rdrr.io/r/base/Random.html). Only `R = 0`
  reproduces the legacy single-ordering point estimate; with `R > 0` the
  point is the mean over the `R` orderings (see Details).

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
  show a migration component only for whichever of these is non-zero).

- `record`: list of per-transition change tables (one per period
  transition). Each table is tidy, with columns `component`, the cell
  covariates (`age` and any `cells`), and `delta` – one row per
  component per cell, holding that cell's total contribution to the
  change for that component over the transition. Summed over cells it
  reproduces the per-component totals in `summary`.

- `draws`: when `R > 0`, a long data.table of per-(draw, period, cell)
  component deltas (columns `draw`, `period`, `component`, `delta`, and
  the cell covariates) from which any aggregate's confidence band can be
  computed; `NULL` when `R = 0`.

## Details

The function estimates mortality, coming-of-age, and net in-migration
from period-to-period population differences within cells, then uses
microsimulation to randomly order demographic events – placed at evenly
spaced times within each inter-period gap – and track their contribution
to aggregate change. The ordering is itself an uncertainty source (it
stands in for the unobserved true event sequence): the point estimate is
the mean decomposition over `max(R, 1)` random orderings, and when
`R > 0` each replicate carries its own ordering (paired with its own
model refit), so the reported band folds ordering and model uncertainty
together. Unequal and multi-year gaps between periods are supported:
when the gap exceeds one year, each entering cohort is assigned to the
specific calendar year within the gap when it crosses the minimum age,
so that post-entry aging is correctly attributed to intraindividual
change rather than coming-of-age. All waves must share a common minimum
age (the youngest age observed with a non-zero count); this single
threshold separates entering cohorts from survivors, and a mismatch
across periods is an error.

By default the survey itself supplies both the cell counts and the
outcomes. Supplying `population` decouples these: the population frame
supplies the cell counts `n` (and hence the inferred demographic
events), while `model` supplies the outcomes. The reported
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
result <- decompose_aggregated(stacked, model, tol = 0.1)
print(result)
#> Overview by period:
#>  period observed_mean modeled_mean intraindividual coming_of_age  mortality
#>    1973         0.184        0.126              NA            NA         NA
#>    1974         0.206        0.135         0.00438       0.00310  0.0037499
#>    1976         0.229        0.149         0.00876       0.00503  0.0024760
#>    1977         0.214        0.161         0.00438       0.00237  0.0036233
#>    1980         0.203        0.188         0.01313       0.00911  0.0036892
#>    1982         0.208        0.206         0.00876       0.00561  0.0021342
#>    1984         0.208        0.230         0.00876       0.00584  0.0083601
#>    1985         0.197        0.231         0.00438       0.00210 -0.0000644
#>    1987         0.183        0.252         0.00876       0.00441  0.0069815
#>    1988         0.183        0.260         0.00438       0.00178  0.0033264
#>    1989         0.211        0.265         0.00438       0.00301  0.0044047
#>    1990         0.183        0.274         0.00438       0.00201  0.0052257
#>    1991         0.207        0.289         0.00438       0.00409  0.0070398
#>    1993         0.282        0.304         0.00876       0.00365  0.0044900
#>    1994         0.284        0.318         0.00438       0.00218  0.0009825
#>    1996         0.337        0.341         0.00876       0.00392  0.0080203
#>    1998         0.357        0.354         0.00876       0.00406  0.0045233
#>    2000         0.351        0.368         0.00876       0.00373  0.0040311
#>    2002         0.397        0.386         0.00876       0.00349  0.0057825
#>    2004         0.367        0.409         0.00876       0.00362  0.0094769
#>    2006         0.388        0.421         0.00876       0.00611  0.0014395
#>    2008         0.432        0.436         0.00876       0.00246  0.0048131
#>    2010         0.488        0.459         0.00876       0.00320  0.0079013
#>    2012         0.496        0.473         0.00876       0.00424  0.0036194
#>    2014         0.546        0.488         0.00876       0.00332  0.0026143
#>    2016         0.572        0.504         0.00876       0.00365  0.0036220
#>  period observed_mean modeled_mean intraindividual coming_of_age  mortality
#>  outmigration inmigration
#>            NA          NA
#>             0  -0.0020913
#>             0  -0.0020617
#>             0   0.0015009
#>             0   0.0011810
#>             0   0.0014766
#>             0   0.0014155
#>             0  -0.0054232
#>             0   0.0008668
#>             0  -0.0019195
#>             0  -0.0067985
#>             0  -0.0026122
#>             0  -0.0008854
#>             0  -0.0010237
#>             0   0.0059980
#>             0   0.0021870
#>             0  -0.0040857
#>             0  -0.0023738
#>             0  -0.0001275
#>             0   0.0005306
#>             0  -0.0036859
#>             0  -0.0010664
#>             0   0.0025789
#>             0  -0.0016415
#>             0   0.0000155
#>             0  -0.0006973
#>  outmigration inmigration
#> 
#> Decomposition of total change:
#>                 Component  Value Percent
#>  At initial (modeled)      0.126        
#>  At end (modeled)          0.504        
#>  Total change              0.378   100.0
#>  - Intraindividual change  0.188    49.8
#>  - Population turnover     0.190    50.2
#>    - Mortality             0.112    29.7
#>    - Coming-of-age         0.096    25.4
#>    - In-migration         -0.019    -5.0
# }
```
