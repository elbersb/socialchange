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
  mortality = NULL,
  seed = NULL
)

# S3 method for class 'social_change_decomp'
plot(x, covariate = NULL, ...)
```

## Arguments

- stacked_data:

  Data frame with columns `age`, `period` (numeric), and `y`, plus
  optional cell identifiers. If a column `n` is present the data is
  treated as already aggregated to cells; otherwise individual-level
  rows are aggregated internally using `weight`.

- model:

  A fitted model object (`lm`, `glm`, or `gam`) predicting the outcome
  from `age`, `period`, and any `cells`; it must use no predictors
  beyond these. Predictions are taken on the response scale via
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
  uncertainty, not demographic uncertainty: the cell counts and any
  supplied `population` or `mortality` inputs are held fixed across
  replicates. For `gam` models each replicate is a full refit plus
  prediction, so large `R` can be slow.

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
  frame must share the survey's minimum age, reach at least its maximum
  age, share the level set of each `cells` column, and cover every
  survey period (extra periods are dropped); these are compared over
  rows with `n > 0`. Population ages above the survey maximum remain
  separate for demographic calculations, but their outcome predictions
  and reported contributions are pooled into the survey's open
  maximum-age cell. Ages from the survey maximum through the frame's
  terminal age must be gap-free in every period. The final age must be
  common across periods and is treated as the terminal open age.

- mortality:

  Optional data frame of annual death probabilities; supplying it
  switches the attribution of survivor-cell change from sign attribution
  to residual migration (see Details). Columns `period`, `age`, `prob`,
  plus optionally any subset of the `cells` columns: probabilities are
  joined on the columns present and broadcast over the rest (e.g. rates
  by age and sex apply to all age x sex x education cells). `prob` is
  the annual death probability q(x) in \[0, 1\]; convert central death
  rates m(x) (e.g. `mortality_us$death_rate`) via
  `prob = 1 - exp(-death_rate)`. The table must cover every calendar
  year from the first survey period up to (but not including) the last
  and every age from the shared minimum through the terminal age in
  `population`, or in `stacked_data` when `population` is omitted,
  without gaps. The mortality table must have the same terminal age;
  both final rows are interpreted as the same open group. Works with or
  without `population`, but is most meaningful with it: on raw survey
  counts the expected deaths round to zero for most small cells and the
  migration residual mostly reflects sampling noise.

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

- `strategy`: how survivor-cell change was attributed –
  `"sign attribution"` (default) or `"residual migration"` (when
  `mortality` is supplied).

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

Survey waves must also share a common maximum outcome age T, interpreted
as an open cell T+. If raw maxima are ragged, create a genuine common
open group first with `age = pmin(age, T)`; a hard eligibility cutoff is
not an open group and is unsupported. A supplied population frame can
extend above T. Its single-age constituents remain separate for aging
and mortality, while the outcome model receives `pmin(age, T)` and all
reported contributions at ages T and above appear at T. Between waves,
all constituents reaching T are matched to the period-2 T+ pool, so they
do not age out of the observed range. The population frame's final age
is itself treated as an open demographic group and must be common across
periods. When mortality is supplied, its final age must represent the
same open group.

By default the survey itself supplies both the cell counts and the
outcomes. Supplying `population` decouples these: the population frame
supplies the cell counts `n` (and hence the inferred demographic
events), while `model` supplies the outcomes. The reported
`modeled_mean` is then weighted by the population frame, whereas
`observed_mean` remains the survey's own observed mean, so the two lines
may diverge when the survey and population age structures differ.

Within each survivor cell the balancing identity is
`n2 = n1 - deaths + net migration`: one equation, two unknowns. The
supplied inputs determine the strategy recorded on the result:

- **Sign attribution** (default; no mortality input): each survivor
  cell's net change is routed by sign. A shrinking cell records only
  mortality (any concurrent in-migration is invisible) and a growing
  cell records only net in-migration (any concurrent deaths are folded
  in), so offsetting flows within a cell cannot be seen and the reported
  out-migration is always zero. On noisy survey cells this relabels
  sampling fluctuation as in-migration and mortality, so the inferred
  in-migration is most meaningful when `population` supplies a true
  population frame, where growing cohorts reflect genuine net
  immigration rather than survey noise.

- **Residual migration** (`mortality` supplied): deaths come from the
  supplied probabilities. A survivor cell aged `a` in year `t` compounds
  annual survival over the gap years,
  `qtilde = 1 - prod_j (1 - q(a + j, t + j))` for `j = 0..gap-1` (lookup
  ages clamped at the mortality table's maximum age), and books
  `deaths = round(n1 * qtilde)`; annual death events are assigned to the
  year implied by that survival path. When `population` extends above
  the survey maximum, the survey's open outcome group sums expected
  deaths over those separate constituent ages, each compounding its own
  path; only the terminal constituent uses the terminal open-group
  probability. Migration is then the *signed* residual
  `n2 - (n1 - deaths)`, so out-migration appears and a cell can carry
  both deaths and in-migration. Approximations: deaths are computed on
  the period-start count `n1` (within-gap mortality of migrants and
  entrants is ignored), migrants take the receiving cell's mean outcome,
  and migration remains net per cell. Bootstrap replicates (`R > 0`)
  hold the mortality input fixed.

Under either strategy, new cohorts (below the minimum age) attribute all
their growth to coming-of-age; migration among entering cohorts is not
modeled.

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
# Top-code at 81 ("81+") so sparse older ages form one open group.
stacked <- as.data.table(gss_homosex)[,
    .(age = pmin(age, 81), period = year, y = homosex)]
model <- stats::lm(y ~ age + period, data = stacked)
result <- decompose_aggregated(stacked, model, tol = 0.11)
print(result)
#> Strategy: sign attribution
#> 
#> Overview by period:
#>  period observed_mean modeled_mean intraindividual coming_of_age mortality
#>    1973         0.187        0.103              NA            NA        NA
#>    1974         0.207        0.112         0.00612      0.000956  0.003526
#>    1976         0.232        0.131         0.01232      0.003827  0.001812
#>    1977         0.218        0.145         0.00615      0.000279  0.004640
#>    1980         0.207        0.176         0.01843      0.005063  0.003495
#>    1982         0.195        0.199         0.01237      0.003471  0.000958
#>    1984         0.210        0.225         0.01238      0.001684  0.008891
#>    1985         0.197        0.228         0.00620      0.000372 -0.001906
#>    1987         0.172        0.253         0.01238      0.002863  0.004183
#>    1988         0.182        0.262         0.00620      0.000189  0.001667
#>    1989         0.211        0.268         0.00621      0.000292  0.001921
#>    1990         0.185        0.279         0.00624      0.000300  0.003663
#>    1991         0.203        0.293         0.00626      0.000304  0.004905
#>    1993         0.285        0.313         0.01243      0.002011  0.002510
#>    1994         0.288        0.328         0.00620      0.000545  0.000116
#>    1996         0.340        0.354         0.01236      0.001679  0.006760
#>    1998         0.359        0.370         0.01240      0.002052  0.001982
#>    2000         0.357        0.389         0.01249      0.002560  0.002202
#>    2002         0.394        0.408         0.01251      0.000612  0.004989
#>    2004         0.370        0.436         0.01245      0.003060  0.008133
#>    2006         0.391        0.450         0.01244      0.003632  0.000000
#>    2008         0.438        0.470         0.01249      0.002186  0.003608
#>    2010         0.492        0.492         0.01247      0.001185  0.004805
#>    2012         0.500        0.513         0.01252      0.002285  0.000862
#>    2014         0.549        0.530         0.01248      0.002498  0.000597
#>    2016         0.571        0.547         0.01245      0.001191  0.001427
#>    2018         0.621        0.579         0.01251      0.002997  0.010593
#>    2021         0.686        0.591         0.01875      0.002143 -0.000412
#>    2022         0.668        0.618         0.00622      0.001005  0.009832
#>    2024         0.604        0.634         0.01240      0.003345  0.001099
#>  period observed_mean modeled_mean intraindividual coming_of_age mortality
#>  outmigration inmigration
#>            NA          NA
#>             0   -0.001845
#>             0    0.001363
#>             0    0.002871
#>             0    0.004242
#>             0    0.006325
#>             0    0.002510
#>             0   -0.001409
#>             0    0.005582
#>             0    0.000858
#>             0   -0.001794
#>             0    0.000413
#>             0    0.002993
#>             0    0.002286
#>             0    0.007936
#>             0    0.005236
#>             0    0.000176
#>             0    0.001799
#>             0    0.000382
#>             0    0.004805
#>             0   -0.001947
#>             0    0.001371
#>             0    0.003983
#>             0    0.004602
#>             0    0.002197
#>             0    0.001518
#>             0    0.006191
#>             0   -0.008541
#>             0    0.009485
#>             0   -0.000700
#>  outmigration inmigration
#> 
#> Decomposition of total change:
#>                 Component  Value Percent
#>  At initial (modeled)      0.103        
#>  At end (modeled)          0.634        
#>  Total change              0.531   100.0
#>  - Intraindividual change  0.317    59.6
#>  - Population turnover     0.214    40.4
#>    - Mortality             0.097    18.2
#>    - Coming-of-age         0.055    10.3
#>    - In-migration          0.063    11.8
# }
```
