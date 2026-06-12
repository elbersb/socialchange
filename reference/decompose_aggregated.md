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
  per-period total first – only the relative cell structure matters.
  Periods and cells need not match the survey exactly; cells absent from
  the survey are still handled because `fun_y` can predict their
  outcome.

- x:

  A \`social_change_decomp\` object returned by
  \[decompose_aggregated()\].

- ...:

  Not used.

## Value

S3 object of class `social_change_decomp` with components:

- `summary`: data.table with decomposition components by period
  (including the `inmigration` and `outmigration` columns; print/plot
  show a migration component only for whichever of these is non-zero)

- `record`: list of detailed event records for each period transition

## Details

The function estimates mortality, coming-of-age, and net in-migration
from period-to-period population differences within cells, then uses
microsimulation to randomly order demographic events and track their
contribution to aggregate change. Unequal and multi-year gaps between
periods are supported: when the gap exceeds one year, each entering
cohort is assigned to the specific calendar year within the gap when it
crosses the minimum age, so that post-entry aging is correctly
attributed to intraindividual change rather than coming-of-age.

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
cannot be seen. New cohorts (below the minimum age in the earlier
period) attribute all their growth to coming-of-age; migration among
entering cohorts is not modeled. On noisy survey cells this strategy
relabels sampling fluctuation as in-migration and mortality, so the
inferred in-migration is most meaningful when `population` supplies a
true population frame, where growing cohorts reflect genuine net
immigration rather than survey noise.

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
#>  1:   1973 0.1876190 0.1286709
#>  2:   1974 0.2092385 0.1373759
#>  3:   1976 0.2334528 0.1534639
#>  4:   1977 0.2187208 0.1643362
#>  5:   1980 0.2072464 0.1908825
#>  6:   1982 0.2085096 0.2085745
#>  7:   1984 0.2101606 0.2318568
#>  8:   1985 0.1969697 0.2336910
#>  9:   1987 0.1809661 0.2541551
#> 10:   1988 0.1820128 0.2630115
#> 11:   1989 0.2107793 0.2680154
#> 12:   1990 0.1845033 0.2771405
#> 13:   1991 0.2041260 0.2898777
#> 14:   1993 0.2840985 0.3062341
#> 15:   1994 0.2878949 0.3195715
#> 16:   1996 0.3406532 0.3423002
#> 17:   1998 0.3588710 0.3557258
#> 18:   2000 0.3579073 0.3714806
#> 19:   2002 0.3956670 0.3872281
#> 20:   2004 0.3680556 0.4121824
#> 21:   2006 0.3907865 0.4232998
#> 22:   2008 0.4371497 0.4400315
#> 23:   2010 0.4913722 0.4592941
#> 24:   2012 0.5004068 0.4759978
#> 25:   2014 0.5475752 0.4906554
#> 26:   2016 0.5724020 0.5027095
#>     period  observed   modeled
#>      <num>     <num>     <num>
#> Warning: Modeled means deviate from observed by up to 52.3% (tol = 5.0%), evaluated on the survey's own age structure. Consider a more flexible model or increase tol.
print(result)
#> Overview by period:
#>  period observed_mean modeled_mean intraindividual coming_of_age mortality
#>    1973        0.1876       0.1287              NA            NA        NA
#>    1974        0.2092       0.1374        0.004560     0.0009304 0.0040052
#>    1976        0.2335       0.1535        0.009119     0.0037889 0.0029460
#>    1977        0.2187       0.1643        0.004560     0.0002757 0.0034757
#>    1980        0.2072       0.1909        0.013679     0.0050986 0.0038653
#>    1982        0.2085       0.2086        0.009119     0.0030527 0.0026645
#>    1984        0.2102       0.2319        0.009119     0.0018615 0.0085626
#>    1985        0.1970       0.2337        0.004560     0.0003676 0.0002926
#>    1987        0.1810       0.2542        0.009119     0.0023430 0.0067125
#>    1988        0.1820       0.2630        0.004560     0.0002361 0.0042905
#>    1989        0.2108       0.2680        0.004560     0.0002856 0.0041283
#>    1990        0.1845       0.2771        0.004560     0.0003085 0.0055015
#>    1991        0.2041       0.2899        0.004560     0.0003058 0.0070991
#>    1993        0.2841       0.3062        0.009119     0.0020286 0.0042457
#>    1994        0.2879       0.3196        0.004560     0.0005338 0.0009168
#>    1996        0.3407       0.3423        0.009119     0.0016655 0.0082342
#>    1998        0.3589       0.3557        0.009119     0.0020468 0.0045996
#>    2000        0.3579       0.3715        0.009119     0.0025720 0.0045068
#>    2002        0.3957       0.3872        0.009119     0.0005919 0.0056089
#>    2004        0.3681       0.4122        0.009119     0.0030458 0.0098300
#>    2006        0.3908       0.4233        0.009119     0.0037174 0.0014697
#>    2008        0.4371       0.4400        0.009119     0.0021395 0.0052961
#>    2010        0.4914       0.4593        0.009119     0.0011823 0.0065167
#>    2012        0.5004       0.4760        0.009119     0.0021721 0.0038592
#>    2014        0.5476       0.4907        0.009119     0.0020404 0.0025305
#>    2016        0.5724       0.5027        0.009119     0.0000000 0.0037670
#>  period observed_mean modeled_mean intraindividual coming_of_age mortality
#>  outmigration inmigration
#>            NA          NA
#>             0  -0.0007903
#>             0   0.0002337
#>             0   0.0025612
#>             0   0.0039033
#>             0   0.0028555
#>             0   0.0037387
#>             0  -0.0033857
#>             0   0.0022892
#>             0  -0.0002299
#>             0  -0.0039697
#>             0  -0.0012446
#>             0   0.0007727
#>             0   0.0009628
#>             0   0.0073271
#>             0   0.0037096
#>             0  -0.0023402
#>             0  -0.0004434
#>             0   0.0004274
#>             0   0.0029592
#>             0  -0.0031891
#>             0   0.0001766
#>             0   0.0024441
#>             0   0.0015531
#>             0   0.0009673
#>             0  -0.0008323
#>  outmigration inmigration
#> 
#> Decomposition of total change:
#>                 Component   Value Percent
#>  At initial (modeled)     0.12867        
#>  At end (modeled)         0.50271        
#>  Total change             0.37404   100.0
#>  - Intraindividual change 0.19607   52.4 
#>  - Population turnover    0.17797   47.6 
#>    - Mortality            0.11492   30.7 
#>    - Coming-of-age        0.04259   11.4 
#>    - In-migration         0.02046   5.5  
# }
```
