# Decomposing General Social Survey data: Attitudes on Homosexuality

The package includes a dataset `gss_homosex`, covering General Social
Survey data on attitudes towards homosexuality in the U.S. This data was
also analyzed in Ekstam (2021).

## Packages

``` r

library("socialchange")
library("modelsummary")
library("ggplot2")
library("mgcv")

data(gss_homosex)
```

## Descriptives

The `homosex` outcome is rescaled to \[0, 1\], where 0 means “always
wrong” and 1 means “not wrong at all”. The table below gives summary
statistics; the line plot shows a clear upward trend in acceptance over
the survey period.

``` r

# compare to Table 1 in Ekstam -- roughly similar
modelsummary::datasummary(
  homosex + year + cohort + age + educ ~ mean + SD + min + max,
  data = gss_homosex, output = "markdown", fmt = 3)
```

|                                  | mean     | SD     | min      | max      |
|----------------------------------|----------|--------|----------|----------|
| homosexual sex relations         | 0.315    | 0.436  | 0.000    | 1.000    |
| gss year for this respondent     | 1993.803 | 12.928 | 1973.000 | 2016.000 |
| cohort                           | 1947.852 | 20.826 | 1892.000 | 1995.000 |
| age of respondent                | 45.950   | 17.485 | 18.000   | 89.000   |
| highest year of school completed | 12.835   | 3.171  | 0.000    | 20.000   |

``` r


# sex and race are categorical, so show their level breakdown instead
modelsummary::datasummary(
  sex + race ~ N + Percent(),
  data = gss_homosex, output = "markdown", fmt = 1)
```

|      |        | N     | Percent |
|------|--------|-------|---------|
| sex  | female | 19403 | 55.3    |
|      | male   | 15703 | 44.7    |
| race | black  | 4460  | 12.7    |
|      | other  | 1836  | 5.2     |
|      | white  | 28810 | 82.1    |

``` r


by_year <- gss_homosex[, list(y = weighted.mean(homosex, wtssall)), by = c("year")]
ggplot(by_year, aes(x = year, y = y)) + geom_line() + ylim(0, 1) + theme_light()
```

![](gss_homosexuality_files/figure-html/unnamed-chunk-2-1.png)

To separate period and cohort effects, we fit a GAM with a
two-dimensional smooth over survey year and birth cohort. The 3D surface
and the contour plot below both shows that acceptance increased more
strongly among younger cohorts and in later periods.

``` r

splinemodel = gam(homosex ~ s(year, cohort), data = gss_homosex)
vis.gam(splinemodel, theta = 40)
```

![](gss_homosexuality_files/figure-html/unnamed-chunk-3-1.png)

``` r

plot_gam_surface(splinemodel)
```

![](gss_homosexuality_files/figure-html/unnamed-chunk-3-2.png)

## CR-IC decomposition

The
[`cr_ic()`](https://elbersb.github.io/socialchange/reference/cr_ic.md)
function implements four methods for separating intracohort change (IC)
from cohort replacement (CR): algebraic decomposition (AD), linear
decomposition (LD), and two model-based improvements (AD+ and Model).
For a detailed explanation of each method, including a replication of
Firebaugh (1989), see the [Replications:
Firebaugh](https://elbersb.github.io/socialchange/articles/replicating_firebaugh.md)
vignette.

Acceptance rose by about 43 percentage points between 1973 and 2018
(from 0.19 to 0.62). Using the full panel, the two model-based methods
both attribute the larger share to intracohort change, though they
differ on the exact split: AD+ gives roughly 63% intracohort change and
37% cohort replacement, while the Model method gives roughly 56% and
44%.

``` r

# complete period
form <- homosex ~ as.factor(year) + as.factor(cohort)
(res <- cr_ic(gss_homosex, homosex ~ year + cohort, weight = "wtssall", model = form))
#> Cohort decomposition (year-over-year) with 26 periods:
#>    1973, 1974, 1976, 1977, 1980, 1982, 1984, 1985, 1987, 1988, 1989, 1990, 1991, 1993, 1994, 1996, 1998, 2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016
#> 
#> Summary for entire period:
#>       1973      2016 Difference
#>      <num>     <num>      <num>
#>  0.1893911 0.5642217  0.3748306
#> 
#> Decompositions:
#>  method factor     value         %
#>  <char> <char>     <num>     <num>
#>      LD  total 0.3748306 100.00000
#>      LD     IC 0.2062929  55.03632
#>      LD     CR 0.1685376  44.96368
#>      LD  resid 0.0000000        NA
#>      AD  total 0.3748306 100.00000
#>      AD     IC 0.2422175  64.62053
#>      AD     CR 0.1326131  35.37947
#>      AD  resid 0.0000000        NA
#>     AD+  total 0.3748306 100.00000
#>     AD+     IC 0.2379691  63.48710
#>     AD+     CR 0.1368615  36.51290
#>     AD+  resid 0.0000000        NA
#>   Model  total 0.3748306 100.00000
#>   Model     IC 0.2047557  54.62619
#>   Model     CR 0.1700749  45.37381
#>   Model  resid 0.0000000        NA
```

``` r

plot(res)
```

![](gss_homosexuality_files/figure-html/unnamed-chunk-5-1.png)

When only the first and last survey years are used, the balance shifts
slightly toward cohort replacement (~54% CR, ~46% IC). This illustrates
how the choice of time points can affect decomposition results, and why
the model-based AD+ and Model methods are preferred over the simpler AD.

``` r

# only beginning and end year (also compare to Baunach 2011)
form <- homosex ~ as.factor(year) + splines::bs(cohort, 10)
(res <- cr_ic(gss_homosex[year %in% c(min(year), max(year))], homosex ~ year + cohort, weight = "wtssall", model = form))
#> Cohort decomposition (year-over-year) with 2 periods:
#>    1973, 2016
#> 
#> Summary for entire period:
#>       1973      2016 Difference
#>      <num>     <num>      <num>
#>  0.1893911 0.5642217  0.3748306
#> 
#> Decompositions:
#>  method factor     value         %
#>  <char> <char>     <num>     <num>
#>      LD  total 0.3748306 100.00000
#>      LD     IC 0.1549435  41.33694
#>      LD     CR 0.2198871  58.66306
#>      LD  resid 0.0000000        NA
#>      AD  total 0.3748306 100.00000
#>      AD     IC 0.0764078  20.38462
#>      AD     CR 0.2984228  79.61538
#>      AD  resid 0.0000000        NA
#>     AD+  total 0.3748306 100.00000
#>     AD+     IC 0.1923286  51.31080
#>     AD+     CR 0.1825020  48.68920
#>     AD+  resid 0.0000000        NA
#>   Model  total 0.3748306 100.00000
#>   Model     IC 0.1961770  52.33750
#>   Model     CR 0.1786536  47.66250
#>   Model  resid 0.0000000        NA
```

## Event-based decomposition

The
[`decompose_aggregated()`](https://elbersb.github.io/socialchange/reference/decompose_aggregated.md)
function separates change in the aggregate outcome into intraindividual
change (attitude shifts within respondents already in the population)
and population turnover (mortality and coming-of-age effects). We rename
`year` and `homosex` to the column names the function expects, then pass
the full dataset directly.

``` r

gss_all <- gss_homosex[, .(age, period = year, sex, y = homosex, wtssall)]
```

We fit a GAM to the individual responses to model acceptance as a smooth
function of age and period.

``` r

set.seed(42)
model <- mgcv::gam(y ~ s(age) + s(period), data = gss_all, weights = wtssall)
predict_y <- function(newdata) as.vector(mgcv::predict.gam(model, newdata = newdata))
```

### The simplest decomposition

We can run the simplest case of the event decomposition with the
following commmand:

``` r

result <- decompose_aggregated(gss_all, predict_y, weight = "wtssall")
print(result, detailed = FALSE)
#>                 Component    Value Percent
#>  At initial (modeled)     0.200317        
#>  At end (modeled)         0.566382        
#>  Total change             0.366065   100.0
#>  - Intraindividual change 0.227625   62.2 
#>  - Population turnover    0.138439   37.8 
#>    - Mortality            0.099504   27.2 
#>    - Coming-of-age        0.032009   8.7  
#>    - In-migration         0.006926   1.9
```

This decomposition compares cells across years, attributing shrinking
cohorts to mortality and the youngest cohorts to coming-of-age. Every
change in a cell’s size must be attributed to *some* demographic event —
the decomposition never leaves a residual — so a survivor cohort that
*grows* between two waves is credited to net in-migration. With only the
survey to work from, this in-migration term is small (about +0.007, or
1.9% of the total change) and largely reflects sampling fluctuation in
cell sizes across waves rather than genuine immigration. It becomes
meaningful only once a true population frame is supplied (see below),
where a growing cohort really does signal net immigration.

Of the roughly 41 percentage point rise in acceptance between 1973 and
2018, the decomposition attributes about 62% to intraindividual change
and 38% to population turnover (mortality of older, less accepting
cohorts; coming-of-age of younger, more accepting ones; and a small net
in-migration term). This falls within the range spanned by the CR-IC
model-based methods above (37–44% turnover), closest to the AD+
estimate.

``` r

plot(result)
```

![](gss_homosexuality_files/figure-html/unnamed-chunk-10-1.png)

What is most interesting about these results is how the components
behave over time. Until the early-to-mid 1990s, intraindividual change
and population turnover offset each other, leaving the aggregate mean
roughly flat. The mean even dipped in the mid-1980s, falling from about
0.21 in 1977 to 0.18 in 1987, as the existing members of the population
became less accepting of homosexuality. Turnover pulled the mean back up
over the same period, so that by the early 1990s acceptance had barely
moved from its 1973 level. Only from the mid-1990s did intraindividual
change start to contribute toward acceptance, and from that point on it
became the main driver of the rising trend. Turnover contributed in the
positive direction throughout. Mortality of older, less accepting
cohorts had a much stronger effect than the coming-of-age of younger
ones, likely because new members of the population are already closer to
the population mean.

One speculative reading of the early decline in intraindividual
acceptance is the backlash associated with the AIDS epidemic of the
1980s. The decomposition itself cannot establish this, but it does
isolate a within-person decline that is invisible in the raw trend line.

### Adding a covariate

We can split the population into covariate cells with the `cells`
argument. Adding `sex` lets the outcome model predict differently for
men and women, and derives the demographic events separately within each
sex cell.

``` r

model <- mgcv::gam(y ~ s(age) + s(period) + sex, data = gss_all, weights = wtssall)
predict_y <- function(newdata) as.vector(mgcv::predict.gam(model, newdata = newdata))
set.seed(42)
result <- decompose_aggregated(gss_all, predict_y, cells = "sex", weight = "wtssall")
print(result, detailed = FALSE)
#>                 Component    Value Percent
#>  At initial (modeled)      0.20030        
#>  At end (modeled)          0.56646        
#>  Total change              0.36616   100.0
#>  - Intraindividual change  0.22339   61.0 
#>  - Population turnover     0.14277   39.0 
#>    - Mortality             0.12283   33.5 
#>    - Coming-of-age         0.03215   8.8  
#>    - In-migration         -0.01221   -3.3
plot(result)
```

![](gss_homosexuality_files/figure-html/unnamed-chunk-11-1.png)

The headline result is unchanged: the same ~61/39
intraindividual-versus-turnover split as the pooled model. Conditioning
on sex barely moves the population-weighted mean, since the sex
composition is roughly stable across waves.

### Adding a population frame

So far the cell counts `n` have come from the survey itself, so the
demographic events are inferred from the noisy survey cell sizes across
waves. When true population counts are available, the `population`
argument lets the survey supply only the outcome model `fun_y`, while an
external frame supplies the cell counts that drive the demographic
events.

The package ships `wpp_us`, US population by age and sex from the UN
World Population Prospects for each survey year. Only the *relative*
cell structure matters, so we rescale each wave to a tractable
per-period total before passing it in.

``` r

data(wpp_us)

survey_years <- sort(unique(gss_all$period))
pop <- wpp_us[period %in% survey_years]
scale_total <- round(mean(gss_all[, .N, by = period]$N))
pop[, n := n / sum(n) * scale_total, by = period]

set.seed(42)
result <- decompose_aggregated(gss_all, predict_y, cells = "sex",
                               weight = "wtssall", population = pop)
print(result, detailed = FALSE)
#>                 Component    Value Percent
#>  At initial (modeled)      0.19663        
#>  At end (modeled)          0.57249        
#>  Total change              0.37586   100.0
#>  - Intraindividual change  0.22978   61.1 
#>  - Population turnover     0.14608   38.9 
#>    - Mortality             0.10116   26.9 
#>    - Coming-of-age         0.06838   18.2 
#>    - In-migration         -0.02347   -6.2
plot(result)
```

![](gss_homosexuality_files/figure-html/unnamed-chunk-12-1.png)

The intraindividual/turnover split is again about 61/39, but the
turnover detail now reflects real demographics: coming-of-age roughly
doubles and mortality falls, because the true US age structure has more
young entrants than the survey implied, and net in-migration becomes a
genuine signal of immigration rather than survey noise.

## Further reading

For APC analysis of the same `gss_homosex` dataset, see the [APC
vignette](https://elbersb.github.io/socialchange/articles/apc.md).
