
<!-- README.md is generated from README.Rmd. Please edit that file -->

# socialchange

<!-- badges: start -->

[![R-CMD-check](https://github.com/elbersb/socialchange/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/elbersb/socialchange/actions/workflows/R-CMD-check.yaml)
[![Codecov](https://codecov.io/gh/elbersb/socialchange/branch/main/graph/badge.svg)](https://app.codecov.io/gh/elbersb/socialchange)
[![pkgdown](https://github.com/elbersb/socialchange/actions/workflows/pkgdown.yaml/badge.svg)](https://elbersb.github.io/socialchange/)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The `socialchange` package provides methods for decomposing
aggregate-level social change into components of intraindividual change
and population turnover. Population turnover includes replacement of
individuals (mortality and coming-of-age) and migration. The package
implements event-based, aggregated-data, and simulation-based
decomposition approaches, as well as classical cohort replacement
vs. intraindividual change (CR-IC) decompositions and Age-Period-Cohort
(APC) models.

## Installation

You can install the development version of socialchange from
[GitHub](https://github.com/elbersb/socialchange) with:

``` r
# install.packages("pak")
pak::pak("elbersb/socialchange")
```

## Example: Direct decomposition of events

As a basic example, we look at a decomposition of EU membership, and
whether population growth in the EU is due to intracountry changes in
population, or due to the admission of new countries. The decomposition
unit is the country. To run the `decompose_events` decomposition
function, we need two datasets: One dataset contains the events, in this
case the dates when countries entered or exited the EU. This dataset is
included in the `socialchange` package:

``` r
library(socialchange)
socialchange::eu_membership
#>           country       date event_type
#> 1         Belgium 1952-07-23    initial
#> 2          France 1952-07-23    initial
#> 3         Germany 1952-07-23    initial
#> 4           Italy 1952-07-23    initial
#> 5      Luxembourg 1952-07-23    initial
#> 6     Netherlands 1952-07-23    initial
#> 7  United Kingdom 1973-01-01      entry
#> 8         Denmark 1973-01-01      entry
#> 9         Ireland 1973-01-01      entry
#> 10         Greece 1981-01-01      entry
#> 11       Portugal 1986-01-01      entry
#> 12          Spain 1986-01-01      entry
#> 13        Austria 1995-01-01      entry
#> 14         Sweden 1995-01-01      entry
#> 15        Finland 1995-01-01      entry
#> 16         Cyprus 2004-05-01      entry
#> 17          Malta 2004-05-01      entry
#> 18        Hungary 2004-05-01      entry
#> 19         Poland 2004-05-01      entry
#> 20       Slovakia 2004-05-01      entry
#> 21         Latvia 2004-05-01      entry
#> 22        Estonia 2004-05-01      entry
#> 23      Lithuania 2004-05-01      entry
#> 24        Czechia 2004-05-01      entry
#> 25       Slovenia 2004-05-01      entry
#> 26       Bulgaria 2007-01-01      entry
#> 27        Romania 2007-01-01      entry
#> 28        Croatia 2013-07-01      entry
#> 29 United Kingdom 2020-01-31       exit
```

The second dataset contains the outcome that we want to decompose at any
point in time where there are change events, plus the start and end
period of interest. The EU was formed in 1952, and then countries
entered or exited in 1973, 1981, 1986, 1995, 2004, 2007, 2013, and 2020.
Countries, of course, entered on specific dates, but most population
data is only provided at yearly resolutions, so we assume that the error
introduced by using a yearly resolution is small. The `socialchange`
package also includes total population data for all countries of the
world from 1950 onwards, so it also includes the required data:

``` r
library(data.table)
wpp_data <- as.data.table(socialchange::wpp_data)
# population data is in 1,000
wpp_data[Location == "Belgium" & Time %in% c(1952, 1973, 1981, 1986, 1995, 2004, 2007, 2013, 2020)]
#>    Location  Time  PopTotal
#>      <char> <num>     <num>
#> 1:  Belgium  1952  8695.582
#> 2:  Belgium  1973  9727.980
#> 3:  Belgium  1981  9843.592
#> 4:  Belgium  1986  9887.815
#> 5:  Belgium  1995 10095.198
#> 6:  Belgium  2004 10456.163
#> 7:  Belgium  2007 10653.697
#> 8:  Belgium  2013 11103.257
#> 9:  Belgium  2020 11561.717
```

To run the `decompose_events` function, we need to ensure that columns
for the unit and time parameters are named the same, so we prepare the
data as follows:

``` r
# rename columns
setnames(wpp_data, c("Location", "Time"), c("country", "year"))
# population in millions
wpp_data[, PopTotal := PopTotal / 1000]

# coarsen to year - required as we don't have daily population data
eu_membership <- as.data.table(socialchange::eu_membership)
eu_membership[, year := year(as.Date(date))]
```

Finally, we run the decomposition. We specify the two datasets, and a
formula of the form `Outcome ~ Unit + Time`. We also need to specify the
end data, and an aggregation function. Often this will be mean, but in
our case it needs to be `sum`:

``` r
ev <- decompose_events(eu_membership, wpp_data, PopTotal ~ country + year,
  end_period = 2021, fun = sum)
print(ev)
#>     event_type      term    total      pct
#>         <char>     <num>    <num>    <num>
#> 1:      change  80.74284 265.3265 30.43151
#> 2: replacement 184.58363 265.3265 69.56849
```

The outcome of the decomposition tells us that intracountry changes in
population account for 30% of the total population increase (or 80
million), while additions and one (Br)exit account for 70% of the total
population increase (or 184 million).

It is also possible to produce a figure that shows the decomposition:

``` r
plot(ev)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

## Example: Decompose aggregated data

In this example we decompose survey data, based on the U.S. General
Social Survey. More details on this example can be found in the
[vignette](https://elbersb.github.io/socialchange/articles/gss_homosexuality.html#event-based-decomposition).

``` r
data(gss_homosex)

gss_all <- gss_homosex[age >= 21, .(age, period = year, sex, y = homosex, wtssall)]
model <- mgcv::gam(y ~ s(age) + s(period), data = gss_all, weights = wtssall)
result <- decompose_aggregated(gss_all, model, cells = "sex", weight = "wtssall", R = 100)
#> Computing 100 bootstrap replicate(s); this can take a while for gam models.
print(result, detailed = FALSE)
#>                 Component    Value Percent             95% CI
#>  At initial (modeled)      0.19709                           
#>  At end (modeled)          0.56741                           
#>  Total change              0.37032   100.0 [0.3487, 0.3959]  
#>  - Intraindividual change  0.19215   51.9  [0.1597, 0.2244]  
#>  - Population turnover     0.17817   48.1  [0.1516, 0.1981]  
#>    - Mortality             0.11958   32.3  [0.1126, 0.1256]  
#>    - Coming-of-age         0.10317   27.9  [0.0768, 0.1247]  
#>    - In-migration         -0.04457   -12.0 [-0.0472, -0.0412]
plot(result)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />
