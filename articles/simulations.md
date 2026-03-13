# Simulating social change

The package contains a function `sim_social_change` that allows the user
to explore different scenarios of how social change can happen. The
complexity of the simulation is up to the user to define. We’ll start
simple and build up a more complicated simulation over time.

## Starting with a simple simulation

Building up a simulation requires three steps:

- Define your population
- Define the outcome function for the variable of interest
- Define the population dynamics (coming of age, mortality, in- and
  out-migration)

**Step 1**: The first step is to define an initial population. As an
example, we define a simple population aged between 20 and 39, with a
uniform distribution until age 30, declining up until age 39. The
dataset that you define here needs at least two columns: `age` and `n`.
You can add more columns if you want to describe more complicated
populations, but we’ll stick with the simple setup for now. We’ll call
each row of this dataset a *population cell*. In the extreme case, you
can also define a population of individuals where each `n` is just 1.

Here’s our simple population:

``` r
library("data.table")

data <- data.table(
    age = c(20:39),
    n = c(rep(100, 10), seq.int(100, 10, by = -10))
)

data
#>       age     n
#>     <int> <num>
#>  1:    20   100
#>  2:    21   100
#>  3:    22   100
#>  4:    23   100
#>  5:    24   100
#>  6:    25   100
#>  7:    26   100
#>  8:    27   100
#>  9:    28   100
#> 10:    29   100
#> 11:    30   100
#> 12:    31    90
#> 13:    32    80
#> 14:    33    70
#> 15:    34    60
#> 16:    35    50
#> 17:    36    40
#> 18:    37    30
#> 19:    38    20
#> 20:    39    10
#>       age     n
#>     <int> <num>
```

**Step 2**: Next, we define the variable of interest that we want to
simulate. To do this, define a function that takes as arguments your
population cells and time as a continuous variable. Time really needs to
be continuous here - we need to know the outcome of every individual in
the population not only in terms of discrete ‘years’ (or whatever your
time scale), but at every point in-between as well. In our example, we
use an outcome that is only dependent on age: at age 20, the outcome is
0, and at age 40 the outcome is 1, with values linearly increasing by
age. This is purely a function of age, not of time, so in a real-world
survey, this outcome could be “support for investing in longevity
research” or “support for increasing pensions” - we’ll just use the
latter as an example:

``` r
support_for_pensions <- function(data, time) {
    data[, (age - 20) * 1 / 20]
}
# e.g., at age 20,   support_for_pensions is 0
#       at age 20.5, support_for_pensions is 0.026
#       at age 40,   support_for_pensions is 1
```

Because we defined our population only in terms of one variable (age),
all members of the same age also have an identical outcome value.

**Step 3**: The next step is to define the population dynamics. This
requires defining four functions:

- `fun_mortality(data, period)` - needs to return a vector of counts for
  each population cell
- `fun_outmigration(data, period)` - needs to return a vector of counts
  for each population cell
- `fun_coming_of_age(data, period)` - needs to return a data frame with
  new population cells and a column `n`
- `fun_inmigration(data, period)` - needs to return a data frame with
  new population cells and a column `n`

All of these functions are defined period-over-period. In practice, this
will often be years. For instance, the mortality function should return
counts that define how many members of your population die within the
next period, in each population cell. The out-migration function is
defined in the same way, just for the number of people migrating out of
each population cell.

The coming of age function and in-migration functions are different:
They should return a data frame that describes the new population
members that are added period-over-period. These functions are flexible
enough to describe very complicated real-world population dynamics. For
our first simple example, we will define these functions in a way that
keeps the population stable: we don’t have any migration, and mortality
and coming-of-age balances out perfectly - every period 100 new members
of age 20 are added, and 100 members age 30 and over die:

``` r
mortality <- function(data, period) {
    data[, ifelse(age >= 30, 10, 0)]
}
outmigration <- function(data, period) {
    0
}
coming_of_age <- function(data, period) {
    data.table(age = 20, n = 100)
}
inmigration <- function(data, period) {
    data.table(n = 0)
}
```

We’re now ready to start the simulation:

``` r
library(socialchange)

simresult <- socialchange::sim_social_change(
    periods = 5,
    data = data,
    fun_y = support_for_pensions,
    fun_mortality = mortality,
    fun_outmigration = outmigration,
    fun_coming_of_age = coming_of_age,
    fun_inmigration = inmigration
)
simresult
#>   period   mean    N intraindividual mortality outmigration coming_of_age
#>  initial 0.3758 1550              NA        NA           NA            NA
#>        1 0.3758 1550            0.05  -0.02414            0      -0.02586
#>        2 0.3758 1550            0.05  -0.02414            0      -0.02586
#>        3 0.3758 1550            0.05  -0.02414            0      -0.02586
#>        4 0.3758 1550            0.05  -0.02411            0      -0.02589
#>        5 0.3758 1550            0.05  -0.02415            0      -0.02585
#>  inmigration
#>           NA
#>            0
#>            0
#>            0
#>            0
#>            0
#> 
#> Decomposition of total change:
#>                 Component   Value
#>  At initial                0.3758
#>  At end                    0.3758
#>  Total change              0.0000
#>  - Intraindividual change  0.2500
#>  - Population turnover    -0.2500
#>    - Mortality            -0.1207
#>    - Out-migration         0.0000
#>    - Coming-of-age        -0.1293
#>    - In-migration          0.0000
```

The simulation result displays both an overview for each period, as well
as a summary for the total decomposition of change. The results by year
show that indeed our population is stable: at the end of every period we
have a population of 1550, and a stable mean of 0.38 - i.e, 38% of this
population supports increasing pensions. However, this stability is a
bit deceptive, because everyone’s opinion actually keeps constantly
changing. We can see that by looking at the “Intraindividual change”
component, which is +0.25. Roughly speaking, without population
turnover, we would have expected that the mean support for increasing
pensions would have increased quite a bit, to 63%. This makes sense, as
we’ve designed this simulation in a way where support increases with
age. In turn, younger people are much less likely to support increasing
pensions. Hence, when they enter the population, that has the effect of
decreasing the mean. The same is true for mortality. Hence, the
population turnover component is negative. In this simulation, the two
components balance out exactly, leading to the expected complete
stabilty in the year-over-year mean.

## Adding a covariate

For a slightly more complex setup, we can add covariates to the
simulation. For instance, we can introduce a gender covariate. As an
example, we split the former population into two equally-distributed
genders:

``` r
data <- data.table(
    age = c(20:39, 20:39),
    gender = c(rep("f", 20), rep("m", 20)),
    n = c(
        c(rep(50, 10), seq.int(50, 5, by = -5)),
        c(rep(50, 10), seq.int(50, 5, by = -5))
    )
)
```

We also adjust the population dynamics functions to keep the population
stable. Note that these don’t depend on gender (but they could):

``` r
mortality <- function(data, period) {
    data[, ifelse(age >= 30, 5, 0)]
}
coming_of_age <- function(data, period) {
    data.table(age = c(20, 20), gender = c("f", "m"), n = c(50, 50))
}
```

If you reran the simulation with these settings, you will get the exact
same results as before. We will introduce a slight variation, though,
and determine that one gender actually has a fixed opinion on the
support for increasing pensions. The other gender behaves as before:

``` r
support_for_pensions <- function(data, time) {
    data[, ifelse(gender == "f", 0.3758, (age - 20) * 1 / 20)]
}
```

``` r
simresult <- socialchange::sim_social_change(
    periods = 5,
    data = data,
    fun_y = support_for_pensions,
    fun_mortality = mortality,
    fun_outmigration = outmigration,
    fun_coming_of_age = coming_of_age,
    fun_inmigration = inmigration
)
simresult
#>   period   mean    N intraindividual mortality outmigration coming_of_age
#>  initial 0.3758 1550              NA        NA           NA            NA
#>        1 0.3758 1550         0.02495  -0.01204            0      -0.01292
#>        2 0.3758 1550         0.02512  -0.01215            0      -0.01297
#>        3 0.3758 1550         0.02504  -0.01209            0      -0.01296
#>        4 0.3758 1550         0.02500  -0.01204            0      -0.01296
#>        5 0.3758 1550         0.02499  -0.01206            0      -0.01293
#>  inmigration
#>           NA
#>            0
#>            0
#>            0
#>            0
#>            0
#> 
#> Decomposition of total change:
#>                 Component    Value
#>  At initial                0.37580
#>  At end                    0.37580
#>  Total change              0.00000
#>  - Intraindividual change  0.12511
#>  - Population turnover    -0.12511
#>    - Mortality            -0.06037
#>    - Out-migration         0.00000
#>    - Coming-of-age        -0.06474
#>    - In-migration          0.00000
```

Although the overall mean doesn’t change over time, the decomposition
results have changed. Because only men now contribute to both population
turnover and intraindividual change (and women do not), the components
are half of what they were before.

## Only intraindividual change

In the previous scenario, the overall outcome was stable, and we had
both intraindividual change as well as population turnover, cancelling
each other out. We can also construct a case that shows only
intraindividual change, by making the outcome function dependent only on
time (and possibly other covariates, as we do here), but not on age.

``` r
ic_only <- function(data, time) {
    data[, ifelse(gender == "f", 0.2, 0.4) + time / 10]
}
```

As expected, the results show that the outcome increased by 0.1
year-over-year, and that all of this change is due to intraindividual
change:

``` r
simresult <- socialchange::sim_social_change(
    periods = 5,
    data = data,
    fun_y = ic_only,
    fun_mortality = mortality,
    fun_outmigration = outmigration,
    fun_coming_of_age = coming_of_age,
    fun_inmigration = inmigration
)
simresult
#>   period mean    N intraindividual    mortality outmigration coming_of_age
#>  initial  0.3 1550              NA           NA           NA            NA
#>        1  0.4 1550             0.1 -0.000014270            0   0.000014270
#>        2  0.5 1550             0.1 -0.000005525            0   0.000005525
#>        3  0.6 1550             0.1  0.000001456            0  -0.000001456
#>        4  0.7 1550             0.1 -0.000007061            0   0.000007061
#>        5  0.8 1550             0.1 -0.000012405            0   0.000012405
#>  inmigration
#>           NA
#>            0
#>            0
#>            0
#>            0
#>            0
#> 
#> Decomposition of total change:
#>                 Component     Value
#>  At initial                0.300000
#>  At end                    0.800000
#>  Total change              0.500000
#>  - Intraindividual change  0.500000
#>  - Population turnover     0.000000
#>    - Mortality            -0.000038
#>    - Out-migration         0.000000
#>    - Coming-of-age         0.000038
#>    - In-migration          0.000000
```

## Only population turnover

Conversely, we can also construct a scenario where all change is
explained by population turnover - older cohorts being replaced by
younger cohorts. For this, we define the outcome as a function of the
individual’s birth cohort:

``` r
pt_only <- function(data, time) {
    data[, ifelse(gender == "f", 0.2, 0.4) + 0.7516 + (time - age + 20) / 10]
}
```

The constants have been chosen such that the two cases align. The
important thing here is that the outcome depends only on the birth
cohort (i.e. cohort = year - age).

The results show the exact same dynamic as before – an initial mean of
0.3 and an increase by 0.1 every year –, but very different
explanations: Now the change is completely explained by population
turnover. No one ever changes their mind, social change only occurs
because older cohorts with low outcome values die out and younger
cohorts with higher outcome values follow. Hence, both mortality and
coming-of-age contribute equally to the total change.

``` r
simresult <- socialchange::sim_social_change(
    periods = 5,
    data = data,
    fun_y = pt_only,
    fun_mortality = mortality,
    fun_outmigration = outmigration,
    fun_coming_of_age = coming_of_age,
    fun_inmigration = inmigration
)
simresult
#>   period mean    N intraindividual mortality outmigration coming_of_age
#>  initial  0.3 1550              NA        NA           NA            NA
#>        1  0.4 1550               0   0.04835            0       0.05165
#>        2  0.5 1550               0   0.04828            0       0.05172
#>        3  0.6 1550               0   0.04832            0       0.05168
#>        4  0.7 1550               0   0.04830            0       0.05170
#>        5  0.8 1550               0   0.04835            0       0.05165
#>  inmigration
#>           NA
#>            0
#>            0
#>            0
#>            0
#>            0
#> 
#> Decomposition of total change:
#>                 Component  Value
#>  At initial               0.3000
#>  At end                   0.8000
#>  Total change             0.5000
#>  - Intraindividual change 0.0000
#>  - Population turnover    0.5000
#>    - Mortality            0.2416
#>    - Out-migration        0.0000
#>    - Coming-of-age        0.2584
#>    - In-migration         0.0000
```
