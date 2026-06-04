# Decomposing social change on aggregated data

To use the event-based decomposition, we assume you have some kind of
aggregated data that measures the same outcome across several points in
time. This could for instance be data from the General Social Survey,
which asks the same population (but not necessarily the same people) the
same question every year. The idea of the event-based decomposition is
based on the idea that we can follow a group of similar people over
time, which are assumed to have similar characteristics. For instance,
we can observe the number of white women, age 35, in year 1, and then
compare that number to the number of white women, age 36, in year 2. If
we assume that individuals don’t move across cells and ignore migration,
it must be true that (Number of white women, age 36 in year 2) = (Number
of white women, age 35 in year 1) - (Mortality between year 1 and year 2
within this cell). Mortality counts are thus directly estimable from the
data. (This might be noisy when dealing with survey data with smaller
sample sizes. Preprocessing might therefore be required.)

If we continue to ignore migration, the only other input that is
required is a function that predicts the outcome for a given cell. This
function can be as complex or simple as needed, it just needs to fulfill
a number of requirements:

- Takes as inputs the defined cells (such as age and gender), plus
  current period. It is not necessary that these are all used for the
  model’s predictions, though.
- Needs to support fully continuous periods (e.g., for a survey it needs
  to provide a prediction not only for 2010 and 2011, but also for
  2010.75 - three quarters of a year into 2010). This is required to
  estimate outcomes throughout the year as individuals leave and enter
  the population.
- Needs to be complex enough to fully model the observed means in each
  period. The decomposition fully acts on the modeled outcomes, not on
  the observed outcomes, so if these means don’t match the decomposition
  results will deviate from the observed data.

## Starting with a simple simulation (Scenario 1)

``` r

library("data.table")

data <- data.table(
    age = c(20:39),
    n = c(rep(100, 10), seq.int(100, 10, by = -10))
)

support_for_pensions <- function(data, time) {
    data[, (age - 20) * 1 / 20]
}
coming_of_age <- function(data, period) {
    data.table(age = 20, n = 100)
}
mortality <- function(data, period) {
    data[, ifelse(age >= 30, 10, 0)]
}

library("socialchange")
simresult <- socialchange::sim_social_change(
    periods = 5,
    data = data,
    fun_y = support_for_pensions,
    fun_coming_of_age = coming_of_age,
    fun_mortality = mortality
)
print(simresult, detailed = FALSE)
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

We now feed the simulated data into the event-based decomposition:

``` r

stacked_data <- rbindlist(simresult$snapshot)
# we know that y depends only on age here
model <- lm(y ~ age, data = stacked_data)
predict_y <- function(newdata) { predict(model, newdata = newdata) }

decompresult <- decompose_aggregated(stacked_data, predict_y)
print(decompresult, detailed = FALSE)
#>                 Component   Value
#>  At initial (modeled)      0.3758
#>  At end (modeled)          0.3758
#>  Total change              0.0000
#>  - Intraindividual change  0.2500
#>  - Population turnover    -0.2500
#>    - Mortality            -0.1207
#>    - Coming-of-age        -0.1293
#> Assumes no in- or out-migration.
```

The event-based decomposition has exactly recovered the results.

## Adding a covariate (Scenario 2)

``` r

data <- data.table(
    age = c(20:39, 20:39),
    gender = c(rep("f", 20), rep("m", 20)),
    n = c(
        c(rep(50, 10), seq.int(50, 5, by = -5)),
        c(rep(50, 10), seq.int(50, 5, by = -5))
    )
)

coming_of_age <- function(data, period) {
    data.table(age = c(20, 20), gender = c("f", "m"), n = c(50, 50))
}
mortality <- function(data, period) {
    data[, ifelse(age >= 30, 5, 0)]
}
support_for_pensions <- function(data, time) {
    data[, ifelse(gender == "f", 0.3758, (age - 20) * 1 / 20)]
}

simresult <- socialchange::sim_social_change(
    periods = 5,
    data = data,
    fun_y = support_for_pensions,
    fun_coming_of_age = coming_of_age,
    fun_mortality = mortality
)
print(simresult, detailed = FALSE)
#>                 Component    Value
#>  At initial                0.37580
#>  At end                    0.37580
#>  Total change              0.00000
#>  - Intraindividual change  0.12491
#>  - Population turnover    -0.12490
#>    - Mortality            -0.06030
#>    - Out-migration         0.00000
#>    - Coming-of-age        -0.06461
#>    - In-migration          0.00000
```

For the event-based decomposition, we first try a misspecified model
that ignores gender entirely. Still, the decomposition in this case
recovers the result almost exactly:

``` r

stacked_data <- rbindlist(simresult$snapshot)
model_no_gender <- lm(y ~ age + period, data = stacked_data)
cor(stacked_data$y, predict(model_no_gender))
#> [1] 0.6870648
predict_y <- function(newdata) { predict(model_no_gender, newdata = newdata) }

decomp <- decompose_aggregated(stacked_data, predict_y)
print(decomp, detailed = FALSE)
#>                 Component    Value
#>  At initial (modeled)      0.37580
#>  At end (modeled)          0.37580
#>  Total change              0.00000
#>  - Intraindividual change  0.12500
#>  - Population turnover    -0.12500
#>    - Mortality            -0.06032
#>    - Coming-of-age        -0.06468
#> Assumes no in- or out-migration.
```

And here are the results for the fully-specified model:

``` r

model_gender <- lm(y ~ age * gender, data = stacked_data)
cor(stacked_data$y, predict(model_gender))
#> [1] 1
predict_y <- function(newdata) { predict(model_gender, newdata = newdata) }

decomp <- decompose_aggregated(stacked_data, predict_y, "gender")
print(decomp, detailed = FALSE)
#>                 Component    Value
#>  At initial (modeled)      0.37580
#>  At end (modeled)          0.37580
#>  Total change              0.00000
#>  - Intraindividual change  0.12511
#>  - Population turnover    -0.12511
#>    - Mortality            -0.06035
#>    - Coming-of-age        -0.06475
#> Assumes no in- or out-migration.
```

## Only intraindividual change (Scenario 3)

``` r

ic_only <- function(data, time) {
    data[, ifelse(gender == "f", 0.2, 0.4) + time / 10]
}

simresult <- socialchange::sim_social_change(
    periods = 5,
    data = data,
    fun_y = ic_only,
    fun_coming_of_age = coming_of_age,
    fun_mortality = mortality
)
print(simresult, detailed = FALSE)
#>                 Component     Value
#>  At initial                0.300000
#>  At end                    0.800000
#>  Total change              0.500000
#>  - Intraindividual change  0.500000
#>  - Population turnover     0.000000
#>    - Mortality             0.000069
#>    - Out-migration         0.000000
#>    - Coming-of-age        -0.000069
#>    - In-migration          0.000000
```

Again, we first try a misspecified model that ignores gender entirely.
Again, the decomposition recovers the result almost exactly despite the
misspecification:

``` r

stacked_data <- rbindlist(simresult$snapshot)
model_no_gender <- lm(y ~ age + period, data = stacked_data)
cor(stacked_data$y, predict(model_no_gender))
#> [1] 0.8629489
predict_y <- function(newdata) { predict(model_no_gender, newdata = newdata) }

decomp <- decompose_aggregated(stacked_data, predict_y)
print(decomp, detailed = FALSE)
#>                 Component Value
#>  At initial (modeled)       0.3
#>  At end (modeled)           0.8
#>  Total change               0.5
#>  - Intraindividual change   0.5
#>  - Population turnover      0.0
#>    - Mortality              0.0
#>    - Coming-of-age          0.0
#> Assumes no in- or out-migration.
```

And here are the results for the fully-specified model:

``` r

model_gender <- lm(y ~ age + period + gender, data = stacked_data)
cor(stacked_data$y, predict(model_gender))
#> [1] 1
predict_y <- function(newdata) { predict(model_gender, newdata = newdata) }

decomp <- decompose_aggregated(stacked_data, predict_y, "gender")
print(decomp, detailed = FALSE)
#>                 Component     Value
#>  At initial (modeled)      0.300000
#>  At end (modeled)          0.800000
#>  Total change              0.500000
#>  - Intraindividual change  0.500000
#>  - Population turnover     0.000000
#>    - Mortality            -0.000005
#>    - Coming-of-age         0.000005
#> Assumes no in- or out-migration.
```

## Only population turnover (Scenario 4)

``` r

pt_only <- function(data, time) {
    data[, ifelse(gender == "f", 0.2, 0.4) + 0.7516 + (time - age + 20) / 10]
}

simresult <- socialchange::sim_social_change(
    periods = 5,
    data = data,
    fun_y = pt_only,
    fun_coming_of_age = coming_of_age,
    fun_mortality = mortality
)
print(simresult, detailed = FALSE)
#>                 Component  Value
#>  At initial               0.3000
#>  At end                   0.8000
#>  Total change             0.5000
#>  - Intraindividual change 0.0000
#>  - Population turnover    0.5000
#>    - Mortality            0.2413
#>    - Out-migration        0.0000
#>    - Coming-of-age        0.2587
#>    - In-migration         0.0000
```

Again, we first try a misspecified model that ignores gender entirely.
Again, the decomposition recovers the result almost exactly despite the
misspecification:

``` r

stacked_data <- rbindlist(simresult$snapshot)
model_no_gender <- lm(y ~ age + period, data = stacked_data)
cor(stacked_data$y, predict(model_no_gender))
#> [1] 0.9864554
predict_y <- function(newdata) { predict(model_no_gender, newdata = newdata) }

decomp <- decompose_aggregated(stacked_data, predict_y)
print(decomp, detailed = FALSE)
#>                 Component  Value
#>  At initial (modeled)     0.3000
#>  At end (modeled)         0.8000
#>  Total change             0.5000
#>  - Intraindividual change 0.0000
#>  - Population turnover    0.5000
#>    - Mortality            0.2415
#>    - Coming-of-age        0.2585
#> Assumes no in- or out-migration.
```

And here are the results for the fully-specified model:

``` r

model_gender <- lm(y ~ age + period + gender, data = stacked_data)
cor(stacked_data$y, predict(model_gender))
#> [1] 1
predict_y <- function(newdata) { predict(model_gender, newdata = newdata) }

decomp <- decompose_aggregated(stacked_data, predict_y, "gender")
print(decomp, detailed = FALSE)
#>                 Component  Value
#>  At initial (modeled)     0.3000
#>  At end (modeled)         0.8000
#>  Total change             0.5000
#>  - Intraindividual change 0.0000
#>  - Population turnover    0.5000
#>    - Mortality            0.2415
#>    - Coming-of-age        0.2585
#> Assumes no in- or out-migration.
```

## Smoking (Scenario 5)

### Static population (Scenario 5a)

``` r

data <- data.table(
    age = c(20:39, 20:39),
    smoking = c(rep("smoker", 20), rep("nonsmoker", 20)),
    n = c(
        c(rep(90, 10), seq.int(90, 0, by = -12), 0, 0),
        c(rep(10, 10), seq.int(10, 1, by = -1))
    )
)
coming_of_age <- function(data, period) {
    data.table(age = c(20, 20), smoking = c("smoker", "nonsmoker"), n = c(90, 10))
}
mortality <- function(data, period) {
    data[, fcase(
        # we need to make sure that we still have enough individuals, therefore the pmin
        age >= 30 & smoking == "smoker", pmin(12, n),
        age >= 30 & smoking == "nonsmoker", 1,
        default = 0
    )]
}
smoking_in_public <- function(data, time) {
    data[, ifelse(smoking == "smoker", 0.9, 0.6)]
}
smoking1 <- socialchange::sim_social_change(
    periods = 5,
    data = data,
    fun_y = smoking_in_public,
    fun_coming_of_age = coming_of_age,
    fun_mortality = mortality
)
print(smoking1, detailed = FALSE)
#>                 Component     Value
#>  At initial                0.867686
#>  At end                    0.867686
#>  Total change              0.000000
#>  - Intraindividual change  0.000000
#>  - Population turnover     0.000000
#>    - Mortality            -0.000793
#>    - Out-migration         0.000000
#>    - Coming-of-age         0.000793
#>    - In-migration          0.000000
```

Compare to decomposition:

``` r

stacked_data <- rbindlist(smoking1$snapshot)
model <- lm(y ~ age + period + smoking, data = stacked_data)
cor(stacked_data$y, predict(model))
#> [1] 1
predict_y <- function(newdata) { predict(model, newdata = newdata) }

decomp <- decompose_aggregated(stacked_data, predict_y, "smoking")
print(decomp, detailed = FALSE)
#>                 Component     Value
#>  At initial (modeled)      0.867686
#>  At end (modeled)          0.867686
#>  Total change              0.000000
#>  - Intraindividual change  0.000000
#>  - Population turnover     0.000000
#>    - Mortality            -0.000729
#>    - Coming-of-age         0.000729
#> Assumes no in- or out-migration.
```

### Intraindividual change (Scenario 5b)

``` r

smoking_in_public <- function(data, time) {
    data[, ifelse(smoking == "smoker", 0.9 - time*0.05, 0.6 - time*0.1)]
}

smoking2 <- socialchange::sim_social_change(
    periods = 5,
    data = data,
    fun_y = smoking_in_public,
    fun_coming_of_age = coming_of_age,
    fun_mortality = mortality
)
print(smoking2, detailed = FALSE)
#>                 Component     Value
#>  At initial                0.867686
#>  At end                    0.590757
#>  Total change             -0.276929
#>  - Intraindividual change -0.276690
#>  - Population turnover    -0.000239
#>    - Mortality            -0.001132
#>    - Out-migration         0.000000
#>    - Coming-of-age         0.000893
#>    - In-migration          0.000000
```

Compare to decomposition:

``` r

stacked_data <- rbindlist(smoking2$snapshot)
model <- lm(y ~ age + period * smoking, data = stacked_data)
cor(stacked_data$y, predict(model))
#> [1] 1
predict_y <- function(newdata) { predict(model, newdata = newdata) }

decomp <- decompose_aggregated(stacked_data, predict_y, "smoking")
print(decomp, detailed = FALSE)
#>                 Component     Value
#>  At initial (modeled)      0.867686
#>  At end (modeled)          0.590757
#>  Total change             -0.276929
#>  - Intraindividual change -0.276890
#>  - Population turnover    -0.000039
#>    - Mortality            -0.001154
#>    - Coming-of-age         0.001115
#> Assumes no in- or out-migration.
```

### Coming of age (Scenario 5c)

``` r

coming_of_age <- function(data, period) {
    data.table(age = c(20, 20), smoking = c("smoker", "nonsmoker"), n = c(90 - period * 15, 10 + period * 15))
}

smoking3 <- socialchange::sim_social_change(
    periods = 5,
    data = data,
    fun_y = smoking_in_public,
    fun_coming_of_age = coming_of_age,
    fun_mortality = mortality
)
print(smoking3, detailed = FALSE)
#>                 Component    Value
#>  At initial                0.86769
#>  At end                    0.50476
#>  Total change             -0.36293
#>  - Intraindividual change -0.29136
#>  - Population turnover    -0.07157
#>    - Mortality            -0.01074
#>    - Out-migration         0.00000
#>    - Coming-of-age        -0.06083
#>    - In-migration          0.00000
```

Compare to decomposition:

``` r

stacked_data <- rbindlist(smoking3$snapshot)
model <- lm(y ~ age + period * smoking, data = stacked_data)
cor(stacked_data$y, predict(model))
#> [1] 1
predict_y <- function(newdata) { predict(model, newdata = newdata) }

decomp <- decompose_aggregated(stacked_data, predict_y, "smoking")
print(decomp, detailed = FALSE)
#>                 Component    Value
#>  At initial (modeled)      0.86769
#>  At end (modeled)          0.50476
#>  Total change             -0.36293
#>  - Intraindividual change -0.29129
#>  - Population turnover    -0.07164
#>    - Mortality            -0.01063
#>    - Coming-of-age        -0.06101
#> Assumes no in- or out-migration.
```
