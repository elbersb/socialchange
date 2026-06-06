# GSS attitudes toward homosexual sex relations

A subset of the General Social Survey (GSS) containing respondents with
valid responses to the `homosex` question, covering survey years
1973–2018 and birth cohorts 1892–1995. Oversample designs (samples 4, 5,
and 7) are excluded. The outcome variable has been rescaled from its
original 1–4 coding to a 0–1 scale. Prepared from the
[gssr](https://cran.r-project.org/package=gssr) package.

## Usage

``` r
gss_homosex
```

## Format

A data.table with 36,494 rows and 19 variables:

- id:

  Respondent ID number.

- year:

  GSS survey year.

- wtssall:

  Survey weight.

- sample:

  Sampling frame and method code.

- vstrat:

  Variance stratum (NA for many years).

- vpsu:

  Variance primary sampling unit (NA for many years).

- homosex:

  Attitude toward sexual relations between two adults of the same sex,
  rescaled to 0–1 (0 = always wrong, 1 = not wrong at all). Original
  4-point scale: 1 = Always Wrong, 2 = Almost Always Wrong, 3 =
  Sometimes Wrong, 4 = Not Wrong at All.

- age:

  Age of respondent at time of interview.

- cohort:

  Birth year, computed as `year - age`.

- sex:

  Sex of respondent (1 = Male, 2 = Female).

- educ:

  Highest year of school completed.

- marital:

  Marital status (1 = Married, 2 = Widowed, 3 = Divorced, 4 = Separated,
  5 = Never Married).

- race:

  Race of respondent (1 = White, 2 = Black, 3 = Other).

- region:

  Region of interview (1–9, Census divisions).

- born:

  Whether respondent was born in the United States (1 = Yes, 2 = No; NA
  for many years).

- physhlth:

  Days of poor physical health in past 30 days (NA for many years).

- compuse:

  Whether respondent uses a computer (1 = Yes, 2 = No; NA for many
  years).

- relig16:

  Religion in which respondent was raised (1 = Protestant, 2 = Catholic,
  3 = Jewish, 4 = None, etc.).

- pray:

  Frequency of prayer (NA for many years).

## Source

Smith, Tom W., Davern, Michael, Freese, Jeremy, and Morgan, Stephen L.
General Social Surveys, 1972–2018. NORC, Chicago. Accessed via the
[gssr](https://cran.r-project.org/package=gssr) R package.
