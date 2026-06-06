# GSS racial attitudes of white Americans

A subset of the General Social Survey (GSS) containing white respondents
with valid responses to racial attitude questions, covering survey years
1972, 1976, 1980, and 1984. The outcome variable `rac` is a composite
scale built from four items (RACDIN, RACSEG, RACPUSH, RACMAR), each
negated and z-scored so that higher values indicate more tolerant
attitudes. Up to two missing items per respondent are imputed by
rescaling the available items. Prepared from the
[gssr](https://cran.r-project.org/package=gssr) package.

## Usage

``` r
gss_rac
```

## Format

A data.table with 7 variables:

- rac:

  Composite racial tolerance scale (higher = more tolerant). Built from
  four z-scored items (each negated), then shifted by +6 to give
  positive values.

- year:

  GSS survey year (1972, 1976, 1980, or 1984).

- age:

  Age of respondent at time of interview.

- cohort:

  Birth year, computed as `year - age`.

- region:

  Region of interview (1–9, Census divisions).

- sex:

  Sex of respondent (1 = Male, 2 = Female).

- wtssall:

  Survey weight.

## Source

Smith, Tom W., Davern, Michael, Freese, Jeremy, and Morgan, Stephen L.
General Social Surveys, 1972–2018. NORC, Chicago. Accessed via the
[gssr](https://cran.r-project.org/package=gssr) R package.

## Details

The four items comprising the scale are:

- RACDIN:

  Whether Black Americans are welcome for dinner.

- RACSEG:

  Whether whites have the right to keep Black Americans out of their
  neighborhoods.

- RACPUSH:

  Whether Black Americans should not push themselves where they are not
  wanted.

- RACMAR:

  Whether there should be laws against Black-white marriage.
