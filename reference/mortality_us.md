# US mortality rates, 1933–2019

Mortality rates by sex and age. Used in examples for
[`decompose_aggregated`](https://elbersb.github.io/socialchange/reference/decompose_aggregated.md).

## Usage

``` r
mortality_us
```

## Format

A data frame with 15,660 rows and 4 variables:

- year:

  Year.

- age:

  Age.

- sex:

  Sex, either 'male' or 'female'.

- death_rate:

  Central death rate m(x): deaths divided by exposure (HMD \`Mx_1x1\`).
  Not a probability – it can exceed 1 at the oldest ages.

## Source

HMD. Human Mortality Database. Max Planck Institute for Demographic
Research (Germany), University of California, Berkeley (USA), and French
Institute for Demographic Studies (France). Available at
www.mortality.org.
