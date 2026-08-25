# US mortality rates, 1933–2024

Mortality rates by sex and age. Used in examples for
[`decompose_aggregated`](https://elbersb.github.io/socialchange/reference/decompose_aggregated.md);
its `mortality` argument expects annual death probabilities, so convert
with `prob = 1 - exp(-death_rate)` (and rename `year` to `period`).

## Usage

``` r
mortality_us
```

## Format

A data frame with 16,560 rows and 4 variables:

- year:

  Year.

- age:

  Age; 89 means 89 and older (top-coded to match the GSS).

- sex:

  Sex, either 'male' or 'female'.

- death_rate:

  Central death rate m(x): deaths divided by exposure (HMD
  \`Deaths_1x1\` / \`Exposures_1x1\`; equals \`Mx_1x1\` for single ages,
  and the exposure-weighted aggregate rate for the open 89+ group). Not
  a probability – it can exceed 1 at the oldest ages.

## Source

HMD. Human Mortality Database. Max Planck Institute for Demographic
Research (Germany), University of California, Berkeley (USA), and French
Institute for Demographic Studies (France). Available at
www.mortality.org.
