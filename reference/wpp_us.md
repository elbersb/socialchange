# US population by age and sex, 1973–2016

United States population by single year of age and sex, from the UN
World Population Prospects (WPP) 2022, for every year from 1973 to 2016
(the span of the
[`gss_homosex`](https://elbersb.github.io/socialchange/reference/gss_homosex.md)
survey waves). Ages run from 21 to 89, with 89 representing "89 or
older" to match the GSS age top-code. Counts are in thousands and
reflect the true US age/sex structure; they are intended for use as the
`population` frame in
[`decompose_aggregated`](https://elbersb.github.io/socialchange/reference/decompose_aggregated.md),
where only the relative cell structure matters (rescale per period as
needed). Prepared via the [tidywpp](https://github.com/PPgp/tidywpp)
package.

## Usage

``` r
wpp_us
```

## Format

A data.table with 6,072 rows and 4 variables:

- period:

  Year (numeric), 1973–2016.

- age:

  Single year of age (21–89; 89 = "89 or older").

- sex:

  Sex (`"male"` or `"female"`), matching `gss_homosex$sex`.

- n:

  Population in thousands (numeric).

## Source

United Nations, Department of Economic and Social Affairs, Population
Division. World Population Prospects 2022.
<https://population.un.org/wpp/>
