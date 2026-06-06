# UN World Population Prospects population data

Total population estimates and projections by location and year from the
UN World Population Prospects (WPP), covering 1950–2100. Includes world
regions and individual countries. Prepared via the
[tidywpp](https://github.com/PPgp/tidywpp) package.

## Usage

``` r
wpp_data
```

## Format

A data frame with 43,186 rows and 3 variables:

- Location:

  Name of the country or region (character).

- Time:

  Year (numeric), ranging from 1950 to 2100.

- PopTotal:

  Total population in thousands (numeric).

## Source

United Nations, Department of Economic and Social Affairs, Population
Division. World Population Prospects 2022.
<https://population.un.org/wpp/>
