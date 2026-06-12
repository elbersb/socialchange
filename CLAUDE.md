# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Package Overview

`socialchange` is an R package for decomposing social change into
components of intraindividual change and population turnover. The
package implements multiple decomposition methods for analyzing how
aggregate-level outcomes change over time due to: -
Intraindividual/within-unit changes (e.g., people changing their
attitudes) - Population turnover (cohort replacement, mortality,
migration, coming-of-age) - Event-driven changes (e.g., countries
joining/leaving the EU)

## Intellectual Positioning

The package has two main intellectual motivations:

1.  **Skepticism of APC models**: The package is partly a critique of
    Age-Period-Cohort (APC) methods. APC models suffer from the
    identification problem (age = period − cohort) and require
    untestable assumptions to resolve it. The package offers
    decomposition-based alternatives that are more directly
    interpretable. (This critique will be developed further in future
    vignettes/papers.)

2.  **Improving on Firebaugh’s CR-IC decomposition**: Firebaugh’s
    algebraic and linear CR-IC decompositions are the classical approach
    to separating cohort replacement from intracohort change, but they
    have known limitations (treatment of entering/exiting cohorts,
    inability to handle non-linearities, residuals in the linear
    method). This package improves on those methods via model-based
    counterfactuals (AD+, Model methods in
    [`cr_ic()`](https://elbersb.github.io/socialchange/reference/cr_ic.md))
    and, more fundamentally, via the simulation-based individual-level
    decomposition
    ([`sim_social_change()`](https://elbersb.github.io/socialchange/reference/sim_social_change.md),
    [`decompose_aggregated()`](https://elbersb.github.io/socialchange/reference/decompose_aggregated.md)),
    which tracks demographic events directly rather than inferring them
    algebraically.

## Key Commands

### Package Development

``` r

# Load package for development
devtools::load_all()

# Build documentation (uses roxygen2)
devtools::document()

# Check package
devtools::check()

# Install package locally
devtools::install()
```

### Building Documentation Website

``` r

# Build pkgdown site
pkgdown::build_site()
```

### Running Examples

See vignettes in `vignettes/` for detailed examples: -
`vignettes/decompose_aggregated.qmd` - Decomposition of aggregated
data - `vignettes/simulate.qmd` - Simulation examples -
`vignettes/apc.qmd` - APC model examples -
`vignettes/replicating_firebaugh.qmd` - Replicating and improving on
Firebaugh’s CR-IC decomposition - `vignettes/gss_homosexuality.qmd` -
Applied example using GSS attitudes toward homosexuality

## Architecture

### Core Decomposition Functions

The package implements three main decomposition approaches:

1.  **Event-based decomposition** (`R/decompose_events.R`)
    - [`decompose_events()`](https://elbersb.github.io/socialchange/reference/decompose_events.md):
      Decomposes change when units enter/exit a population at discrete
      time points
    - Works with two datasets: events data (entry/exit dates) and
      outcomes data (values at each time point)
    - Formula interface: `Outcome ~ Unit + Time`
    - Separates change into “change” (within-unit) and “replacement”
      (turnover) components
    - Computes counterfactuals showing what would happen with only
      change or only replacement
2.  **Aggregated data decomposition** (`R/decompose_aggregated.R`)
    - [`decompose_aggregated()`](https://elbersb.github.io/socialchange/reference/decompose_aggregated.md):
      For data aggregated by age and other covariates
    - Uses microsimulation approach to randomly order demographic events
    - Requires: stacked panel data with age, period, and cell counts
    - Requires: `fun_y` function that computes the outcome from data
    - Decomposes into: intraindividual change, coming-of-age, mortality,
      and net in-migration (the last derived as a residual from
      cell-count growth; see Known Limitations → Migration)
    - Optional `population` argument supplies a true cell × period count
      frame, overriding survey counts for the population frame and event
      derivation
3.  **Simulation-based** (`R/simulate.R`)
    - [`sim_social_change()`](https://elbersb.github.io/socialchange/reference/sim_social_change.md):
      Forward simulation of social change dynamics
    - User provides functions for: outcome (`fun_y`), mortality,
      coming-of-age, migration, and state transitions
    - Returns detailed event-by-event records showing exact contribution
      of each component

### Additional Methods

- **CR-IC decomposition** (`R/cr_ic.R`): Classical cohort replacement
  vs. intraindividual change decomposition using algebraic, linear, and
  model-based approaches
- **APC models** (`R/apc.R`): Age-Period-Cohort models using orthogonal
  polynomial contrasts to handle identification issues. Also includes
  [`plot_gam_surface()`](https://elbersb.github.io/socialchange/reference/plot_gam_surface.md)
  for visualizing GAM smooths as 2D surfaces.

### Data Structure Conventions

All three main approaches use `data.table` extensively: - Input data
should be data frames or data.tables - Formula interfaces follow R
conventions: `Outcome ~ Unit + Time` - Functions internally rename
columns to standardized names (unit, index, outcome, etc.) - Time/period
columns are coerced to numeric for calculations - All functions return
S3 objects with class attributes for custom print/plot methods

### Return Objects

All main functions return list objects with custom S3 classes: -
`decompose_events` → class `decompose_events` - `decompose_aggregated` →
class `social_change_decomp` - `sim_social_change` → class
`social_change_sim` - `cr_ic` → class `cr_ic_decomposition` - `apc` →
class `apc_model`

Each has custom [`print()`](https://rdrr.io/r/base/print.html) and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods defined
(all five classes have both).

## Dependencies

Key dependencies: - `data.table`: All data manipulation (imported) -
`ggplot2`: Plotting (imported) - `checkmate`: Input validation -
`weightedcontrasts`: Custom package for APC models (from GitHub:
elbersb/weightedcontrasts)

## Documentation

- All exported functions use roxygen2 documentation
- Documentation is in the same files as functions
- roxygen2 version 8.0.0 (see DESCRIPTION)
- Use `@import data.table` and `@import ggplot2` directives
- Use `@export` for exported functions
- S3 methods are exported with `@export` and registered with
  `S3method()` in NAMESPACE

## Included Datasets

Located in `data/`: - `gss_rac.rda`: General Social Survey data on
racial attitudes of white Americans (1972, 1976, 1980, 1984) -
`gss_homosex.rda`: General Social Survey data on attitudes toward
homosexual sex (1973–2018) - `eu_membership.rda`: EU country entry/exit
dates - `wpp_data.rda`: UN World Population Prospects data (1950+)

Dataset documentation is in `R/data.R` and `man/*.Rd` files. Data
preparation scripts are in `data-raw/`.

## Known Limitations

### Migration

**Only net in-migration is recovered, and only as a residual from
cell-count growth.** Per the “never a residual” principle, every change
in a cell’s size between two waves is attributed to a demographic event:
a survivor cohort that *shrinks* is credited to mortality, one that
*grows* is credited to net in-migration (`derive_events()`:
`inmigration = pmax(0, n2 - n1)` for survivors). This is **always-on** —
it is not gated on a population frame, and there is no `migration`
argument to toggle it. There is also no `migration` field on the output:
the `inmigration`/`outmigration` columns are always present in
`summary`, and print/plot show a migration component only for whichever
of the two is non-zero (so today: in-migration when present, never
out-migration).

Consequences and limits: - **Out-migration is never separately
identified.** Gross out-migration is not separable from deaths (a
survivor loss could be either), so it is folded into mortality and the
reported `outmigration` is always `0`. - **On survey data the
in-migration term is mostly noise.** Run on raw survey cells, a
“growing” cohort usually reflects sampling fluctuation in cell sizes
across waves rather than genuine immigration. The term is therefore
small (e.g. ~1.9% / +0.007 on the GSS homosexuality example) and should
be read as honest accounting of where the cell-count change went, not as
a demographic estimate. It becomes meaningful only when `population`
supplies a true population frame, where a growing survivor cohort really
does signal net immigration. - Migration among coming-of-age cohorts is
not modeled (new cohorts get all their growth as coming-of-age).

The remaining migration work (feature (b) in the planned-features notes
below) is supplying mortality/migration **inputs**
(e.g. `fun_mortality`) to close the accounting identity properly rather
than reading net change off cell-count differences.

### Aggregated Decomposition with Transitions

**[`decompose_aggregated()`](https://elbersb.github.io/socialchange/reference/decompose_aggregated.md)
does not properly handle within-cell transitions** (e.g., smokers
becoming non-smokers). The function decomposes change into: -
Intraindividual change - Mortality - Coming-of-age - Net in-migration
(residual from cell-count growth; see above)

When the input data includes transitions between cells, these transition
effects are **not separately identified** and instead get absorbed into
the intraindividual change component. Additionally:

- Mortality, coming-of-age, and net in-migration are estimated from
  simple population differences between periods: for survivors
  `mortality = pmax(0, n1 - n2)` and `inmigration = pmax(0, n2 - n1)`;
  for new cohorts `coming_of_age = pmax(0, n2 - n1)`
- When transitions occur, these differences no longer accurately reflect
  demographic events
- The `pmax(0, ...)` guard routes each cell’s net change to exactly one
  of mortality / in-migration depending on its sign, which masks any
  offsetting flows
- Cells with net inflow from transitions will show 0 mortality even if
  deaths occurred (the inflow is labeled in-migration instead)

**Workaround**: This is a fundamental limitation of the aggregated
decomposition approach. The simulation function
[`sim_social_change()`](https://elbersb.github.io/socialchange/reference/sim_social_change.md)
properly tracks transitions as a separate component, but
[`decompose_aggregated()`](https://elbersb.github.io/socialchange/reference/decompose_aggregated.md)
cannot recover this from already-aggregated data.

**In practice**: Use
[`decompose_aggregated()`](https://elbersb.github.io/socialchange/reference/decompose_aggregated.md)
only for data without significant within-cell transitions, or be aware
that transition effects will be misattributed to intraindividual change.

### Survey Weights in `decompose_aggregated()`

**The current weight normalization is an approximation that should be
revisited.**

When `weight` is provided with individual-level data, weights are
normalized within each period to sum to the period sample size (`.N`),
then cells get `n = round(sum(normalized_weight))`. This approach:

- Correctly adjusts **relative** population structure across cells
  (e.g., oversampled subgroups get their proper share)
- Keeps event counts (mortality, coming-of-age) tractable for the
  simulation loop
- Avoids the problem of population-scaled weights (e.g., inverse
  probability weights summing to millions) making the simulation
  intractable

**What it gets wrong**: The ideal would use true population counts per
cell per period to estimate demographic event magnitudes. Normalized
survey weights approximate relative structure but don’t recover absolute
population sizes. Additionally, rounding introduces small errors,
especially for cells with few respondents.

**Ideal future approach**: If true population counts (e.g., from census
or official statistics) are available alongside survey data, they should
be used directly as `n` in pre-aggregated input, bypassing the `weight`
argument entirely. The normalization approach is a practical stopgap for
the common case where only survey weights are available.
