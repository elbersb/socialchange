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
`vignettes/gss_homosexuality.qmd` - Applied example using GSS attitudes
toward homosexuality \*\*KEY VIGNETTE\* -
`vignettes/decompose_aggregated.qmd` - Decomposition of aggregated
data - `vignettes/simulate.qmd` - Simulation examples -
`vignettes/apc.qmd` - APC model examples -
`vignettes/replicating_firebaugh.qmd` - Replicating and improving on
Firebaugh’s CR-IC decomposition

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

To generate scenarios, there is `R/simulate.R`: -
[`sim_social_change()`](https://elbersb.github.io/socialchange/reference/sim_social_change.md):
Forward simulation of social change dynamics - User provides functions
for: outcome (`fun_y`), mortality, coming-of-age, migration, and state
transitions - Returns detailed event-by-event records showing exact
contribution of each component

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
in a cell’s size between waves is attributed to a demographic event: a
survivor cohort that *shrinks* is credited to mortality, one that
*grows* to net in-migration (`derive_events()`). This is always-on (not
gated on a population frame, no toggle); print/plot show a migration
component only for whichever of in/out is non-zero.

Consequences and limits: - **Out-migration is never separately
identified.** Gross out-migration is not separable from deaths (a
survivor loss could be either), so it is folded into mortality and the
reported `outmigration` is always `0`. - **On survey data the
in-migration term is mostly noise.** Run on raw survey cells, a
“growing” cohort usually reflects sampling fluctuation across waves
rather than genuine immigration (e.g. ~1.9% / +0.007 on the GSS
homosexuality example) — honest accounting of where the cell-count
change went, not a demographic estimate. It becomes meaningful only when
`population` supplies a true frame. - Migration among coming-of-age
cohorts is not modeled (new cohorts get all their growth as
coming-of-age).

The remaining migration work (feature (b) below) is supplying
mortality/migration **inputs** (e.g. `fun_mortality`) to close the
accounting identity properly rather than reading net change off
cell-count differences.

### Top-coding: Aging out of the top of the age window is labeled mortality

**A survivor cohort that ages past the observed age range is recorded as
deaths, not as window-exit.** This is a *design choice, not a code bug*:
it falls out of the same `n1 - n2` accounting as everything else.

- **For a survey with a top-coded or capped age range it misattributes
  window-exit to mortality.** Examples: GSS top-codes age at 89, so the
  89→90+ cohort vanishes and is booked as deaths; an analysis restricted
  to an 18–65 sample sends everyone aging past 65 to mortality
  regardless of whether they died.

**In practice**: restrict to an age range whose top is open-ended
relative to the data (or use a true `population` frame - but that won’t
really help because population and survey need to match), or read the
mortality component as “deaths + exits past the top of the window” when
the range is capped.

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

------------------------------------------------------------------------

# Performance profile of `decompose_aggregated()`

*(Profiled on the GSS homosexuality example: 34,026 rows, 26 periods,
ages 21–89. Headline: with a realistic GAM `fun_y`, ~75% of wall time is
the user’s model prediction, not the simulation logic. Captured ahead of
a planned C++ rewrite of the hot loop.)*

**Measured cost (per
[`decompose_aggregated()`](https://elbersb.github.io/socialchange/reference/decompose_aggregated.md)
call):** - GAM `fun_y` (`s(age) + s(period)`): **~1.43 s** - lm `fun_y`
(`age + period`): **~0.35 s**

So ~1.08 s of the GAM call is *prediction alone* (`predict.gam` →
`PredictMat`: `matrix`/`t.default`/`.Fortran`/`.C`). The remaining ~0.35
s is the simulation machinery.

**Why prediction dominates: one giant `fun_y` call per period.**
`simulate_schedule()` evaluates `fun_y(data_stack)` once over an
`n_cells × (n_ev + 1)` stacked frame (every event time × every cell).
Runtime scales as `n_cells × n_ev × cost(fun_y)` — and feature (a)’s
bootstrap will multiply this by `R` draws. The remaining non-prediction
time splits between `simulate_schedule` (the O(`n_ev × n_cells`) inner
loop re-summing `y_cur * n_vec` every event) and data.table
ordering/grouping in `schedule_events()`.

**Optimization leverage (for the eventual C++ rewrite or before
it):** 1. **Cheaper prediction** — reuse a
`predict.gam(type = "lpmatrix")` basis instead of letting `PredictMat`
rebuild the spline basis on all ~800k rows. 2. **Fewer prediction rows**
— `y` depends only on `(age, period, cells)` and aging is linear in
tick, so the `n_ev + 1` slices collapse to far fewer distinct rows;
predict on unique rows and reindex. 3. **C++ hot loop** — the
`simulate_schedule()` running-sum replay and `schedule_events()`
ordering are the natural targets once prediction is cheap. The
schedule/simulate split already isolates the deterministic replay (no
RNG) as the unit to port.

**Parallelism: the period loop is embarrassingly parallel, but it’s the
wrong axis.** Each main-loop iteration reads only shared read-only state
and writes disjoint output slots, so it could be an
`mclapply`/`future_lapply` over `i_period`. But: (i) it breaks RNG
reproducibility (`schedule_events()`’s sequential
[`runif()`](https://rdrr.io/r/stats/Uniform.html) stream needs
parallel-safe RNG, regenerating the golden master); and (ii) it’s the
lowest-leverage axis (~25 transitions; `fun_y` dominates). The bigger
single-threaded win is the per-period prediction cost (1–2 above). For
feature (a)’s bootstrap, parallelize over the `R` draws instead —
coarser units, each one full sequential run with its own seed.

------------------------------------------------------------------------

# Planned Features for `decompose_aggregated()`

What remains: (a) standard errors and the rest of (b) migration
estimation.

## Design vision: keep the three inputs decoupled

The architecture keeps three logically-separate inputs decoupled, with a
pluggable event generator between the population frame and the
simulation core. The remaining features each attach to one seam of this
pipeline:

    [survey data] --fun_y-->  Y(cell, time)                         <- feature (a) perturbs this (model draws)
    [population frame]        n per cell per period                 <- `population` arg supplies (shipped); (a) perturbs (demographic draws)
            |
            v
    [event generator]  derive per-cell {mortality, coming_of_age, inmigration, outmigration}   <- feature (b): pluggable strategies
            |
            v
    [simulation core]  random event ordering + interleaved IC change <- feature (a): repeat for ordering uncertainty
            |
            v
    [summarize]  point estimate + SE/CI, per-period & cumulative, optionally split by covariate cell & event type

The old scripts (`old_scripts/functions/functions_clean.R`) are the
reference implementations for the unbuilt pieces, but might contains
bugs as they are substantively untested: `eventDecompositionMat` (all
four event types — the direct ancestor of
[`decompose_aggregated()`](https://elbersb.github.io/socialchange/reference/decompose_aggregated.md)),
`fitBSModels`/`splineForBS` (Dirichlet-bootstrap GAM),
`createPopFrame`/`calcSurvivingPop` (external WPP/HMD frame +
mortality-residual migration), and
`prepComponentData`/`decomposeChangeByVar` (bootstrap CIs +
per-covariate/per-event breakdown).

## Feature (a): Standard errors

Three distinct uncertainty sources, in decreasing order of typical
importance:

1.  **Model uncertainty** — `fun_y` is a fitted model with sampling
    variability. Old scripts handled this with a Dirichlet bootstrap of
    the GAM (`fitBSModels`, 100 draws). This is the dominant source in
    practice.
2.  **Simulation uncertainty** — Monte Carlo noise from the random event
    ordering. Small when event counts are large (hundreds+). Should be
    *averaged out* of the point estimate, not reported as estimand
    uncertainty.
3.  **Demographic uncertainty** — uncertainty in the event counts / cell
    `n` themselves (survey sampling error in `n1`,`n2`; uncertainty in
    supplied mortality/migration). New beyond the old scripts, which
    treated the WPP/HMD frame as fixed.

------------------------------------------------------------------------

## Feature (b): Migration estimation

Per cell (birth cohort × covariates) the accounting identity is:

    n2 = n1 + coming_of_age + inmigration - deaths - outmigration

For survivor cohorts (coming_of_age = 0) this is **1 equation, 3
unknowns** (deaths, inmigration, outmigration). Gross in- and
out-migration are *not separately identified* from cell counts alone —
only **net** migration per cell is. (The old scripts confirm this:
`calcSurvivingPop` produces only net per cell, split into `Immig` if
positive / `Emig` if negative.) Flexibility = which inputs the user
supplies to close the system:

- **Default (current behavior)**: no mortality/migration supplied → net
  change attributed to coming-of-age, mortality (shrinking survivors),
  or net in-migration (growing survivors). Out-migration = 0, deaths and
  migration never co-occur in the same cell. *(The in-migration half of
  this is already implemented — see “Always-on net in-migration”
  above.)*
- **Supply mortality (recommended default for most cases)**:
  `deaths = fun_mortality(cell)` (count, or a rate × `n1`). Then
  `net_migration = n2 - (n1 - deaths)`; positive ⇒ inmigration, negative
  ⇒ outmigration. This is exactly the old-scripts strategy (HMD
  mortality rate → `PopSurvive = Pop1 - deaths` →
  `NetImmig = Pop2 - PopSurvive`).
- **Supply migration (net)** → back out deaths from the identity.
- **Supply both** → system is over-determined; reconcile (let implied
  `n2` be a check, or warn on inconsistency beyond a tolerance).

**Proposed API**: `fun_mortality = NULL` and `fun_migration = NULL`
arguments (function-based inputs, evaluated per cell), consumed by
`derive_events()` from the refactor. Default `NULL` = current
[`pmax()`](https://rdrr.io/r/base/Extremes.html) strategy (net change →
mortality if shrinking, net in-migration if growing). The print/plot
methods already render the migration component (each migration row is
shown when its own contribution is non-zero); supplying these inputs
would additionally make out-migration and same-cell death+migration
non-zero, so the out-migration row would start appearing.

**Open modeling choices to document**: (i) migrants assumed to take the
receiving cell’s mean Y (no migrant-specific outcome model); (ii)
migration among coming-of-age cohorts is ignored (old scripts set their
deaths=0, mig=0); (iii) net-only, gross requires external flow data.

------------------------------------------------------------------------

## How the two features overlap

- **(a) ↔︎ (b)** — (b) defines the event table; (a)’s
  demographic-uncertainty branch perturbs exactly that table. The
  mortality/migration estimates (b) introduces are a *new* uncertainty
  source (a) must propagate (e.g., stochastic `fun_mortality`).
- **Both** converge on the **event-table abstraction**
  (`derive_events()`) and the **separation of Y model / population frame
  / event counts**, both already in place.

**Suggested sequencing**: feature (b)’s remaining half
(`fun_mortality`/`fun_migration` inputs to close the identity →
out-migration, same-cell death+migration) → feature (a) (bootstrap
wrapper over the now-pluggable inputs).

------------------------------------------------------------------------

## Possible future helper: cross-wave harmonization / raking

For the “I have true population counts per cell per wave” case, the
shipped `population` argument is the right tool: supply the counts
directly, let the survey supply only `fun_y`, and survey age-structure
noise across waves drops out entirely. This is the recommended path and
needs no new machinery.

A raking helper would only add value in the weaker data situation: you
have *marginal* population targets (e.g. WPP age distribution, race
composition, proportion newly age-eligible — as in the old scripts
`jm_gss.R`/`gss_bootstrap.R`) but not their joint distribution across
cells, so you can’t supply a `population` frame. Raking/IPF reconstructs
an adjusted joint `n` from those margins. It is standalone preprocessing
that produces an adjusted `n`/weight column before
[`decompose_aggregated()`](https://elbersb.github.io/socialchange/reference/decompose_aggregated.md)
— sketch: `harmonize_cells(data, targets, by, within = "period")`.
Standard IPF already exists (`survey::rake`, `anesrake`, `autumn`); the
only real value-add of bundling is enforcing the two easy-to-get-wrong
rules: rake *within* wave, and — under the feature (a) bootstrap —
re-rake *inside* each draw, or the SE is wrong.

Not near-term work. Revisit if a margins-only application comes up.
