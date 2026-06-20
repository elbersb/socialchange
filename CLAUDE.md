# CLAUDE.md

Guidance for Claude Code working in this repository.

## Package Overview

`socialchange` is an R package for decomposing social change into intraindividual change vs. population turnover. It separates aggregate-level change over time into:
- Intraindividual/within-unit change (people changing their attitudes)
- Population turnover (cohort replacement, mortality, migration, coming-of-age)
- Event-driven change (countries joining/leaving the EU)

**Intellectual positioning:** (1) A critique of APC methods — they suffer the identification problem (age = period − cohort) and need untestable assumptions; this package offers more directly interpretable decomposition alternatives. (2) An improvement on Firebaugh's algebraic/linear CR-IC decomposition (which mishandles entering/exiting cohorts, non-linearities, and leaves residuals) via model-based counterfactuals (`cr_ic()`) and simulation-based individual-level decomposition (`sim_social_change()`, `decompose_aggregated()`) that tracks demographic events directly.

## Key Commands

```r
devtools::load_all()      # load for development
devtools::document()      # rebuild roxygen2 docs
devtools::check()         # check package
devtools::install()       # install locally
pkgdown::build_site()     # build docs website
```

## Architecture

Three main decomposition approaches, all using `data.table`, formula interfaces (`Outcome ~ Unit + Time`), and S3 classes with custom `print()`/`plot()`:

1. **Event-based** (`R/decompose_events.R`) — `decompose_events()`: units enter/exit at discrete times. Two datasets (events + outcomes). Separates "change" (within-unit) from "replacement" (turnover) with counterfactuals. → class `decompose_events`
2. **Aggregated** (`R/decompose_aggregated.R`) — `decompose_aggregated()`: data aggregated by age + covariates. Microsimulation randomly orders demographic events. Needs stacked panel data (age, period, cell counts) and a fitted `model` (`lm`/`glm`/`gam`) predicting outcome from age/period/cells. Decomposes into intraindividual change, coming-of-age, mortality, net in-migration. Optional `population` arg supplies a true cell × period count frame (overrides survey counts). `R > 0` adds model-uncertainty SEs via Dirichlet bootstrap. → class `social_change_decomp`. `print` and `plot` methods live in `R/decompose_aggregated_output.R`.
3. **Simulation** (`R/simulate.R`) — `sim_social_change()`: forward simulation. User supplies functions for outcome (`fun_y`), mortality, coming-of-age, migration, state transitions. Returns event-by-event records. → class `social_change_sim`

Additional methods:
- **CR-IC** (`R/cr_ic.R`) — classical cohort-replacement vs. intraindividual change (algebraic, linear, model-based). → class `cr_ic_decomposition`
- **APC** (`R/apc.R`) — APC models via orthogonal polynomial contrasts; includes `plot_gam_surface()`. → class `apc_model`

Functions rename input columns to standardized names internally; period columns are coerced to numeric.

## Dependencies

`data.table` (all data manipulation), `ggplot2` (plotting), `checkmate` (validation), `weightedcontrasts` (APC, from GitHub elbersb/weightedcontrasts). roxygen2 8.0.0; S3 methods registered with `S3method()` in NAMESPACE.

## Datasets (`data/`, docs in `R/data.R`, prep in `data-raw/`)

- `gss_rac.rda` — GSS racial attitudes, white Americans (1972–1984)
- `gss_homosex.rda` — GSS attitudes toward homosexual sex (1973–2018)
- `eu_membership.rda` — EU entry/exit dates
- `wpp_data.rda` — UN World Population Prospects (1950+)
- `mortality_us.rda` — US mortality rates (1933-2019)

## Vignettes (`vignettes/`)

`gss_homosexuality.qmd` (**KEY**), `decompose_aggregated.qmd`, `simulate.qmd`, `apc.qmd`, `replicating_firebaugh.qmd`.

## Known Limitations of `decompose_aggregated()`

Events are derived from cell-count differences ("never a residual"): for survivors `mortality = pmax(0, n1 - n2)`, `inmigration = pmax(0, n2 - n1)`; for new cohorts `coming_of_age = pmax(0, n2 - n1)`. Consequences:

- **Migration: only net in-migration, as a residual.** Out-migration is never separately identified (a survivor loss could be death or exit, so it's folded into mortality; reported `outmigration` is always 0). On raw survey cells the in-migration term is mostly sampling noise (~1.9% on the GSS homosexuality example); meaningful only with a true `population` frame. Migration among coming-of-age cohorts is not modeled.
- **Top-coding → mortality.** A survivor cohort aging past the observed age range is booked as deaths (GSS top-codes age at 89; an 18–65 sample sends everyone aging past 65 to mortality). Design choice, not a bug — same `n1 - n2` accounting. *In practice:* use an open-ended top age, or read mortality as "deaths + exits past the window top".
- **Transitions misattributed.** Within-cell transitions (e.g. smokers → non-smokers) are not separately identified and get absorbed into intraindividual change; the `pmax(0, …)` guard masks offsetting flows (a cell with net transition inflow shows 0 mortality even if deaths occurred). Use only on data without significant transitions. `sim_social_change()` tracks transitions properly but can't recover them from aggregated data.
- **Survey weights are an approximation.** With individual-level `weight`, weights are normalized within period to sum to `.N`, then `n = round(sum(normalized_weight))`. Adjusts *relative* structure correctly and keeps event counts tractable, but doesn't recover absolute population sizes; rounding adds small errors. Prefer supplying true `population` counts when available.

## Performance profile of `decompose_aggregated()`

Profiled on GSS homosexuality (34,026 rows, 26 periods, ages 21–89). **With a GAM `fun_y` (`s(age)+s(period)`) ~75% of wall time is the user's model prediction, not simulation logic:** GAM call ~1.43 s (of which ~1.08 s is `predict.gam`/`PredictMat`), lm call ~0.35 s; simulation machinery ~0.35 s. `simulate_schedule()` evaluates `fun_y` once over an `n_cells × (n_ev+1)` stacked frame, so cost scales `n_cells × n_ev × cost(fun_y)` — and the bootstrap multiplies by `R`.

Remaining leverage: the `O(n_ev × n_cells × R)` replay loop in `replay_schedule()`. The R-level wins (lpmatrix basis reuse across replicates, draw-vectorized replay) are spent; the deterministic (no-RNG) replay is the natural unit for a C++ port. The period loop is embarrassingly parallel but the wrong axis (breaks RNG reproducibility, lowest leverage); for the bootstrap, parallelize over `R` draws instead.

## Planned features for `decompose_aggregated()`

Remaining: the demographic-uncertainty branch of (a), and the rest of (b). Architecture keeps three inputs decoupled with a pluggable event generator between them:

```
[survey data] --fun_y--> Y(cell,time)          <- (a) perturbs (model draws)
[population frame] n per cell per period        <- shipped; (a) perturbs (demographic draws)
        v
[event generator] derive {mortality, coming_of_age, inmigration, outmigration}  <- (b): pluggable
        v
[simulation core] random ordering + interleaved IC change  <- (a): repeat for ordering uncertainty
        v
[summarize] point estimate + SE/CI, per-period & cumulative, optional split by cell & event
```

Reference (untested, may have bugs): `old_scripts/functions/functions_clean.R` — `eventDecompositionMat`, `fitBSModels`/`splineForBS`, `createPopFrame`/`calcSurvivingPop`, `prepComponentData`/`decomposeChangeByVar`.

**Feature (a): Standard errors.** Three uncertainty sources:
1. **Model uncertainty — implemented.** `R > 0` refits `model` on `R` Dirichlet-reweighted copies of its training data (`y_replicates()` generic in `R/y_model.R`), replaying the *same* event schedule (common random numbers). Returns a long per-(draw, period, cell) `draws` table; print/plot derive the cumulative 95% CI (`cumulative_series()`). Reweighting microdata (not `vcov()` posterior sim) is deliberate — captures misspecification and survey-weight variance.
2. **Simulation uncertainty** — Monte Carlo noise from random ordering; averaged out via common random numbers, not reported.
3. **Demographic uncertainty — unimplemented.** Sampling error in `n1`/`n2` and supplied mortality/migration. With `population`, the bootstrap reweights only the survey, so external counts carry no uncertainty.

Deferred limitations of the bootstrap: survey design ignored (reweights rows not PSUs); refit reads original data by name (`getCall()$data`), breaks if out of scope; refitting transiently holds all `R` refits at once (the predictor itself keeps only their coefficients); replicates reuse one design matrix, so a model whose prediction isn't a fixed `linkinv(Xβ)` (a weight-dependent basis) errors rather than falling back to per-replicate prediction.

**Feature (b): Migration estimation.** Per cell the identity is `n2 = n1 + coming_of_age + inmigration − deaths − outmigration`. For survivors that's 1 equation, 3 unknowns — only *net* migration is identified. User closes the system by which inputs they supply:
- Default (current): net change → coming-of-age / mortality (shrinking) / net in-migration (growing); out-migration = 0.
- Supply mortality (recommended): `deaths = fun_mortality(cell)`, then `net_migration = n2 − (n1 − deaths)`; positive ⇒ in, negative ⇒ out.
- Supply net migration → back out deaths.
- Supply both → over-determined; reconcile / warn beyond tolerance.

Proposed API: `fun_mortality = NULL`, `fun_migration = NULL`, consumed by `derive_events()`. Default `NULL` = current `pmax()` strategy. Print/plot already render migration rows when non-zero; supplying inputs makes out-migration appear. Modeling choices to document: (i) migrants take receiving cell's mean Y; (ii) migration among coming-of-age cohorts ignored; (iii) net-only.

**Overlap & sequencing:** (b) defines the event table; (a)'s demographic branch perturbs it. Both converge on `derive_events()`. Do (b)'s remaining half first, then (a)'s bootstrap over the pluggable inputs.

**Possible future helper — raking.** Only adds value when you have *marginal* population targets but not their joint distribution (can't supply a `population` frame). IPF reconstructs an adjusted joint `n` as preprocessing (`survey::rake`/`anesrake`/`autumn`); sketch `harmonize_cells(data, targets, by, within="period")`. The value-add is enforcing: rake *within* wave, and re-rake *inside* each bootstrap draw or the SE is wrong. Not near-term.
