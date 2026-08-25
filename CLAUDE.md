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

Use a timeout of at least 600 seconds for `devtools::check()` and Quarto renders. Vignette builds can exceed 300 seconds with little intermediate output. Direct Quarto renders read installed package data, so reinstall after changing an `.rda` file.

## Architecture

Three main decomposition approaches, all using `data.table`, formula interfaces (`Outcome ~ Unit + Time`), and S3 classes with custom `print()`/`plot()`:

1. **Event-based** (`R/decompose_events.R`) — `decompose_events()`: units enter/exit at discrete times. Two datasets (events + outcomes). Separates "change" (within-unit) from "replacement" (turnover) with counterfactuals. → class `decompose_events`
2. **Aggregated** (`R/decompose_aggregated.R`) — `decompose_aggregated()`: data aggregated by age + covariates. Microsimulation randomly orders demographic events. Needs stacked panel data (age, period, cell counts) and a fitted `model` (`lm`/`glm`/`gam`) predicting outcome from age/period/cells. Decomposes into intraindividual change, coming-of-age, mortality, migration. Optional `population` arg supplies a true cell × period count frame (overrides survey counts). Optional `mortality` arg (annual death probabilities by period × age, optionally × cells) switches survivor-cell attribution from sign attribution to residual migration; the result records `strategy`. `R > 0` adds bootstrap SEs (combined ordering + model uncertainty). → class `social_change_decomp`. `print` and `plot` methods live in `R/decompose_aggregated_output.R`.
3. **Simulation** (`R/simulate.R`) — `sim_social_change()`: forward simulation. User supplies functions for outcome (`fun_y`), mortality, coming-of-age, migration, state transitions. Returns event-by-event records. → class `social_change_sim`

Additional methods:
- **CR-IC** (`R/cr_ic.R`) — classical cohort-replacement vs. intraindividual change (algebraic, linear, model-based). → class `cr_ic_decomposition`
- **APC** (`R/apc.R`) — APC models via orthogonal polynomial contrasts; includes `plot_gam_surface()`. → class `apc_model`

Functions rename input columns to standardized names internally. `decompose_aggregated()` requires numeric `period`; `cr_ic()` validates that periods/cohorts parse as integers and errors otherwise.

## Dependencies

`data.table` (all data manipulation), `ggplot2` (plotting), `checkmate` (validation), `weightedcontrasts` (APC, from GitHub elbersb/weightedcontrasts). roxygen2 8.0.0; S3 methods registered with `S3method()` in NAMESPACE.

## Datasets (`data/`, docs in `R/data.R`, prep in `data-raw/`)

- `gss_rac.rda` — GSS racial attitudes, white Americans (1972–1984)
- `gss_homosex.rda` — GSS attitudes toward homosexual sex (1973–2024)
- `eu_membership.rda` — EU entry/exit dates
- `wpp_data.rda` — UN World Population Prospects (1950+)
- `mortality_us.rda` — US mortality rates (1933–2024)

## Vignettes (`vignettes/`)

`gss_homosexuality.qmd` (**KEY**), `decompose_aggregated.qmd`, `simulate.qmd`, `apc.qmd`, `replicating_firebaugh.qmd`.

## Known Limitations of `decompose_aggregated()`

Events are derived from cell-count differences ("never a residual"). New cohorts always get `coming_of_age = pmax(0, n2 - n1)`. Survivor cells depend on the strategy stored as `strategy`: **sign attribution** (default) `mortality = pmax(0, n1 - n2)`, `inmigration = pmax(0, n2 - n1)`, `outmigration = 0`; **residual migration** (`mortality` arg supplied) `deaths = round(n1 × q̃)` with q̃ the death probability compounded over the gap years (open group: per-constituent paths summed), migration the signed residual `n2 − (n1 − deaths)`. Consequences:

- **Migration is net per cell and residual under either strategy.** Sign attribution: out-migration never identified (survivor loss could be death or exit, folded into mortality; `outmigration` always 0), and offsetting flows are masked. Residual migration: out-migration appears and a cell can carry both deaths and in-migration, but the residual bundles genuine migration with any count-vs-rates inconsistency. On raw survey cells either residual is mostly sampling noise — with `mortality` but no `population`, deaths round to ~0 in small cells and nearly all survivor change lands in the residual; meaningful only with a true `population` frame. Migration among coming-of-age cohorts is not modeled.
- **Open top age ("T+").** Survey waves share an open outcome maximum `T`. A `population` frame can extend above `T`: true constituent ages drive mortality, while predictions and output are clamped to `T`. `align_periods()` pools period-1 constituents from `T−gap` through the population terminal age into one cell at `T`, matched to all period-2 ages `T+` (tick-0 `y` = constituents' n-weighted prediction mean, kept per-replicate via `"open_constituents"`). The population and mortality frames must share one terminal open age; uncertainty within that final open group remains. Hard eligibility cutoffs are unsupported and must not be labeled as open groups.
- **Transitions misattributed.** Within-cell transitions (e.g. smokers → non-smokers) are not separately identified and get absorbed into intraindividual change; the `pmax(0, …)` guard masks offsetting flows (a cell with net transition inflow shows 0 mortality even if deaths occurred). Use only on data without significant transitions. `sim_social_change()` tracks transitions properly but can't recover them from aggregated data.
- **Survey weights are an approximation.** With individual-level `weight`, weights are normalized within period to sum to `.N`, then `n = round(sum(normalized_weight))`. Adjusts *relative* structure correctly and keeps event counts tractable, but doesn't recover absolute population sizes; rounding adds small errors. Prefer supplying true `population` counts when available.
- **Limitations of the bootstrap**: Survey design ignored (reweights rows not PSUs); `population`/`mortality` inputs held fixed across replicates (no demographic uncertainty); refit reads original data by name (`getCall()$data`), breaks if out of scope; refitting transiently holds all `R` refits at once (the predictor itself keeps only their coefficients); replicates reuse one design matrix, so a model whose prediction isn't a fixed `linkinv(Xβ)` (a weight-dependent basis) errors rather than falling back to per-replicate prediction.


## Performance profile of `decompose_aggregated()`

Earlier profile on the 1973–2016 GSS homosexuality subset (34,081 rows, 26 periods, ages 21–89). **With a GAM `fun_y` (`s(age)+s(period)`) ~75% of wall time is the user's model prediction, not simulation logic:** GAM call ~1.43 s (of which ~1.08 s is `predict.gam`/`PredictMat`), lm call ~0.35 s; simulation machinery ~0.35 s. `simulate_schedule()` evaluates `fun_y` once over an `n_cells × (n_ev+1)` stacked frame, so cost scales `n_cells × n_ev × cost(fun_y)` — and the bootstrap multiplies by `R`.

**Bootstrap breakdown (`cells="sex"`, `R=100`, GAM `s(age)+s(period)`, model pre-fit, ~19.5 s total, `Rprof`):**

| Chunk | ~Time | Nature |
|---|---|---|
| 100 GAM refits (`y_replicates`) | ~7 s | `mgcv::gam` REML/penalized fit (`.C`/`.Fortran`/`magic`/`am.fit`) — compiled, not our R |
| GAM surface prediction (`predict.gam`/`PredictMat`) | ~4.5 s | lpmatrix build, once per transition — compiled, not our R |
| `replay_schedule` | ~5.8 s (3.96 self) | the deterministic per-event replay loop — **our pure R** |
| data.table + matrix glue | ~2 s | surface assembly, aggregation — our R |

**~11.5 s of the 19.5 s is mgcv's own compiled code (refit + predict); C++ on our side cannot touch it.** Floor with every optimization below: ~10–11 s, then ~90% mgcv. Moving past that means cutting the mgcv cost itself (lower `R`, fix `sp`, `bam`/discretization), not R-vs-C++.

Remaining leverage, highest first:
- **C++ port of `replay_schedule` (~5 s, the one big win on our side).** The `O(n_ev × n_cells × K)` replay loop (`K = max(R, 1)` columns, one per `(ordering, draw)` replicate); no RNG, deterministic — the natural Rcpp unit. Runs 2×(n_periods−1) times at K=R. Per-column vectorized replay is already spent; ~5.8 s → <1 s in C++.
- **Pure-R: share the stack lpmatrix between point and draws (~2 s).** Each transition hits `PredictMat` twice on the *same* `stack` — `predict_y(model, stack)` (point) and `make_X(stack)` inside `replicate_predict` (draws). Build `X` once, then `point = linkinv(X %*% coef(model))`, `draws = linkinv(X %*% beta)`. `make_predictor()`'s probe already guarantees `linkinv(X%*%coef)==predict`, so the point stays byte-identical.
- **Pure-R: fix smoothing params in refits, `sp = model$sp` (~1.5–2 s, ~25% off refit).** Conditions the bootstrap on the selected smoothness — defensible, but shifts SEs, so gate behind a flag rather than change the default silently.
- Micro-opts in `replay_schedule` (~1 s): hoist `rep(sum_n, each=n_cells)`, fold the two per-iteration `colSums`.

The period loop is embarrassingly parallel but the wrong axis (breaks RNG reproducibility, lowest leverage); for the bootstrap, parallelize over the `K` columns instead.
