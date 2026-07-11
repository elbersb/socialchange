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
2. **Aggregated** (`R/decompose_aggregated.R`) — `decompose_aggregated()`: data aggregated by age + covariates. Microsimulation randomly orders demographic events. Needs stacked panel data (age, period, cell counts) and a fitted `model` (`lm`/`glm`/`gam`) predicting outcome from age/period/cells. Decomposes into intraindividual change, coming-of-age, mortality, net in-migration. Optional `population` arg supplies a true cell × period count frame (overrides survey counts). `R > 0` adds bootstrap SEs (combined ordering + model uncertainty). → class `social_change_decomp`. `print` and `plot` methods live in `R/decompose_aggregated_output.R`.
3. **Simulation** (`R/simulate.R`) — `sim_social_change()`: forward simulation. User supplies functions for outcome (`fun_y`), mortality, coming-of-age, migration, state transitions. Returns event-by-event records. → class `social_change_sim`

Additional methods:
- **CR-IC** (`R/cr_ic.R`) — classical cohort-replacement vs. intraindividual change (algebraic, linear, model-based). → class `cr_ic_decomposition`
- **APC** (`R/apc.R`) — APC models via orthogonal polynomial contrasts; includes `plot_gam_surface()`. → class `apc_model`

Functions rename input columns to standardized names internally. `decompose_aggregated()` requires numeric `period`; `cr_ic()` validates that periods/cohorts parse as integers and errors otherwise.

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
- **Open top age ("T+").** The shared maximum age is always treated as an open interval: `align_periods()` pools period-1 survivor ages `T−gap…T` into one cell at `T`, matched to the period-2 pool recorded at `T` (tick-0 `y` = constituents' n-weighted prediction mean, kept per-replicate via the `"open_constituents"` attribute; `build_event_stack()` clamps ages at `T`, so the model never predicts past the data). All waves must share a maximum age (`pmin(age, T)` on input if ragged), and a `population` frame must share it too. Residual caveat, hard-truncation designs only (sample stops at 65 by eligibility, not top-coding): window exits still read as mortality, pooled at `T`.
- **Transitions misattributed.** Within-cell transitions (e.g. smokers → non-smokers) are not separately identified and get absorbed into intraindividual change; the `pmax(0, …)` guard masks offsetting flows (a cell with net transition inflow shows 0 mortality even if deaths occurred). Use only on data without significant transitions. `sim_social_change()` tracks transitions properly but can't recover them from aggregated data.
- **Survey weights are an approximation.** With individual-level `weight`, weights are normalized within period to sum to `.N`, then `n = round(sum(normalized_weight))`. Adjusts *relative* structure correctly and keeps event counts tractable, but doesn't recover absolute population sizes; rounding adds small errors. Prefer supplying true `population` counts when available.
- **Limitations of the bootstrap**: Survey design ignored (reweights rows not PSUs); refit reads original data by name (`getCall()$data`), breaks if out of scope; refitting transiently holds all `R` refits at once (the predictor itself keeps only their coefficients); replicates reuse one design matrix, so a model whose prediction isn't a fixed `linkinv(Xβ)` (a weight-dependent basis) errors rather than falling back to per-replicate prediction.

## Performance profile of `decompose_aggregated()`

Profiled on GSS homosexuality (34,026 rows, 26 periods, ages 21–89). **With a GAM `fun_y` (`s(age)+s(period)`) ~75% of wall time is the user's model prediction, not simulation logic:** GAM call ~1.43 s (of which ~1.08 s is `predict.gam`/`PredictMat`), lm call ~0.35 s; simulation machinery ~0.35 s. `simulate_schedule()` evaluates `fun_y` once over an `n_cells × (n_ev+1)` stacked frame, so cost scales `n_cells × n_ev × cost(fun_y)` — and the bootstrap multiplies by `R`.

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
