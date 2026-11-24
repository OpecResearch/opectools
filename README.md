# opectools

**Reusable, configurable analysis workflows for entomological surveillance data**
*Opec Research Consult*

<!-- badges: start -->
[![R-CMD-check](https://github.com/OpecResearch/opectools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/OpecResearch/opectools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`opectools` turns a mosquito-collection study analysis into a reusable workflow.
The statistical code is generic; everything specific to a study (sites, seasons,
sampling design, candidate models, contrasts) lives in a short YAML
configuration file. The same workflow can be rerun on a new dataset from another
place or year by editing the configuration, not the code.

It handles the workflow from raw field data to manuscript tables:

| Step | What it does |
|---|---|
| **Standardise** | Converts field sheets into one standard long format ([`catch_schema()`](docs/data-schema.md)): site, date, species, count, effort, method, location… |
| **Quality control** | Negative counts, invalid effort, unparseable dates, duplicated species rows, incomplete site × month × location grids, households per site-month, stage counts that do not add up |
| **Sampling units** | Sums species rows to the real unit of inference (e.g. site × month × location, household × month) and refuses to sum effort by mistake |
| **Descriptives** | Species composition, diversity (Shannon, Pielou, Simpson), catch per unit effort, zero proportions, trend figures |
| **Models** | Candidate count models fitted side by side with glmmTMB: Poisson, negative binomial, COM-Poisson, zero-inflated; random effects, `ar1()`; `log(effort)` offset |
| **Model checking** | Convergence and Hessian checks, AIC/BIC comparison, likelihood-ratio tests, DHARMa simulation diagnostics, collinearity screening |
| **Effects** | Estimated marginal means and contrasts (rate ratios per unit effort, or differences in expected counts), rate ratios for climate variables |
| **Exploratory** | Period-level climate correlations; lagged regressions of incidence on abundance with Newey–West SEs |
| **Outputs** | CSV tables, Excel workbook, figures, diagnostics plots, `sessionInfo`, and a Quarto report |

Built-in safeguards reflect good practice in vector ecology: **collection methods are never pooled**,
variables declared mutually exclusive (e.g. *season* and *rainfall*) cannot enter the same model,
and **the analyst — not AIC — designates the primary model**.

## Installation

```r
install.packages("remotes")
remotes::install_github("OpecResearch/opectools")
```

Requires R ≥ 4.1. Optional: `car` (VIF), `sandwich` + `lmtest` (HAC standard errors),
`writexl` (Excel output), `readxl` (Excel input), Quarto (report).

## Quick start

```r
library(opectools)

new_analysis("my_study")          # copies config, report, run script and simulated example data
res <- run_analysis("my_study/config.yml")
```

Then open `my_study/results/`, or render the report:

```bash
cd my_study
quarto render report.qmd
```

The example data are **simulated**, with known effects, so you can see the
workflow recover them before using your own data.

## Using it for a new study

1. **Get the data into the standard format.** If your sheet already has the
   standard columns, point `data: catch:` at it (with `column_map:` for
   renaming). For messy workbooks, write a small `import_data(config)`
   function, as in `import_example.R`. See [Adding a new study](docs/adding-a-new-study.md).
2. **Edit `config.yml`:** seasons, sampling unit, candidate models, contrasts.
3. **Run, then review** `qa_checks.csv`, `comparison.csv` and `diagnostics.csv`
   for each analysis.
4. **Designate the primary model** (`primary:`) and rerun to get effects and the report.

A configuration excerpt:

```yaml
analyses:
  trap_season:
    method: CDC
    sampling_unit: [site, period, location]
    exclusive_terms: [[season, rainfall, rainfall_z]]
    candidates:
      nb:   {formula: "count ~ season * location + site + (1 | period)", family: nbinom2}
      zinb: {formula: "count ~ season * location + site + (1 | period)", zi: "~ location", family: nbinom2}
    primary: zinb
    effects:
      - {specs: "season | location", component: cond}
```

## Functions can also be used on their own

```r
d <- simulate_trap_data(sites = 6, n_months = 24)$catch
d <- add_time_vars(d, seasons = list(Wet = 4:10, Dry = c(11, 12, 1:3)), reference_season = "Dry")
u <- aggregate_units(d, c("site", "period", "location"), constant_vars = c("effort", "season"))
fit <- fit_count_model(list(formula = "count ~ season + location + site", family = "nbinom2"), u)
tidy_model(fit)
run_diagnostics(fit)$tests
emm_effects(fit, "season", component = "cond")$contrasts
diversity_indices(c(2865, 64, 38, 25, 21))
```

## Documentation

- [Standard data format](docs/data-schema.md)
- [Adding a new study](docs/adding-a-new-study.md)
- [Statistical methods and assumptions](docs/methods.md)

## Data policy

This repository contains **code and simulated data only**. Field data, client
data and study-specific import scripts are kept in private repositories. Do not
commit real data: `data/` and `results/` folders are git-ignored.

## Citation

If you use `opectools`, please cite it (see [`CITATION.cff`](CITATION.cff)).

## Licence

MIT © Opec Research Consult
