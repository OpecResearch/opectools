# Adding a new study

## 1. Create a project

```r
library(opectools)
new_analysis("~/projects/2027-kano-larval-survey", example_data = FALSE)
```

This creates `config.yml`, `run_analysis.R`, `report.qmd`, `import_example.R`
and an empty `data/` folder. Keep the project — and its data — in a **private**
repository or folder.

## 2. Get the data into the standard format

**If the data are already tidy**, point the config at the file and rename columns:

```yaml
data:
  catch: data/collections.csv
  column_map: {site: village, count: n_mosquitoes, effort: trap_nights}
  constants: {method: CDC}
```

**If the workbook is messy**, rename `import_example.R` to `import.R`,
adapt it, and point the config at it:

```yaml
data:
  workbook: data/field_workbook.xlsx
  import_script: import.R
```

Useful helpers for the import script:

| Problem | Helper |
|---|---|
| Inconsistent column names | `clean_names()` |
| Spelling variants (`Vilage A`, `An. gambiae s.l`) | `recode_patterns()` |
| Months with no year | `months_to_dates(month, start = "2020-08")` |
| Separate Indoor / Outdoor columns | `pivot_locations()` |
| Documented year error in dates | `shift_dates_years()` (writes an audit trail and refuses to shift twice) |

## 3. Define the design in `config.yml`

- `time: seasons:` for the local rainfall pattern (can be more than two seasons,
  e.g. long rains / short rains / dry).
- `sampling_unit:` the real unit of inference for each method.
- `validation:` expected grid, households per site-month, stage-count checks.
- `candidates:` the models that are plausible *a priori*. Use the standard
  column names in formulas.

## 4. Run, review, decide

```r
res <- run_analysis("config.yml")
```

Review, for each analysis:

1. `qa.csv` and the top-level `qa_checks.csv`
2. `comparison.csv`: converged? positive-definite Hessian? AIC?
3. `diagnostics.csv` and `diagnostics/*.png`: uniformity, dispersion, zero inflation
4. `warnings.csv`

Then set `primary:` for each analysis and rerun. Effects, contrasts and VIF are
reported for the primary model only.

## 5. Report

```bash
quarto render report.qmd            # HTML
quarto render report.qmd --to docx  # Word
```

## 6. Archive

Commit `config.yml`, `import.R` and `results/sessionInfo.txt` to the private
project repository and tag the commit used for a manuscript, so every published
number can be regenerated.
