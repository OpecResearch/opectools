# opectools 0.2.0

* New publication figures, written automatically by `run_analysis()` and
  included in the report: species composition (`plot_composition()`), seasonal
  trajectories with climate (`plot_seasonal()`), forest plot of adjusted rate
  ratios across methods (`plot_effects()`), DHARMa diagnostics
  (`plot_diagnostics()`), model comparison (`plot_model_comparison()`) and
  abdominal status (`plot_stages()`).
* New `figures:` config section (formats, dpi, colours, site order, labels).
* Analyses accept `label:` and `effort_label:`; effects accept `figure: false`.
* Helpers: `species_labels()` (automatic italics), `period_labels()`,
  `group_palette()`, `opec_theme()`, `save_figure()`.

# opectools 0.1.0

* First release: standard catch format and validation, time and season handling,
  sampling-unit aggregation, descriptive summaries and diversity indices,
  candidate count models with glmmTMB, DHARMa diagnostics, estimated marginal
  means and contrasts, climate correlations and lagged regressions,
  data simulators, config-driven pipeline (`run_analysis()`), project template
  (`new_analysis()`) and Quarto report.
