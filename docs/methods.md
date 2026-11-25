# Statistical methods and assumptions

## Unit of inference

Counts are modelled at the **sampling unit** (e.g. site × month × location for
light traps; household × month for resting collections), never at the species
row. Species rows are summed; effort and covariates are carried over and must be
constant within a unit.

## Count models

Models are fitted by maximum likelihood with `glmmTMB`, using a log link and an
offset `log(effort)`, so coefficients are effects on **catch per unit effort**.

| Family | When |
|---|---|
| `poisson` | Baseline; rarely adequate for mosquito counts |
| `nbinom2` | Overdispersed counts (variance = μ + μ²/θ) |
| `nbinom1` | Overdispersion proportional to the mean |
| `compois` | Under- or overdispersion (Conway–Maxwell–Poisson), useful for small resting-catch counts |
| `zi:` formula | Extra (structural) zeros, e.g. outdoor traps with many empty nights |

Random effects (e.g. `(1 | period)` for shared monthly conditions,
`(1 | unit_id)` for repeated households) and temporal autocorrelation
(`ar1(time + 0 | series)`) are written directly in the formula.

**Few groups.** With four sites, site is usually best treated as a fixed
effect; a random-effect variance estimated from four levels is unreliable.

## Choosing the primary model

All candidates are reported with convergence status, Hessian check, AIC and BIC.
AIC is only compared among models fitted to the same rows. The analyst
designates the primary model (`primary:`) after considering:

1. the study design and the question (prespecified structure first);
2. simulation-based residual diagnostics (DHARMa: uniformity, dispersion,
   zero inflation, outliers, residuals against predictors);
3. information criteria and likelihood-ratio tests of specific terms.

The package never picks a model automatically.

## Effects

`emm_effects()` reports estimated marginal means and pairwise contrasts:

- `component: cond`: catch rate in the count process; contrasts are **rate ratios**.
- `component: response`: expected count including extra zeros; contrasts are
  **differences** in expected counts.
- `component: zi`: probability of an extra zero.

By default estimates are **per unit effort** (offset set to 0). Use
`per_unit_effort: false` for expected counts at the observed effort.

## Season versus climate

Season is largely a classification of rainfall. Season and rainfall should not
enter the same model; declare them in `exclusive_terms:` and fit separate
season and climate analyses. Continuous climate variables can be standardised
(`standardise:`) so rate ratios are per 1 SD.

## Monthly correlations and lagged regressions

With about 12 monthly values, correlations and lagged regressions of incidence
on abundance are **descriptive/exploratory**. Newey–West (HAC) standard errors
are used when `sandwich` and `lmtest` are installed. These associations are not
causal and are confounded by shared seasonality.

## Collinearity

Generalised VIFs are computed from an auxiliary linear model with the same fixed
effects (the design matrix is what matters). Adjusted GVIF = GVIF^(1/(2·Df)).

## Figures

Figure 3 shows all effects as **rate ratios on one log scale** (count component,
per unit effort), so effects from different collection methods and from climate
models can be compared directly. Differences in expected counts (including
zero inflation) remain available in `contrasts.csv` for the supplement.
Species names are italicised automatically (genus and epithet; qualifiers such
as s.l. or group upright).

## Reproducibility

DHARMa diagnostics use a fixed, configurable seed. Each run writes
`sessionInfo.txt`. Tag the commit and config used for each manuscript.
