# Standard data format

Every analysis in `opectools` reads catch data in **one long format**:
**one row per species per sampling occasion**. Run `opectools::catch_schema()`
in R to see this table.

| Column | Required | Description |
|---|---|---|
| `site` | yes | Study site, community or village |
| `date` | yes | Sampling date (`YYYY-MM-DD`). If only the month is known, use the first day of the month, or give a `month` column plus `time: study_start` in the config |
| `species` | yes | Taxon label. Harmonise spellings with `recode_patterns()` |
| `count` | yes | Non-negative whole number |
| `effort` | yes | Sampling effort for that occasion: trap-nights, room-days, person-nights… |
| `method` | no | Collection method (`CDC`, `PSC`, `HLC`…). Methods are never pooled for inference |
| `location` | no | `Indoor` / `Outdoor`, or another within-site position |
| `unit_id` | no | Household, trap or collector ID for repeated sampling units |
| *covariates* | no | Any unit-level measurement, e.g. `temperature`, `rh` |

Additional count columns (e.g. abdominal stages `blood_fed`, `gravid`) can be
summed to the sampling unit by listing them under `sum_vars:`.

## Effort is per sampling unit, not per species row

If a household visit has `effort = 1` room-day and three species rows, each
row carries `effort = 1`. When species rows are summed to the household-visit
unit, effort stays `1`. `aggregate_units()` stops with an error if effort
differs between rows of the same unit, so it can never be summed by mistake.

## Monthly covariates

Period-level covariates (rainfall, incidence) go in a separate file with a
`date` (or `month`) column and are joined by `period` (`YYYY-MM`).

## Variables created automatically

| Variable | Meaning |
|---|---|
| `period` | `YYYY-MM` factor in study order (use as `(1 \| period)`) |
| `time_index`, `time` | Consecutive month number from study start (integer / factor for `ar1()`) — correct across years |
| `month`, `month_label`, `year` | Calendar components |
| `season` | From the `seasons:` definition in the config |
| `series` | Site × location × unit ID, for `ar1(time + 0 \| series)` |
| `<var>_z` | Standardised copies of variables listed under `standardise:` |
