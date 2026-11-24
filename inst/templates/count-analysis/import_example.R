# =============================================================================
# Example import adapter (not used unless config.yml sets data: import_script)
# -----------------------------------------------------------------------------
# Real field workbooks rarely arrive in the standard format. Write ONE small
# function per study that converts them; keep every study-specific quirk here
# (typos in column names, months without years, wide indoor/outdoor columns,
# spelling variants of sites and species, documented date corrections).
#
# The function must be called import_data(config) and return:
#   list(catch = <standard catch data>, covariates = <optional monthly data>)
# See opectools::catch_schema() for the standard columns.
# Any extra keys you add under `data:` in config.yml are available here,
# e.g. config$data$workbook.
# =============================================================================

import_data <- function(config) {
  f <- config$data$workbook

  # 1. Harmonise spelling of sites and species (regex = standard label)
  sites <- c("site.?a" = "Site A", "site.?b" = "Site B")
  species <- c("gambiae" = "An. gambiae s.l.", "funestus" = "An. funestus group")

  # 2. Light-trap sheet with one row per site x month x species and separate
  #    Indoor / Outdoor count and climate columns, months recorded without year
  cdc <- opectools::clean_names(readxl::read_excel(f, sheet = "cdc"))
  cdc <- data.frame(
    site    = opectools::recode_patterns(cdc$location, sites),
    date    = opectools::months_to_dates(cdc$month, config$time$study_start),
    species = opectools::recode_patterns(cdc$species, species),
    indoor  = cdc$indoor,  outdoor = cdc$outdoor,
    t_in    = cdc$temperature_indoor, t_out = cdc$temperature_outdoor
  )
  catch <- opectools::pivot_locations(
    cdc, count_cols = c(Indoor = "indoor", Outdoor = "outdoor"),
    covariate_cols = list(temperature = c(Indoor = "t_in", Outdoor = "t_out")))
  catch$method <- "CDC"
  catch$effort <- 3            # trap-nights per location per month

  # 3. Monthly covariates (rainfall, incidence) with a date column
  covs <- opectools::clean_names(readxl::read_excel(f, sheet = "monthly"))
  covs <- data.frame(date = opectools::months_to_dates(covs$month, config$time$study_start),
                     rainfall = covs$rainfall, malaria_rate = covs$malaria_rate)

  list(catch = catch, covariates = covs)
}
