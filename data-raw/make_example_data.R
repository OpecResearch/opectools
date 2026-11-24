# Regenerates the simulated example data shipped in inst/extdata.
# All data are SIMULATED; no real study data are included in this repository.
# Run from the package root: Rscript data-raw/make_example_data.R
devtools::load_all(quiet = TRUE)

trap <- simulate_trap_data(sites = c("Site A", "Site B", "Site C", "Site D"),
                           start = "2022-08", n_months = 12, seed = 101)
hh <- simulate_household_data(sites = c("Site A", "Site B", "Site C", "Site D"),
                              households_per_site = 10, start = "2022-08",
                              n_months = 12, seed = 202)

catch <- dplyr::bind_rows(trap$catch, hh$catch)
catch$date <- format(catch$date, "%Y-%m-%d")
covs <- trap$covariates
covs$date <- format(covs$date, "%Y-%m-%d")

dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
utils::write.csv(catch, "inst/extdata/example_catch.csv", row.names = FALSE, na = "")
utils::write.csv(covs, "inst/extdata/example_monthly_covariates.csv", row.names = FALSE)
saveRDS(list(trap = trap$truth, household = hh$truth), "inst/extdata/example_truth.rds")
message("Wrote ", nrow(catch), " catch rows and ", nrow(covs), " covariate rows.")
