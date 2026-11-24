test_that("template project runs end to end on example data", {
  skip_on_cran()
  dir <- file.path(tempdir(), "opec_template_test")
  unlink(dir, recursive = TRUE)
  new_analysis(dir)
  cfg <- read_config(file.path(dir, "config.yml"))
  for (nm in names(cfg$analyses)) cfg$analyses[[nm]]$diagnostics$n_sim <- 100
  cfg$analyses$resting_season$candidates <- cfg$analyses$resting_season$candidates["nb"]
  cfg$analyses$resting_season$primary <- "nb"
  res <- suppressMessages(run_analysis(cfg))
  expect_named(res$analyses, c("trap_season", "trap_climate", "resting_season"))
  expect_true(file.exists(file.path(dir, "results", "results.rds")))
  expect_true(file.exists(file.path(dir, "results", "trap_season", "comparison.csv")))
  expect_gt(nrow(res$analyses$trap_season$contrasts), 0)
  expect_error(prepare_units(res$data$catch, list(candidates = list())), "never pooled")
})
