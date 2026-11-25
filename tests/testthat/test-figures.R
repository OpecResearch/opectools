test_that("label helpers", {
  l <- species_labels(c("An. gambiae s.l.", "Anopheles funestus group", "Unidentified"))
  expect_true(is.call(l[[1]]) || is.expression(l[[1]]))
  expect_equal(l[[3]], "Unidentified")
  expect_equal(period_labels(c("2020-12", "2021-01", "2021-02")), c("Dec\n2020", "Jan\n2021", "Feb"))
  pal <- group_palette(c("A", "B"), list(B = "#000000"))
  expect_equal(unname(pal["B"]), "#000000")
})

test_that("all figures are produced from a run", {
  skip_on_cran()
  dir <- file.path(tempdir(), "opec_fig_test")
  unlink(dir, recursive = TRUE)
  new_analysis(dir)
  cfg <- read_config(file.path(dir, "config.yml"))
  for (nm in names(cfg$analyses)) cfg$analyses[[nm]]$diagnostics$n_sim <- 100
  cfg$analyses$resting_season$candidates <- cfg$analyses$resting_season$candidates["nb"]
  cfg$analyses$resting_season$primary <- "nb"
  cfg$figures$dpi <- 72
  res <- suppressMessages(run_analysis(cfg))
  expect_setequal(names(res$figures), c("composition", "seasonal", "effects", "diagnostics",
                                        "model_comparison", "stages"))
  expect_true(all(file.exists(res$figures)))
  ed <- effects_data(res)
  expect_true(all(ed$estimate > 0))
  expect_false(any(grepl("Site B / Site A", ed$label)))   # figure: false respected
})
