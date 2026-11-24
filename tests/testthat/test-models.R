sim_units <- function() {
  s <- simulate_trap_data(sites = 6, n_months = 24, start = "2021-01", seed = 11,
                          season_rr = 2, zi_prob = c(Indoor = 0, Outdoor = 0))
  d <- add_time_vars(s$catch, seasons = list(Wet = 4:10, Dry = c(11, 12, 1:3)),
                     reference_season = "Dry")
  u <- aggregate_units(d, c("site", "period", "location"),
                       constant_vars = c("effort", "season", "time"))
  u$location <- stats::relevel(factor(u$location), "Outdoor")
  u
}

test_that("negative binomial model with offset recovers the simulated effects", {
  u <- sim_units()
  f <- fit_count_model(list(formula = "count ~ season + location + site", family = "nbinom2"), u)
  expect_true(model_ok(f))
  ci <- tidy_model(f)
  wet <- ci[ci$term == "seasonWet", ]
  expect_true(wet$exp_conf_low < 2 && wet$exp_conf_high > 2)
  ind <- ci[ci$term == "locationIndoor", ]
  expect_true(ind$exp_conf_low < 4 && ind$exp_conf_high > 4)
})

test_that("candidates are compared, tested and checked for exclusive terms", {
  u <- sim_units()
  cands <- list(add = list(formula = "count ~ season + location", family = "nbinom2"),
                int = list(formula = "count ~ season * location", family = "nbinom2"))
  fits <- fit_candidates(cands, u)
  cmp <- compare_models(fits)
  expect_equal(nrow(cmp), 2)
  expect_true(all(cmp$converged))
  lr <- lr_test(fits$add, fits$int)
  expect_equal(lr$df, 1)
  expect_error(check_exclusive_terms("count ~ season + rainfall", list(c("season", "rainfall"))),
               "mutually exclusive")
  e <- emm_effects(fits$add, "season", component = "cond")
  expect_equal(e$contrasts$contrast_scale, "rate ratio")
})

test_that("a failed model is reported, not fatal", {
  u <- sim_units()
  f <- fit_count_model(list(formula = "count ~ season", family = "nbinom2"),
                       transform(u, count = count + 0.5))
  expect_false(is.null(f$name))
  expect_error(fit_count_model(list(formula = "count ~ nonexistent", family = "nbinom2"), u),
               "unknown column")
})
