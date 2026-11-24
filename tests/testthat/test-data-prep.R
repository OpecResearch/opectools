test_that("clean_names and recode_patterns", {
  expect_equal(names(clean_names(data.frame(`Number Mosquito` = 1, `Half-gravid` = 2,
                                             check.names = FALSE))),
               c("number_mosquito", "half_gravid"))
  expect_equal(recode_patterns(c("An. gambiae s.l", "Vilage A", "other"),
                               c("gambiae" = "An. gambiae s.l.", "vil+age.?a" = "Village A")),
               c("An. gambiae s.l.", "Village A", "other"))
})

test_that("pivot_locations converts wide indoor/outdoor columns", {
  w <- data.frame(site = "A", indoor = 3, outdoor = 1, t_in = 27, t_out = 25)
  l <- pivot_locations(w, c(Indoor = "indoor", Outdoor = "outdoor"),
                       list(temperature = c(Indoor = "t_in", Outdoor = "t_out")))
  expect_equal(nrow(l), 2)
  expect_equal(l$count, c(3, 1))
  expect_equal(l$temperature, c(27, 25))
})

test_that("aggregate_units sums counts but never sums effort", {
  d <- data.frame(site = "A", period = "2020-01", species = c("x", "y"),
                  count = c(2, 3), effort = 3, temperature = 27)
  u <- aggregate_units(d, c("site", "period"), constant_vars = c("effort", "temperature"))
  expect_equal(u$count, 5)
  expect_equal(u$effort, 3)
  d$effort <- c(3, 2)
  expect_error(aggregate_units(d, c("site", "period"), constant_vars = "effort"), "differ")
})

test_that("standardise_catch maps columns and checks requirements", {
  raw <- data.frame(community = "A", day = "2021-01-05", sp = "x", n = 2)
  s <- standardise_catch(raw, map = c(site = "community", date = "day", species = "sp", count = "n"),
                         constants = list(effort = 1, method = "PSC"))
  expect_s3_class(s$date, "Date")
  expect_equal(s$method, "PSC")
  expect_error(standardise_catch(raw), "Missing required")
})
