test_that("parse_month handles names, abbreviations, numbers and dates", {
  expect_equal(parse_month(c("Aug", "Sept", "december", " jan ", "x")), c(8L, 9L, 12L, 1L, NA))
  expect_equal(parse_month(c(3, 13)), c(3L, NA))
  expect_equal(parse_month(as.Date("2021-07-15")), 7L)
})

test_that("months_to_dates crosses the year boundary", {
  d <- months_to_dates(c("Aug", "Dec", "Jan", "Jul"), start = "2020-08")
  expect_equal(d, as.Date(c("2020-08-01", "2020-12-01", "2021-01-01", "2021-07-01")))
})

test_that("assign_season validates the definition", {
  s <- list(Wet = 4:10, Dry = c(11, 12, 1:3))
  expect_equal(as.character(assign_season(c("Jan", "Jun"), s)), c("Dry", "Wet"))
  expect_equal(levels(assign_season("Jan", s, reference = "Dry"))[1], "Dry")
  expect_error(assign_season("Jan", list(A = 1:6, B = 6:12)), "more than one season")
  expect_error(assign_season("Dec", list(A = 1:6)), "not covered")
})

test_that("add_time_vars gives a continuous index across years", {
  d <- data.frame(date = as.Date(c("2020-11-03", "2021-01-20", "2022-02-01")))
  out <- add_time_vars(d, seasons = list(Wet = 4:10, Dry = c(11, 12, 1:3)))
  expect_equal(out$time_index, c(1L, 3L, 16L))
  expect_equal(as.character(out$period), c("2020-11", "2021-01", "2022-02"))
  expect_true(is.factor(out$time))
})

test_that("shift_dates_years audits and refuses double shifts", {
  s <- shift_dates_years(as.Date(c("2019-08-23", "2020-07-01")), 1, "2019-08-01", "2020-07-31")
  expect_equal(s$dates, as.Date(c("2020-08-23", "2021-07-01")))
  expect_equal(nrow(s$audit), 2)
  expect_error(shift_dates_years(s$dates, 1, "2019-08-01", "2020-07-31"), "refusing")
})
