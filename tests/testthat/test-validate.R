base <- function() data.frame(site = "A", unit_id = c("h1", "h2"), date = as.Date("2021-01-01"),
                              species = "x", count = c(1, 2), effort = 1,
                              blood_fed = c(1, 1), unfed = c(0, 0))

test_that("validation fails on negative counts and bad effort", {
  d <- base(); d$count[1] <- -1
  expect_error(validate_catch(d), "negative")
  qa <- validate_catch(d, stop_on_fail = FALSE)
  expect_equal(qa$status[qa$check == "Counts non-negative"], "fail")
  d <- base(); d$effort <- 0
  expect_error(validate_catch(d), "effort")
})

test_that("expected units and sum checks warn", {
  d <- add_time_vars(base())
  qa <- validate_catch(d, expected_units = list(by = c("site", "period"), units = 10),
                       sum_checks = list(list(total = "count", parts = c("blood_fed", "unfed"))))
  expect_equal(qa$status[grepl("sampling units per", qa$check)], "warn")
  expect_equal(qa$status[grepl("^count =", qa$check)], "warn")
  expect_error(validate_catch(d, expected_units = list(by = "site", n = 10)), "units")
})
