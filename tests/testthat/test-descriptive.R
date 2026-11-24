test_that("diversity indices match known values", {
  d <- diversity_indices(c(10, 10, 10, 10))
  expect_equal(d$shannon_H, log(4))
  expect_equal(d$pielou_J, 1)
  expect_equal(d$simpson_1_minus_D, 0.75)
  expect_equal(d$inverse_simpson, 4)
  expect_true(is.na(diversity_indices(5)$pielou_J))
})

test_that("summarise_catch computes density per effort", {
  u <- data.frame(site = c("A", "A"), count = c(3, 0), effort = c(3, 3))
  s <- summarise_catch(u, "site")
  expect_equal(s$density_per_effort, 0.5)
  expect_equal(s$zero_prop, 0.5)
})
