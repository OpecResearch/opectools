rzinb <- function(n, mu, theta, pi0) {
  y <- stats::rnbinom(n, mu = mu, size = theta)
  y[stats::runif(n) < pi0] <- 0L
  y
}

monthly_climate <- function(dates, wet) {
  m <- as.integer(format(dates, "%m"))
  w <- m %in% wet
  tibble::tibble(
    date = dates,
    rainfall = round(pmax(0, ifelse(w, stats::rnorm(length(m), 260, 80),
                                    stats::rnorm(length(m), 25, 20))), 1),
    malaria_rate = round(pmax(0, ifelse(w, stats::rnorm(length(m), 85, 12),
                                        stats::rnorm(length(m), 55, 10))))
  )
}

#' Simulate light-trap catch data in the standard format
#'
#' Generates species-level rows for site x month x location sampling units
#' with known effects, overdispersion, extra zeros and sampling effort. Use
#' it for examples, teaching, testing and power checks; models should
#' recover the `truth` values.
#'
#' @param sites Site labels (or a number of sites).
#' @param start First month (Date or "YYYY-MM").
#' @param n_months Number of consecutive months.
#' @param locations Named vector of location rate multipliers.
#' @param zi_prob Named vector of extra-zero probabilities per location.
#' @param species Named vector of species proportions.
#' @param base_rate Mean catch per unit effort indoors in the dry season.
#' @param season_rr Wet / Dry rate ratio.
#' @param wet_months Months of the wet season.
#' @param site_sd SD of site effects on the log scale.
#' @param theta Negative-binomial size parameter.
#' @param effort Effort per unit (e.g. 3 trap-nights).
#' @param method Method label.
#' @param seed Random seed.
#' @return A list with `catch` (species-level standard format), `covariates`
#'   (monthly rainfall and malaria rate) and `truth`.
#' @export
simulate_trap_data <- function(sites = 4, start = "2022-01", n_months = 12,
                               locations = c(Indoor = 1, Outdoor = 0.25),
                               zi_prob = c(Indoor = 0.05, Outdoor = 0.35),
                               species = c("An. gambiae s.l." = 0.88, "An. funestus group" = 0.07,
                                           "An. coustani" = 0.05),
                               base_rate = 2, season_rr = 1.8, wet_months = 4:10,
                               site_sd = 0.4, theta = 1.5, effort = 3,
                               method = "CDC", seed = 1) {
  set.seed(seed)
  if (is.numeric(sites) && length(sites) == 1) sites <- paste("Site", LETTERS[seq_len(sites)])
  start <- as.Date(paste0(substr(as.character(start), 1, 7), "-01"))
  dates <- seq(start, by = "month", length.out = n_months)
  site_eff <- stats::setNames(stats::rnorm(length(sites), 0, site_sd), sites)
  units <- expand.grid(site = sites, date = dates, location = names(locations),
                       stringsAsFactors = FALSE)
  m <- as.integer(format(units$date, "%m"))
  wet <- m %in% wet_months
  mu <- base_rate * effort * exp(site_eff[units$site]) * ifelse(wet, season_rr, 1) *
    locations[units$location]
  units$total <- rzinb(nrow(units), mu, theta, zi_prob[units$location])
  units$temperature <- round(stats::rnorm(nrow(units), ifelse(wet, 26, 28), 1) +
                               ifelse(units$location == "Indoor", 1, 0), 2)
  units$rh <- round(stats::rnorm(nrow(units), ifelse(wet, 85, 65), 4) +
                      ifelse(units$location == "Indoor", 2, 0), 1)
  rows <- lapply(seq_len(nrow(units)), function(i) {
    n <- as.vector(stats::rmultinom(1, units$total[i], species))
    data.frame(site = units$site[i], date = units$date[i], location = units$location[i],
               method = method, species = names(species), count = n, effort = effort,
               temperature = units$temperature[i], rh = units$rh[i],
               stringsAsFactors = FALSE)
  })
  list(
    catch = tibble::as_tibble(do.call(rbind, rows)),
    covariates = monthly_climate(dates, wet_months),
    truth = list(season_rr = season_rr, location_rr = locations / locations[[1]],
                 zi_prob = zi_prob, theta = theta, site_effects = site_eff,
                 base_rate = base_rate)
  )
}

#' Simulate household resting-collection (e.g. PSC) data
#'
#' Repeated monthly visits to the same households, with household and month
#' random effects and abdominal-status counts.
#'
#' @inheritParams simulate_trap_data
#' @param households_per_site Households sampled per site each month.
#' @param household_sd,month_sd SDs of household and month random effects.
#' @return A list with `catch`, `covariates` and `truth`.
#' @export
simulate_household_data <- function(sites = 4, households_per_site = 10, start = "2022-01",
                                    n_months = 12,
                                    species = c("An. gambiae s.l." = 0.97, "An. funestus group" = 0.03),
                                    base_rate = 0.6, season_rr = 3, wet_months = 4:10,
                                    site_sd = 0.3, household_sd = 0.4, month_sd = 0.3,
                                    theta = 3, method = "PSC", seed = 2) {
  set.seed(seed)
  if (is.numeric(sites) && length(sites) == 1) sites <- paste("Site", LETTERS[seq_len(sites)])
  start <- as.Date(paste0(substr(as.character(start), 1, 7), "-01"))
  dates <- seq(start, by = "month", length.out = n_months)
  site_eff <- stats::setNames(stats::rnorm(length(sites), 0, site_sd), sites)
  hh <- expand.grid(site = sites, h = seq_len(households_per_site), stringsAsFactors = FALSE)
  hh$unit_id <- sprintf("S%d-H%02d", match(hh$site, sites), hh$h)
  hh_eff <- stats::setNames(stats::rnorm(nrow(hh), 0, household_sd), hh$unit_id)
  month_eff <- stats::rnorm(n_months, 0, month_sd)
  units <- merge(hh[c("site", "unit_id")], data.frame(date = dates, mi = seq_len(n_months)))
  wet <- as.integer(format(units$date, "%m")) %in% wet_months
  mu <- base_rate * exp(site_eff[units$site] + hh_eff[units$unit_id] + month_eff[units$mi]) *
    ifelse(wet, season_rr, 1)
  units$total <- stats::rnbinom(nrow(units), mu = mu, size = theta)
  units$occupants <- sample(2:7, nrow(units), replace = TRUE)
  rows <- lapply(seq_len(nrow(units)), function(i) {
    n <- as.vector(stats::rmultinom(1, units$total[i], species))
    st <- t(vapply(n, function(k) as.vector(stats::rmultinom(1, k, c(0.55, 0.15, 0.1, 0.2))),
                   numeric(4)))
    data.frame(site = units$site[i], unit_id = units$unit_id[i], date = units$date[i],
               location = "Indoor", method = method, species = names(species), count = n,
               effort = 1, occupants = units$occupants[i],
               blood_fed = st[, 1], half_gravid = st[, 2], gravid = st[, 3], unfed = st[, 4],
               stringsAsFactors = FALSE)
  })
  list(catch = tibble::as_tibble(do.call(rbind, rows)),
       covariates = monthly_climate(dates, wet_months),
       truth = list(season_rr = season_rr, theta = theta, site_effects = site_eff,
                    household_sd = household_sd, month_sd = month_sd))
}
