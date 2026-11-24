#' Add lagged copies of variables
#'
#' Lag `L` means the value from `L` periods earlier (exposure precedes outcome).
#'
#' @param dat Data with one row per period.
#' @param vars Variables to lag.
#' @param max_lag Maximum lag (default 2).
#' @param order_by Column defining time order.
#' @param log1p Also add `log1p()` versions (useful for counts).
#' @return `dat` ordered by time with `<var>_lag<L>` columns.
#' @export
add_lags <- function(dat, vars, max_lag = 2, order_by = "time_index", log1p = FALSE) {
  dat <- dat[order(dat[[order_by]]), , drop = FALSE]
  if (anyDuplicated(dat[[order_by]])) stop("add_lags() needs one row per period.")
  for (v in vars) for (L in 0:max_lag) {
    nm <- paste0(v, "_lag", L)
    dat[[nm]] <- if (L == 0) dat[[v]] else dplyr::lag(dat[[v]], L)
    if (log1p) dat[[paste0("log1p_", nm)]] <- log1p(dat[[nm]])
  }
  dat
}

#' Simple lagged regressions of an outcome on each predictor
#'
#' One linear model per predictor. With few periods these are exploratory.
#' If sandwich and lmtest are installed, Newey-West (HAC) standard errors
#' are used for robustness to serial correlation.
#'
#' @param dat Period-level data (see [add_lags()]).
#' @param outcome Outcome column (e.g. malaria incidence rate).
#' @param predictors Predictor columns.
#' @param hac_lag Newey-West lag (`NULL` for ordinary SEs).
#' @return Tibble.
#' @export
lag_regressions <- function(dat, outcome, predictors, hac_lag = 1) {
  use_hac <- !is.null(hac_lag) && requireNamespace("sandwich", quietly = TRUE) &&
    requireNamespace("lmtest", quietly = TRUE)
  dplyr::bind_rows(lapply(predictors, function(p) {
    d <- stats::na.omit(as.data.frame(dat)[c(outcome, p)])
    if (nrow(d) < 5) return(tibble::tibble(predictor = p, n = nrow(d)))
    mod <- stats::lm(stats::reformulate(p, response = outcome), data = d)
    ct <- if (use_hac) {
      lmtest::coeftest(mod, vcov. = sandwich::NeweyWest(mod, lag = hac_lag,
                                                         prewhite = FALSE, adjust = TRUE))
    } else summary(mod)$coefficients
    tibble::tibble(predictor = p, n = nrow(d), estimate = ct[2, 1], std_error = ct[2, 2],
                   statistic = ct[2, 3], p_value = ct[2, 4],
                   r_squared = summary(mod)$r.squared,
                   se_type = if (use_hac) paste0("Newey-West (lag ", hac_lag, ")") else "OLS")
  }))
}
