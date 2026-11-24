#' Estimated marginal means and pairwise contrasts
#'
#' Wraps emmeans for glmmTMB count models.
#'
#' * `component = "cond"`: rates from the count process; contrasts are
#'   **rate ratios**.
#' * `component = "response"`: expected counts combining the count and
#'   zero-inflation parts; contrasts are **differences** in expected count.
#' * `component = "zi"`: probability of an extra (structural) zero.
#'
#' @param fit An `opec_fit`.
#' @param specs Right-hand side for emmeans, e.g. `"season | location"`.
#' @param component `"cond"`, `"response"` or `"zi"`.
#' @param per_unit_effort If `TRUE` (default), estimates are per one unit of
#'   effort (e.g. per trap-night) by setting the offset to 0. Otherwise they
#'   are at the average offset in the data.
#' @param reverse If `TRUE`, contrasts are later level vs earlier level
#'   (e.g. Wet / Dry when Dry is the reference).
#' @param adjust Multiple-comparison adjustment (e.g. `"tukey"`, `"holm"`, `"none"`).
#' @param adjust_across_by Apply `adjust` across all `by` groups together.
#' @return A list with `means` and `contrasts` tibbles.
#' @export
emm_effects <- function(fit, specs, component = "cond", per_unit_effort = TRUE,
                        reverse = TRUE, adjust = "tukey", adjust_across_by = FALSE) {
  if (!model_ok(fit)) stop("Model '", fit$name, "' failed convergence checks.")
  f <- stats::as.formula(paste("~", specs))
  args <- list(object = fit$model, specs = f, component = component)
  if (component != "zi") args$type <- "response"
  if (per_unit_effort && component != "zi") args$offset <- 0
  emm <- suppressMessages(do.call(emmeans::emmeans, args))
  means <- summary(emm, type = "response", infer = c(TRUE, FALSE))
  pr <- graphics::pairs(emm, reverse = reverse, adjust = adjust)
  con <- if (adjust_across_by) {
    summary(pr, type = "response", infer = c(TRUE, TRUE), by = NULL, adjust = adjust)
  } else summary(pr, type = "response", infer = c(TRUE, TRUE))
  scale <- switch(component, cond = "rate ratio", response = "difference in expected count",
                  zi = "odds ratio (extra zeros)")
  unit <- if (per_unit_effort && component != "zi") "per unit effort" else "at observed effort"
  list(
    means = tibble::add_column(tibble::as_tibble(as.data.frame(means)),
                               component = component, effort_scale = unit, .before = 1),
    contrasts = tibble::add_column(tibble::as_tibble(as.data.frame(con)),
                                   component = component, contrast_scale = scale,
                                   effort_scale = unit, .before = 1)
  )
}
