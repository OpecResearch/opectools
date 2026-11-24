htest_row <- function(h, label) {
  g <- function(x) if (length(x)) unname(as.numeric(x[[1]])) else NA_real_
  tibble::tibble(test = label, statistic = g(h$statistic), p_value = g(h$p.value),
                 method = if (length(h$method)) paste(h$method, collapse = " ") else NA_character_)
}

#' Simulation-based residual diagnostics with DHARMa
#'
#' Runs uniformity, dispersion, zero-inflation and outlier tests and, if
#' `out_dir` is given, saves the overall DHARMa plot and residual plots
#' against selected variables.
#'
#' @param fit An `opec_fit`.
#' @param n_sim Number of simulations.
#' @param seed Random seed (diagnostics are reproducible).
#' @param plot_vs Variables to plot residuals against.
#' @param out_dir Optional directory for PNG plots.
#' @param prefix File-name prefix.
#' @return A list with `tests` (tibble) and `sim` (DHARMa object).
#' @export
run_diagnostics <- function(fit, n_sim = 1000, seed = 12345, plot_vs = NULL,
                            out_dir = NULL, prefix = fit$name) {
  if (!model_ok(fit)) {
    return(list(tests = tibble::tibble(model = fit$name, test = NA_character_,
                                       note = "model failed convergence checks"), sim = NULL))
  }
  set.seed(seed)
  sim <- DHARMa::simulateResiduals(fit$model, n = n_sim, plot = FALSE)
  tests <- dplyr::bind_rows(
    htest_row(DHARMa::testUniformity(sim, plot = FALSE), "Uniformity (KS)"),
    htest_row(DHARMa::testDispersion(sim, plot = FALSE), "Dispersion"),
    htest_row(DHARMa::testZeroInflation(sim, plot = FALSE), "Zero inflation"),
    htest_row(DHARMa::testOutliers(sim, plot = FALSE), "Outliers")
  )
  tests <- dplyr::mutate(tests, model = fit$name, n_sim = n_sim, seed = seed, .before = 1)
  if (!is.null(out_dir)) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    grDevices::png(file.path(out_dir, paste0(prefix, "_DHARMa.png")),
                   width = 2000, height = 1100, res = 200)
    try(plot(sim), silent = TRUE)
    grDevices::dev.off()
    for (v in intersect(plot_vs, names(fit$data))) {
      grDevices::png(file.path(out_dir, paste0(prefix, "_residuals_vs_", v, ".png")),
                     width = 1400, height = 1100, res = 200)
      try(DHARMa::plotResiduals(sim, form = fit$data[[v]], xlab = v), silent = TRUE)
      grDevices::dev.off()
    }
  }
  list(tests = tests, sim = sim)
}
