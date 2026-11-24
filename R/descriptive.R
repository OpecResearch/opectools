#' Diversity indices from species counts
#'
#' @param n Numeric vector of counts per species.
#' @return One-row tibble: total, richness, Shannon H, Pielou's evenness J,
#'   Simpson's 1 - D and inverse Simpson 1/D (proportion-based forms).
#' @export
#' @examples
#' diversity_indices(c(2865, 64, 38, 25, 21))
diversity_indices <- function(n) {
  n <- n[!is.na(n) & n > 0]
  N <- sum(n); S <- length(n)
  p <- n / N
  H <- -sum(p * log(p))
  D <- sum(p^2)
  tibble::tibble(
    total_n = N, richness = S, shannon_H = H,
    pielou_J = if (S > 1) H / log(S) else NA_real_,
    simpson_1_minus_D = 1 - D, inverse_simpson = 1 / D
  )
}

#' Species composition table
#'
#' @param dat Species-level data.
#' @param by Optional grouping columns (e.g. `"site"` or `"method"`).
#' @return Tibble with counts, proportions and percentages.
#' @export
species_composition <- function(dat, by = NULL) {
  by <- intersect(by, names(dat))
  out <- dplyr::summarise(dplyr::group_by(dat, dplyr::across(dplyr::all_of(c(by, "species")))),
                          n = sum(.data$count, na.rm = TRUE), .groups = "drop")
  out <- dplyr::mutate(dplyr::group_by(out, dplyr::across(dplyr::all_of(by))),
                       proportion = .data$n / sum(.data$n), percent = 100 * .data$proportion)
  dplyr::arrange(dplyr::ungroup(out), dplyr::across(dplyr::all_of(by)), dplyr::desc(.data$n))
}

#' Diversity indices by group
#' @param dat Species-level data.
#' @param by Optional grouping columns.
#' @return Tibble.
#' @export
diversity_table <- function(dat, by = NULL) {
  comp <- species_composition(dat, by)
  by <- intersect(by, names(comp))
  if (!length(by)) return(diversity_indices(comp$n))
  dplyr::reframe(dplyr::group_by(comp, dplyr::across(dplyr::all_of(by))),
                 diversity_indices(.data$n))
}

#' Summarise counts, effort and density
#'
#' @param units Unit-level data (after [aggregate_units()]).
#' @param by Grouping columns, e.g. `"period"` or `c("site", "location")`.
#' @return Tibble with units, total count, total effort, density per unit
#'   effort, mean/median count and zero proportion.
#' @export
summarise_catch <- function(units, by) {
  by <- intersect(by, names(units))
  dplyr::summarise(
    dplyr::group_by(units, dplyr::across(dplyr::all_of(by))),
    n_units = dplyr::n(),
    total_count = sum(.data$count, na.rm = TRUE),
    total_effort = sum(.data$effort, na.rm = TRUE),
    density_per_effort = .data$total_count / .data$total_effort,
    mean_count = mean(.data$count, na.rm = TRUE),
    median_count = stats::median(.data$count, na.rm = TRUE),
    zero_prop = mean(.data$count == 0, na.rm = TRUE),
    .groups = "drop"
  )
}

#' Correlations between a response and predictors
#'
#' Pearson and Spearman. Intended as descriptive support (e.g. 12 monthly
#' values), not as primary inference.
#'
#' @param dat Data frame.
#' @param response Response column.
#' @param predictors Predictor columns.
#' @return Tibble.
#' @export
correlation_table <- function(dat, response, predictors) {
  predictors <- intersect(predictors, names(dat))
  dplyr::bind_rows(lapply(predictors, function(v) {
    d <- stats::na.omit(data.frame(y = dat[[response]], x = dat[[v]]))
    if (nrow(d) < 4) {
      return(tibble::tibble(predictor = v, n = nrow(d), pearson_r = NA_real_,
                            pearson_p = NA_real_, spearman_rho = NA_real_, spearman_p = NA_real_))
    }
    p <- stats::cor.test(d$y, d$x, method = "pearson")
    s <- suppressWarnings(stats::cor.test(d$y, d$x, method = "spearman", exact = FALSE))
    tibble::tibble(predictor = v, n = nrow(d),
                   pearson_r = unname(p$estimate), pearson_p = p$p.value,
                   spearman_rho = unname(s$estimate), spearman_p = s$p.value)
  }))
}

#' Plot a time series of a summary measure
#'
#' @param dat Summary data (e.g. from [summarise_catch()] by `period`).
#' @param y Column to plot.
#' @param group Optional grouping column (separate lines).
#' @param x Time column (default `"period"`).
#' @param ylab,title Labels.
#' @return A ggplot object.
#' @export
plot_trend <- function(dat, y, group = NULL, x = "period", ylab = y, title = NULL) {
  aes_args <- list(x = rlang::sym(x), y = rlang::sym(y))
  if (!is.null(group)) {
    aes_args$group <- rlang::sym(group); aes_args$linetype <- rlang::sym(group)
    aes_args$shape <- rlang::sym(group)
  } else aes_args$group <- 1
  ggplot2::ggplot(dat, do.call(ggplot2::aes, aes_args)) +
    ggplot2::geom_line() + ggplot2::geom_point(size = 2) +
    ggplot2::labs(x = NULL, y = ylab, title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
