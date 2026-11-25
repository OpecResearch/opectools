# ---- Helpers -----------------------------------------------------------------

#' Publication theme used by opectools figures
#' @param base_size Base font size.
#' @return A ggplot2 theme.
#' @export
opec_theme <- function(base_size = 11) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "#F1F4F7", colour = NA),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.subtitle = ggplot2::element_text(size = base_size - 1),
      legend.position = "bottom"
    )
}

#' Italicise scientific names for plot labels
#'
#' Genus and species epithet are set in italics; qualifiers such as "s.l.",
#' "s.s.", "group" or "complex" stay upright, e.g. *An. gambiae* s.l.
#' Labels that do not look like a binomial are returned unchanged.
#'
#' @param x Character vector of species labels.
#' @return A list of plotmath expressions, usable as ggplot2 `labels`.
#' @export
#' @examples
#' species_labels(c("An. gambiae s.l.", "An. funestus group", "Unidentified"))
species_labels <- function(x) {
  x <- as.character(x)
  rx <- "^((?:[A-Z][a-z]*\\.?)\\s+[a-z][a-z-]+)(.*)$"
  lapply(x, function(s) {
    if (!grepl(rx, s, perl = TRUE)) return(s)
    ital <- sub(rx, "\\1", s, perl = TRUE)
    rest <- sub(rx, "\\2", s, perl = TRUE)
    esc <- function(t) gsub("\"", "\\\\\"", t)
    txt <- if (nzchar(trimws(rest))) {
      sprintf('italic("%s")*"%s"', esc(ital), esc(rest))
    } else sprintf('italic("%s")', esc(ital))
    parse(text = txt)[[1]]
  })
}

#' Readable month labels for study periods
#'
#' "2020-08" becomes "Aug" over "2020" (two lines) for the first period and each January, and
#' "Sep" otherwise, so multi-year studies remain readable.
#'
#' @param periods Character vector of "YYYY-MM" periods, in order.
#' @return Character vector of labels.
#' @export
period_labels <- function(periods) {
  d <- as.Date(paste0(periods, "-01"))
  m <- month.abb[as.integer(format(d, "%m"))]
  y <- format(d, "%Y")
  show_year <- seq_along(d) == 1 | format(d, "%m") == "01"
  ifelse(show_year, paste0(m, "\n", y), m)
}

#' Colour palette for a set of groups
#'
#' Uses user-supplied colours where given and a colourblind-safe palette
#' (Okabe-Ito, then HCL "Dark 3") for the rest.
#'
#' @param groups Group labels.
#' @param colours Optional named vector/list of colours.
#' @return Named character vector.
#' @export
group_palette <- function(groups, colours = NULL) {
  groups <- unique(as.character(groups))
  okabe <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9",
             "#F0E442", "#000000")
  base <- if (length(groups) <= length(okabe)) okabe[seq_along(groups)] else
    grDevices::hcl.colors(length(groups), "Dark 3")
  pal <- stats::setNames(base, groups)
  colours <- unlist(colours)
  hit <- intersect(names(colours), groups)
  pal[hit] <- colours[hit]
  pal
}

pretty_var <- function(x, labels = NULL, units = FALSE) {
  short <- c(rainfall = "Rainfall", temperature = "Temperature", rh = "Relative humidity",
             relative_humidity = "Relative humidity", malaria_rate = "Malaria incidence")
  unit <- c(rainfall = " (mm)", temperature = " (\u00B0C)", rh = " (%)", relative_humidity = " (%)")
  user <- unlist(labels)
  base <- sub("_z$", "", as.character(x))
  out <- vapply(base, function(b) {
    if (b %in% names(user)) return(user[[b]])
    lab <- if (b %in% names(short)) short[[b]] else {
      t <- gsub("_", " ", b); paste0(toupper(substr(t, 1, 1)), substring(t, 2))
    }
    if (units && b %in% names(unit)) paste0(lab, unit[[b]]) else lab
  }, character(1))
  unname(out)
}

#' Save a figure in several formats
#'
#' @param plot A ggplot or patchwork object.
#' @param stem Path without extension.
#' @param formats Any of "png", "pdf", "tiff", "svg".
#' @param width,height Size in inches.
#' @param dpi Resolution for raster formats.
#' @return Invisibly, the written file paths.
#' @export
save_figure <- function(plot, stem, formats = c("png", "pdf"), width = 10, height = 7,
                        dpi = 300) {
  dir.create(dirname(stem), showWarnings = FALSE, recursive = TRUE)
  files <- character()
  for (f in unique(formats)) {
    path <- paste0(stem, ".", f)
    args <- list(filename = path, plot = plot, width = width, height = height,
                 units = "in", bg = "white", limitsize = FALSE)
    if (f %in% c("png", "tiff")) args$dpi <- dpi
    if (f == "tiff") args$compression <- "lzw"
    if (f == "pdf" && capabilities("cairo")) args$device <- grDevices::cairo_pdf
    do.call(ggplot2::ggsave, args)
    files <- c(files, path)
  }
  invisible(files)
}

panel_tag <- function(i) LETTERS[i]

analysis_label <- function(a) if (!is.null(a$config$label)) a$config$label else a$name

effort_label <- function(a) {
  if (!is.null(a$config$effort_label)) return(a$config$effort_label)
  "unit effort"
}

method_of <- function(a) {
  m <- unlist(a$config$method)
  if (is.null(m) && "method" %in% names(a$units)) m <- as.character(unique(a$units$method))
  if (is.null(m)) "All" else paste(m, collapse = ", ")
}

# One analysis per method (the first listed) unless analyses are given.
pick_analyses <- function(res, analyses = NULL) {
  if (!is.null(analyses)) return(res$analyses[intersect(analyses, names(res$analyses))])
  m <- vapply(res$analyses, method_of, character(1))
  res$analyses[!duplicated(m)]
}

period_lookup <- function(res) {
  d <- unique(as.data.frame(res$data$catch)[c("time_index", "period")])
  d <- d[order(d$time_index), ]
  d$label <- period_labels(as.character(d$period))
  d
}

# ---- Figure 1: species composition -------------------------------------------

#' Species composition by site and collection method
#'
#' Panel A: share of the catch by species at each site (methods in separate
#' panels, never pooled), with the number caught at the end of each bar.
#' Panel B: total catch per species on a log scale, which keeps rare
#' species visible.
#'
#' @param res Result of [run_analysis()].
#' @param methods Methods to show (default: those used in analyses).
#' @param site_order Optional site order.
#' @param species_colours Optional named colours.
#' @return A patchwork object.
#' @export
plot_composition <- function(res, methods = NULL, site_order = NULL, species_colours = NULL) {
  d <- as.data.frame(res$data$catch)
  if (!"method" %in% names(d)) d$method <- "All"
  if (is.null(methods)) methods <- unique(vapply(res$analyses, method_of, character(1)))
  d <- d[d$method %in% methods, ]
  comp <- dplyr::summarise(dplyr::group_by(d, .data$method, .data$site, .data$species),
                           n = sum(.data$count, na.rm = TRUE), .groups = "drop")
  comp <- dplyr::mutate(dplyr::group_by(comp, .data$method, .data$site),
                        total = sum(.data$n), share = ifelse(.data$total > 0, .data$n / .data$total, 0))
  comp <- dplyr::ungroup(comp)
  sp_tot <- dplyr::summarise(dplyr::group_by(comp, .data$species), n = sum(.data$n), .groups = "drop")
  sp_levels <- sp_tot$species[order(-sp_tot$n)]
  comp$species <- factor(comp$species, levels = sp_levels)
  sites <- if (is.null(site_order)) sort(unique(as.character(comp$site))) else site_order
  comp$site <- factor(comp$site, levels = rev(sites))
  comp$method <- factor(comp$method, levels = methods)
  pal <- group_palette(sp_levels, species_colours)
  if (is.null(species_colours)) pal[] <- grDevices::hcl.colors(length(sp_levels), "Dark 3")
  labs_sp <- species_labels(sp_levels)

  totals <- dplyr::distinct(comp, .data$method, .data$site, .data$total)
  pa <- ggplot2::ggplot(comp, ggplot2::aes(x = .data$share, y = .data$site, fill = .data$species)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(data = totals, inherit.aes = FALSE,
                       ggplot2::aes(x = 1.01, y = .data$site,
                                    label = paste0("n = ", format(.data$total, big.mark = ","))),
                       hjust = 0, size = 3.2) +
    ggplot2::facet_wrap(~ method, ncol = 1) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                                limits = c(0, 1.18), breaks = seq(0, 1, 0.25),
                                expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::scale_fill_manual(values = pal, labels = labs_sp, name = NULL, drop = FALSE) +
    ggplot2::labs(title = "A  |  Species composition", x = "Share of catch", y = NULL) +
    opec_theme() + ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2))

  bsp <- dplyr::summarise(dplyr::group_by(comp, .data$method, .data$species),
                          n = sum(.data$n), .groups = "drop")
  bsp <- bsp[bsp$n > 0, ]
  bsp$species <- factor(bsp$species, levels = rev(sp_levels))
  pb <- ggplot2::ggplot(bsp, ggplot2::aes(x = .data$n, y = .data$species)) +
    ggplot2::geom_segment(ggplot2::aes(x = 1, xend = .data$n, yend = .data$species),
                          colour = "#AAB5BE", linewidth = 0.6) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$species), size = 3, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = format(.data$n, big.mark = ",")),
                       hjust = -0.35, size = 3) +
    ggplot2::facet_wrap(~ method, ncol = 1, scales = "free_y") +
    ggplot2::scale_x_log10(labels = scales::label_comma(),
                           expand = ggplot2::expansion(mult = c(0.02, 0.25))) +
    ggplot2::scale_y_discrete(labels = function(x) species_labels(x)) +
    ggplot2::scale_colour_manual(values = pal) +
    ggplot2::labs(title = "B  |  Catch by species", x = "Mosquitoes (log scale)", y = NULL) +
    opec_theme() + ggplot2::theme(plot.margin = ggplot2::margin(5.5, 18, 5.5, 5.5))
  patchwork::wrap_plots(pa, pb, widths = c(1.6, 1))
}

# ---- Figure 2: seasonal trajectories -----------------------------------------

#' Catch per unit effort over time, with climate
#'
#' One panel per collection method (each with its own denominator), lines by
#' location when there is more than one, otherwise by site; followed by
#' rainfall bars and other monthly climate series.
#'
#' @param res Result of [run_analysis()].
#' @param analyses Analysis names to show (default: first analysis per method).
#' @param climate_vars Climate variables for the lower panels.
#' @param site_colours,location_colours Optional named colours.
#' @param labels Optional named list of variable labels.
#' @return A patchwork object.
#' @export
plot_seasonal <- function(res, analyses = NULL, climate_vars = c("rainfall", "temperature", "rh"),
                          site_colours = NULL, location_colours = NULL, labels = NULL) {
  pl <- period_lookup(res)
  xs <- ggplot2::scale_x_continuous(breaks = pl$time_index, labels = pl$label,
                                    expand = ggplot2::expansion(add = 0.3))
  sel <- pick_analyses(res, analyses)
  panels <- list(); i <- 0
  for (a in sel) {
    u <- as.data.frame(a$units)
    grp <- if ("location" %in% names(u) && length(unique(u$location)) > 1) "location" else "site"
    s <- dplyr::summarise(dplyr::group_by(u, .data$time_index, grp = .data[[grp]]),
                          rate = sum(.data$count) / sum(.data$effort), .groups = "drop")
    pal <- if (grp == "location") group_palette(s$grp, location_colours) else
      group_palette(sort(unique(as.character(s$grp))), site_colours)
    i <- i + 1
    panels[[i]] <- ggplot2::ggplot(s, ggplot2::aes(.data$time_index, .data$rate,
                                                   colour = .data$grp, group = .data$grp)) +
      ggplot2::geom_line(linewidth = 0.85) + ggplot2::geom_point(size = 1.8) +
      ggplot2::scale_colour_manual(values = pal, name = NULL) + xs +
      ggplot2::labs(title = paste0(panel_tag(i), "  |  ", method_of(a), ": mosquitoes per ",
                                   effort_label(a)),
                    x = NULL, y = paste("Mosquitoes /", effort_label(a))) +
      opec_theme()
  }
  # Climate: period means across all units of all analyses
  allu <- dplyr::bind_rows(lapply(res$analyses, function(a) as.data.frame(a$units)))
  cv <- intersect(climate_vars, names(allu))
  if (length(cv)) {
    clim <- dplyr::summarise(dplyr::group_by(allu, .data$time_index),
                             dplyr::across(dplyr::all_of(cv), ~ mean(.x, na.rm = TRUE)),
                             .groups = "drop")
    if ("rainfall" %in% cv) {
      i <- i + 1
      panels[[i]] <- ggplot2::ggplot(clim, ggplot2::aes(.data$time_index, .data$rainfall)) +
        ggplot2::geom_col(fill = "#80B1D3", width = 0.7) + xs +
        ggplot2::labs(title = paste0(panel_tag(i), "  |  Rainfall"), x = NULL,
                      y = pretty_var("rainfall", labels, units = TRUE)) + opec_theme()
    }
    other <- setdiff(cv, "rainfall")
    if (length(other)) {
      cl <- tidyr_longer(clim, other)
      cl$variable <- factor(pretty_var(cl$variable, labels, units = TRUE),
                            levels = pretty_var(other, labels, units = TRUE))
      i <- i + 1
      panels[[i]] <- ggplot2::ggplot(cl, ggplot2::aes(.data$time_index, .data$value)) +
        ggplot2::geom_line(colour = "#4D4D4D", linewidth = 0.8) +
        ggplot2::geom_point(colour = "#4D4D4D", size = 1.5) +
        ggplot2::facet_wrap(~ variable, ncol = 1, scales = "free_y") + xs +
        ggplot2::labs(title = paste0(panel_tag(i), "  |  Temperature and humidity (mean across traps)"),
                      x = "Month", y = NULL) +
        opec_theme()
    }
  }
  heights <- c(rep(1.3, length(sel)), if ("rainfall" %in% cv) 0.75,
               if (length(setdiff(cv, "rainfall"))) 0.65 * length(setdiff(cv, "rainfall")))
  patchwork::wrap_plots(panels, ncol = 1, heights = heights)
}

tidyr_longer <- function(dat, vars) {
  dplyr::bind_rows(lapply(vars, function(v)
    data.frame(time_index = dat$time_index, variable = v, value = dat[[v]])))
}

# ---- Figure 3: adjusted effects as rate ratios ---------------------------------

#' Collect rate-ratio contrasts and climate rate ratios from primary models
#'
#' Uses contrasts computed with `component: cond` (rate ratios) and the
#' `rate_ratios:` of each analysis. Effects with `figure: false` in the
#' config are skipped.
#'
#' @param res Result of [run_analysis()].
#' @param labels Optional named list of variable labels.
#' @return Tibble with analysis, group, label, estimate, conf_low, conf_high, p_value.
#' @export
effects_data <- function(res, labels = NULL) {
  out <- list()
  for (a in res$analyses) {
    lab <- paste0(analysis_label(a), " (", method_of(a), ")")
    con <- a$contrasts
    if (!is.null(con) && nrow(con)) {
      con <- as.data.frame(con[con$component == "cond", , drop = FALSE])
      skip <- unlist(lapply(a$config$effects, function(e)
        if (isFALSE(e$figure)) e$specs))
      con <- con[!con$specs %in% skip, , drop = FALSE]
      if (nrow(con)) {
        lo <- intersect(c("asymp.LCL", "lower.CL"), names(con))[1]
        hi <- intersect(c("asymp.UCL", "upper.CL"), names(con))[1]
        byv <- lapply(con$specs, function(s) {
          b <- if (grepl("\\|", s)) trimws(sub("^[^|]*\\|", "", s)) else ""
          trimws(unlist(strsplit(b, "[*+,]")))
        })
        bylab <- vapply(seq_len(nrow(con)), function(r) {
          v <- byv[[r]]; v <- v[nzchar(v) & v %in% names(con)]
          if (!length(v)) "" else paste(vapply(v, function(k) as.character(con[r, k]), ""), collapse = ", ")
        }, character(1))
        out[[length(out) + 1]] <- tibble::tibble(
          analysis = lab, group = "Contrasts",
          label = ifelse(nzchar(bylab), paste0(con$contrast, "  (", bylab, ")"),
                         as.character(con$contrast)),
          estimate = con$ratio, conf_low = con[[lo]], conf_high = con[[hi]],
          p_value = if ("p.value" %in% names(con)) con$p.value else NA_real_)
      }
    }
    rr <- a$rate_ratios
    if (!is.null(rr) && nrow(rr)) {
      out[[length(out) + 1]] <- tibble::tibble(
        analysis = lab, group = "Climate",
        label = paste0(pretty_var(rr$term, labels), "  (", rr$reporting_unit, ")"),
        estimate = rr$rate_ratio, conf_low = rr$conf_low, conf_high = rr$conf_high,
        p_value = rr$p_value)
    }
  }
  dplyr::bind_rows(out)
}

#' Forest plot of adjusted rate ratios
#'
#' All effects on one log scale (null = 1), grouped by analysis, so effects
#' from different collection methods can be compared directly. Season and
#' location contrasts are from the count component of each primary model;
#' climate effects are per reporting unit (e.g. per 1 SD).
#'
#' @param res Result of [run_analysis()].
#' @param labels Optional named list of variable labels.
#' @return A ggplot object (or `NULL` if there are no effects).
#' @export
plot_effects <- function(res, labels = NULL) {
  d <- effects_data(res, labels)
  if (!nrow(d)) return(NULL)
  d$analysis <- factor(d$analysis, levels = unique(d$analysis))
  d$label <- factor(paste(d$analysis, d$label, sep = "\r"),
                    levels = rev(unique(paste(d$analysis, d$label, sep = "\r"))))
  d$sig <- ifelse(!is.na(d$p_value) & d$p_value < 0.05, "p < 0.05", "not significant")
  d$text <- sprintf("%.2f (%.2f, %.2f)", d$estimate, d$conf_low, d$conf_high)
  rng <- range(c(d$conf_low, d$conf_high), na.rm = TRUE)
  xlim <- c(min(rng[1], 1) / 1.15, max(rng[2], 1) * 1.15)
  ggplot2::ggplot(d, ggplot2::aes(.data$estimate, .data$label)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "#6B7280") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$conf_low, xmax = .data$conf_high),
                            height = 0.2, linewidth = 0.7) +
    ggplot2::geom_point(ggplot2::aes(fill = .data$sig), shape = 21, size = 3) +
    ggplot2::geom_text(ggplot2::aes(x = xlim[2] * 1.3, label = .data$text), hjust = 0, size = 3.1) +
    ggplot2::facet_grid(analysis ~ ., scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_discrete(labels = function(x) sub("^.*\r", "", x)) +
    ggplot2::scale_fill_manual(values = c("p < 0.05" = "#D55E00", "not significant" = "white"),
                               name = NULL) +
    ggplot2::coord_cartesian(xlim = xlim, clip = "off") +
    ggplot2::labs(title = "Adjusted rate ratios (95% CI)",
                  subtitle = "Count component of each primary model, per unit effort; log scale, null = 1",
                  x = "Rate ratio (log scale)", y = NULL) +
    opec_theme() +
    ggplot2::theme(strip.placement = "outside",
                   strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1, face = "bold"),
                   panel.grid.major.y = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(5.5, 120, 5.5, 5.5))
}

# ---- Supplementary figures ---------------------------------------------------

#' DHARMa residual diagnostics for each primary model
#'
#' For each analysis: a uniform QQ plot of scaled residuals and residuals
#' against (rank-transformed) predictions, with the test p-values from the
#' analysis in the subtitle.
#'
#' @param res Result of [run_analysis()].
#' @param n_sim Number of simulations (default: as in each analysis config).
#' @return A patchwork object (or `NULL`).
#' @export
plot_diagnostics <- function(res, n_sim = NULL) {
  rows <- list()
  for (a in res$analyses) {
    if (is.null(a$primary) || !model_ok(a$fits[[a$primary]])) next
    dg <- if (is.null(a$config$diagnostics)) list() else a$config$diagnostics
    set.seed(if (is.null(dg$seed)) 12345 else dg$seed)
    ns <- if (!is.null(n_sim)) n_sim else if (is.null(dg$n_sim)) 1000 else dg$n_sim
    sim <- DHARMa::simulateResiduals(a$fits[[a$primary]]$model, n = ns, plot = FALSE)
    r <- sim$scaledResiduals; n <- length(r)
    tests <- a$diagnostics[a$diagnostics$model == a$primary, , drop = FALSE]
    pv <- function(t) { x <- tests$p_value[tests$test == t]; if (length(x)) sprintf("%.3f", x) else "NA" }
    sub <- sprintf("KS p = %s;  dispersion p = %s;  zero-inflation p = %s",
                   pv("Uniformity (KS)"), pv("Dispersion"), pv("Zero inflation"))
    qq <- ggplot2::ggplot(data.frame(e = (seq_len(n) - 0.5) / n, o = sort(r)),
                          ggplot2::aes(.data$e, .data$o)) +
      ggplot2::geom_abline(colour = "#D55E00") + ggplot2::geom_point(size = 1, alpha = 0.7) +
      ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
      ggplot2::labs(title = paste0(analysis_label(a), ": ", a$primary), subtitle = sub,
                    x = "Expected", y = "Observed scaled residual") + opec_theme(10)
    rp <- ggplot2::ggplot(data.frame(p = rank(sim$fittedPredictedResponse, ties.method = "average") / n,
                                     r = r), ggplot2::aes(.data$p, .data$r)) +
      ggplot2::geom_hline(yintercept = c(0.25, 0.5, 0.75), linetype = "dashed", colour = "#9CA3AF") +
      ggplot2::geom_point(size = 1, alpha = 0.7) +
      ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "#D55E00",
                           linewidth = 0.8) +
      ggplot2::coord_cartesian(ylim = c(0, 1)) +
      ggplot2::labs(title = " ", subtitle = "Residuals vs predicted",
                    x = "Model prediction (rank transformed)", y = "Scaled residual") +
      opec_theme(10)
    rows[[length(rows) + 1]] <- patchwork::wrap_plots(qq, rp, widths = c(1, 1.4))
  }
  if (!length(rows)) return(NULL)
  patchwork::wrap_plots(rows, ncol = 1)
}

#' Model comparison plot (delta AIC)
#'
#' @param res Result of [run_analysis()].
#' @return A ggplot object.
#' @export
plot_model_comparison <- function(res) {
  d <- dplyr::bind_rows(lapply(res$analyses, function(a) {
    cmp <- as.data.frame(a$comparison)
    tibble::tibble(analysis = paste0(analysis_label(a), " (", method_of(a), ")"), model = cmp$model, delta_AIC = cmp$delta_AIC,
                   status = ifelse(is.na(cmp$AIC), "failed checks",
                                   ifelse(!is.null(a$primary) & cmp$model %in% a$primary,
                                          "primary", "candidate")))
  }))
  failed <- d[d$status == "failed checks", ]
  d <- d[d$status != "failed checks", ]
  d$key <- factor(paste(d$analysis, d$model, sep = "\r"),
                  levels = rev(unique(paste(d$analysis, d$model, sep = "\r")[order(d$analysis, d$delta_AIC)])))
  ggplot2::ggplot(d, ggplot2::aes(.data$delta_AIC, .data$key)) +
    ggplot2::geom_vline(xintercept = 2, linetype = "dotted", colour = "#9CA3AF") +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$delta_AIC, yend = .data$key),
                          colour = "#AAB5BE") +
    ggplot2::geom_point(ggplot2::aes(fill = .data$status), shape = 21, size = 3) +
    ggplot2::facet_grid(analysis ~ ., scales = "free_y", space = "free_y") +
    ggplot2::scale_y_discrete(labels = function(x) sub("^.*\r", "", x)) +
    ggplot2::scale_fill_manual(values = c(primary = "#D55E00", candidate = "white"), name = NULL) +
    ggplot2::labs(title = "Candidate model comparison",
                  subtitle = "AIC difference from the best model in each analysis (dotted line: 2)",
                  x = expression(Delta * "AIC"), y = NULL,
                  caption = if (nrow(failed)) paste("Failed convergence/Hessian checks:",
                                                    paste(failed$analysis, failed$model, sep = ": ",
                                                          collapse = "; "))) +
    opec_theme() + ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, hjust = 0))
}

#' Abdominal (gonotrophic) status composition
#'
#' For analyses that sum stage columns (`sum_vars:` such as unfed,
#' blood_fed, half_gravid, gravid): proportion of each stage by site and season.
#'
#' @param res Result of [run_analysis()].
#' @param stages Stage columns, in order.
#' @return A ggplot object (or `NULL` if no stage data).
#' @export
plot_stages <- function(res, stages = c("unfed", "blood_fed", "half_gravid", "gravid")) {
  d <- dplyr::bind_rows(lapply(res$analyses, function(a) {
    st <- intersect(stages, intersect(unlist(a$config$sum_vars), names(a$units)))
    if (!length(st)) return(NULL)
    u <- as.data.frame(a$units)
    grp <- intersect(c("site", "season"), names(u))
    dplyr::bind_rows(lapply(st, function(s) {
      x <- dplyr::summarise(dplyr::group_by(u, dplyr::across(dplyr::all_of(grp))),
                            n = sum(.data[[s]], na.rm = TRUE), .groups = "drop")
      x$stage <- s; x$analysis <- paste0(analysis_label(a), " (", method_of(a), ")"); x
    }))
  }))
  if (!nrow(d)) return(NULL)
  d <- dplyr::mutate(dplyr::group_by(d, dplyr::across(dplyr::any_of(c("analysis", "site", "season")))),
                     prop = .data$n / sum(.data$n), total = sum(.data$n))
  d <- dplyr::ungroup(d)
  lv <- intersect(stages, unique(d$stage))
  d$stage <- factor(d$stage, levels = rev(lv),
                    labels = rev(pretty_var(lv)))
  x <- if ("season" %in% names(d)) "season" else "site"
  stage_cols <- c(unfed = "#D1D5DB", blood_fed = "#B91C1C", half_gravid = "#F59E0B",
                  gravid = "#7C3AED")
  cols <- stats::setNames(ifelse(lv %in% names(stage_cols), stage_cols[lv],
                                 grDevices::hcl.colors(length(lv), "Dark 3")), pretty_var(lv))
  tot <- dplyr::distinct(d, dplyr::across(dplyr::any_of(c("analysis", "site", "season"))), .data$total)
  p <- ggplot2::ggplot(d, ggplot2::aes(.data[[x]], .data$prop, fill = .data$stage)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(data = tot, inherit.aes = FALSE,
                       ggplot2::aes(x = .data[[x]], y = 1.04, label = paste0("n = ", .data$total)),
                       size = 3) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                breaks = seq(0, 1, 0.25)) +
    ggplot2::scale_fill_manual(values = cols, breaks = pretty_var(lv), name = NULL) +
    ggplot2::labs(title = "Abdominal status of resting mosquitoes",
                  x = NULL, y = "Share of mosquitoes") + opec_theme()
  if (x == "season" && "site" %in% names(d)) {
    p <- p + if (length(unique(d$analysis)) > 1) ggplot2::facet_grid(analysis ~ site) else
      ggplot2::facet_wrap(~ site, nrow = 1)
  }
  p
}

# ---- Write all figures -------------------------------------------------------

figure_specs <- list(
  composition = list(stem = "Figure_1_species_composition", fun = "plot_composition",
                     width = 12, height = 7),
  seasonal = list(stem = "Figure_2_seasonal_trajectories", fun = "plot_seasonal",
                  width = 11, height = NA),
  effects = list(stem = "Figure_3_adjusted_rate_ratios", fun = "plot_effects",
                 width = 11, height = NA),
  diagnostics = list(stem = "Figure_S1_model_diagnostics", fun = "plot_diagnostics",
                     width = 10, height = NA),
  model_comparison = list(stem = "Figure_S2_model_comparison", fun = "plot_model_comparison",
                          width = 9, height = NA),
  stages = list(stem = "Figure_S3_abdominal_status", fun = "plot_stages",
                width = 10, height = 5)
)

#' Create and save all figures for an analysis run
#'
#' Called by [run_analysis()] unless `figures: false` in the config.
#' Settings (all optional) in the config's `figures:` section:
#' `include`, `formats`, `dpi`, `site_order`, `site_colours`,
#' `location_colours`, `species_colours`, `climate_vars`,
#' `seasonal_analyses`, `labels`.
#'
#' @param res Result of [run_analysis()].
#' @param out_dir Folder for the figures.
#' @param fcfg The `figures:` config list.
#' @return Named character vector of PNG paths (for reports).
#' @export
write_figures <- function(res, out_dir, fcfg = NULL) {
  if (is.null(fcfg) || isTRUE(fcfg)) fcfg <- list()
  include <- if (is.null(fcfg$include)) names(figure_specs) else unlist(fcfg$include)
  formats <- unique(c("png", if (is.null(fcfg$formats)) "pdf" else unlist(fcfg$formats)))
  dpi <- if (is.null(fcfg$dpi)) 300 else fcfg$dpi
  n_rows <- function(f) {
    switch(f,
      seasonal = {
        sel <- pick_analyses(res, unlist(fcfg$seasonal_analyses))
        allv <- unique(unlist(lapply(res$analyses, function(a) names(a$units))))
        cv <- intersect(if (is.null(fcfg$climate_vars)) c("rainfall", "temperature", "rh")
                        else unlist(fcfg$climate_vars), allv)
        3.3 * length(sel) + 2 * ("rainfall" %in% cv) + 1.6 * length(setdiff(cv, "rainfall")) + 0.5
      },
      effects = 1.5 + 0.38 * max(1, nrow(effects_data(res, fcfg$labels))),
      diagnostics = 0.5 + 4.2 * sum(vapply(res$analyses, function(a) !is.null(a$primary), logical(1))),
      model_comparison = 1.8 + 0.35 * sum(vapply(res$analyses, function(a) nrow(a$comparison), integer(1))),
      NA)
  }
  files <- character()
  for (f in intersect(include, names(figure_specs))) {
    sp <- figure_specs[[f]]
    p <- tryCatch(switch(f,
      composition = plot_composition(res, site_order = unlist(fcfg$site_order),
                                     species_colours = fcfg$species_colours),
      seasonal = plot_seasonal(res, analyses = unlist(fcfg$seasonal_analyses),
                               climate_vars = if (is.null(fcfg$climate_vars))
                                 c("rainfall", "temperature", "rh") else unlist(fcfg$climate_vars),
                               site_colours = fcfg$site_colours,
                               location_colours = fcfg$location_colours, labels = fcfg$labels),
      effects = plot_effects(res, labels = fcfg$labels),
      diagnostics = plot_diagnostics(res),
      model_comparison = plot_model_comparison(res),
      stages = plot_stages(res)),
      error = function(e) { warning("Figure '", f, "' failed: ", conditionMessage(e)); NULL })
    if (is.null(p)) next
    h <- if (is.na(sp$height)) n_rows(f) else sp$height
    save_figure(p, file.path(out_dir, sp$stem), formats = formats, width = sp$width,
                height = h, dpi = dpi)
    files[f] <- file.path(out_dir, paste0(sp$stem, ".png"))
  }
  files
}
