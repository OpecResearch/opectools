#' Read an analysis configuration file
#'
#' Relative paths in the file are resolved against the file's folder.
#'
#' @param path Path to a YAML file.
#' @return A list.
#' @export
read_config <- function(path) {
  cfg <- yaml::read_yaml(path)
  base <- dirname(normalizePath(path))
  resolve <- function(p) {
    if (is.null(p) || !nzchar(p)) return(p)
    if (grepl("^(/|[A-Za-z]:|~)", p)) p else file.path(base, p)
  }
  for (k in c("catch", "covariates", "import_script", "workbook")) cfg$data[[k]] <- resolve(cfg$data[[k]])
  cfg$output_dir <- resolve(if (is.null(cfg$output_dir)) "results" else cfg$output_dir)
  cfg$.config_path <- normalizePath(path)
  cfg
}

read_table_file <- function(path, sheet = NULL) {
  if (grepl("\\.xlsx?$", path, ignore.case = TRUE)) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("Install 'readxl' to read Excel files.")
    return(tibble::as_tibble(readxl::read_excel(path, sheet = if (is.null(sheet)) 1 else sheet)))
  }
  tibble::as_tibble(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE))
}

ensure_dates <- function(dat, study_start) {
  if (!"date" %in% names(dat) && "month" %in% names(dat)) {
    if (is.null(study_start)) stop("Data have 'month' but no 'date'; set time: study_start in the config.")
    dat$date <- months_to_dates(dat$month, study_start)
    dat$month <- NULL
  }
  dat$date <- as.Date(dat$date)
  dat
}

#' Load and prepare data as specified in a configuration
#'
#' Either reads `data$catch` (already in the standard format, optionally
#' renamed through `data$column_map`) or sources `data$import_script`, which
#' must define `import_data(config)` returning a list with `catch` and,
#' optionally, `covariates`. Adds time variables and joins covariates.
#'
#' @param config A config list from [read_config()].
#' @return A list with `catch` and `covariates`.
#' @export
prepare_data <- function(config) {
  dcfg <- config$data
  if (!is.null(dcfg$import_script)) {
    env <- new.env(parent = globalenv())
    sys.source(dcfg$import_script, envir = env)
    if (!exists("import_data", envir = env, inherits = FALSE)) {
      stop("import_script must define a function import_data(config).")
    }
    imp <- env$import_data(config)
    catch <- imp$catch; covs <- imp$covariates
  } else {
    catch <- read_table_file(dcfg$catch, dcfg$catch_sheet)
    covs <- if (!is.null(dcfg$covariates)) read_table_file(dcfg$covariates, dcfg$covariates_sheet)
  }
  study_start <- config$time$study_start
  catch <- ensure_dates(catch, study_start)
  catch <- standardise_catch(catch, map = unlist(dcfg$column_map), constants = dcfg$constants)
  seasons <- config$time$seasons
  origin <- min(as.Date(format(catch$date, "%Y-%m-01")))
  catch <- add_time_vars(catch, seasons = seasons,
                         reference_season = config$time$reference_season, origin = origin)
  if (!is.null(covs)) {
    covs <- ensure_dates(covs, study_start)
    covs <- add_time_vars(covs, seasons = seasons,
                          reference_season = config$time$reference_season, origin = origin)
    keys <- if (is.null(dcfg$covariate_join)) "period" else unlist(dcfg$covariate_join)
    new_cols <- setdiff(names(covs), names(catch))
    covs_join <- covs[c(keys, new_cols)]
    if (anyDuplicated(covs_join[keys])) stop("Covariate file has duplicate rows for join key(s): ",
                                             paste(keys, collapse = ", "))
    catch <- dplyr::left_join(catch, covs_join, by = keys)
  }
  list(catch = catch, covariates = covs)
}

time_cols <- c("month", "month_label", "year", "period", "time_index", "time", "season")

#' Prepare unit-level data for one analysis
#' @param catch Prepared species-level data.
#' @param acfg One analysis block from the config.
#' @return Unit-level tibble.
#' @export
prepare_units <- function(catch, acfg) {
  d <- catch
  if (!is.null(acfg$method)) d <- d[d$method %in% unlist(acfg$method), , drop = FALSE]
  if (!is.null(acfg$species)) d <- d[d$species %in% unlist(acfg$species), , drop = FALSE]
  if (!nrow(d)) stop("No rows left after filtering by method/species.")
  all_na <- vapply(d, function(x) all(is.na(x)), logical(1))
  d <- d[, !all_na, drop = FALSE]
  if ("method" %in% names(d) && length(unique(d$method)) > 1) {
    stop("Analysis includes several methods (", paste(unique(d$method), collapse = ", "),
         "). Collection methods are never pooled; set 'method:' for this analysis.")
  }
  unit <- unlist(acfg$sampling_unit)
  if (is.null(unit)) unit <- intersect(c("site", "unit_id", "period", "location", "method"), names(d))
  sum_vars <- unique(c("count", unlist(acfg$sum_vars)))
  drop <- unique(c("species", "date", unlist(acfg$drop_columns)))
  constant <- setdiff(names(d), c(unit, sum_vars, drop))
  u <- aggregate_units(d, unit, sum_vars = sum_vars, constant_vars = constant)
  for (v in intersect(c("site", "location", "unit_id", "method"), names(u))) u[[v]] <- factor(u[[v]])
  if ("location" %in% names(u) && !is.null(acfg$reference_location)) {
    u$location <- stats::relevel(u$location, ref = acfg$reference_location)
  }
  if ("site" %in% names(u) && !is.null(acfg$reference_site)) {
    u$site <- stats::relevel(u$site, ref = acfg$reference_site)
  }
  # Series for ar1(): one time series per site (x location) (x unit_id).
  sk <- intersect(c("site", "unit_id", "location"), names(u))
  u$series <- interaction(u[sk], drop = TRUE)
  if (length(acfg$standardise)) {
    u <- standardise_vars(u, intersect(unlist(acfg$standardise), names(u)))
  }
  tibble::as_tibble(u)
}

period_level <- function(units) {
  num <- names(units)[vapply(units, is.numeric, logical(1))]
  num <- setdiff(num, c("count", "effort", "n_rows", "time_index", "month", "year"))
  num <- num[!grepl("_z$", num)]
  g <- dplyr::group_by(units, .data$period, .data$time_index)
  out <- dplyr::summarise(g, count = sum(.data$count), effort = sum(.data$effort),
                          density = .data$count / .data$effort,
                          dplyr::across(dplyr::all_of(num), ~ mean(.x, na.rm = TRUE)),
                          .groups = "drop")
  if ("season" %in% names(units)) {
    s <- dplyr::distinct(units[c("period", "season")])
    out <- dplyr::left_join(out, s, by = "period")
  }
  out
}

#' Run one configured analysis
#'
#' @param catch Prepared species-level data.
#' @param acfg Analysis block from the config.
#' @param name Analysis name.
#' @param out_dir Output folder for this analysis (or `NULL` for none).
#' @return A list of results.
#' @export
run_one_analysis <- function(catch, acfg, name, out_dir = NULL) {
  message("== Analysis: ", name)
  units <- prepare_units(catch, acfg)
  sp <- catch
  if (!is.null(acfg$method)) sp <- sp[sp$method %in% unlist(acfg$method), ]
  if (!is.null(acfg$species)) sp <- sp[sp$species %in% unlist(acfg$species), ]
  if (!is.null(acfg$validation)) {
    v <- acfg$validation
    sp_v <- sp[, !vapply(sp, function(x) all(is.na(x)), logical(1)), drop = FALSE]
    qa_a <- validate_catch(sp_v, unit = unlist(acfg$sampling_unit),
                           expected_grid = unlist(v$expected_grid),
                           expected_units = v$expected_units, sum_checks = v$sum_checks,
                           stop_on_fail = FALSE)
  } else qa_a <- NULL
  res <- list(name = name, config = acfg, units = units, qa = qa_a,
              scaling = attr(units, "scaling"),
              composition = species_composition(sp),
              diversity = diversity_table(sp))
  by_vars <- intersect(c("site", "location", "season"), names(units))
  res$summary_period <- summarise_catch(units, intersect(c("period", "location"), names(units)))
  res$summary_groups <- summarise_catch(units, by_vars)
  res$zeros <- summarise_catch(units, intersect(c("site", "location"), names(units)))

  offset <- if (isFALSE(acfg$offset)) NULL else if (is.null(acfg$offset)) "effort" else acfg$offset
  fits <- fit_candidates(acfg$candidates, units, offset = offset,
                         exclusive = acfg$exclusive_terms)
  res$fits <- fits
  res$comparison <- compare_models(fits)
  res$warnings <- dplyr::bind_rows(lapply(fits, function(f)
    if (length(f$warnings)) tibble::tibble(model = f$name, warning = f$warnings)))

  dg <- if (is.null(acfg$diagnostics)) list() else acfg$diagnostics
  plot_dir <- if (!is.null(out_dir)) file.path(out_dir, "diagnostics")
  diag <- lapply(fits, function(f) run_diagnostics(
    f, n_sim = if (is.null(dg$n_sim)) 1000 else dg$n_sim,
    seed = if (is.null(dg$seed)) 12345 else dg$seed,
    plot_vs = unlist(dg$plot_vs), out_dir = plot_dir)$tests)
  res$diagnostics <- dplyr::bind_rows(diag)
  res$coefficients <- dplyr::bind_rows(lapply(fits, function(f) {
    if (!model_ok(f)) return(NULL)
    dplyr::bind_rows(tidy_model(f, "cond"), tidy_model(f, "zi")) |>
      tibble::add_column(model = f$name, .before = 1)
  }))
  res$lrt <- dplyr::bind_rows(lapply(acfg$lrt, function(p) {
    p <- unlist(p); lr_test(fits[[p[1]]], fits[[p[2]]])
  }))

  primary <- acfg$primary
  res$primary <- primary
  if (is.null(primary)) {
    message("   No primary model designated: review model comparison and diagnostics, ",
            "then set 'primary:' in the config.")
  } else {
    if (!primary %in% names(fits)) stop("primary '", primary, "' is not a candidate name")
    pf <- fits[[primary]]
    if (!model_ok(pf)) {
      warning("Primary model '", primary, "' failed convergence checks; effects not computed.")
    } else {
      res$effects <- lapply(acfg$effects, function(e) {
        out <- tryCatch(emm_effects(
          pf, e$specs, component = if (is.null(e$component)) "cond" else e$component,
          per_unit_effort = if (is.null(e$per_unit_effort)) TRUE else e$per_unit_effort,
          reverse = if (is.null(e$reverse)) TRUE else e$reverse,
          adjust = if (is.null(e$adjust)) "tukey" else e$adjust,
          adjust_across_by = isTRUE(e$adjust_across_by)),
          error = function(err) { warning("effects '", e$specs, "': ", conditionMessage(err)); NULL })
        if (!is.null(out)) {
          out$means <- tibble::add_column(out$means, specs = e$specs, .before = 1)
          out$contrasts <- tibble::add_column(out$contrasts, specs = e$specs, .before = 1)
        }
        out
      })
      res$means <- dplyr::bind_rows(lapply(res$effects, `[[`, "means"))
      res$contrasts <- dplyr::bind_rows(lapply(res$effects, `[[`, "contrasts"))
      res$rate_ratios <- dplyr::bind_rows(lapply(acfg$rate_ratios, function(r)
        scaled_rate_ratio(pf, r$term, if (is.null(r$multiplier)) 1 else r$multiplier,
                          if (is.null(r$label)) paste("per", r$multiplier) else r$label)))
      res$vif <- vif_table(pf)
    }
  }

  pl <- period_level(units)
  res$period_data <- pl
  if (!is.null(acfg$correlations)) {
    cc <- acfg$correlations
    res$correlations <- correlation_table(pl, if (is.null(cc$response)) "count" else cc$response,
                                          unlist(cc$predictors))
  }
  if (!is.null(acfg$lag_regression)) {
    lr <- acfg$lag_regression
    preds <- unlist(lr$predictors)
    ml <- if (is.null(lr$max_lag)) 2 else lr$max_lag
    lagged <- add_lags(pl, preds, max_lag = ml, log1p = isTRUE(lr$log1p))
    pn <- unlist(lapply(preds, function(p) c(paste0(p, "_lag", 0:ml),
                                             if (isTRUE(lr$log1p)) paste0("log1p_", p, "_lag", 0:ml))))
    res$lag_regressions <- lag_regressions(lagged, lr$outcome, pn,
                                           hac_lag = if (is.null(lr$hac_lag)) 1 else lr$hac_lag)
  }
  res$trend_plot <- plot_trend(
    res$summary_period, "density_per_effort",
    group = if ("location" %in% names(res$summary_period)) "location" else NULL,
    ylab = "Mosquitoes per unit effort", title = paste(name, ": catch per unit effort"))
  res
}

#' Run every analysis in a configuration file and save the outputs
#'
#' Outputs (per analysis folder): CSV tables, diagnostic plots, a trend
#' figure, plus `results.xlsx` (if writexl is installed), `results.rds`,
#' `qa_checks.csv` and `sessionInfo.txt` in the output folder.
#'
#' @param config Path to a YAML config, or a config list.
#' @param write Write outputs to disk (default `TRUE`).
#' @return Invisibly, a list with `config`, `qa`, `data` and `analyses`.
#' @export
#' @examples
#' \dontrun{
#' new_analysis("my_study")
#' res <- run_analysis("my_study/config.yml")
#' }
run_analysis <- function(config, write = TRUE) {
  if (is.character(config)) config <- read_config(config)
  dat <- prepare_data(config)
  v <- if (is.null(config$validation)) list() else config$validation
  qa <- validate_catch(dat$catch, unit = c("site", "unit_id", "period", "location", "method"),
                       expected_grid = unlist(v$expected_grid),
                       expected_units = v$expected_units, sum_checks = v$sum_checks,
                       stop_on_fail = !isFALSE(v$stop_on_fail))
  out <- config$output_dir
  if (write) dir.create(out, showWarnings = FALSE, recursive = TRUE)
  analyses <- lapply(names(config$analyses), function(nm) run_one_analysis(
    dat$catch, config$analyses[[nm]], nm, out_dir = if (write) file.path(out, nm)))
  names(analyses) <- names(config$analyses)
  res <- list(config = config, qa = qa, data = dat, analyses = analyses,
              session = utils::sessionInfo())
  if (write) write_results(res, out)
  message("Done. Outputs in: ", out)
  invisible(res)
}

result_tables <- c("qa", "composition", "diversity", "summary_period", "summary_groups", "zeros",
                   "comparison", "diagnostics", "coefficients", "lrt", "means", "contrasts",
                   "rate_ratios", "vif", "scaling", "correlations", "lag_regressions",
                   "warnings", "units")

#' Write results to CSV, Excel and RDS
#' @param res Result of [run_analysis()].
#' @param out Output folder.
#' @return Invisibly `out`.
#' @export
write_results <- function(res, out) {
  utils::write.csv(res$qa, file.path(out, "qa_checks.csv"), row.names = FALSE)
  sheets <- list(README = tibble::tibble(
    note = c(paste("Generated by opectools", utils::packageVersion("opectools"), "on", Sys.time()),
             paste("Config:", res$config$.config_path),
             "Check qa_checks and model diagnostics before reporting.",
             "Collection methods are analysed separately and never pooled.")),
    qa_checks = res$qa)
  for (nm in names(res$analyses)) {
    a <- res$analyses[[nm]]
    d <- file.path(out, nm)
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
    for (t in result_tables) {
      x <- a[[t]]
      if (is.null(x) || !nrow(x)) next
      x <- as.data.frame(x)
      utils::write.csv(x, file.path(d, paste0(t, ".csv")), row.names = FALSE)
      if (t != "units") sheets[[substr(paste0(nm, "_", t), 1, 31)]] <- x
    }
    ggplot2::ggsave(file.path(d, "trend.png"), a$trend_plot, width = 8, height = 5, dpi = 300)
    for (f in a$fits) if (!is.null(f$model)) {
      utils::capture.output(summary(f$model), file = file.path(d, paste0("model_", f$name, ".txt")))
    }
  }
  if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(lapply(sheets, as.data.frame), file.path(out, "results.xlsx"))
  }
  slim <- res
  slim$analyses <- lapply(res$analyses, function(a) { a$trend_plot <- NULL; a })
  saveRDS(slim, file.path(out, "results.rds"))
  writeLines(utils::capture.output(res$session), file.path(out, "sessionInfo.txt"))
  invisible(out)
}

#' Start a new analysis project from the template
#'
#' Copies the configuration template, report template, run script and
#' (optionally) simulated example data into a new folder.
#'
#' @param path New project folder.
#' @param template Template name (currently `"count-analysis"`).
#' @param example_data Copy simulated example data (default `TRUE`).
#' @return Invisibly `path`.
#' @export
new_analysis <- function(path, template = "count-analysis", example_data = TRUE) {
  src <- system.file("templates", template, package = "opectools")
  if (!nzchar(src)) stop("Template not found: ", template)
  if (dir.exists(path) && length(list.files(path))) stop("Folder is not empty: ", path)
  dir.create(file.path(path, "data"), recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(src, full.names = TRUE), path, recursive = TRUE)
  if (example_data) {
    ex <- list.files(system.file("extdata", package = "opectools"), full.names = TRUE)
    file.copy(ex, file.path(path, "data"))
  }
  message("Created ", path, ". Edit config.yml, then run run_analysis.R or render report.qmd.")
  invisible(path)
}
