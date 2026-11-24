qa_row <- function(check, ok, detail = "", severity = c("fail", "warn")) {
  severity <- match.arg(severity)
  tibble::tibble(check = check,
                 status = if (isTRUE(ok)) "pass" else severity,
                 detail = as.character(detail))
}

#' Quality-control checks for catch data in the standard format
#'
#' Runs structural checks and returns a QA table instead of silently
#' continuing. Checks with status `"fail"` stop the analysis when
#' `stop_on_fail = TRUE`.
#'
#' @param dat Species-level data in the standard format (see [catch_schema()]).
#' @param unit Columns defining a sampling unit (for duplicate-species checks).
#' @param expected_grid Optional character vector of columns whose full
#'   combination is expected to be present (e.g. `c("site", "period", "location")`).
#' @param expected_units Optional named list, e.g.
#'   `list(by = c("site", "period"), units = 10)` to check 10 households per
#'   site-month. (Do not use `n:` as a YAML key: YAML reads it as `FALSE`.)
#' @param sum_checks Optional list of `list(total = "col", parts = c(...))`
#'   checks that parts add up to a total on each row.
#' @param stop_on_fail Stop with an error if any check fails.
#' @return A tibble with columns `check`, `status`, `detail`.
#' @export
validate_catch <- function(dat, unit = NULL, expected_grid = NULL,
                           expected_units = NULL, sum_checks = NULL,
                           stop_on_fail = TRUE) {
  req <- catch_schema()$column[catch_schema()$required]
  miss <- setdiff(req, names(dat))
  out <- list(qa_row("Required columns present", !length(miss),
                     if (length(miss)) paste("Missing:", paste(miss, collapse = ", ")) else ""))
  if (length(miss)) {
    res <- dplyr::bind_rows(out)
    if (stop_on_fail) stop("Required columns missing: ", paste(miss, collapse = ", "))
    return(res)
  }
  n_na_date <- sum(is.na(dat$date))
  out <- c(out, list(
    qa_row("Dates parse", n_na_date == 0, paste(n_na_date, "missing/invalid dates")),
    qa_row("Counts non-missing", !any(is.na(dat$count)),
           paste(sum(is.na(dat$count)), "missing counts"), "warn"),
    qa_row("Counts non-negative", !any(dat$count < 0, na.rm = TRUE),
           paste(sum(dat$count < 0, na.rm = TRUE), "negative counts")),
    qa_row("Counts are whole numbers",
           all(abs(dat$count - round(dat$count)) < 1e-8, na.rm = TRUE),
           paste(sum(abs(dat$count - round(dat$count)) >= 1e-8, na.rm = TRUE),
                 "non-integer counts (count models need integers)")),
    qa_row("Effort positive and non-missing",
           !any(is.na(dat$effort) | dat$effort <= 0),
           paste(sum(is.na(dat$effort) | dat$effort <= 0, na.rm = TRUE), "invalid effort values"))
  ))
  if ("method" %in% names(dat)) {
    m <- unique(dat$method)
    out <- c(out, list(qa_row(
      "Collection methods", TRUE,
      paste0(paste(m, collapse = ", "),
             if (length(m) > 1) " (analysed separately; never pooled)" else "")
    )))
  }
  if (!is.null(unit)) {
    u <- intersect(c(unit, "species"), names(dat))
    dup <- dplyr::count(dat, dplyr::across(dplyr::all_of(u)))
    nd <- sum(dup$n > 1)
    out <- c(out, list(qa_row("No duplicated species rows within a sampling unit",
                              nd == 0, paste(nd, "duplicated unit x species combinations"),
                              "warn")))
  }
  if (!is.null(expected_grid)) {
    g <- intersect(expected_grid, names(dat))
    full <- do.call(expand.grid, c(lapply(g, function(v) unique(dat[[v]])),
                                   stringsAsFactors = FALSE))
    names(full) <- g
    obs <- unique(as.data.frame(dat[g]))
    key <- function(d) do.call(paste, c(lapply(d, as.character), sep = "\r"))
    absent <- sum(!key(full) %in% key(obs))
    out <- c(out, list(qa_row(
      paste("Complete grid:", paste(g, collapse = " x ")), absent == 0,
      paste(absent, "of", nrow(full), "combinations absent"), "warn")))
  }
  if (!is.null(expected_units)) {
    n_exp <- expected_units$units
    if (is.null(n_exp) || !is.numeric(n_exp)) {
      stop("expected_units needs 'by' and a numeric 'units' entry, e.g. {by: [site, period], units: 10}")
    }
    by <- intersect(expected_units$by, names(dat))
    uid <- if ("unit_id" %in% names(dat)) "unit_id" else unit
    cnt <- dplyr::summarise(dplyr::group_by(dat, dplyr::across(dplyr::all_of(by))),
                            n_units = dplyr::n_distinct(dplyr::across(dplyr::all_of(uid))),
                            .groups = "drop")
    off <- sum(cnt$n_units != n_exp)
    out <- c(out, list(qa_row(
      paste0(n_exp, " sampling units per ", paste(by, collapse = " x ")),
      off == 0, paste(off, "groups differ from expected"), "warn")))
  }
  for (sc in sum_checks) {
    parts <- intersect(sc$parts, names(dat))
    s <- rowSums(as.data.frame(lapply(dat[parts], function(v) ifelse(is.na(v), 0, v))))
    bad <- sum(abs(dat[[sc$total]] - s) > 1e-8, na.rm = TRUE)
    out <- c(out, list(qa_row(
      paste(sc$total, "=", paste(parts, collapse = " + ")), bad == 0,
      paste(bad, "rows do not add up"), if (is.null(sc$severity)) "warn" else sc$severity)))
  }
  res <- dplyr::bind_rows(out)
  if (stop_on_fail && any(res$status == "fail")) {
    f <- res[res$status == "fail", ]
    stop("Data validation failed:\n", paste0(" - ", f$check, ": ", f$detail, collapse = "\n"))
  }
  res
}
