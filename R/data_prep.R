#' Clean column names to snake_case
#'
#' Lower-cases, replaces non-alphanumeric characters with `_` and de-duplicates.
#' A dependency-free alternative to `janitor::clean_names()`.
#'
#' @param dat A data frame.
#' @return `dat` with cleaned names.
#' @export
clean_names <- function(dat) {
  n <- tolower(names(dat))
  n <- gsub("[^a-z0-9]+", "_", n)
  n <- gsub("^_+|_+$", "", n)
  n[n == ""] <- "x"
  names(dat) <- make.unique(n, sep = "_")
  dat
}

#' Recode labels using regular-expression patterns
#'
#' Harmonises inconsistent spellings of site or species names. The first
#' matching pattern wins.
#'
#' @param x Character vector.
#' @param patterns Named character vector: names are regex patterns, values
#'   are the standard labels, e.g. `c("gambiae" = "An. gambiae s.l.")`.
#' @param ignore_case Case-insensitive matching (default `TRUE`).
#' @param keep_unmatched Keep unmatched values (squished) instead of `NA`.
#' @return Character vector.
#' @export
#' @examples
#' recode_patterns(c("An. gambiae s.l", "an funestus"),
#'                 c("gambiae" = "An. gambiae s.l.", "funestus" = "An. funestus group"))
recode_patterns <- function(x, patterns, ignore_case = TRUE, keep_unmatched = TRUE) {
  x <- gsub("\\s+", " ", trimws(as.character(x)))
  out <- rep(NA_character_, length(x))
  for (i in seq_along(patterns)) {
    hit <- is.na(out) & grepl(names(patterns)[i], x, ignore.case = ignore_case)
    out[hit] <- patterns[[i]]
  }
  if (keep_unmatched) out[is.na(out)] <- x[is.na(out)]
  out
}

#' Columns of the standard ("canonical") catch format
#'
#' Every analysis in opectools reads data in one long format: one row per
#' species per sampling unit. Write a small import function for each new
#' study that produces this shape; everything downstream is then generic.
#'
#' @return A tibble describing each column.
#' @export
catch_schema <- function() {
  tibble::tribble(
    ~column,    ~required, ~description,
    "site",     TRUE,  "Study site / community / village",
    "date",     TRUE,  "Sampling date (Date). Use first day of month if only month is known",
    "species",  TRUE,  "Taxon label (harmonise with recode_patterns())",
    "count",    TRUE,  "Non-negative integer count",
    "effort",   TRUE,  "Sampling effort: trap-nights, room-days, person-nights...",
    "method",   FALSE, "Collection method (CDC, PSC, HLC...). Methods are never pooled",
    "location", FALSE, "Indoor / Outdoor or other within-site position",
    "unit_id",  FALSE, "Household, trap or collector ID for repeated sampling units",
    "<covariates>", FALSE, "Any unit-level covariates, e.g. temperature, rh"
  )
}

#' Rename and type-convert data to the standard catch format
#'
#' @param dat A data frame.
#' @param map Named character vector `c(standard_name = "source_name")`.
#'   Columns already using standard names need not be mapped.
#' @param constants Named list of constant columns to add when absent, e.g.
#'   `list(method = "CDC", effort = 3)`.
#' @return A tibble.
#' @export
standardise_catch <- function(dat, map = NULL, constants = NULL) {
  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  if (length(map)) {
    missing <- setdiff(unname(map), names(dat))
    if (length(missing)) stop("Source columns not found: ", paste(missing, collapse = ", "))
    for (nm in names(map)) dat[[nm]] <- dat[[map[[nm]]]]
    drop <- setdiff(unname(map), names(map))
    dat <- dat[, setdiff(names(dat), drop), drop = FALSE]
  }
  for (nm in names(constants)) if (!nm %in% names(dat)) dat[[nm]] <- constants[[nm]]
  req <- catch_schema()$column[catch_schema()$required]
  miss <- setdiff(req, names(dat))
  if (length(miss)) stop("Missing required column(s): ", paste(miss, collapse = ", "),
                         ". See catch_schema().")
  dat$date <- as.Date(dat$date)
  dat$count <- suppressWarnings(as.numeric(dat$count))
  dat$effort <- suppressWarnings(as.numeric(dat$effort))
  for (nm in intersect(c("site", "species", "method", "location", "unit_id"), names(dat))) {
    dat[[nm]] <- as.character(dat[[nm]])
  }
  tibble::as_tibble(dat)
}

#' Convert wide location columns (e.g. Indoor / Outdoor) to long format
#'
#' @param dat A data frame with one count column per location.
#' @param count_cols Named character vector `c(Indoor = "indoor", Outdoor = "outdoor")`.
#' @param covariate_cols Optional named list; each element maps locations to
#'   source columns, e.g. `list(temperature = c(Indoor = "temp_in", Outdoor = "temp_out"))`.
#' @param keep Columns to carry over unchanged (default: all others).
#' @return A tibble with `location` and `count` columns.
#' @export
pivot_locations <- function(dat, count_cols, covariate_cols = list(), keep = NULL) {
  used <- c(unname(count_cols), unlist(covariate_cols, use.names = FALSE))
  if (is.null(keep)) keep <- setdiff(names(dat), used)
  out <- lapply(names(count_cols), function(loc) {
    d <- dat[, keep, drop = FALSE]
    d$location <- loc
    d$count <- suppressWarnings(as.numeric(dat[[count_cols[[loc]]]]))
    for (cv in names(covariate_cols)) {
      src <- covariate_cols[[cv]][[loc]]
      d[[cv]] <- if (is.null(src)) NA_real_ else suppressWarnings(as.numeric(dat[[src]]))
    }
    d
  })
  tibble::as_tibble(dplyr::bind_rows(out))
}

#' Aggregate species rows to sampling units
#'
#' The sampling unit (e.g. site x month x location for light traps, or
#' household x month for resting collections) is the unit of inference, not
#' the species row. Counts are summed; effort and covariates must be constant
#' within a unit and are carried over (an error lists any unit where they
#' differ, so effort is never summed across species rows by mistake).
#'
#' @param dat Species-level data in the standard format.
#' @param unit Columns defining a sampling unit.
#' @param sum_vars Columns to sum (default `count` plus any listed).
#' @param constant_vars Columns that must be constant within a unit.
#' @param on_inconsistent `"error"` (default) or `"warn"`.
#' @return A tibble with one row per unit and `n_rows` (species rows per unit).
#' @export
aggregate_units <- function(dat, unit, sum_vars = "count", constant_vars = NULL,
                            on_inconsistent = c("error", "warn")) {
  on_inconsistent <- match.arg(on_inconsistent)
  unit <- intersect(unit, names(dat))
  sum_vars <- intersect(sum_vars, names(dat))
  constant_vars <- setdiff(intersect(constant_vars, names(dat)), c(unit, sum_vars))
  g <- dplyr::group_by(dat, dplyr::across(dplyr::all_of(unit)))
  sums <- dplyr::summarise(
    g, dplyr::across(dplyr::all_of(sum_vars), ~ sum(.x, na.rm = TRUE)),
    n_rows = dplyr::n(), .groups = "drop"
  )
  if (!length(constant_vars)) return(sums)
  nd <- dplyr::summarise(
    g, dplyr::across(dplyr::all_of(constant_vars),
                     ~ dplyr::n_distinct(.x[!is.na(.x)])), .groups = "drop"
  )
  bad <- nd[apply(nd[constant_vars] > 1, 1, any), , drop = FALSE]
  if (nrow(bad)) {
    msg <- paste0(nrow(bad), " sampling unit(s) have values of ",
                  paste(constant_vars[colSums(nd[constant_vars] > 1) > 0], collapse = ", "),
                  " that differ between rows. First: ",
                  paste(utils::capture.output(print(as.data.frame(bad[1, unit]))), collapse = " "))
    if (on_inconsistent == "error") stop(msg) else warning(msg)
  }
  consts <- dplyr::summarise(
    g, dplyr::across(dplyr::all_of(constant_vars), first_nonmissing), .groups = "drop"
  )
  dplyr::left_join(sums, consts, by = unit)
}

#' First non-missing value (helper)
#' @param x A vector.
#' @return The first non-`NA` element, or `NA`.
#' @export
first_nonmissing <- function(x) {
  y <- x[!is.na(x)]
  if (length(y) == 0) x[NA_integer_][1] else y[[1]]
}

#' Standardise (z-score) numeric covariates
#'
#' @param dat A data frame.
#' @param vars Columns to standardise; new columns get `suffix`.
#' @param suffix Default `"_z"`.
#' @return `dat` with new columns; the attribute `"scaling"` holds means and SDs
#'   so effects can be reported per original unit.
#' @export
standardise_vars <- function(dat, vars, suffix = "_z") {
  sc <- lapply(vars, function(v) {
    x <- as.numeric(dat[[v]])
    mu <- mean(x, na.rm = TRUE); s <- stats::sd(x, na.rm = TRUE)
    dat[[paste0(v, suffix)]] <<- (x - mu) / s
    tibble::tibble(variable = v, mean = mu, sd = s)
  })
  attr(dat, "scaling") <- dplyr::bind_rows(sc)
  dat
}
