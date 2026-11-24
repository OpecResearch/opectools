#' Parse month names, abbreviations, numbers or dates to month numbers
#'
#' Accepts "Aug", "August", "Sept", "sep ", 8, or a `Date`.
#'
#' @param x A character, numeric or Date vector.
#' @return Integer vector of months (1-12); unrecognised values become `NA`.
#' @export
#' @examples
#' parse_month(c("Aug", "Sept", "december", 3))
parse_month <- function(x) {
  if (inherits(x, "Date")) return(as.integer(format(x, "%m")))
  if (is.numeric(x)) {
    out <- as.integer(x)
    out[!out %in% 1:12] <- NA_integer_
    return(out)
  }
  z <- tolower(trimws(as.character(x)))
  num <- suppressWarnings(as.integer(z))
  out <- match(substr(z, 1, 3), tolower(month.abb))
  out[is.na(out) & num %in% 1:12] <- num[is.na(out) & num %in% 1:12]
  out
}

#' Reconstruct dates for month names recorded without a year
#'
#' Field sheets often record only the month. Given the first month of the
#' study, each month is assigned to its first occurrence on or after
#' `start`. Valid only for studies lasting 12 months or less; for longer
#' studies record the year in the source data.
#'
#' @param month Month names or numbers.
#' @param start Study start, e.g. `"2020-08"` or a Date.
#' @return A Date vector (first day of each month).
#' @export
#' @examples
#' months_to_dates(c("Aug", "Dec", "Jan", "Jul"), start = "2020-08")
months_to_dates <- function(month, start) {
  start <- as.Date(paste0(substr(as.character(start), 1, 7), "-01"))
  m <- parse_month(month)
  sm <- as.integer(format(start, "%m"))
  sy <- as.integer(format(start, "%Y"))
  total <- sm - 1L + ((m - sm) %% 12L)
  out <- as.Date(sprintf("%04d-%02d-01", sy + total %/% 12L, total %% 12L + 1L))
  out[is.na(m)] <- NA
  out
}

#' Assign seasons from dates or months
#'
#' @param x Dates, month names or month numbers.
#' @param seasons Named list mapping season labels to months, e.g.
#'   `list(Wet = 4:10, Dry = c(11, 12, 1:3))`. Month names are accepted.
#'   Every month present in `x` must belong to exactly one season.
#' @param reference Optional season used as the first (reference) level.
#' @return A factor.
#' @export
#' @examples
#' assign_season(c("Jan", "Jun"), list(Wet = 4:10, Dry = c(11, 12, 1:3)))
assign_season <- function(x, seasons, reference = NULL) {
  m <- parse_month(x)
  season_months <- lapply(seasons, parse_month)
  all_m <- unlist(season_months)
  if (anyDuplicated(all_m)) {
    stop("A month is assigned to more than one season: ",
         paste(month.abb[unique(all_m[duplicated(all_m)])], collapse = ", "))
  }
  lookup <- rep(names(seasons), lengths(season_months))
  out <- lookup[match(m, all_m)]
  uncovered <- unique(m[!is.na(m) & is.na(out)])
  if (length(uncovered)) {
    stop("Months not covered by any season: ",
         paste(month.abb[uncovered], collapse = ", "))
  }
  lev <- names(seasons)
  if (!is.null(reference)) {
    if (!reference %in% lev) stop("reference season '", reference, "' not defined")
    lev <- c(reference, setdiff(lev, reference))
  }
  factor(out, levels = lev)
}

#' Add standard time variables to a data frame
#'
#' Adds `month` (1-12), `month_label`, `year`, `period` (an ordered "YYYY-MM"
#' factor in study order), `time_index` (consecutive months from the first
#' observed month, safe across years), `time` (factor version of
#' `time_index`, for `ar1()` terms) and, if `seasons` is given, `season`.
#'
#' @param dat A data frame.
#' @param date_col Name of the Date column.
#' @param seasons Optional season definition, see [assign_season()].
#' @param reference_season Optional reference level for `season`.
#' @param origin Optional Date that defines `time_index == 1`. Defaults to the
#'   first month in `dat`; pass a common origin when combining data sets.
#' @return `dat` with added columns.
#' @export
add_time_vars <- function(dat, date_col = "date", seasons = NULL,
                          reference_season = NULL, origin = NULL) {
  d <- as.Date(dat[[date_col]])
  if (any(is.na(d))) stop("Column '", date_col, "' contains missing or unparseable dates.")
  month_start <- as.Date(format(d, "%Y-%m-01"))
  y <- as.integer(format(d, "%Y"))
  m <- as.integer(format(d, "%m"))
  if (is.null(origin)) origin <- min(month_start)
  origin <- as.Date(origin)
  oy <- as.integer(format(origin, "%Y"))
  om <- as.integer(format(origin, "%m"))
  idx <- (y - oy) * 12L + (m - om) + 1L
  period_levels <- format(sort(unique(month_start)), "%Y-%m")

  dat$month <- m
  dat$month_label <- factor(month.abb[m], levels = unique(month.abb[m[order(idx)]]))
  dat$year <- y
  dat$period <- factor(format(month_start, "%Y-%m"), levels = period_levels)
  dat$time_index <- idx
  dat$time <- factor(idx, levels = sort(unique(idx)))
  if (!is.null(seasons)) {
    dat$season <- assign_season(m, seasons, reference = reference_season)
  }
  dat
}

#' Shift dates by whole years, with an audit trail
#'
#' For documented administrative corrections (e.g. a data-entry year error).
#' Refuses to run unless every original date lies in the expected window, so
#' an already-corrected file is never shifted twice.
#'
#' @param dates Original dates.
#' @param years Number of years to add (may be negative).
#' @param expected_from,expected_to Window that all original dates must fall in.
#' @return A list with `dates` (shifted) and `audit` (distinct original/new pairs).
#' @export
shift_dates_years <- function(dates, years, expected_from, expected_to) {
  dates <- as.Date(dates)
  if (any(is.na(dates))) stop("Missing or unparseable dates; cannot shift.")
  if (min(dates) < as.Date(expected_from) || max(dates) > as.Date(expected_to)) {
    stop("Original dates fall outside ", expected_from, " to ", expected_to,
         "; refusing to shift (already corrected?).")
  }
  y <- as.integer(format(dates, "%Y")) + as.integer(years)
  new <- as.Date(sprintf("%04d-%s", y, format(dates, "%m-%d")))
  if (any(is.na(new))) stop("Shift produced an invalid date (29 February?).")
  audit <- unique(tibble::tibble(original_date = dates, corrected_date = new))
  list(dates = new, audit = audit[order(audit$original_date), ])
}
