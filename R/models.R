#' Resolve a count-model family from its name
#'
#' @param family One of `"poisson"`, `"nbinom2"`, `"nbinom1"`, `"compois"`,
#'   `"genpois"`, `"truncated_nbinom2"`, or a family object.
#' @return A family object.
#' @export
resolve_family <- function(family) {
  if (inherits(family, "family") || is.list(family)) return(family)
  switch(family,
         poisson = stats::poisson(link = "log"),
         nbinom2 = glmmTMB::nbinom2(),
         nbinom1 = glmmTMB::nbinom1(),
         compois = glmmTMB::compois(),
         genpois = glmmTMB::genpois(),
         truncated_nbinom2 = glmmTMB::truncated_nbinom2(),
         stop("Unknown family '", family, "'. Use poisson, nbinom2, nbinom1, ",
              "compois, genpois or truncated_nbinom2."))
}

#' Check that mutually exclusive terms are not in the same model
#'
#' E.g. season and rainfall: season is largely a rainfall classification,
#' so including both makes their effects uninterpretable.
#'
#' @param formula A formula or string.
#' @param exclusive List of character vectors; at most one variable from each
#'   vector may appear in the formula.
#' @return Invisibly `TRUE`; errors otherwise.
#' @export
check_exclusive_terms <- function(formula, exclusive) {
  v <- all.vars(stats::as.formula(formula))
  for (grp in exclusive) {
    hit <- intersect(unlist(grp), v)
    if (length(hit) > 1) {
      stop("Model '", deparse1(stats::as.formula(formula)), "' contains ",
           paste(hit, collapse = " and "), ", which are declared mutually exclusive.")
    }
  }
  invisible(TRUE)
}

#' Fit one count model with glmmTMB
#'
#' @param spec A list with `formula` (string or formula; random effects and
#'   `ar1()` allowed), `family` (see [resolve_family()]) and optional `zi`
#'   (zero-inflation formula, e.g. `"~ location"`) and `dispformula`.
#' @param data Unit-level data.
#' @param offset Name of the effort column; `log(effort)` is added as an
#'   offset. `NULL` for no offset.
#' @param name Label for the model.
#' @return An object of class `opec_fit`: a list with `name`, `spec`, `model`
#'   (or `NULL` if fitting failed), `data` (rows used), `error`, `warnings`.
#' @export
fit_count_model <- function(spec, data, offset = "effort", name = "model") {
  f_txt <- if (inherits(spec$formula, "formula")) deparse1(spec$formula) else spec$formula
  if (!is.null(offset) && !grepl("offset\\(", f_txt)) {
    f_txt <- paste0(f_txt, " + offset(log(", offset, "))")
  }
  f <- stats::as.formula(f_txt)
  zi <- if (is.null(spec$zi)) ~0 else stats::as.formula(spec$zi)
  disp <- if (is.null(spec$dispformula)) ~1 else stats::as.formula(spec$dispformula)
  vars <- unique(c(all.vars(f), all.vars(zi), all.vars(disp)))
  miss <- setdiff(vars, names(data))
  if (length(miss)) stop("Model '", name, "' uses unknown column(s): ", paste(miss, collapse = ", "))
  d <- data[stats::complete.cases(as.data.frame(data)[vars]), , drop = FALSE]
  d <- droplevels(d)
  warns <- character()
  model <- tryCatch(
    withCallingHandlers(
      glmmTMB::glmmTMB(f, ziformula = zi, dispformula = disp,
                       family = resolve_family(spec$family), data = d),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning")
      }),
    error = function(e) e)
  err <- NULL
  if (inherits(model, "error")) { err <- conditionMessage(model); model <- NULL }
  structure(list(name = name, spec = spec, formula = f, zi = zi, model = model,
                 data = d, error = err, warnings = unique(warns)),
            class = "opec_fit")
}

#' @export
print.opec_fit <- function(x, ...) {
  cat("<opec_fit>", x$name, "\n  ", deparse1(x$formula), "\n")
  if (!is.null(x$spec$zi)) cat("   zi:", x$spec$zi, "\n")
  cat("   family:", x$spec$family, "| n =", nrow(x$data),
      "| OK:", model_ok(x), "\n")
  if (!is.null(x$error)) cat("   error:", x$error, "\n")
  invisible(x)
}

#' Did a model converge with a positive-definite Hessian?
#' @param fit An `opec_fit` or `glmmTMB` object.
#' @return Logical.
#' @export
model_ok <- function(fit) {
  m <- if (inherits(fit, "opec_fit")) fit$model else fit
  if (is.null(m) || !inherits(m, "glmmTMB")) return(FALSE)
  isTRUE(m$fit$convergence == 0L) && isTRUE(m$sdr$pdHess) &&
    is.finite(as.numeric(stats::logLik(m)))
}

#' Fit a set of candidate models
#'
#' @param candidates Named list of model specs (see [fit_count_model()]).
#' @param data Unit-level data.
#' @param offset Effort column name, or `NULL`.
#' @param exclusive Optional list of mutually exclusive variable groups.
#' @return Named list of `opec_fit` objects.
#' @export
fit_candidates <- function(candidates, data, offset = "effort", exclusive = NULL) {
  if (is.null(names(candidates)) || any(names(candidates) == "")) {
    stop("Candidate models must be named.")
  }
  for (nm in names(candidates)) {
    if (length(exclusive)) check_exclusive_terms(candidates[[nm]]$formula, exclusive)
  }
  stats::setNames(lapply(names(candidates), function(nm)
    fit_count_model(candidates[[nm]], data, offset = offset, name = nm)), names(candidates))
}

#' Compare fitted candidate models
#'
#' Information criteria are only reported for models that converged with a
#' positive-definite Hessian. A note flags models fitted to different rows,
#' whose AICs are not comparable.
#'
#' @param fits Named list of `opec_fit` objects.
#' @return Tibble ordered by AIC.
#' @export
compare_models <- function(fits) {
  rows <- lapply(fits, function(f) {
    ok <- model_ok(f)
    m <- f$model
    ll <- if (ok) stats::logLik(m) else NULL
    tibble::tibble(
      model = f$name, family = as.character(f$spec$family),
      formula = deparse1(f$formula),
      zi = if (is.null(f$spec$zi)) "none" else as.character(f$spec$zi),
      n = nrow(f$data),
      converged = !is.null(m) && isTRUE(m$fit$convergence == 0L),
      pd_hessian = !is.null(m) && isTRUE(m$sdr$pdHess),
      df = if (ok) as.integer(attr(ll, "df")) else NA_integer_,
      logLik = if (ok) as.numeric(ll) else NA_real_,
      AIC = if (ok) stats::AIC(m) else NA_real_,
      BIC = if (ok) stats::BIC(m) else NA_real_,
      note = if (!is.null(f$error)) f$error else if (!ok) "failed convergence/Hessian check" else ""
    )
  })
  out <- dplyr::bind_rows(rows)
  out$delta_AIC <- out$AIC - suppressWarnings(min(out$AIC, na.rm = TRUE))
  if (length(unique(out$n[!is.na(out$AIC)])) > 1) {
    out$note <- trimws(paste(out$note, "| models use different rows; AIC not comparable"))
  }
  out[order(out$AIC), ]
}

#' Likelihood-ratio test between two nested candidate models
#'
#' @param reduced,full `opec_fit` objects (reduced nested in full).
#' @return One-row tibble.
#' @export
lr_test <- function(reduced, full) {
  label <- paste(reduced$name, "vs", full$name)
  if (!model_ok(reduced) || !model_ok(full)) {
    return(tibble::tibble(comparison = label, chisq = NA_real_, df = NA_real_,
                          p_value = NA_real_, note = "a model failed convergence checks"))
  }
  if (nrow(reduced$data) != nrow(full$data)) {
    return(tibble::tibble(comparison = label, chisq = NA_real_, df = NA_real_,
                          p_value = NA_real_, note = "models fitted to different rows"))
  }
  a <- suppressWarnings(stats::anova(reduced$model, full$model))
  tibble::tibble(comparison = label, chisq = a$Chisq[2], df = a$`Chi Df`[2],
                 p_value = a$`Pr(>Chisq)`[2], note = "")
}

#' Tidy coefficients of a glmmTMB model
#'
#' Exponentiated conditional coefficients are rate ratios (interaction terms
#' are ratios of rate ratios); exponentiated zero-inflation coefficients are
#' odds ratios for the extra-zero process. Wald intervals.
#'
#' @param fit An `opec_fit` or `glmmTMB` object.
#' @param component `"cond"` or `"zi"`.
#' @return Tibble.
#' @export
tidy_model <- function(fit, component = c("cond", "zi")) {
  component <- match.arg(component)
  m <- if (inherits(fit, "opec_fit")) fit$model else fit
  if (is.null(m)) return(tibble::tibble())
  mat <- summary(m)$coefficients[[component]]
  if (is.null(mat) || !nrow(mat)) return(tibble::tibble())
  z <- stats::qnorm(0.975)
  scale_lab <- if (component == "cond") "rate ratio" else "odds ratio (extra zeros)"
  tibble::tibble(
    component = component, term = rownames(mat),
    estimate = mat[, 1], std_error = mat[, 2], statistic = mat[, 3], p_value = mat[, 4],
    exp_estimate = exp(mat[, 1]),
    exp_conf_low = exp(mat[, 1] - z * mat[, 2]),
    exp_conf_high = exp(mat[, 1] + z * mat[, 2]),
    scale = scale_lab
  )
}

#' Rate ratio for a rescaled continuous predictor
#'
#' E.g. per 100 mm rainfall, or per 1 SD for standardised covariates.
#'
#' @param fit An `opec_fit` or `glmmTMB` object.
#' @param term Conditional-model term.
#' @param multiplier Units per reported change.
#' @param label Description of the reporting unit.
#' @return One-row tibble.
#' @export
scaled_rate_ratio <- function(fit, term, multiplier = 1, label = paste("per", multiplier)) {
  m <- if (inherits(fit, "opec_fit")) fit$model else fit
  mat <- summary(m)$coefficients$cond
  if (!term %in% rownames(mat)) stop("Term not in conditional model: ", term)
  b <- mat[term, 1] * multiplier; se <- mat[term, 2] * abs(multiplier)
  z <- stats::qnorm(0.975)
  tibble::tibble(term = term, reporting_unit = label, rate_ratio = exp(b),
                 conf_low = exp(b - z * se), conf_high = exp(b + z * se),
                 p_value = mat[term, 4])
}

#' Collinearity screen for the fixed effects of a model
#'
#' VIF depends only on the design matrix, so an auxiliary linear model with
#' the same fixed-effect terms (random effects and offsets dropped) is used.
#' Requires the car package.
#'
#' @param fit An `opec_fit`.
#' @return Tibble with (generalised) VIFs; adjusted GVIF^(1/(2*Df)) should be
#'   compared with sqrt(5) or sqrt(10) rules of thumb.
#' @export
vif_table <- function(fit) {
  if (!requireNamespace("car", quietly = TRUE)) {
    return(tibble::tibble(term = NA_character_, note = "install 'car' for VIF"))
  }
  labs <- attr(stats::terms(fit$formula), "term.labels")
  labs <- labs[!grepl("\\|", labs)]
  if (length(labs) < 2) return(tibble::tibble(term = labs, note = "fewer than 2 terms"))
  aux <- stats::lm(stats::reformulate(labs, response = "log1p(count)"), data = fit$data)
  v <- tryCatch(suppressMessages(car::vif(aux, type = "predictor")),
                error = function(e) tryCatch(car::vif(aux), error = function(e2) e2))
  if (inherits(v, "error")) return(tibble::tibble(term = NA_character_, note = conditionMessage(v)))
  if (is.matrix(v) || is.data.frame(v)) {
    v <- as.data.frame(v)
    tibble::tibble(term = rownames(v), gvif = v[["GVIF"]], df = v[["Df"]],
                   adjusted_gvif = v[[grep("GVIF\\^", names(v))[1]]], note = "")
  } else {
    tibble::tibble(term = names(v), gvif = as.numeric(v), df = 1,
                   adjusted_gvif = sqrt(as.numeric(v)), note = "")
  }
}
