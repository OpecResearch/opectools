# =============================================================================
# Run the analysis described in config.yml
# -----------------------------------------------------------------------------
# 1. Edit config.yml (data paths, seasons, candidate models, contrasts).
# 2. Run this script from this folder (or open the .Rproj / setwd() here).
# 3. Review results/qa_checks.csv, each analysis' comparison.csv and
#    diagnostics.csv, THEN set `primary:` for each analysis and re-run.
# 4. Optionally render the report:  quarto render report.qmd
# =============================================================================

# install.packages("remotes")
# remotes::install_github("OpecResearch/opectools")
library(opectools)

res <- run_analysis("config.yml")

# Quick look at the most important tables
for (a in res$analyses) {
  cat("\n====", a$name, "====\n")
  print(a$comparison[, c("model", "n", "converged", "pd_hessian", "AIC", "delta_AIC")])
  if (!is.null(a$contrasts)) print(a$contrasts)
}
