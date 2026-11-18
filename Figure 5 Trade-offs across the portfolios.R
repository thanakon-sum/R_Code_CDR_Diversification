# ============================================================================
# FIGURE 5 TRADE-OFFS ACROSS THE PORTFOLIOS
# ============================================================================
# (a-d) Eight indicators of every portfolio in the High, Med, Low and vLow pathways (fossil energy, energy for DACCS,
# total CDR and CCS as means over 2020-2100); the numbers are the values and the dot area grows with the magnitude of the
# value within each indicator. Full-combo portfolio highlighted.
# Data: data/figure_data.csv (rows with figure == "Fig5")
# Output: output/Figure_5.png and output/Figure_5.pdf
# Run: Rscript "Figure 5 Trade-offs across the portfolios.R" (or source() it, or open the .Rproj in RStudio); run_all.R draws all figures.
# ============================================================================
# ---- run from this folder (Rscript, source() or RStudio) and install missing packages ----
if (!l10n_info()[["UTF-8"]]) for (l in c("C.UTF-8", "en_US.UTF-8", "English_United States.utf8"))  # the scripts hold UTF-8 text
  if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", l)))) break
local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (!length(f)) f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (length(f) && nzchar(f[1])) setwd(dirname(normalizePath(gsub("~+~", " ", f[1], fixed = TRUE))))
})
source("R/setup.R")
source("R/style.R")
source("R/fig5_panels.R")

save_fig(fig5, "Figure_5", 183, 170)
print(fig5)
