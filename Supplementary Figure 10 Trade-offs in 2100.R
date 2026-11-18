# ============================================================================
# SUPPLEMENTARY FIGURE 10 TRADE-OFFS IN 2100
# ============================================================================
# The eight indicators of Fig. 5 with fossil energy, energy for DACCS, total CDR and CCS in 2100 instead of their
# 2020-2100 means.
# Data: data/figure_data.csv (rows with figure == "SFig10")
# Output: output/Supplementary_Figure_10.png and output/Supplementary_Figure_10.pdf
# Run: Rscript "Supplementary Figure 10 Trade-offs in 2100.R" (or source() it, or open the .Rproj in RStudio); run_all.R draws all figures.
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
FIG5_ID <- "SFig10"; FIG5_WHEN <- "2100"  # the Fig. 5 panels, drawn from the values in 2100
source("R/fig5_panels.R")

save_fig(fig5, "Supplementary_Figure_10", 183, 170)
print(fig5)
