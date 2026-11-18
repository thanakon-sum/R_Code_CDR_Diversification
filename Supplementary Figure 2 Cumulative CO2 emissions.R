# ============================================================================
# SUPPLEMENTARY FIGURE 2 CUMULATIVE CO2 EMISSIONS
# ============================================================================
# Cumulative net CO2 emissions of the four pathways (Full-combo; all portfolios of a pathway follow the same path).
# Data: data/figure_data.csv (rows with figure == "SFig2")
# Output: output/Supplementary_Figure_2.png and output/Supplementary_Figure_2.pdf
# Run: Rscript "Supplementary Figure 2 Cumulative CO2 emissions.R" (or source() it, or open the .Rproj in RStudio); run_all.R draws all figures.
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
source("R/sfig2_panels.R")

fig <- p_s5
save_fig(fig, "Supplementary_Figure_2", 89, 80)
print(fig)
