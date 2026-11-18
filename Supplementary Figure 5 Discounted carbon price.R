# ============================================================================
# SUPPLEMENTARY FIGURE 5 DISCOUNTED CARBON PRICE
# ============================================================================
# Carbon price discounted at 5% (mean of the annual values 2025-2100) of each portfolio.
# Data: data/figure_data.csv (rows with figure == "SFig5")
# Output: output/Supplementary_Figure_5.png and output/Supplementary_Figure_5.pdf
# Run: Rscript "Supplementary Figure 5 Discounted carbon price.R" (or source() it, or open the .Rproj in RStudio); run_all.R draws all figures.
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
source("R/sfig45_panels.R")

fig <- box_dots_si("SFig5", "Carbon price, NPV at 5% (US$2010 tCO2-1)")
save_fig(fig, "Supplementary_Figure_5", 89, 95)
print(fig)
