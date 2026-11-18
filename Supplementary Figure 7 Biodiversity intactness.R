# ============================================================================
# SUPPLEMENTARY FIGURE 7 BIODIVERSITY INTACTNESS
# ============================================================================
# (a) Biodiversity Intactness Index by region in 2100 (Full-combo, Med). (b) World BII: BaU, range across the other
# portfolios, Full-combo.
# Data: data/figure_data.csv (rows with figure == "SFig7") and data/map_polygons.csv
# Output: output/Supplementary_Figure_7.png and output/Supplementary_Figure_7.pdf
# Run: Rscript "Supplementary Figure 7 Biodiversity intactness.R" (or source() it, or open the .Rproj in RStudio); run_all.R draws all figures.
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
source("R/sfig7_panels.R")

fig <- (p_a | p_b) + plot_layout(widths = c(1.45, 1)) + plot_annotation(tag_levels = "a") & tags()
save_fig(fig, "Supplementary_Figure_7", 183, 68)
print(fig)
