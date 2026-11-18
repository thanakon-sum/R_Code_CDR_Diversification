# ============================================================================
# SUPPLEMENTARY FIGURE 6 SECTORAL EMISSIONS AND REGIONAL CDR
# ============================================================================
# (a) CO2 emissions by sector in BaU and in the Full-combo portfolio of the High and vLow pathways (line: net CO2).
# (b-g) CDR by technology summed over 2025-2100 in each region (Full-combo, vLow).
# Data: data/figure_data.csv (rows with figure == "SFig6") and data/map_polygons.csv
# Output: output/Supplementary_Figure_6.png and output/Supplementary_Figure_6.pdf
# Run: Rscript "Supplementary Figure 6 Sectoral emissions and regional CDR.R" (or source() it, or open the .Rproj in RStudio); run_all.R draws all figures.
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
source("R/sfig6_panels.R")

fig <- (p_a / wrap_plots(maps, ncol = 2)) + plot_layout(heights = c(1, 2.25)) + plot_annotation(tag_levels = "a") & tags()
save_fig(fig, "Supplementary_Figure_6", 183, 178)
print(fig)
