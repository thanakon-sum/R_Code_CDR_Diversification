# ============================================================================
# FIGURE 4 ENERGY SYSTEM AND DACCS ENERGY USE
# ============================================================================
# (a-c) Global primary, secondary and final energy and (d) residential electricity price. (e, f) Primary and final
# energy mix in the vLow pathway (BaU, Full-combo, no BECCS, no DACCS). (g) DACCS share of secondary energy, 2040-2100.
# (h-k) Regional bioenergy, non-biomass renewables, fossil fuels with CCS and energy used for DACCS in 2100 (vLow, Full-combo).
# Data: data/figure_data.csv (rows with figure == "Fig4") and data/map_polygons.csv
# Output: output/Figure_4.png and output/Figure_4.pdf
# Run: Rscript "Figure 4 Energy system and DACCS energy use.R" (or source() it, or open the .Rproj in RStudio); run_all.R draws all figures.
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
source("R/fig4_panels.R")

row1 <- (p_a | p_b | p_c | p_d) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
row2 <- (p_e | p_f) + plot_layout(widths = c(1, 1))
row4 <- wrap_plots(maps, ncol = 2)
fig <- (row1 / row2 / p_g / row4) + plot_layout(heights = c(1, 1.45, 0.8, 1.75)) + plot_annotation(tag_levels = "a") & tags()
save_fig(fig, "Figure_4", 183, 235)
print(fig)
