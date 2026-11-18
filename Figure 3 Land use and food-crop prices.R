# ============================================================================
# FIGURE 3 LAND USE AND FOOD-CROP PRICES
# ============================================================================
# (a-d) Global area of energy crops, other natural land, pasture and food crops (BaU, range across portfolios, Full-combo).
# (e-h) Land-use change from 2030 in the High pathway: BaU, Full-combo, BECCS only, DACCS only. (i-m) Regional land-use
# change 2030-2100 and (n) food-crop price change 2010-2100, Full-combo portfolio, High pathway.
# Data: data/figure_data.csv (rows with figure == "Fig3") and data/map_polygons.csv
# Output: output/Figure_3.png and output/Figure_3.pdf
# Run: Rscript "Figure 3 Land use and food-crop prices.R" (or source() it, or open the .Rproj in RStudio); run_all.R draws all figures.
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
source("R/fig3_panels.R")

row1 <- (p_a | p_b | p_c | p_d) + plot_layout(guides = "collect") & theme(legend.position = "bottom", legend.box = "horizontal")
row2 <- (p_e | p_f | p_g | p_h) + plot_layout(guides = "collect") & theme(legend.position = "bottom") &
  guides(fill = guide_legend(nrow = 1))
row3 <- wrap_plots(maps, ncol = 3)
fig <- (row1 / row2 / row3) + plot_layout(heights = c(1, 1.1, 1.25)) + plot_annotation(tag_levels = "a") & tags()
save_fig(fig, "Figure_3", 183, 195)
print(fig)
