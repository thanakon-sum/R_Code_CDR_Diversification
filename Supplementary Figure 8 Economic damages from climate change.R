# ============================================================================
# SUPPLEMENTARY FIGURE 8 ECONOMIC DAMAGES FROM CLIMATE CHANGE
# ============================================================================
# (a) Climate damages by sector (% of GDP): Full-combo with the primary emulator, range of eight other emulators, BaU.
# (b) Total damage in 2100 by region and (c) sector damages of the 17 regions (Full-combo, High).
# Data: data/figure_data.csv (rows with figure == "SFig8") and data/map_polygons.csv
# Output: output/Supplementary_Figure_8.png and output/Supplementary_Figure_8.pdf
# Run: Rscript "Supplementary Figure 8 Economic damages from climate change.R" (or source() it, or open the .Rproj in RStudio); run_all.R draws all figures.
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
source("R/sfig8_panels.R")

row2 <- (wrap_elements(full = p_b) + labs(tag = "b") | wrap_elements(full = leg_a)) + plot_layout(widths = c(3.2, 1))  # map fills its cell
fig <- (p_a + labs(tag = "a")) / row2 / (p_c + labs(tag = "c")) + plot_layout(heights = c(1, 0.95, 1)) & tags()
save_fig(fig, "Supplementary_Figure_8", 183, 206)
print(fig)
