# ============================================================================
# SUPPLEMENTARY FIGURE 9 NORMALISED PERFORMANCE OF THE PORTFOLIOS
# ============================================================================
# (a-d) The eight indicators of Fig. 5 normalised within each pathway (0 = best, 1 = worst).
# Data: data/figure_data.csv (rows with figure == "SFig9")
# Output: output/Supplementary_Figure_9.png and output/Supplementary_Figure_9.pdf
# Run: Rscript "Supplementary Figure 9 Normalised performance of the portfolios.R" (or source() it, or open the .Rproj in RStudio); run_all.R draws all figures.
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
source("R/sfig9_panels.R")

fig <- wrap_plots(panels, ncol = 2) + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") & tags() &
  theme(legend.position = "bottom")
save_fig(fig, "Supplementary_Figure_9", 183, 165)
print(fig)
