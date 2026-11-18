# ============================================================================
# SUPPLEMENTARY FIGURE 3 FEASIBILITY OF THE PORTFOLIOS ACROSS THE SSPS
# ============================================================================
# (a-e) Feasibility of the 16 portfolios in the five pathways for SSP1 to SSP5.
# Data: data/figure_data.csv (rows with figure == "SFig3")
# Output: output/Supplementary_Figure_3.png and output/Supplementary_Figure_3.pdf
# Run: Rscript "Supplementary Figure 3 Feasibility of the portfolios across the SSPs.R" (or source() it, or open the .Rproj in RStudio); run_all.R draws all figures.
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
source("R/sfig3_panels.R")

fig <- wrap_plots(panels, nrow = 1) + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") & tags() & theme(legend.position = "bottom")
save_fig(fig, "Supplementary_Figure_3", 183, 80)
print(fig)
