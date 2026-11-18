# ============================================================================
# SUPPLEMENTARY FIGURE 12 SENSITIVITY TO DACCS AND EW ASSUMPTIONS
# ============================================================================
# Change of the Fig. 5 indicators of the Full-combo portfolio under optimistic and pessimistic cost and capacity
# assumptions for (a) DACCS and (b) enhanced weathering.
# Data: data/figure_data.csv (rows with figure == "SFig12")
# Output: output/Supplementary_Figure_12.png and output/Supplementary_Figure_12.pdf
# Run: Rscript "Supplementary Figure 12 Sensitivity to DACCS and EW assumptions.R" (or source() it, or open the .Rproj in RStudio); run_all.R draws all figures.
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
source("R/sfig12_panels.R")

fig <- (p_a / p_b) + plot_layout(guides = "collect") & tags() & theme(legend.position = "bottom")
save_fig(fig, "Supplementary_Figure_12", 183, 215)
print(fig)
