# ============================================================================
# FIGURE 1 PATHWAYS, FEASIBILITY AND CDR DEPLOYMENT OF THE PORTFOLIOS
# ============================================================================
# (a) Net CO2 emission constraint of the five pathways (solid, left axis) and median global mean surface air temperature
# from MAGICC v7.5.3 (dashed, right axis). (b) Feasibility of the 16 CDR portfolios in each pathway. (c) Annual CDR by
# technology of the 12 feasible portfolios, 2020-2100 (dashed: total). (d-f) Total CDR, CCS and DACCS: range across the
# feasible portfolios of each pathway (ribbon) and the Full-combo portfolio (dashed).
# Data: data/figure_data.csv (rows with figure == "Fig1")
# Output: output/Figure_1.png and output/Figure_1.pdf
# Run: Rscript "Figure 1 Pathways, feasibility and CDR deployment of the portfolios.R" (or source() it, or open the .Rproj in RStudio); run_all.R draws all figures.
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
source("R/fig1_panels.R")

top <- (p_a | p_b) + plot_layout(widths = c(1.6, 1))
right <- (p_d / p_e / p_f) + plot_layout(heights = c(1, 1, 1))
bottom <- (p_c | right) + plot_layout(widths = c(1.6, 1))
fig <- (top / bottom) + plot_layout(heights = c(1, 2.35)) +
  plot_annotation(tag_levels = "a") & tags()
save_fig(fig, "Figure_1", 183, 235)
print(fig)
