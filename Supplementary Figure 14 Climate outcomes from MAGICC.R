# ============================================================================
# SUPPLEMENTARY FIGURE 14 CLIMATE OUTCOMES FROM MAGICC
# ============================================================================
# MAGICC v7.5.3 (600-member ensemble): (a, b) exceedance probabilities of each run, (c) GSAT of each run (thin), pathway
# median (bold) and 5th-95th percentile envelope.
# Data: data/figure_data.csv (rows with figure == "SFig14")
# Output: output/Supplementary_Figure_14.png and output/Supplementary_Figure_14.pdf
# Run: Rscript "Supplementary Figure 14 Climate outcomes from MAGICC.R" (or source() it, or open the .Rproj in RStudio); run_all.R draws all figures.
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
source("R/sfig14_panels.R")

fig <- ((p_a | p_b) / p_c) + plot_layout(heights = c(1, 1)) + plot_annotation(tag_levels = "a") & tags()
save_fig(fig, "Supplementary_Figure_14", 183, 150)
print(fig)
