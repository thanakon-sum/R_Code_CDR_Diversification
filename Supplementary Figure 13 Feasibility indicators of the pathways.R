# ============================================================================
# SUPPLEMENTARY FIGURE 13 FEASIBILITY INDICATORS OF THE PATHWAYS
# ============================================================================
# (a-w) Feasibility indicators: other portfolios (10th-90th percentile) and Full-combo; shaded zones = medium and high
# concern thresholds of IPCC AR6 WGIII Annex III.
# Data: data/figure_data.csv (rows with figure == "SFig13")
# Output: output/Supplementary_Figure_13.png and output/Supplementary_Figure_13.pdf
# Run: Rscript "Supplementary Figure 13 Feasibility indicators of the pathways.R" (or source() it, or open the .Rproj in RStudio); run_all.R draws all figures.
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
source("R/sfig13_panels.R")

rows <- list(c("Geophysical", "a", "b", "c", "d"), c("Economic", "e", "f", "g", NA),
             c("Technological", "h", "i", "j", "k"), c("Technological", "l", "m", "n", "o"),
             c("Tech./socio-cultural", "p", "q", "r", "s"), c("Socio-cultural", "t", "u", "v", "w"))
cells <- unlist(lapply(rows, function(r) c(list(row_label(r[1])), lapply(r[-1], function(k) if (is.na(k)) leg else panels[[k]]))),
                recursive = FALSE)
fig <- wrap_plots(cells, ncol = 5, widths = c(0.13, 1, 1, 1, 1)) & tags()
save_fig(fig, "Supplementary_Figure_13", 183, 225)
print(fig)
