# ============================================================================
# SUPPLEMENTARY FIGURE 11 CDR BY TECHNOLOGY IN THE SENSITIVITY RUNS
# ============================================================================
# Annual CDR by technology of the Full-combo portfolio, 2020-2100, in the four sensitivity cases (rows: DACCS and
# enhanced weathering, optimistic and pessimistic) and the four pathways (columns); dashed line = total removal.
# Data: data/figure_data.csv (rows with figure == "SFig11")
# Output: output/Supplementary_Figure_11.png and output/Supplementary_Figure_11.pdf
# Run: Rscript "Supplementary Figure 11 CDR by technology in the sensitivity runs.R" (or source() it, or open the .Rproj in RStudio); run_all.R draws all figures.
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
source("R/sfig11_panels.R")

save_fig(p_s9, "Supplementary_Figure_11", 183, 150)
print(p_s9)
