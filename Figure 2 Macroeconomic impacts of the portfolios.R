# ============================================================================
# FIGURE 2 MACROECONOMIC IMPACTS OF THE PORTFOLIOS
# ============================================================================
# (a) Policy cost (NPV at 5% of the consumption loss rate, 2020-2100). (b) Intergenerational equity: relative difference of
# the Ramsey-discounted consumption loss of the current (2020-2060) and future (2061-2100) generation. (c) GDP loss and (d) consumption loss rates.
# (e-h) Carbon price of each portfolio (left) and its distribution over 2025-2100 (right) in the High, Med, Low and vLow pathways.
# Data: data/figure_data.csv (rows with figure == "Fig2")
# Output: output/Figure_2.png and output/Figure_2.pdf
# Run: Rscript "Figure 2 Macroeconomic impacts of the portfolios.R" (or source() it, or open the .Rproj in RStudio); run_all.R draws all figures.
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
source("R/fig2_panels.R")

tg <- function(p, t) p + labs(tag = t)
row1 <- (tg(p_a, "a") | tg(p_b, "b") | tg(p_c, "c") | tg(p_d, "d")) + plot_layout(widths = c(1, 1, 1.15, 1.15))
pair <- function(x, t) list(tg(x[[1]], t), x[[2]])
row2 <- wrap_plots(c(pair(p_e, "e"), pair(p_f, "f")), nrow = 1, widths = c(1.25, 1, 1.25, 1))
row3 <- wrap_plots(c(pair(p_g, "g"), pair(p_h, "h")), nrow = 1, widths = c(1.25, 1, 1.25, 1))
fig <- (row1 / row2 / row3) + plot_layout(heights = c(1, 1.15, 1.15), guides = "collect") &
  tags() & theme(legend.position = "bottom", legend.box = "vertical", legend.spacing.y = unit(2, "pt"))
save_fig(fig, "Figure_2", 183, 190)
print(fig)
