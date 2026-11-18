# ---- run from this folder (Rscript, source() or RStudio) and install missing packages ----
if (!l10n_info()[["UTF-8"]]) for (l in c("C.UTF-8", "en_US.UTF-8", "English_United States.utf8"))  # the scripts hold UTF-8 text
  if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", l)))) break
local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (!length(f)) f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (length(f) && nzchar(f[1])) setwd(dirname(normalizePath(gsub("~+~", " ", f[1], fixed = TRUE))))
})
source("R/setup.R")

figs <- list.files(".", pattern = "^(Figure|Supplementary Figure) [0-9]+ .*\\.R$")
figs <- figs[order(grepl("^Supp", figs), as.numeric(sub("^\\D*([0-9]+) .*", "\\1", figs)))]
rs <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
for (f in figs) {  # each figure in its own R session
  cat("\n==== ", f, "\n")
  if (system2(rs, shQuote(f)) != 0) stop("failed: ", f)
}
cat("\nDone: the figures are in", normalizePath("output"), "\n")
