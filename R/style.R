# Shared look of all figures: data access, colours (colour-vision safe, no red-green pairs), text sizes, units and saving.
suppressMessages({library(ggplot2); library(dplyr); library(tidyr); library(patchwork); library(ggtext)})
update_geom_defaults("text", list(family = "Helvetica"))  # annotate() and geom_text() text in Helvetica as well

# ---- data: one long table with every number drawn (figure, panel, series, portfolio, pathway, region, year, value, unit)
fig_data <- local({
  d <- NULL
  function(fig, panel = NULL) {
    if (is.null(d)) d <<- read.csv("data/figure_data.csv", stringsAsFactors = FALSE, na.strings = "")
    x <- d[d$figure == fig, ]
    if (!is.null(panel)) x <- x[x$panel %in% panel, ]
    x
  }
})

# ---- pathways and portfolios
PATHWAYS <- c("vHigh", "High", "Med", "Low", "vLow")
P12 <- c("D", "A+D", "D+O", "A+D+O", "B", "A+B", "B+D", "B+O", "A+B+D", "B+D+O", "A+B+O", "A+B+D+O")  # one order everywhere
P16 <- c("No CDR", "O", "A", "A+O", P12)
FC <- "A+B+D+O"
col_path <- c(vHigh = "#7A7A7A", High = "#332288", Med = "#DDAA33", Low = "#33BBEE", vLow = "#CC3311", BaU = "#000000")
# portfolios: hue = backbone (blue = BECCS without DACCS, orange = DACCS without BECCS, grey = both), darker = fewer options
col_port <- c("D" = "#8C2D04", "A+D" = "#CC4C02", "D+O" = "#EC7014", "A+D+O" = "#FE9929",
              "B" = "#08306B", "A+B" = "#2171B5", "B+O" = "#4292C6", "A+B+O" = "#7FB4DC",
              "B+D" = "#4D4D4D", "A+B+D" = "#7A7A7A", "B+D+O" = "#A6A6A6", "A+B+D+O" = "#000000")[P12]
TECH <- c("BECCS", "DACCS", "Enhanced weathering", "Biochar", "Soil carbon sequestration", "Afforestation")  # bottom to top
col_tech <- c("BECCS" = "#0072B2", "DACCS" = "#D55E00", "Enhanced weathering" = "#CC79A7", "Biochar" = "#DDCC77",
              "Soil carbon sequestration" = "#56B4E9", "Afforestation" = "#7F7F7F")
LAND <- c("Energy crops", "Food crops", "Pasture", "Forest", "Other natural land", "Built-up area")
col_land <- c("Energy crops" = "#E69F00", "Food crops" = "#0072B2", "Pasture" = "#C2A5CF", "Forest" = "#1B5E20",
              "Other natural land" = "#44AA99", "Built-up area" = "#7F7F7F")
col_policy <- "#E7A2B8"   # start of climate policy (2025)

# ---- text: Nature Communications sizes (5-7 pt), Helvetica, bold lowercase panel letters
FS <- 6.5; FS_SMALL <- 5.6; FS_TAG <- 8
pt <- function(x) x / .pt  # font size in pt -> geom_text size
# units with a real subscript/superscript, e.g. md("Net CO2 emissions (GtCO2 yr-1)")
md <- function(x) {
  x <- gsub("CO2", "CO<sub>2</sub>", x, fixed = TRUE)
  x <- gsub("yr-1", "yr<sup>\u22121</sup>", x, fixed = TRUE)
  x <- gsub("tCO<sub>2</sub>-1", "tCO<sub>2</sub><sup>\u22121</sup>", x, fixed = TRUE)
  x <- gsub("GJ-1", "GJ<sup>\u22121</sup>", x, fixed = TRUE)
  x
}
num <- scales::label_number(style_negative = "minus", big.mark = ",")
pct <- function(...) scales::label_percent(style_negative = "minus", ...)
theme_paper <- function() {
  theme_classic(base_size = FS, base_family = "Helvetica") %+replace%
    theme(text = element_text(colour = "black", size = FS, family = "Helvetica", lineheight = 1.15),
          axis.text = element_text(size = FS_SMALL, colour = "black"),
          axis.title = element_markdown(size = FS), axis.title.y = element_markdown(size = FS, angle = 90, margin = margin(r = 2)),
          axis.title.x = element_markdown(size = FS, margin = margin(t = 2)),
          axis.line = element_line(linewidth = 0.3, colour = "black"), axis.ticks = element_line(linewidth = 0.3, colour = "black"),
          axis.ticks.length = unit(1.2, "pt"),
          legend.text = element_markdown(size = FS_SMALL, lineheight = 1.15), legend.title = element_markdown(size = FS, lineheight = 1.15),
          legend.key.size = unit(7, "pt"), legend.background = element_blank(), legend.key = element_blank(),
          legend.margin = margin(0, 0, 0, 0), legend.box.spacing = unit(3, "pt"),
          strip.text = element_markdown(size = FS, margin = margin(1, 1, 1, 1), lineheight = 1.15), strip.background = element_blank(),
          plot.title = element_markdown(size = FS, hjust = 0, margin = margin(b = 2)),
          plot.tag = element_text(size = FS_TAG, face = "bold", family = "Helvetica"),
          plot.margin = margin(3, 3, 3, 3), panel.background = element_rect(fill = "white", colour = NA),
          plot.background = element_rect(fill = "white", colour = NA))
}
tags <- function() theme(plot.tag = element_text(size = FS_TAG, face = "bold", family = "Helvetica"),
                         plot.tag.position = c(0, 1))
policy_line <- function() geom_vline(xintercept = 2025, colour = col_policy, linetype = "22", linewidth = 0.35)
zero_line <- function() geom_hline(yintercept = 0, colour = "black", linewidth = 0.25)

# ---- maps: AIM-Hub regions (country borders thin, Robinson projection, coordinates in km)
map_poly <- local({
  d <- NULL
  function() { if (is.null(d)) d <<- read.csv("data/map_polygons.csv", stringsAsFactors = FALSE, na.strings = ""); d }
})
map_panel <- function(vals, scale, title = NULL) {
  g <- map_poly() %>% left_join(vals %>% select(region, value), by = "region")
  ggplot(g, aes(x, y, group = piece, subgroup = ring, fill = value)) +
    geom_polygon(colour = "#9A9A9A", linewidth = 0.06) + scale +
    coord_equal(expand = FALSE, xlim = c(-12500, 16000), ylim = c(-6300, 8400)) + labs(title = title) +
    theme_void(base_size = FS, base_family = "Helvetica") +
    theme(legend.position = "right", legend.key.width = unit(4, "pt"), legend.key.height = unit(11, "pt"),
          legend.title = element_markdown(size = FS_SMALL, lineheight = 1.15), legend.text = element_text(size = FS_SMALL),
          plot.title = element_markdown(size = FS, hjust = 0, margin = margin(b = 1, l = 9)),
          plot.background = element_rect(fill = "white", colour = NA), plot.margin = margin(2, 2, 2, 2),
          plot.tag = element_text(size = FS_TAG, face = "bold", family = "Helvetica"))
}
fill_seq <- function(high, name, low = "#F7F7F7", ...) scale_fill_gradient(low = low, high = high, name = name, labels = num,
                                                                          na.value = "#E3E3E3", ...)
fill_div <- function(v, name, ...) scale_fill_gradient2(low = "#542788", mid = "#F7F7F7", high = "#B35806", midpoint = 0,
                                                       limits = c(-1, 1) * max(abs(v)), name = name, labels = num,
                                                       na.value = "#E3E3E3", ...)

# ---- saving: PNG (600 dpi, ragg) and PDF (Cairo, fonts embedded); width in mm (single column 89, double column 183)
save_fig <- function(plot, name, width_mm = 183, height_mm) {
  png <- file.path("output", paste0(name, ".png")); pdf <- file.path("output", paste0(name, ".pdf"))
  ggsave(png, plot, width = width_mm, height = height_mm, units = "mm", dpi = 600, bg = "white", device = ragg::agg_png)
  ggsave(pdf, plot, width = width_mm, height = height_mm, units = "mm", bg = "white", device = grDevices::cairo_pdf)
  if (nzchar(Sys.getenv("FIG_CHECK_DIR")) && requireNamespace("svglite", quietly = TRUE))  # text boxes for check_figures.R
    ggsave(file.path(Sys.getenv("FIG_CHECK_DIR"), paste0(name, ".svg")), plot, width = width_mm, height = height_mm,
           units = "mm", device = svglite::svglite)
  cat("Saved", png, "and", pdf, "\n")
}
