# Supplementary Figure 13 panels: feasibility indicators of the four pathways (World).
# Ribbon = 10th-90th percentile of the portfolios other than the Full-combo; dashed line = Full-combo (A+B+D+O);
# shaded zones = medium and high feasibility concern (thresholds of IPCC AR6 WGIII, Annex III).
d <- fig_data("SFig13")
pw4 <- c("High", "Med", "Low", "vLow")
col_med <- "#D6EAF5"; col_high <- "#E6DAF0"; line_med <- "#4A90C2"; line_high <- "#7B4FA6"

# panel title, axis unit, and whether the concern zones lie above (TRUE) or below (FALSE) the thresholds
S7 <- list(
  a = list("Biomass primary energy", "EJ yr-1", TRUE), b = list("Wind primary energy", "EJ yr-1", TRUE),
  c = list("Solar primary energy", "EJ yr-1", TRUE), d = list("Cumulative CCS since 2025", "GtCO2", TRUE),
  e = list("GDP loss", "%", TRUE), f = list("Carbon price (NPV at 5%)", "US$2010 tCO2-1", TRUE),
  g = list("Energy investment", "Ratio to BaU", TRUE), h = list("Wind scale-up", "pp per 10 yr", TRUE),
  i = list("Solar scale-up", "pp per 10 yr", TRUE), j = list("Nuclear scale-up", "pp per 10 yr", TRUE),
  k = list("BECCS", "GtCO2 yr-1", TRUE), l = list("Biofuels in transport", "pp per 10 yr", TRUE),
  m = list("Electricity in transport", "pp per 10 yr", TRUE), n = list("Hydrogen in transport", "pp per 10 yr", TRUE),
  o = list("DAC electricity share", "pp per 10 yr", TRUE), p = list("Fossil CCS", "GtCO2 yr-1", TRUE),
  q = list("Total energy demand", "% per 10 yr", FALSE), r = list("Industry energy demand", "% per 10 yr", FALSE),
  s = list("Transport energy demand", "% per 10 yr", FALSE), t = list("Buildings energy demand", "% per 10 yr", FALSE),
  u = list("Livestock share of food demand", "pp per 10 yr", FALSE), v = list("Forest cover", "% per 10 yr", TRUE),
  w = list("Pasture cover", "% per 10 yr", FALSE))

s7_panel <- function(p) {
  s <- S7[[p]]; x <- d %>% filter(panel == p)
  rb <- x %>% filter(series %in% c("other_p10", "other_p90")) %>% pivot_wider(names_from = series, values_from = value) %>%
    mutate(pathway = factor(pathway, pw4))
  ln <- x %>% filter(series == "full_combo") %>% mutate(pathway = factor(pathway, pw4))
  tm <- x$value[x$series == "threshold_medium"]; th <- x$value[x$series == "threshold_high"]
  # shown range: the data and 0, plus the thresholds that lie within five times the largest absolute data value
  dv <- c(rb$other_p10, rb$other_p90, ln$value, 0)
  shown <- c(tm, th)[abs(c(tm, th)) <= 5 * max(abs(dv))]
  yl <- range(c(dv, shown)); yl <- yl + c(-1, 1) * 0.04 * diff(yl)
  off <- setdiff(c(tm, th), shown)
  title <- s[[1]]
  if (length(off)) title <- paste0(title, "<br><span style='font-size:5pt'>", if (length(off) == 2) "thresholds " else "threshold ",
                                   paste(num(off), collapse = " and "), if (length(off) == 2) " lie " else " lies ",
                                   if (off[1] > yl[2]) "above" else "below", " the axis</span>")
  g <- ggplot()
  if (s[[3]]) g <- g + annotate("rect", xmin = -Inf, xmax = Inf, ymin = tm, ymax = th, fill = col_med) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = th, ymax = Inf, fill = col_high)
  else g <- g + annotate("rect", xmin = -Inf, xmax = Inf, ymin = th, ymax = tm, fill = col_med) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = th, fill = col_high)
  g + geom_hline(yintercept = tm, colour = line_med, linetype = "22", linewidth = 0.3) +
    geom_hline(yintercept = th, colour = line_high, linetype = "22", linewidth = 0.3) +
    zero_line() +
    geom_ribbon(data = rb, aes(year, ymin = other_p10, ymax = other_p90, fill = pathway), alpha = 0.3) +
    geom_line(data = ln, aes(year, value, colour = pathway), linetype = "42", linewidth = 0.5) +
    scale_fill_manual(values = col_path, limits = pw4, guide = "none") +
    scale_colour_manual(values = col_path, limits = pw4, guide = "none") +
    scale_x_continuous(breaks = seq(2040, 2100, 20), expand = expansion(add = c(2.5, 0))) +
    scale_y_continuous(labels = num) + coord_cartesian(ylim = yl, expand = FALSE) +
    labs(x = NULL, y = md(s[[2]]), title = title, tag = p) + theme_paper() +
    theme(plot.title = element_markdown(size = FS, hjust = 0, margin = margin(b = 2), lineheight = 1.1))
}
panels <- lapply(names(S7), s7_panel)
names(panels) <- names(S7)

# row labels (indicator groups of IPCC AR6 WGIII Annex III)
row_label <- function(txt) ggplot() + annotate("text", x = 0, y = 0, label = txt, angle = 90, size = pt(FS), fontface = "bold",
                                               lineheight = 1.15) +
  coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) + theme_void()

# legend drawn as a small key panel (pathway colours, ribbon, line, concern zones)
leg <- local({
  y <- c(9.5, 8.6, 7.7, 6.8); kx <- c(0, 0.9)
  g <- ggplot()
  for (k in seq_along(pw4)) g <- g +
    annotate("rect", xmin = kx[1], xmax = kx[2], ymin = y[k] - 0.32, ymax = y[k] + 0.32, fill = col_path[[pw4[k]]], alpha = 0.3) +
    annotate("segment", x = kx[1], xend = kx[2], y = y[k], yend = y[k], colour = col_path[[pw4[k]]], linetype = "42", linewidth = 0.5) +
    annotate("text", x = 1.15, y = y[k], label = pw4[k], hjust = 0, size = pt(FS_SMALL))
  g + annotate("rect", xmin = kx[1], xmax = kx[2], ymin = 4.9, ymax = 5.5, fill = "#7A7A7A", alpha = 0.3) +
    annotate("text", x = 1.15, y = 5.2, label = "Other portfolios, 10th\u2013\n90th percentile", hjust = 0, size = pt(FS_SMALL),
             lineheight = 1.15) +
    annotate("segment", x = kx[1], xend = kx[2], y = 3.75, yend = 3.75, linetype = "42", linewidth = 0.5) +
    annotate("text", x = 1.15, y = 3.75, label = "Full-combo (A+B+D+O)", hjust = 0, size = pt(FS_SMALL)) +
    annotate("rect", xmin = kx[1], xmax = kx[2], ymin = 2.5, ymax = 3.1, fill = col_med) +
    annotate("segment", x = kx[1], xend = kx[2], y = 2.5, yend = 2.5, colour = line_med, linetype = "22", linewidth = 0.3) +
    annotate("text", x = 1.15, y = 2.8, label = "Medium concern", hjust = 0, size = pt(FS_SMALL)) +
    annotate("rect", xmin = kx[1], xmax = kx[2], ymin = 1.4, ymax = 2.0, fill = col_high) +
    annotate("segment", x = kx[1], xend = kx[2], y = 1.4, yend = 1.4, colour = line_high, linetype = "22", linewidth = 0.3) +
    annotate("text", x = 1.15, y = 1.7, label = "High concern", hjust = 0, size = pt(FS_SMALL)) +
    annotate("text", x = 0, y = 10.3, label = "Pathway", hjust = 0, size = pt(FS)) +
    coord_cartesian(xlim = c(-0.2, 9), ylim = c(1, 10.8), expand = FALSE) + theme_void()
})
