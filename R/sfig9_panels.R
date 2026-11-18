# Supplementary Figure 9 panels: the eight Fig. 5 indicators normalised within each pathway (0 = best, 1 = worst),
# drawn as radar charts with straight edges (axes clockwise from the top)
d <- fig_data("SFig9")
AX <- c(CDR = "Total CDR", DACCS = "DACCS energy", Fossil = "Fossil energy", FoodCrop = "Less food-crop\nexpansion",
        LandDisp = "Land\ndisplacement", Equity = "Intergenerational\ninequity", PolicyCost = "Policy cost", CCS = "CCS")
ang <- setNames(pi / 2 - 2 * pi * (seq_along(AX) - 1) / length(AX), names(AX))
xy <- function(r, a) data.frame(x = r * cos(a), y = r * sin(a))
grid_rings <- do.call(rbind, lapply(c(0.25, 0.5, 0.75, 1), function(r) cbind(xy(r, c(ang, ang[1])), ring = r)))
spokes <- data.frame(x = 0, y = 0, xend = cos(ang), yend = sin(ang))
labs_ax <- data.frame(xy(1.13, ang), label = AX) %>%
  mutate(hjust = ifelse(abs(x) < 0.05, 0.5, ifelse(x > 0, 0, 1)), vjust = ifelse(abs(y) < 0.05, 0.5, ifelse(y > 0, 0, 1)))
radar_panel <- function(p, guide = "none") {
  v <- d %>% filter(panel == p) %>% select(portfolio, series, value) %>%
    complete(portfolio, series = names(AX), fill = list(value = 0)) %>%       # no DACCS in a portfolio = 0
    mutate(a = ang[series], x = value * cos(a), y = value * sin(a), k = match(series, names(AX))) %>%
    arrange(portfolio, k) %>% mutate(portfolio = factor(portfolio, P12))
  fc <- v %>% filter(portfolio == FC)
  ggplot() +
    geom_path(data = grid_rings, aes(x, y, group = ring), colour = "#CFCFCF", linewidth = 0.25) +
    geom_segment(data = spokes, aes(x, y, xend = xend, yend = yend), colour = "#CFCFCF", linewidth = 0.25) +
    annotate("text", x = 0.03, y = c(0.5, 1) + 0.03, label = c("0.5", "1"), hjust = 0, vjust = 0, size = pt(5), colour = "#5A5A5A") +
    geom_polygon(data = fc, aes(x, y), fill = "#BDBDBD", alpha = 0.45, colour = NA) +
    geom_polygon(data = v %>% filter(portfolio != FC), aes(x, y, colour = portfolio, group = portfolio), fill = NA,
                 linewidth = 0.35, key_glyph = "path") +
    geom_polygon(data = fc, aes(x, y, colour = portfolio), fill = NA, linewidth = 0.7, key_glyph = "path") +
    geom_text(data = labs_ax, aes(x, y, label = label, hjust = hjust, vjust = vjust), size = pt(FS_SMALL), lineheight = 1.15) +
    scale_colour_manual(values = col_port, limits = P12, drop = FALSE, name = "Portfolio", guide = guide) +
    coord_equal(xlim = c(-1.75, 1.75), ylim = c(-1.3, 1.3), clip = "off") +
    labs(title = unique(d$pathway[d$panel == p])) +
    theme_void(base_size = FS, base_family = "Helvetica") +
    theme(plot.title = element_text(size = FS, hjust = 0.5), legend.text = element_text(size = FS_SMALL),
          legend.title = element_text(size = FS), plot.margin = margin(3, 3, 3, 3),
          plot.background = element_rect(fill = "white", colour = NA))
}
panels <- list(radar_panel("a", guide_legend(nrow = 1, override.aes = list(linewidth = 0.8))), radar_panel("b"), radar_panel("c"), radar_panel("d"))
