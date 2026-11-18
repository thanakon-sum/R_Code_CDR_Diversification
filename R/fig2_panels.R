# Figure 2 panels: (a) policy cost, (b) intergenerational equity, (c) GDP loss, (d) consumption loss,
# (e-h) carbon price by portfolio in each pathway (lines) and its distribution over 2025-2100 (boxes)
d <- fig_data("Fig2")
pw4 <- c("High", "Med", "Low", "vLow")
scale_port_col <- function(guide = "none") scale_colour_manual(values = col_port, limits = P12, drop = FALSE, name = "Portfolio", guide = guide)
scale_port_fill <- function() scale_fill_manual(values = col_port, limits = P12, drop = FALSE, guide = "none")

# (a, b) one box per pathway; each portfolio always at the same horizontal offset inside the box
box_dots <- function(p, ylab, ylabels = num, guide = "none") {
  x <- d %>% filter(panel == p) %>% mutate(pathway = factor(pathway, pw4), portfolio = factor(portfolio, P12),
                                          xo = as.numeric(pathway) + (as.numeric(portfolio) - 6.5) * 0.062)
  ggplot(x, aes(pathway, value)) +
    geom_boxplot(width = 0.75, colour = "#6E6E6E", fill = NA, linewidth = 0.3, outlier.shape = NA) +
    geom_point(aes(x = xo, colour = portfolio), size = 0.8) +
    scale_port_col(guide) + scale_y_continuous(labels = ylabels) +
    labs(x = NULL, y = ylab) + theme_paper()
}
p_a <- box_dots("a", "Policy cost, NPV at 5% (%)", scales::label_number(accuracy = 0.1),
                guide = guide_legend(nrow = 1, override.aes = list(size = 2)))
p_b <- box_dots("b", "Intergenerational equity (%)", scales::label_number(style_negative = "minus"))

# (c, d) range across the feasible portfolios (ribbon) and the Full-combo (dashed)
ribbon_panel <- function(p, ylab) {
  x <- d %>% filter(panel == p)
  rb <- x %>% filter(series %in% c("range_min", "range_max")) %>% pivot_wider(names_from = series, values_from = value) %>%
    mutate(pathway = factor(pathway, pw4))
  ln <- x %>% filter(series == "full_combo") %>% mutate(pathway = factor(pathway, pw4))
  ggplot() +
    geom_ribbon(data = rb, aes(year, ymin = range_min, ymax = range_max, fill = pathway), alpha = 0.25) +
    geom_line(data = ln, aes(year, value, colour = pathway), linetype = "42", linewidth = 0.55) +
    scale_fill_manual(values = col_path, limits = pw4, name = "Pathway", guide = if (p == "c") "legend" else "none") +
    scale_colour_manual(values = col_path, limits = pw4, name = "Pathway", guide = if (p == "c") "legend" else "none") +
    scale_x_continuous(breaks = seq(2030, 2100, 20), expand = c(0, 0)) + scale_y_continuous(labels = num) +
    labs(x = NULL, y = ylab) + theme_paper()
}
p_c <- ribbon_panel("c", "GDP loss (%)")
p_d <- ribbon_panel("d", "Consumption loss (%)")

# (e-h) carbon price
# (e-h) one y range for all four pathways and one y axis per row (on the left), with light grid lines to read across the row
CP_MAX <- max(d$value[d$panel %in% c("e", "f", "g", "h")], na.rm = TRUE)
cp_y <- scale_y_continuous(labels = num, limits = c(0, CP_MAX), breaks = seq(0, 2500, 500), expand = expansion(mult = c(0, 0.04)))
cp_grid <- theme(panel.grid.major.y = element_line(colour = "#E3E3E3", linewidth = 0.25))
no_y <- theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(), axis.title.y = element_blank())
cp <- function(p, axis = FALSE) {
  x <- d %>% filter(panel == p) %>% mutate(portfolio = factor(portfolio, P12))
  ln <- ggplot(x, aes(year, value, colour = portfolio)) + geom_line(linewidth = 0.4) +
    scale_port_col() + scale_x_continuous(breaks = seq(2030, 2100, 20), expand = c(0, 0)) + cp_y +
    labs(x = NULL, y = md("Carbon price (US$2010 tCO2-1)"), title = unique(x$pathway)) + theme_paper() + cp_grid
  bx <- ggplot(x, aes(portfolio, value, colour = portfolio, fill = portfolio)) +
    geom_boxplot(width = 0.7, linewidth = 0.3, alpha = 0.35, outlier.size = 0.25, outlier.stroke = 0.3) +
    scale_port_col() + scale_port_fill() + scale_x_discrete(drop = FALSE) + cp_y +
    labs(x = NULL, y = NULL) + theme_paper() + cp_grid + no_y +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5.2))
  list(if (axis) ln else ln + no_y + theme(plot.title = element_markdown(size = FS, hjust = 0, margin = margin(b = 2, l = 14))), bx)  # room for the tag f/h
}
p_e <- cp("e", axis = TRUE); p_f <- cp("f"); p_g <- cp("g", axis = TRUE); p_h <- cp("h")
