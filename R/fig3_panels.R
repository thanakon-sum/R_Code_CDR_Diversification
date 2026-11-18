# Figure 3 panels: (a-d) global land area, (e-h) land-use change from 2030 in the High pathway,
# (i-m) regional land-use change 2030-2100 and (n) food-crop price change 2010-2100 (Full-combo, High pathway)
d <- fig_data("Fig3")
pw4 <- c("High", "Med", "Low", "vLow")

land_panel <- function(p, title) {
  x <- d %>% filter(panel == p)
  rb <- x %>% filter(series %in% c("range_min", "range_max")) %>% pivot_wider(names_from = series, values_from = value) %>%
    mutate(pathway = factor(pathway, pw4))
  ln <- x %>% filter(series == "full_combo") %>% mutate(pathway = factor(pathway, pw4))
  bau <- x %>% filter(series == "BaU")
  ggplot() +
    geom_ribbon(data = rb, aes(year, ymin = range_min, ymax = range_max, fill = pathway), alpha = 0.25) +
    geom_line(data = bau, aes(year, value, colour = "BaU", linetype = "BaU"), linewidth = 0.5) +
    geom_line(data = ln, aes(year, value, colour = pathway, linetype = "Full-combo"), linewidth = 0.55) +
    scale_fill_manual(values = col_path, limits = pw4, name = "Pathway (range across portfolios)") +
    scale_colour_manual(values = col_path, limits = c("BaU", pw4), name = NULL, guide = "none") +
    scale_linetype_manual(values = c("BaU" = "solid", "Full-combo" = "42"), name = NULL,
                          labels = c("BaU" = "BaU (no climate policy)", "Full-combo" = "Full-combo (A+B+D+O)")) +
    guides(fill = guide_legend(order = 1, override.aes = list(alpha = 0.4)),
           linetype = guide_legend(order = 2, override.aes = list(colour = "black"))) +
    scale_x_continuous(breaks = seq(2030, 2100, 20), expand = c(0, 0)) + scale_y_continuous(labels = num) +
    labs(x = NULL, y = "Area (Mha)", title = title) + theme_paper()
}
p_a <- land_panel("a", "Energy crops")
p_b <- land_panel("b", "Other natural land")
p_c <- land_panel("c", "Pasture")
p_d <- land_panel("d", "Food crops")

ch <- d %>% filter(panel %in% c("e", "f", "g", "h")) %>% mutate(series = factor(series, LAND))
lim <- ch %>% group_by(panel, year) %>% summarise(lo = sum(pmin(value, 0)), hi = sum(pmax(value, 0)), .groups = "drop")
ylim <- c(floor(min(lim$lo) / 200) * 200, ceiling(max(lim$hi) / 200) * 200)
bar_panel <- function(p, title) {
  ggplot(ch %>% filter(panel == p), aes(year, value, fill = series)) +
    geom_col(width = 4, linewidth = 0) + zero_line() +
    scale_fill_manual(values = col_land, limits = LAND, name = NULL) +
    scale_x_continuous(breaks = seq(2040, 2100, 20)) +
    scale_y_continuous(limits = ylim, breaks = seq(-2000, 2000, 500), labels = num) +
    labs(x = NULL, y = "Change from 2030 (Mha)", title = title) + theme_paper()
}
p_e <- bar_panel("e", "BaU")
p_f <- bar_panel("f", "Full-combo (A+B+D+O)")
p_g <- bar_panel("g", "BECCS only (B)")
p_h <- bar_panel("h", "DACCS only (D)")

mp <- function(p, title, type = "div") {
  v <- d %>% filter(panel == p)
  sc <- if (type == "log") fill_seq("#B35806", "%", low = "#FFF4DD", trans = "log10", breaks = c(30, 300, 3000))
        else fill_div(v$value, "%")
  map_panel(v, sc, title)
}
maps <- list(mp("i", "Energy crops, change 2030\u20132100", "log"), mp("j", "Food crops, change 2030\u20132100"),
             mp("k", "Pasture, change 2030\u20132100"), mp("l", "Forest, change 2030\u20132100"),
             mp("m", "Other natural land, change 2030\u20132100"), mp("n", "Food-crop price, change 2010\u20132100"))
