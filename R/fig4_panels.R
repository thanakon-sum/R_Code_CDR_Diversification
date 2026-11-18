# Figure 4 panels: (a-d) global primary, secondary and final energy and the residential electricity price,
# (e, f) primary and final energy mix in the vLow pathway, (g) DACCS share of secondary energy, (h-k) regional supply in 2100
d <- fig_data("Fig4")
pw4 <- c("High", "Med", "Low", "vLow")

energy_panel <- function(p, ylab) {
  x <- d %>% filter(panel == p)
  rb <- x %>% filter(series %in% c("range_min", "range_max")) %>% pivot_wider(names_from = series, values_from = value) %>%
    mutate(pathway = factor(pathway, pw4))
  ln <- x %>% filter(series == "full_combo") %>% mutate(pathway = factor(pathway, pw4))
  ggplot() + policy_line() +
    geom_ribbon(data = rb, aes(year, ymin = range_min, ymax = range_max, fill = pathway), alpha = 0.25) +
    geom_line(data = x %>% filter(series == "BaU"), aes(year, value, linetype = "BaU"), colour = "black", linewidth = 0.5) +
    geom_line(data = ln, aes(year, value, colour = pathway, linetype = "Full-combo"), linewidth = 0.55) +
    scale_fill_manual(values = col_path, limits = pw4, name = "Pathway (range across portfolios)") +
    scale_colour_manual(values = col_path, limits = pw4, guide = "none") +
    scale_linetype_manual(values = c("BaU" = "solid", "Full-combo" = "42"), name = NULL,
                          labels = c("BaU" = "BaU (no climate policy)", "Full-combo" = "Full-combo (A+B+D+O)")) +
    guides(fill = guide_legend(order = 1, override.aes = list(alpha = 0.4)), linetype = guide_legend(order = 2)) +
    scale_x_continuous(breaks = c(2030, 2060, 2090), expand = c(0, 0)) +
    scale_y_continuous(labels = num, limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(x = NULL, y = md(ylab)) + theme_paper()
}
p_a <- energy_panel("a", "Primary energy (EJ yr-1)")
p_b <- energy_panel("b", "Secondary energy (EJ yr-1)")
p_c <- energy_panel("c", "Final energy (EJ yr-1)")
p_d <- energy_panel("d", "Electricity price (US$2010 GJ-1)")

MIXP <- c("BaU" = "BaU", "A+B+D+O" = "Full-combo", "A+D+O" = "No BECCS (A+D+O)", "A+B+O" = "No DACCS (A+B+O)")
PRM <- c("Fossil w/o CCS", "Fossil w/ CCS", "Biomass w/o CCS", "Biomass w/ CCS", "Nuclear", "Non-biomass renewables")
col_prm <- c("Fossil w/o CCS" = "#3A3A3A", "Fossil w/ CCS" = "#9A9A9A", "Biomass w/o CCS" = "#DDCC77", "Biomass w/ CCS" = "#0072B2",
             "Nuclear" = "#CC79A7", "Non-biomass renewables" = "#88CCEE")
FIN <- c("Solids", "Liquids", "Gases", "Hydrogen", "Heat", "Electricity", "Energy for DACCS")
col_fin <- c("Solids" = "#5A5A5A", "Liquids" = "#DDAA33", "Gases" = "#BBBBBB", "Hydrogen" = "#332288", "Heat" = "#CC6677",
             "Electricity" = "#88CCEE", "Energy for DACCS" = "#D55E00")
mix_panel <- function(p, lev, cols, ylab) {
  x <- d %>% filter(panel == p) %>% mutate(series = factor(series, rev(lev)), case = factor(MIXP[portfolio], MIXP))
  ggplot(x, aes(year, value, fill = series)) + geom_col(width = 4, linewidth = 0) +
    facet_wrap(~case, nrow = 2) +
    scale_fill_manual(values = cols, breaks = lev, name = NULL) +
    scale_x_continuous(breaks = c(2025, 2060, 2095)) + scale_y_continuous(labels = num, expand = expansion(mult = c(0, 0.04))) +
    labs(x = NULL, y = md(ylab)) + guides(fill = guide_legend(ncol = 1)) +
    theme_paper() + theme(legend.position = "right", strip.text = element_markdown(size = FS_SMALL))
}
p_e <- mix_panel("e", PRM, col_prm, "Primary energy, vLow (EJ yr-1)")
p_f <- mix_panel("f", FIN, col_fin, "Final energy and DACCS input, vLow (EJ yr-1)")

DP <- P12[grepl("D", P12)]
g <- d %>% filter(panel == "g") %>% mutate(portfolio = factor(portfolio, DP), pathway = factor(pathway, pw4)) %>%
  complete(pathway, portfolio)
p_g <- ggplot(g, aes(pathway, value / 100, fill = portfolio)) +
  geom_col(position = position_dodge(width = 0.86), width = 0.8, linewidth = 0, na.rm = TRUE) +
  scale_fill_manual(values = col_port[DP], name = "Portfolio", drop = FALSE) +
  scale_y_continuous(labels = pct(accuracy = 1), expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "DACCS share (%)") +
  guides(fill = guide_legend(nrow = 1)) + theme_paper() + theme(legend.position = "bottom")

mp <- function(p, high, title) map_panel(d %>% filter(panel == p), fill_seq(high, md("EJ yr-1")), title)
maps <- list(mp("h", "#7A5C00", "Bioenergy (primary)"), mp("i", "#0B5394", "Non-biomass renewables (primary)"),
             mp("j", "#3A3A3A", "Fossil fuels with CCS (primary)"), mp("k", "#A23B00", "Energy used for DACCS (secondary)"))
