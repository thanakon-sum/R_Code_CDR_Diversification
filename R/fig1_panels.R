# Figure 1 panels: (a) net CO2 constraint and temperature, (b) feasibility, (c) CDR by technology, (d-f) CDR, CCS and DACCS
d <- fig_data("Fig1")
pw4 <- c("High", "Med", "Low", "vLow")

# (a) left axis: net CO2 (GtCO2 yr-1); right axis: temperature, mapped linearly so that 0.7 degC -> -20 and 1.9 degC -> 50
k <- 70 / 1.2; to_y <- function(t) (t - 0.7) * k - 20; to_t <- function(y) (y + 20) / k + 0.7
ea <- d %>% filter(panel == "a", series == "net_co2_constraint") %>% mutate(pathway = factor(pathway, PATHWAYS))
ta <- d %>% filter(panel == "a", series == "gsat_median") %>% mutate(pathway = factor(pathway, PATHWAYS), y = to_y(value))
p_a <- ggplot() +
  geom_hline(yintercept = to_y(1.5), colour = "#8C8C8C", linetype = "42", linewidth = 0.3) +
  annotate("text", x = 2100, y = to_y(1.5) + 1.2, label = "1.5 \u00b0C", hjust = 1, vjust = 0, size = pt(FS_SMALL), colour = "#4D4D4D") +
  policy_line() + zero_line() +
  geom_line(data = ea, aes(year, value, colour = pathway, linetype = "Net CO2 emissions (left axis)"), linewidth = 0.55) +
  geom_line(data = ta, aes(year, y, colour = pathway, linetype = "Temperature, MAGICC median (right axis)"), linewidth = 0.55) +
  scale_colour_manual(values = col_path, breaks = PATHWAYS, name = "Pathway") +
  scale_linetype_manual(values = c("solid", "22"), name = NULL, labels = function(x) md(x)) +
  scale_x_continuous(breaks = seq(2000, 2100, 20), expand = expansion(mult = 0.01)) +
  scale_y_continuous(limits = c(-20, 50), breaks = seq(-20, 50, 10), labels = num,
                     sec.axis = sec_axis(to_t, breaks = seq(0.8, 1.8, 0.2), labels = scales::label_number(accuracy = 0.1),
                                         name = "Global mean surface air temperature (\u00b0C)")) +
  labs(x = NULL, y = md("Net CO2 emissions (GtCO2 yr-1)")) +
  guides(colour = guide_legend(order = 1, nrow = 1, override.aes = list(linewidth = 0.8)),
         linetype = guide_legend(order = 2, nrow = 1, override.aes = list(colour = "black", linewidth = 0.5))) +
  theme_paper() + theme(legend.position = "bottom", legend.box = "vertical", legend.spacing.y = unit(1, "pt"),
                        legend.key.width = unit(14, "pt"), axis.title.y.right = element_markdown(size = FS, angle = -90, margin = margin(l = 3)))

# (b) feasibility: portfolios in the order of panel c (the four portfolios that are never feasible first)
fb <- d %>% filter(panel == "b") %>% mutate(portfolio = factor(portfolio, rev(P16)), pathway = factor(pathway, PATHWAYS),
                                           status = ifelse(value == 1, "Feasible", "Infeasible"))
p_b <- ggplot(fb, aes(pathway, portfolio, fill = status)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  scale_fill_manual(values = c(Feasible = "#2C6E9B", Infeasible = "#E3E3E3"), name = NULL) +
  scale_x_discrete(position = "top", expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Pathway", y = "Portfolio") +
  theme_paper() + theme(axis.line = element_blank(), axis.ticks = element_blank(), legend.position = "bottom",
                        axis.title.x.top = element_markdown(size = FS, margin = margin(b = 2)))

# (c) annual CDR by technology, 2020-2100; dashed line = total removal; empty panel = infeasible
cc <- d %>% filter(panel == "c", series %in% TECH) %>%
  mutate(series = factor(series, rev(TECH)), portfolio = factor(portfolio, P12), pathway = factor(pathway, pw4))
tot <- d %>% filter(panel == "c", series == "total") %>% mutate(portfolio = factor(portfolio, P12), pathway = factor(pathway, pw4))
infeas <- expand.grid(portfolio = factor(P12, P12), pathway = factor(pw4, pw4)) %>%
  anti_join(tot %>% distinct(portfolio, pathway), by = c("portfolio", "pathway")) %>% mutate(year = 2060, value = 12, label = "Infeasible")
p_c <- ggplot(cc, aes(year, value)) +
  geom_area(aes(fill = series), position = "stack", linewidth = 0) +
  geom_line(data = tot, linetype = "22", linewidth = 0.3) +
  geom_text(data = infeas, aes(label = label), size = pt(FS_SMALL), colour = "#5A5A5A") +
  facet_grid(portfolio ~ pathway) +
  scale_fill_manual(values = col_tech, breaks = TECH, name = NULL) +
  scale_x_continuous(breaks = c(2040, 2080), limits = c(2020, 2100), expand = c(0, 0)) +
  scale_y_continuous(breaks = c(0, 10, 20), limits = c(0, 25), expand = c(0, 0)) +
  labs(x = NULL, y = md("CDR (GtCO2 yr-1)")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_paper() + theme(panel.border = element_rect(colour = "#9A9A9A", fill = NA, linewidth = 0.25), axis.line = element_blank(),
                        panel.spacing.x = unit(2.5, "pt"), panel.spacing.y = unit(1.5, "pt"), legend.position = "bottom",
                        strip.text.y.right = element_text(angle = 0, hjust = 0, size = FS_SMALL),
                        axis.text = element_text(size = 5, colour = "black"))

# (d-f) range across the feasible portfolios of each pathway (ribbon) and the Full-combo (dashed)
ribbon_panel <- function(p, ylab, ylim, key = FALSE) {
  x <- d %>% filter(panel == p)
  rb <- x %>% filter(series %in% c("range_min", "range_max")) %>% pivot_wider(names_from = series, values_from = value) %>%
    mutate(pathway = factor(pathway, pw4))
  ln <- x %>% filter(series == "full_combo") %>% mutate(pathway = factor(pathway, pw4))
  ggplot() + zero_line() +
    geom_ribbon(data = rb, aes(year, ymin = range_min, ymax = range_max, fill = pathway,
                               alpha = "Range across the feasible portfolios")) +
    geom_line(data = ln, aes(year, value, colour = pathway, linetype = "Full-combo (A+B+D+O)"), linewidth = 0.55) +
    scale_fill_manual(values = col_path, guide = "none") + scale_colour_manual(values = col_path, guide = "none") +
    scale_alpha_manual(values = 0.25, name = NULL, guide = if (key) "legend" else "none") +
    scale_linetype_manual(values = "42", name = NULL, guide = if (key) "legend" else "none") +
    (if (key) guides(alpha = guide_legend(order = 1, override.aes = list(fill = "#7A7A7A")),
                     linetype = guide_legend(order = 2, override.aes = list(colour = "black")))) +
    scale_x_continuous(breaks = seq(2030, 2100, 10), limits = c(2030, 2100), expand = expansion(add = c(2, 0))) +
    scale_y_continuous(labels = num, expand = expansion(mult = c(0, 0.02))) + coord_cartesian(ylim = ylim) +
    labs(x = NULL, y = md(ylab)) + theme_paper() +
    theme(legend.position = "bottom", legend.box = "vertical", legend.key.width = unit(14, "pt"), legend.spacing.y = unit(0, "pt"))
}
p_d <- ribbon_panel("d", "Total CDR (GtCO2 yr-1)", c(0, 25))
p_e <- ribbon_panel("e", "CCS (GtCO2 yr-1)", c(0, 30))
p_f <- ribbon_panel("f", "DACCS (GtCO2 yr-1)", c(0, 16), key = TRUE)
