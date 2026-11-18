# Supplementary Figure 14 panels: MAGICC v7.5.3 climate outcomes of the 38 runs (600-member ensemble)
d <- fig_data("SFig14")
pw4 <- c("High", "Med", "Low", "vLow")
ex <- d %>% filter(panel == "a-b") %>% select(pathway, portfolio, series, value) %>%
  pivot_wider(names_from = series, values_from = value) %>% mutate(pathway = factor(pathway, pw4))
ref_lab <- function(...) annotate("text", ..., size = pt(FS_SMALL), colour = "#4D4D4D")
ref_line <- function(v, dir) if (dir == "v") geom_vline(xintercept = v, colour = "#8C8C8C", linetype = "42", linewidth = 0.3) else
  geom_hline(yintercept = v, colour = "#8C8C8C", linetype = "42", linewidth = 0.3)
pct_axis <- function(lim, by) list(scale_x_continuous(limits = lim, breaks = seq(lim[1], lim[2], by), labels = function(x) paste0(x, "%"),
                                                      expand = expansion(add = 1.5)))
scatter <- function(xvar, xlab, xlim, xby, refs) {
  ggplot(ex, aes(.data[[xvar]], p_exc_15, fill = pathway)) +
    lapply(refs$x, ref_line, dir = "v") + ref_line(67, "h") +
    geom_point(shape = 21, size = 1.6, stroke = 0.25, colour = "black", alpha = 0.9) +
    ref_lab(x = refs$x + 0.6, y = 88.5, label = paste0(refs$x, "%"), hjust = 0, vjust = 1) +
    ref_lab(x = xlim[2] - 0.6, y = 67.8, label = "67%", hjust = 1, vjust = 0) +
    scale_fill_manual(values = col_path, limits = pw4, guide = "none") +
    pct_axis(xlim, xby) +
    scale_y_continuous(limits = c(60, 90), breaks = seq(60, 90, 5), labels = function(x) paste0(x, "%"),
                       expand = expansion(add = 1)) +
    labs(x = xlab, y = "P(warming > 1.5 \u00b0C during the 21st century)") + theme_paper() +
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.3), axis.line = element_blank(),
          plot.margin = margin(3, 10, 3, 3))
}
p_a <- scatter("p_exc_20", "P(warming > 2 \u00b0C during the 21st century)", c(0, 40), 10, list(x = c(10, 33)))
p_b <- scatter("p_exc_15_2100", "P(warming > 1.5 \u00b0C in 2100)", c(20, 60), 10, list(x = 50))

g <- d %>% filter(panel == "c") %>% mutate(pathway = factor(pathway, pw4))
rb <- g %>% filter(series %in% c("envelope_p5_min", "envelope_p95_max")) %>% select(pathway, year, series, value) %>%
  pivot_wider(names_from = series, values_from = value)
p_c <- ggplot() +
  geom_ribbon(data = rb, aes(year, ymin = envelope_p5_min, ymax = envelope_p95_max, fill = pathway), alpha = 0.12) +
  geom_hline(yintercept = c(1.5, 2), colour = "#4D4D4D", linetype = "42", linewidth = 0.3) +
  ref_lab(x = 2001, y = c(1.5, 2) + 0.03, label = c("1.5 \u00b0C", "2 \u00b0C"), hjust = 0, vjust = 0) +
  geom_line(data = g %>% filter(series == "gsat_p50"), aes(year, value, colour = pathway, group = paste(pathway, portfolio)),
            linewidth = 0.2, alpha = 0.7) +
  geom_line(data = g %>% filter(series == "pathway_median"), aes(year, value, colour = pathway), linewidth = 0.8) +
  scale_colour_manual(values = col_path, limits = pw4, name = "Pathway") +
  scale_fill_manual(values = col_path, limits = pw4, name = "Pathway") +
  scale_x_continuous(breaks = seq(2000, 2100, 10), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0.5, 2.5, 0.5), expand = expansion(mult = 0.02)) +
  labs(x = NULL, y = "Global mean surface air temperature (\u00b0C)") + theme_paper() +
  theme(legend.position = "bottom")
