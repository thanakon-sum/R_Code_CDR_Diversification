# Supplementary Figure 2: cumulative net CO2 emissions since 2020 (Full-combo; every portfolio of a pathway follows the same path)
pw4 <- c("High", "Med", "Low", "vLow")
x <- fig_data("SFig2") %>% filter(series == "full_combo") %>% mutate(pathway = factor(pathway, pw4))
p_s5 <- ggplot(x, aes(year, value, colour = pathway, linetype = pathway)) +
  geom_hline(yintercept = 400, colour = "#8C8C8C", linetype = "42", linewidth = 0.3) +
  annotate("text", x = 2100, y = 385, label = "400~GtCO[2]", parse = TRUE, hjust = 1, vjust = 1, size = pt(FS_SMALL), colour = "#4D4D4D") +
  geom_line(linewidth = 0.6) +
  scale_colour_manual(values = col_path, limits = pw4, name = "Pathway") +
  scale_linetype_manual(values = c(High = "solid", Med = "42", Low = "4212", vLow = "12"), limits = pw4, name = "Pathway") +
  scale_x_continuous(breaks = seq(2020, 2100, 10), expand = expansion(add = c(1.5, 0))) +
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 200), labels = num, expand = c(0, 0)) +
  labs(x = NULL, y = md("Cumulative net CO2 emissions since 2020 (GtCO2)")) +
  guides(colour = guide_legend(override.aes = list(linewidth = 0.7))) +
  theme_paper() + theme(legend.position = "right", legend.key.width = unit(16, "pt"), plot.margin = margin(3, 8, 3, 3))
