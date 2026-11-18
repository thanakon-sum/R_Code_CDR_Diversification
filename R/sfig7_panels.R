# Supplementary Figure 7 panels: Biodiversity Intactness Index (BII).
# (a) BII in 2100 by region, Full-combo, Med pathway; (b) World BII: BaU, range across the portfolios other than the
# Full-combo (ribbon) and the Full-combo (dashed) in each pathway
d <- fig_data("SFig7")
pw4 <- c("High", "Med", "Low", "vLow")

cividis_r <- c("#FFEA46", "#BCAF6F", "#7C7B78", "#414D6B", "#00204D")  # light = low BII, dark = high BII (colour-vision safe)
p_a <- map_panel(d %>% filter(panel == "a"),
                 scale_fill_gradientn(colours = cividis_r, name = "BII (%)", breaks = seq(78, 90, 4), labels = num,
                                      na.value = "#E3E3E3"))

x <- d %>% filter(panel == "b")
rb <- x %>% filter(series %in% c("range_min", "range_max")) %>% pivot_wider(names_from = series, values_from = value) %>%
  mutate(pathway = factor(pathway, pw4))
ln <- x %>% filter(series == "full_combo") %>% mutate(pathway = factor(pathway, pw4))
bau <- x %>% filter(series == "BaU")
p_b <- ggplot() +
  geom_ribbon(data = rb, aes(year, ymin = range_min, ymax = range_max, fill = pathway), alpha = 0.25) +
  geom_line(data = bau, aes(year, value, linetype = "BaU"), colour = "black", linewidth = 0.55) +
  geom_line(data = ln, aes(year, value, colour = pathway, linetype = "Full-combo"), linewidth = 0.55) +
  scale_fill_manual(values = col_path, limits = pw4, name = "Pathway (range across<br>the other portfolios)") +
  scale_colour_manual(values = col_path, limits = pw4, guide = "none") +
  scale_linetype_manual(values = c("BaU" = "solid", "Full-combo" = "42"), name = NULL,
                        labels = c("BaU" = "BaU (no climate policy)", "Full-combo" = "Full-combo (A+B+D+O)")) +
  guides(fill = guide_legend(order = 1, override.aes = list(alpha = 0.4)),
         linetype = guide_legend(order = 2, override.aes = list(colour = "black"))) +
  scale_x_continuous(breaks = seq(2010, 2100, 20), expand = expansion(add = c(2.5, 0))) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
  labs(x = NULL, y = "Biodiversity Intactness Index, World (%)") + theme_paper() +
  theme(legend.position = "right")
