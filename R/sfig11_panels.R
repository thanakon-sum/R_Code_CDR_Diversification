# Supplementary Figure 11 panels: annual CDR by technology of the Full-combo portfolio, 2020-2100, in the four sensitivity
# cases (rows) and the four pathways (columns); dashed line = total removal.
d <- fig_data("SFig11") %>% separate(series, c("tech", "case"), sep = "\\|")
pw4 <- c("High", "Med", "Low", "vLow")
CASES <- c("DACCS Optimistic", "DACCS Pessimistic", "EW Optimistic", "EW Pessimistic")
d <- d %>% mutate(tech = factor(tech, rev(TECH)), pathway = factor(pathway, pw4),
                  case = factor(case, CASES, labels = c("DACCS<br>optimistic", "DACCS<br>pessimistic", "EW<br>optimistic", "EW<br>pessimistic")))
tot <- d %>% group_by(case, pathway, year) %>% summarise(value = sum(value), .groups = "drop")
p_s9 <- ggplot(d, aes(year, value)) +
  geom_area(aes(fill = tech), position = "stack", linewidth = 0) +
  geom_line(data = tot, linetype = "22", linewidth = 0.3) +
  facet_grid(case ~ pathway) +
  scale_fill_manual(values = col_tech, breaks = TECH, name = NULL) +
  scale_x_continuous(breaks = seq(2030, 2090, 30), limits = c(2020, 2100), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 30, 10), limits = c(0, 30), expand = c(0, 0)) +  # vLow with cheaper DACCS reaches 27 Gt
  labs(x = NULL, y = md("CDR (GtCO2 yr-1)")) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_paper() + theme(panel.border = element_rect(colour = "#9A9A9A", fill = NA, linewidth = 0.25), axis.line = element_blank(),
                        panel.spacing = unit(4, "pt"), legend.position = "bottom",
                        strip.text = element_markdown(size = FS, face = "bold"),
                        strip.text.y.right = element_markdown(angle = 0, hjust = 0, size = FS))
