# Supplementary Figure 8 panels: economic damages from climate change (% of GDP), from the climate-damage emulators.
# (a) World, by damage sector: primary emulator for the Full-combo of each pathway (line), 10th-90th percentile of eight
#     other emulators applied to the pathway mean (ribbon), and BaU (dashed); (b) total damage in 2100 by region and
# (c) damage by sector and region, Full-combo, High pathway; line colour = mean cooling and heating damage in 2080-2100
d <- fig_data("SFig8")
pw4 <- c("High", "Med", "Low", "vLow")
SECT <- c("Total", "Agricultural productivity", "Undernourishment", "Heat mortality", "Cooling and heating demand",
          "Occupational health", "Hydropower", "Thermal power", "Fluvial flood", "Coastal inundation")
SECT_LAB <- setNames(c("Total", "Agricultural<br>productivity", "Undernourishment", "Heat mortality", "Cooling and heating",
                       "Occupational health", "Hydropower", "Thermal power", "Fluvial flood", "Coastal inundation"), SECT)
REG_NAME <- c(USA = "United States", XE25 = "European Union", XER = "Rest of Europe", TUR = "Turkey",
              XOC = "Australia and New Zealand", CHN = "China", IND = "India", JPN = "Japan",
              XSE = "Rest of East and South-East Asia", XSA = "Rest of Asia", CAN = "Canada", BRA = "Brazil",
              XLM = "Rest of Latin America", CIS = "Former Soviet Union", XME = "Middle East", XNF = "North Africa",
              XAF = "Sub-Saharan Africa")
sec_strip <- theme(strip.text = element_markdown(size = FS_SMALL, margin = margin(1, 1, 2, 1)),
                   axis.title.y = element_markdown(size = FS, angle = 90, margin = margin(r = 9)))

# (a)
a <- d %>% filter(panel == "a") %>% separate(series, c("sector", "kind"), sep = "\\|") %>%
  mutate(sector = factor(sector, SECT, SECT_LAB[SECT]))
rb <- a %>% filter(kind %in% c("emulators_p10", "emulators_p90")) %>% select(sector, pathway, year, kind, value) %>%
  pivot_wider(names_from = kind, values_from = value) %>% mutate(pathway = factor(pathway, pw4))
ln <- a %>% filter(kind == "full_combo") %>% mutate(pathway = factor(pathway, pw4))
bau <- a %>% filter(kind == "BaU")
p_a <- ggplot() + zero_line() +
  geom_ribbon(data = rb, aes(year, ymin = emulators_p10, ymax = emulators_p90, fill = pathway,
                            alpha = "Eight other emulators<br>(10th\u201390th percentile)")) +
  geom_line(data = bau, aes(year, value, linetype = "BaU"), colour = "black", linewidth = 0.45) +
  geom_line(data = ln, aes(year, value, colour = pathway, linetype = "Full-combo"), linewidth = 0.5) +
  facet_wrap(~sector, ncol = 5, scales = "free_y") +
  scale_fill_manual(values = col_path, limits = pw4, name = "Pathway") +
  scale_colour_manual(values = col_path, limits = pw4, name = "Pathway") +
  scale_linetype_manual(values = c("BaU" = "22", "Full-combo" = "solid"), name = NULL,
                        labels = c("BaU" = "BaU (no climate policy)", "Full-combo" = "Full-combo, primary emulator")) +
  scale_alpha_manual(values = 0.22, name = NULL) +
  guides(colour = guide_legend(order = 1), fill = guide_legend(order = 1),
         linetype = guide_legend(order = 2, override.aes = list(colour = "black")),
         alpha = guide_legend(order = 3, override.aes = list(fill = "#7A7A7A"))) +
  scale_x_continuous(breaks = c(2020, 2060, 2100), limits = c(2005, 2100), expand = c(0, 0)) +
  scale_y_continuous(labels = num) +
  labs(x = NULL, y = "Damage (% of GDP)") + theme_paper() + sec_strip +
  theme(panel.spacing.x = unit(5, "pt"), legend.position = "right", legend.box = "vertical",
        legend.box.just = "left", legend.spacing.y = unit(4, "pt"))
leg_a <- gtable::gtable_filter(ggplotGrob(p_a), "guide-box")
p_a <- p_a + theme(legend.position = "none")
# (b)
p_b <- map_panel(d %>% filter(panel == "b"), fill_seq("#8C2D04", "% of GDP", low = "#FFF5EB"))
p_b <- p_b + theme(legend.position = "inside", legend.position.inside = c(0.04, 0.06), legend.justification = c(0, 0))  # colour bar in the empty South Pacific, so the map can be larger

# (c)
cc <- d %>% filter(panel == "c")
ch <- cc %>% filter(series == "Cooling and heating demand", year >= 2080) %>% group_by(region) %>%
  summarise(ch = mean(value), .groups = "drop") %>% arrange(ch) %>%
  mutate(lab = paste0(REG_NAME[region], " (", sub("^-", "\u2212", sprintf("%+.3f", ch)), ")"))
cc <- cc %>% mutate(sector = factor(series, SECT, SECT_LAB[SECT]), lab = factor(ch$lab[match(region, ch$region)], ch$lab))
# blue = net heating savings (negative), orange = net cooling costs (positive); shade by rank within each sign
neg <- ch$ch < 0
col_reg <- setNames(character(nrow(ch)), ch$lab)
col_reg[neg] <- scales::gradient_n_pal(c("#08306B", "#6BAED6"))(seq(0, 1, length.out = sum(neg)))
col_reg[!neg] <- scales::gradient_n_pal(c("#FDAE6B", "#E6550D", "#7F2704"))(seq(0, 1, length.out = sum(!neg)))
p_c <- ggplot(cc, aes(year, value, colour = lab)) + zero_line() + geom_line(linewidth = 0.35) +
  facet_wrap(~sector, ncol = 5, scales = "free_y") +
  scale_colour_manual(values = col_reg, name = "Region (mean cooling and heating<br>damage 2080\u20132100, % of GDP)") +
  scale_x_continuous(breaks = c(2020, 2060, 2100), limits = c(2005, 2100), expand = c(0, 0)) +
  scale_y_continuous(labels = num) +
  labs(x = NULL, y = "Damage (% of GDP)") + theme_paper() + sec_strip +
  theme(legend.position = "right", legend.key.height = unit(6.5, "pt"), legend.key.width = unit(10, "pt"),
        panel.spacing.x = unit(5, "pt")) +
  guides(colour = guide_legend(ncol = 1, override.aes = list(linewidth = 0.8)))
