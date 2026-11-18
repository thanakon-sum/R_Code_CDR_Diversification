# Supplementary Figure 6 panels: (a) CO2 emissions by sector (BaU and Full-combo in the High and vLow pathways),
# (b-g) CDR by technology summed over 2025-2100 in each region (Full-combo, vLow pathway)
d <- fig_data("SFig6")
SECT <- c("AFOLU", "Energy demand", "Energy supply", "Industrial processes", "Product use", "CDR (other than afforestation and BECCS)")
col_sect <- c("AFOLU" = "#7F7F7F", "Energy demand" = "#DDAA33", "Energy supply" = "#332288", "Industrial processes" = "#CC79A7",
              "Product use" = "#88CCEE", "CDR (other than afforestation and BECCS)" = "#D55E00")
a <- d %>% filter(panel == "a") %>%
  mutate(case = factor(case_when(portfolio == "BaU" ~ "BaU", TRUE ~ paste0(pathway, ", Full-combo")),
                       c("BaU", "High, Full-combo", "vLow, Full-combo")))
p_a <- ggplot(a %>% filter(series != "Net CO2") %>% mutate(series = factor(series, rev(SECT))), aes(year, value)) +
  geom_col(aes(fill = series), width = 4, linewidth = 0) + zero_line() + policy_line() +
  geom_line(data = a %>% filter(series == "Net CO2"), aes(linetype = "Net CO2 emissions"), linewidth = 0.55) +
  facet_wrap(~case, nrow = 1) +
  scale_fill_manual(values = col_sect, breaks = SECT, name = NULL, labels = md) +
  scale_linetype_manual(values = "solid", name = NULL, labels = md) +
  scale_x_continuous(breaks = seq(2025, 2100, 25)) + scale_y_continuous(labels = num, breaks = seq(-20, 40, 10)) +
  labs(x = NULL, y = md("CO2 emissions (GtCO2 yr-1)")) +
  guides(fill = guide_legend(order = 1, ncol = 1), linetype = guide_legend(order = 2)) +
  theme_paper() + theme(legend.position = "right")

TECH6 <- c(b = "Biochar", c = "BECCS", d = "DACCS", e = "Soil carbon sequestration", f = "Enhanced weathering", g = "Afforestation")
map_hi <- c("Biochar" = "#8C7A1E", "BECCS" = "#0072B2", "DACCS" = "#D55E00", "Soil carbon sequestration" = "#2B83BA",
            "Enhanced weathering" = "#A0407F", "Afforestation" = "#3A3A3A")
maps <- lapply(names(TECH6), function(p) map_panel(d %>% filter(panel == p), fill_seq(map_hi[[TECH6[[p]]]], md("GtCO2"), limits = c(0, NA)),
                                                  TECH6[[p]]))
