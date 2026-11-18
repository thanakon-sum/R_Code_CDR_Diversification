# Figure 5 panels a-d: eight indicators of every portfolio in the High, Med, Low and vLow pathways.
# Numbers are the values; dot area grows with the magnitude of the value within each indicator (same scale in all panels).
# Fossil energy, energy for DACCS, total CDR and CCS are means over 2020-2100; a caller may set FIG5_ID and FIG5_WHEN
# to draw the same panels from the values in 2100.
if (!exists("FIG5_ID")) FIG5_ID <- "Fig5"
if (!exists("FIG5_WHEN")) FIG5_WHEN <- "mean 2020\u20132100"
d <- fig_data(FIG5_ID, c("a", "b", "c", "d"))
IND <- c(PolicyCost = "Policy cost, NPV at 5% (%)", Equity = "Intergenerational equity (%)",
         LandDisp = "Land displacement, 2020\u20132100 (Mha)", FoodCrop = "Food-crop area change, 2020\u20132100 (Mha)",
         Fossil = paste0("Fossil primary energy, ", FIG5_WHEN, " (EJ yr-1)"), DACCS = paste0("Energy for DACCS, ", FIG5_WHEN, " (EJ yr-1)"),
         CDR = paste0("Total CDR, ", FIG5_WHEN, " (GtCO2 yr-1)"), CCS = paste0("CCS, ", FIG5_WHEN, " (GtCO2 yr-1)"))
GROUP <- c(PolicyCost = "Economy", Equity = "Economy", LandDisp = "Land", FoodCrop = "Land", Fossil = "Energy", DACCS = "Energy",
           CDR = "CDR and CCS", CCS = "CDR and CCS")
# colour: each value against the Full-combo of the same pathway; better or worse only if the difference exceeds 5%
LOWER_BETTER <- c("PolicyCost", "LandDisp", "Fossil", "DACCS", "CDR", "CCS")  # equity and food-crop change: higher is better
CLS <- c(better = "Better than the Full-combo (>5%)", same = "Within 5% of the Full-combo",
         worse = "Worse than the Full-combo (>5%)", fc = "Full-combo")
col_cls <- setNames(c("#8EC1EA", "#DCDCDC", "#F3A35C", "#FFFFFF"), CLS)
# equity: negative values put more of the cost on the future generation, so a higher value is better
x <- d %>% left_join(d %>% filter(portfolio == FC) %>% select(panel, series, fcv = value), by = c("panel", "series")) %>%
  mutate(rel = ifelse(abs(fcv) > 1e-9, (value - fcv) / abs(fcv), ifelse(abs(value) > 1e-9, sign(value), 0)),
         rel = ifelse(series %in% LOWER_BETTER, rel, -rel),
         cls = case_when(portfolio == FC ~ CLS[["fc"]], rel < -0.05 ~ CLS[["better"]], rel > 0.05 ~ CLS[["worse"]],
                         TRUE ~ CLS[["same"]]),
         cls = factor(cls, CLS)) %>%
  mutate(ind = factor(series, rev(names(IND))), portfolio = factor(portfolio, P12)) %>%
  group_by(series) %>% mutate(a = abs(value), size = 0.12 + (a - min(a)) / (max(a) - min(a))) %>% ungroup() %>%
  mutate(label = case_when(series %in% c("CDR", "CCS", "PolicyCost") ~ formatC(value, format = "f", digits = 1),
                           series == "DACCS" & abs(value) < 9.95 ~ formatC(value, format = "f", digits = 1),
                           TRUE ~ formatC(round(value), format = "d", big.mark = ",")),
         label = sub("^-", "\u2212", label))
# row labels: a column of its own (group heading left, indicator name right, two lines each), drawn once per row of panels
ROWS <- rev(names(IND))  # bottom to top
IND2 <- c(PolicyCost = "Policy cost,<br>NPV at 5% (%)", Equity = "Intergenerational<br>equity (%)",
          LandDisp = "Land<br>displacement (Mha)", FoodCrop = "Food-crop<br>area change (Mha)",
          Fossil = "Fossil primary<br>energy (EJ yr-1)", DACCS = "Energy for<br>DACCS (EJ yr-1)",
          CDR = "Total CDR<br>(GtCO2 yr-1)", CCS = "CCS<br>(GtCO2 yr-1)")
WHEN_G <- if (grepl("mean", FIG5_WHEN)) "mean<br>2020\u20132100" else paste("in", FIG5_WHEN)
GROUP2 <- c(Economy = "<b>Economy</b><br>2020\u20132100", Land = "<b>Land</b><br>change<br>2020\u20132100",
            Energy = paste0("<b>Energy</b><br>", WHEN_G), "CDR and CCS" = paste0("<b>CDR and CCS</b><br>", WHEN_G))
BAND_MIN <- c(0.5, 4.5); BAND_MAX <- c(2.5, 6.5)  # shaded indicator groups (CDR and CCS, Land)
label_panel <- function() {
  lab <- data.frame(y = match(names(IND2), ROWS), text = md(unname(IND2)))
  grp <- data.frame(g = unique(GROUP)) %>% mutate(y = sapply(g, function(k) mean(match(names(GROUP)[GROUP == k], ROWS))),
                                                  text = md(GROUP2[g]))
  ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = BAND_MIN, ymax = BAND_MAX, fill = "#F3F3F3") +
    geom_richtext(data = lab, aes(1, y, label = text), hjust = 1, size = pt(FS_SMALL), family = "Helvetica", lineheight = 1.1,
                  fill = NA, label.colour = NA, label.padding = unit(c(0, 3, 0, 0), "pt")) +
    geom_richtext(data = grp, aes(0, y, label = text), hjust = 0, size = pt(FS_SMALL), family = "Helvetica", lineheight = 1.1,
                  fill = NA, label.colour = NA, label.padding = unit(c(0, 0, 0, 2), "pt")) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) + scale_y_continuous(limits = c(0.5, 8.5), expand = c(0, 0)) +
    theme_void() + theme(plot.margin = margin(2, 0, 2, 0))
}
dot_panel <- function(p, title) {
  y <- x %>% filter(panel == p) %>% mutate(pathway = paste0("<span style='font-size:8pt'>", p, "</span>\u2003", title))
  infeas <- setdiff(P12, as.character(y$portfolio))
  nfc <- match(FC, P12)
  ggplot(y, aes(portfolio, ind)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = BAND_MIN, ymax = BAND_MAX, fill = "#F3F3F3") +
    annotate("rect", xmin = nfc - 0.5, xmax = nfc + 0.5, ymin = -Inf, ymax = Inf, fill = "#FFF1BF") +       # Full-combo column
    (if (length(infeas)) annotate("text", x = match(infeas, P12), y = 4.5, label = "infeasible", angle = 90,
                                  size = pt(FS_SMALL), colour = "#9A9A9A")) +
    geom_point(data = y %>% filter(portfolio != FC), aes(size = size, fill = cls), shape = 21, colour = "white", stroke = 0) +
    geom_point(data = y %>% filter(portfolio == FC), aes(size = size, fill = cls), shape = 21, colour = "white", stroke = 0) +
    geom_text(aes(label = label), size = pt(5), colour = "black") +
    facet_wrap(~pathway) +
    scale_size_area(max_size = 6.2, guide = "none") +  # largest dot about as wide as one portfolio column: no overlap
    scale_fill_manual(values = col_cls, name = NULL, drop = FALSE) +
    scale_x_discrete(drop = FALSE) + scale_y_discrete(drop = FALSE, expand = expansion(add = 0.5)) +
    labs(x = NULL, y = NULL) +
    theme_paper() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.line = element_blank(),
                          axis.ticks = element_blank(), axis.text.y = element_blank(),
                          panel.border = element_rect(colour = "#4D4D4D", fill = NA, linewidth = 0.4),
                          strip.background = element_rect(fill = "#E6E6E6", colour = "#4D4D4D", linewidth = 0.4),
                          strip.text = element_markdown(size = FS, face = "bold", hjust = 0, margin = margin(2, 4, 2, 4)),
                          legend.position = "bottom", plot.margin = margin(2, 2, 2, 4))
}
p_a <- dot_panel("a", "High pathway")
p_b <- dot_panel("b", "Med pathway")
p_c <- dot_panel("c", "Low pathway")
p_d <- dot_panel("d", "vLow pathway")
fig5 <- label_panel() + p_a + p_b + label_panel() + p_c + p_d +
  plot_layout(ncol = 3, widths = c(0.40, 1, 1), guides = "collect") &
  theme(legend.position = "bottom") &
  guides(fill = guide_legend(override.aes = list(size = 3, colour = c("white", "white", "white", "#A6A6A6"), stroke = c(0, 0, 0, 0.3))))  # outline in the legend only
