# Supplementary Figure 12 panels: change of the eight Fig. 5 indicators of the Full-combo portfolio when the DACCS (a) or the
# enhanced-weathering (b) cost and capacity assumptions are optimistic or pessimistic, relative to the default assumptions.
# Bars are cut at -100% and +100%; the number gives the full change. "n/a": the default value is zero.
d <- fig_data("SFig12", c("a", "b"))
pw4 <- c("High", "Med", "Low", "vLow")
IND8 <- c(FoodCrop = "Food-crop area change", DACCS = "DACCS energy", Fossil = "Fossil energy", PolicyCost = "Policy cost",
          CCS = "CCS", LandDisp = "Land displacement", CDR = "Total CDR", Equity = "Intergenerational equity")
col_case <- c(Optimistic = "#0072B2", Pessimistic = "#E69F00")
lab_pct <- function(v) {
  s <- ifelse(abs(v) >= 99.95, sprintf("%+.0f%%", v), sprintf("%+.1f%%", v))
  s <- sub("^-", "\u2212", s)
  ifelse(abs(v) < 0.05, "0.0%", s)
}

s8_panel <- function(p, tag) {
  x <- d %>% filter(panel == p) %>%
    separate(series, c("ind", "case"), sep = "\\|") %>%
    mutate(ind = factor(IND8[ind], rev(IND8)), case = factor(case, names(col_case)), pathway = factor(pathway, pw4),
           y = as.numeric(ind) + ifelse(case == "Optimistic", 0.2, -0.2),
           bar = pmax(pmin(value, 100), -100),
           label = ifelse(is.na(value), "n/a", lab_pct(value)),
           lx = ifelse(is.na(value) | value >= 0, pmax(bar, 0, na.rm = TRUE) + 4, pmin(bar, 0, na.rm = TRUE) - 4),
           hj = ifelse(is.na(value) | value >= 0, 0, 1))
  bands <- data.frame(y = seq(2, length(IND8), 2))
  ggplot(x) +
    geom_rect(data = bands, aes(xmin = -Inf, xmax = Inf, ymin = y - 0.5, ymax = y + 0.5), fill = "#F2F2F2") +
    geom_vline(xintercept = c(-100, 100), colour = "#9A9A9A", linetype = "12", linewidth = 0.25) +
    geom_rect(aes(xmin = 0, xmax = bar, ymin = y - 0.17, ymax = y + 0.17, fill = case), na.rm = TRUE) +
    geom_vline(xintercept = 0, colour = "black", linewidth = 0.3) +
    geom_text(aes(lx, y, label = label, hjust = hj), size = pt(5), colour = "black") +
    facet_wrap(~pathway, ncol = 2) +
    scale_fill_manual(values = col_case, name = NULL,
                      labels = c(Optimistic = "Optimistic (lower cost, relaxed deployment limits)",
                                 Pessimistic = "Pessimistic (higher cost, stricter limits)")) +
    scale_x_continuous(limits = c(-150, 150), breaks = seq(-100, 100, 50), labels = function(b) paste0(num(b), "%"),
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = seq_along(IND8), labels = rev(IND8), limits = c(0.5, length(IND8) + 0.5), expand = c(0, 0)) +
    labs(x = "Change from the default assumptions", y = NULL, tag = tag) + theme_paper() +
    theme(panel.border = element_rect(colour = "#9A9A9A", fill = NA, linewidth = 0.25), axis.line = element_blank(),
          panel.spacing = unit(6, "pt"), axis.ticks.y = element_blank(),
          strip.text = element_markdown(size = FS, face = "bold", margin = margin(1, 1, 2, 1)),
          legend.position = "bottom")
}
p_a <- s8_panel("a", "a") + labs(title = "DACCS cost and capacity")
p_b <- s8_panel("b", "b") + labs(title = "Enhanced-weathering cost and capacity")
