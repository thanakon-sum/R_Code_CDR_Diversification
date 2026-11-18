# Supplementary Figure 3 panels: feasibility of the 16 portfolios in each pathway, one panel per SSP (SSP2 = Fig. 1b)
d <- fig_data("SFig3") %>% rename(ssp = region) %>%
  mutate(portfolio = factor(portfolio, rev(P16)), pathway = factor(pathway, PATHWAYS),
         status = factor(ifelse(value == 1, "Feasible", "Infeasible"), c("Feasible", "Infeasible")))
ssp_panel <- function(s, first = FALSE) {
  ggplot(d %>% filter(ssp == s), aes(pathway, portfolio, fill = status)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    scale_fill_manual(values = c(Feasible = "#2C6E9B", Infeasible = "#E3E3E3"), drop = FALSE, name = NULL) +
    scale_x_discrete(position = "top", expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
    labs(x = NULL, y = if (first) "Portfolio" else NULL, title = s) + theme_paper() +
    theme(axis.line = element_blank(), axis.ticks = element_blank(), legend.position = "bottom",
          axis.text.x.top = element_text(size = FS_SMALL, angle = 90, hjust = 0, vjust = 0.5),
          axis.text.y = if (first) element_text(size = FS_SMALL) else element_blank(),
          plot.title = element_markdown(size = FS, hjust = 0.5, margin = margin(b = 2)))
}
panels <- lapply(paste0("SSP", 1:5), function(s) ssp_panel(s, s == "SSP1"))
