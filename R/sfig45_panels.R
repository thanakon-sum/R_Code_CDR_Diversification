# Supplementary Figures 4 and 5: one box per pathway with every portfolio at a fixed horizontal offset (as in Fig. 2a, b)
pw4 <- c("High", "Med", "Low", "vLow")
box_dots_si <- function(fig, ylab, ylabels = num) {
  x <- fig_data(fig) %>% mutate(pathway = factor(pathway, pw4), portfolio = factor(portfolio, P12),
                                xo = as.numeric(pathway) + (as.numeric(portfolio) - 6.5) * 0.062)
  ggplot(x, aes(pathway, value)) +
    geom_boxplot(width = 0.75, colour = "#6E6E6E", fill = NA, linewidth = 0.3, outlier.shape = NA) +
    geom_point(aes(x = xo, colour = portfolio), size = 1.1) +
    scale_colour_manual(values = col_port, limits = P12, drop = FALSE, name = "Portfolio",
                        guide = guide_legend(nrow = 3, byrow = TRUE, override.aes = list(size = 1.8))) +
    scale_y_continuous(labels = ylabels) + labs(x = "Pathway", y = md(ylab)) + theme_paper() +
    theme(legend.position = "bottom", legend.title.position = "top")
}
