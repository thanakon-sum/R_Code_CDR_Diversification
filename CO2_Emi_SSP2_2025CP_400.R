library(dplyr)
library(ggplot2)
library(readr)
library(patchwork)  # ✅ Needed to combine plots

# Load the data
data <- read_delim("/Users/suku/Downloads/dice_2025CP_400.csv", delim = "\t")

# Filter only CO2 emissions and limit to year <= 2100
co2_data <- data %>%
  filter(Emission == "CO2", Year <= 2100) %>%
  mutate(Value = Value / 1000)  # Convert to GtCO₂

# Reorder the scenarios
scenario_levels <- c(
  "SSP2-2025CP-CO2T-0400-vhigh",
  "SSP2-2025CP-CO2T-0400-high",
  "SSP2-2025CP-CO2T-0400-med",
  "SSP2-2025CP-CO2T-0400-low",
  "SSP2-2025CP-CO2T-0400-vlow"
)

co2_data$Scenario <- factor(co2_data$Scenario, levels = scenario_levels)

# Subset just the year 2100 for annotation
highlight_2100 <- co2_data %>% filter(Year == 2100)

# Define custom colors
custom_colors <- c("#7F8C8D", "#1ABC9C", "#F1C40F", "#3498DB", "#E74C3C")
names(custom_colors) <- scenario_levels

# --- Line Plot ---
line_plot <- ggplot(co2_data, aes(x = Year, y = Value, color = Scenario, group = Scenario)) +
  geom_line(size = 1.3) +
  geom_point(color = "white", size = 2.2, shape = 21, stroke = 0.8) +  # White balls for regular points
  geom_point(data = highlight_2100, aes(color = Scenario), shape = 18, size = 4) +  # Diamonds at 2100
  geom_text(data = highlight_2100, aes(label = round(Value, 1), color = Scenario), 
            vjust = -1, size = 5, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(2000, 2100, by = 10)) +
  scale_y_continuous(breaks = seq(-60, ceiling(max(co2_data$Value)), by = 5)) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = expression("CO"[2]*" Emissions by Scenario"),
    x = "Year",
    y = expression("CO"[2]*" Emissions (GtCO"[2]*"/yr)"),
    color = "Scenario"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray85", linewidth = 0.5),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(ncol = 1))

# --- Cumulative Emission Boxplot ---
box_plot <- ggplot(co2_data, aes(x = Scenario, y = Value, fill = Scenario)) +
  geom_boxplot(outlier.shape = 21, outlier.fill = "white", outlier.size = 2) +
  scale_fill_manual(values = custom_colors) +
  labs(
    title = expression("CO"[2]*" Emissions Distribution (2000–2100)"),
    x = NULL, y = expression("GtCO"[2]*"/yr")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  )

# --- Combine horizontally and share legend ---
final_plot <- line_plot + box_plot +
  plot_layout(guides = "collect", widths = c(2, 1))

# Show
print(final_plot)

ggsave("co2_emission_summary.png", plot = final_plot, width = 14, height = 7, dpi = 300)