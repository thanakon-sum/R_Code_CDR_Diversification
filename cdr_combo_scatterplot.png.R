library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(RColorBrewer)

# Load and clean
df <- read_csv("/Users/suku/actual_indicator_values_heatmap_with_equity.csv")

df <- df %>%
  mutate(
    Combo = str_extract(Row, "(?<=_).*"),
    Combo = str_replace_all(Combo, "_", "+"),
    Indicator = str_replace_all(Indicator, "\\\\n", ": "),
    Actual = ifelse(str_detect(Indicator, "GenEquity"), -Actual, Actual),
    Indicator = ifelse(str_detect(Indicator, "GenEquity"), "GenInequity \n (% Rel. Dif)", Indicator)
  )

# Add Category
df <- df %>%
  mutate(
    Category = case_when(
      str_detect(Indicator, "PolicyCost|GenInequity") ~ "Economic",
      str_detect(Indicator, "LandDisp|FoodCrop") ~ "Land",
      str_detect(Indicator, "DACCS|Fossil") ~ "Energy",
      str_detect(Indicator, "CDR|CCS") ~ "CDR/CCS",
      TRUE ~ "Other"
    )
  )

# Scale size within each indicator
df <- df %>%
  group_by(Indicator) %>%
  mutate(
    ScaledSize = (Actual - min(Actual, na.rm = TRUE)) /
      (max(Actual, na.rm = TRUE) - min(Actual, na.rm = TRUE)) + 0.05
  ) %>%
  ungroup()

# Define ColorGroup: only A+B+D+O get their Indicator's color
df <- df %>%
  mutate(
    ColorGroup = ifelse(Combo == "A+B+D+O", Indicator, "Other")
  )

# Prepare color palette
indicator_levels <- unique(df$Indicator)
palette_colors <- setNames(
  c("grey", brewer.pal(n = length(indicator_levels), name = "Dark2")),
  c("Other", indicator_levels)
)

# Set desired order of emission levels
df$Emission_Level <- factor(df$Emission_Level, levels = c("High", "Med", "Low", "vLow"))

# Plot
# Create grouped y-axis labels: "Category | Indicator"
df <- df %>%
  mutate(
    Indicator_Label = paste(Category, Indicator, sep = " | ")
  )

# Plot with Indicator_Label on y-axis
ggplot(df, aes(x = Combo, y = Indicator_Label, size = ScaledSize, color = ColorGroup)) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = round(Actual, 1)), vjust = 0, size = 3, color = "black") +  # ← label here
  scale_size_continuous(name = "Relative Size\n(within indicator)", range = c(4, 16)) +
  facet_wrap(~Emission_Level, ncol = 2) + 
  scale_color_manual(
    values = palette_colors,
    guide = guide_legend(override.aes = list(size = 5))
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.major.y = element_blank(),
    strip.text.y = element_text(angle = 0, face = "bold"),
    legend.position = "right"
  ) +
  labs(
    x = "CDR Combination",
    y = NULL,
    title = "CDR Portfolio Performance by Indicator Group",
    color = "Indicator (A+B+D+O only)"
  )
ggsave("cdr_combo_scatterplot.png", plot = p, width = 14, height = 8, dpi = 300)
