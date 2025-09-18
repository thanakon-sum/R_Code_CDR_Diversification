library(gdxrrw)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(ggtext)

# --- Step 1: GAMS Initialization ---
igdx("/Library/Frameworks/GAMS.framework/Versions/49/Resources")

# --- Step 2: Target Variables ---
target_vars <- c("Prc_Car_NPV_5pc", "Pol_Cos_Cns_Los_rat_NPV_5pc")

# --- Step 3: Read GDX Data Function ---
read_variable <- function(gdx_path, emission_label, vars) {
  rgdx.param(gdx_path, "IAMC_template") %>%
    filter(i2 == "World", i3 %in% vars) %>%
    rename(Scenario = i1, Region = i2, Parameter = i3, Year = i4, Value = value) %>%
    mutate(Year = as.numeric(as.character(Year)), Emission_Level = emission_label)
}

# # --- Step 4: Load GDX Files ---
 # high_data <- read_variable("/Users/suku/Downloads/Results/High/feas/gdx/global_17_merged.gdx", "High", target_vars)
 # med_data  <- read_variable("/Users/suku/Downloads/Results/Med/feas/gdx/global_17_merged.gdx",  "Med",  target_vars)
 # low_data  <- read_variable("/Users/suku/Downloads/Results/Low/feas/gdx/global_17_merged.gdx",  "Low",  target_vars)
 # vlow_data <- read_variable("/Users/suku/Downloads/Results/vLow/feas/gdx/global_17_merged.gdx", "vLow", target_vars)


data_all <- bind_rows(high_data, med_data, low_data, vlow_data)

# --- Step 5: Separate summaries for each variable ---
summary_carbon <- data_all %>%
  filter(Parameter == "Prc_Car_NPV_5pc", Year >= 2025 & Year <= 2100) %>%
  group_by(Scenario, Parameter, Emission_Level) %>%
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")

summary_policy <- data_all %>%
  filter(Parameter == "Pol_Cos_Cns_Los_rat_NPV_5pc", Year == 2100) %>%
  select(Scenario, Parameter, Emission_Level, Value)

# --- Combine summaries ---
summary_data <- bind_rows(summary_carbon, summary_policy)

# --- Step 6: Combo Map with Clean Labels ---
combo_map <- list(
  "B" = 1,
  "D" = 2,
  "A+B" = 3,
  "B+O" = 4,
  "D+O" = 5,
  "B+D" = 6,
  "A+D" = 7,
  "B+D+O" = 8,
  "A+D+O" = 9,
  "A+B+O" = 10,
  "A+B+D" = 11,
  "A+B+D+O" = 12
)

# Reverse mapping: 1 → B, 2 → D, etc.
combo_label_map <- setNames(names(combo_map), unlist(combo_map))

get_combo_label <- function(scn) {
  has <- function(x) grepl(x, scn, ignore.case = TRUE)
  used <- c()
  if (has("BECCS"))  used <- c(used, "B")
  if (has("DACCS"))  used <- c(used, "D")
  if (has("Affor"))  used <- c(used, "A")
  if (has("Other"))  used <- c(used, "O")
  if (length(used) == 0) return(NA_integer_)
  key <- paste0(sort(used), collapse = "+")
  combo_map[[key]]
}
# --- Updated combo label with numeric prefix ---
combo_label_map <- setNames(
  paste0(1:12, " = ", names(combo_map)),
  as.character(1:12)
)

# Apply to summary_data
summary_data <- summary_data %>%
  mutate(
    Combo = sapply(Scenario, get_combo_label),
    ComboLabel = factor(combo_label_map[as.character(Combo)],
                        levels = combo_label_map[as.character(1:12)])
  ) %>%
  filter(!is.na(Combo))

# --- Boxplot Data Preparation ---
box_data <- summary_data %>%
  mutate(
    ComboLabel = as.factor(ComboLabel),
    Emission_Level = factor(Emission_Level, levels = c("High", "Med", "Low", "vLow"))
  )

# --- Custom color palette ---
custom_hex_colors <- c(
  "#205080FF", "#4890C0FF", "#98E0F0FF", "#505020FF",
  "#B0A870FF", "#F85048FF", "#D0C090FF", "#808040FF",
  "#B02820FF", "#F8C0B8FF", "#D0D0D0FF", "#801010FF"
)
combo_colors <- setNames(custom_hex_colors, combo_label_map[as.character(1:12)])



library(patchwork)

plot_carbon_data <- summary_data %>%
  filter(Parameter == "Prc_Car_NPV_5pc") %>%
  mutate(Emission_Level = factor(Emission_Level, levels = c("High", "Med", "Low", "vLow")))

plot_carbon <- ggplot(plot_carbon_data, aes(x = Emission_Level, y = Value)) +
  geom_boxplot(
    alpha = 0.3, width = 0.6, color = "grey",
    outlier.shape = 21, outlier.size = 1.5, outlier.fill = "black",
    show.legend = FALSE
  ) +
  geom_point(aes(color = ComboLabel), size = 0, alpha = 0, show.legend = TRUE) +
  geom_text(aes(color = ComboLabel, label = Combo),
            position = position_jitter(width = 0.25), size = 4, show.legend = FALSE) +
  labs(
    title = "(c) Carbon Price Discouted at 5% (2025 - 2100)",
    x = NULL,
    y = expression("USD2010/t CO"[2])
  ) +
  scale_color_manual(
    name = NULL,
    values = combo_colors,
    labels = paste0("<span style='color:", combo_colors, "'><b>", names(combo_colors), "</b></span>"),
    guide = guide_legend(override.aes = list(shape = 12, size = 7))
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = -0.3, size = 12),
    legend.text = ggtext::element_markdown(size = 12, lineheight = 0.1),
    legend.title = element_text(face = "bold", size = 12),
    legend.position = "right",
    axis.title.y = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1),
    axis.line.y.right = element_blank(),
    axis.line.x.top = element_blank()
  )


# --- Plot for Policy Cost ---
plot_policy_data <- summary_data %>%
  filter(Parameter == "Pol_Cos_Cns_Los_rat_NPV_5pc") %>%
  mutate(Emission_Level = factor(Emission_Level, levels = c("High", "Med", "Low", "vLow")))

plot_policy <- ggplot(plot_policy_data, aes(x = Emission_Level, y = Value)) +
  geom_boxplot(
    alpha = 0.3, width = 0.6, color = "grey",
    outlier.shape = 21, outlier.size = 1.5, outlier.fill = "black",
    show.legend = FALSE
  ) +
  geom_point(aes(color = ComboLabel), size = 0, alpha = 0, show.legend = FALSE) +
  geom_text(aes(color = ComboLabel, label = Combo),
            position = position_jitter(width = 0.25), size = 4, show.legend = FALSE) +
  labs(
    title = "(d) Policy Cost Discounted at 5% (2025 - 2100)",
    x = NULL,
    y = "NPV of cumulative Consumption loss rate (%)"
  ) +
  scale_y_continuous(limits = c(3, 6), breaks = seq(0, 6, 0.5)) +
  scale_color_manual(values = combo_colors) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = -0.3, size = 12),
    legend.text = ggtext::element_markdown(size = 12, lineheight = 0.1),
    legend.title = element_text(face = "bold", size = 12),
    legend.position = "right",
    axis.title.y = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1),
    axis.line.y.right = element_blank(),
    axis.line.x.top = element_blank()
  )


library(patchwork)

final_plot <- (plot_carbon | plot_spacer() | plot_policy) +
  plot_layout(widths = c(1, 0, 1), guides = "collect") &
  theme(
    legend.position = "right",
    legend.text = ggtext::element_markdown(size = 12, lineheight = 0)
  
  )

print(final_plot)

ggsave("cumulative_carbon_and_policy_cost.png", plot = final_plot, width = 16, height = 18, dpi = 300)