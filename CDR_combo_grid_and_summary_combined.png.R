library(gdxrrw)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

# Initialize GAMS
igdx("/Library/Frameworks/GAMS.framework/Versions/49/Resources")

# --- Load Data ---
read_CDR <- function(gdx_path, emission_label) {
  rgdx.param(gdx_path, "IAMC_template") %>%
    filter(i2 == "World", i3 %in% c(
      "Car_Rem_Bio", "Car_Rem_Bio_wit_CCS", "Car_Rem_Dir_Air_Cap_wit_CCS",
      "Car_Rem_Soi_Car_Seq", "Car_Rem_Enh_Wea", "Car_Rem_Frs"
    )) %>%
    rename(Scenario = i1, Country = i2, Parameter = i3, Year = i4, Value = value) %>%
    mutate(Year = as.numeric(as.character(Year)), Emission_Level = emission_label)
}

read_variable <- function(gdx_path, emission_label, vars) {
  rgdx.param(gdx_path, "IAMC_template") %>%
    filter(i2 == "World", i3 %in% vars) %>%
    rename(Scenario = i1, Country = i2, Parameter = i3, Year = i4, Value = value) %>%
    mutate(
      Year = as.numeric(as.character(Year)),
      Emission_Level = emission_label,
      Value = Value / 1000
    )
}

# Load GDX files
#high_data <- read_CDR("/Users/suku/Downloads/Results/High/feas/gdx/global_17_merged.gdx", "High")
#med_data  <- read_CDR("/Users/suku/Downloads/Results/Med/feas/gdx/global_17_merged.gdx", "Med")
#low_data  <- read_CDR("/Users/suku/Downloads/Results/Low/feas/gdx/global_17_merged.gdx", "Low")
#vlow_data <- read_CDR("/Users/suku/Downloads/Results/vLow/feas/gdx/global_17_merged.gdx", "vLow")

cdr_all <- bind_rows(high_data, med_data, low_data, vlow_data)
data <- cdr_all

data <- data %>%
  mutate(Parameter = recode(Parameter,
                            "Car_Rem_Frs"               = "Afforestation",
                            "Car_Rem_Bio"               = "Biochar",
                            "Car_Rem_Bio_wit_CCS"       = "BECCS",
                            "Car_Rem_Dir_Air_Cap_wit_CCS" = "DACCS",
                            "Car_Rem_Enh_Wea"           = "Enhanced Weathering",
                            "Car_Rem_Soi_Car_Seq"       = "Soil Carbon Sequestration"
  ))

detect_combo <- function(scenario_name) {
  has_D <- grepl("DACCS", scenario_name, ignore.case = TRUE)
  has_B <- grepl("BECCS", scenario_name, ignore.case = TRUE)
  has_A <- grepl("Affor|Afforestation", scenario_name, ignore.case = TRUE)
  has_O <- grepl("Other", scenario_name, ignore.case = TRUE)
  parts <- c()
  if (has_A) parts <- c(parts, "A")
  if (has_B) parts <- c(parts, "B")
  if (has_D) parts <- c(parts, "D")
  if (has_O) parts <- c(parts, "O")
  if (length(parts) == 0) return("None")
  return(paste(sort(parts), collapse = "+"))
}

combo_order <- c("D", "A+D", "D+O", "A+D+O", "B", "A+B", "B+D", "B+O", "A+B+D", "B+D+O", "A+B+O", "A+B+D+O")

cdr_colors <- c(
  "Soil Carbon Sequestration" = "#8E44AD",
  "Biochar" = "#F1C40F",
  "Enhanced Weathering" = "#1ABC9C",
  "Afforestation" = "#7F8C8D",
  "BECCS" = "#3498DB",
  "DACCS" = "#E74C3C"
)

data <- data %>%
  mutate(Combo = sapply(Scenario, detect_combo),
         Value = Value / 1000,
         Emission_Level = factor(Emission_Level, levels = c("High", "Med", "Low", "vLow")),
         Combo = factor(Combo, levels = combo_order)) %>%
  filter(!is.na(Combo))

# Fill infeasible
missing_combos <- setdiff(combo_order, unique(data$Combo))
year_range <- sort(unique(data$Year))
infeasible_fill <- expand.grid(
  Year = year_range,
  Combo = missing_combos,
  Parameter = "Infeasible"
) %>%
  mutate(Value = 0, Combo = factor(Combo, levels = combo_order))

plot_data <- bind_rows(data, infeasible_fill)

total_data <- data %>%
  group_by(Year, Combo, Emission_Level) %>%
  summarise(Total_Value = sum(Value, na.rm = TRUE), .groups = "drop")

infeasible_labels <- tibble::tibble(
  Combo = factor(missing_combos, levels = combo_order),
  x = 2055, y = 7.5, label = "Infeasible"
)

# CDR Combo Plot (left side)
p_grid <- ggplot() +
  geom_area(data = plot_data %>% filter(Parameter != "Infeasible"),
            aes(x = Year, y = Value, fill = Parameter), alpha = 0.9) +
  geom_line(data = total_data, aes(x = Year, y = Total_Value),
            color = "black", linetype = "dashed", linewidth = 0.9) +
  geom_text(data = infeasible_labels, aes(x = x, y = y, label = label),
            color = "grey40", size = 5, fontface = "bold") +
  facet_grid(Combo ~ Emission_Level) +
  scale_fill_manual(values = cdr_colors, na.value = "grey90") +
  scale_x_continuous(breaks = seq(2000, 2100, 20)) +
  labs(x = NULL, y = expression("GtCO"[2]*"/yr"), fill = "CDR Type") +
  theme_minimal(base_size = 14) + 
  ggtitle ("CDR Portfolio across emission pathways")+
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    axis.title = element_text(face = "bold", size = 12),
    #axis.text = element_text(face = "bold", size = 12),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12),
    panel.background = element_rect(fill = "white")
  )

# === RIGHT SIDE: CDR / CCS / DAC ribbon ===
target_vars <- c("Car_Rem", "Car_Seq_CCS", "Car_Seq_Dir_Air_Cap")
#high_d2 <- read_variable("/Users/suku/Downloads/Results/High/feas/gdx/global_17_merged.gdx", "High", target_vars)
#med_d2  <- read_variable("/Users/suku/Downloads/Results/Med/feas/gdx/global_17_merged.gdx", "Med", target_vars)
#low_d2  <- read_variable("/Users/suku/Downloads/Results/Low/feas/gdx/global_17_merged.gdx", "Low", target_vars)
#vlow_d2 <- read_variable("/Users/suku/Downloads/Results/vLow/feas/gdx/global_17_merged.gdx", "vLow", target_vars)
all_data <- bind_rows(high_d2, med_d2, low_d2, vlow_d2) %>%
  filter(!grepl("BaU", Scenario, ignore.case = TRUE))

scenario_colors <- c(
  "High" = "#3C36D9FF", "Med" = "#F1C40F",
  "Low" = "#25B8D9FF", "vLow" = "#E74C3C"
)

summarize_ribbon <- function(df, param_name) {
  df %>%
    filter(Parameter == param_name) %>%
    group_by(Year, Emission_Level) %>%
    summarise(
      ymin = quantile(Value, 0.10, na.rm = TRUE),
      ymax = quantile(Value, 0.90, na.rm = TRUE),
      ymed = quantile(Value, 0.50, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(Year >= 2030)
}

plot_ribbon <- function(df, title_text) {
  df$Emission_Level <- factor(df$Emission_Level, levels = c("High", "Med", "Low", "vLow"))
  ggplot(df, aes(x = Year)) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = Emission_Level), alpha = 0.3) +
    geom_line(aes(y = ymed, color = Emission_Level), linewidth = 1.3, linetype = "dashed") +
    geom_vline(xintercept = 2025, linetype = "dashed", color = "pink", linewidth = 1) +
    geom_hline(yintercept = 0, color = "black", linewidth = 1) +
    scale_fill_manual(values = scenario_colors) +
    scale_color_manual(values = scenario_colors) +
    scale_x_continuous(breaks = seq(2000, 2100, 10)) +
    labs(title = title_text, x = NULL, y = expression("GtCO"[2]*"/yr"),
         fill = "Emission Pathway", color = "Emission Pathway") +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 1, margin = margin(b = 2)),
      plot.title.position = "plot",
      
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(face = "bold", size = 12),
      legend.position = "bottom",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
    )
}

cdr_plot <- plot_ribbon(summarize_ribbon(all_data, "Car_Rem"), "Total CDR Removal")
ccs_plot <- plot_ribbon(summarize_ribbon(all_data, "Car_Seq_CCS"), "Total CCS Sequestration")
dac_plot <- plot_ribbon(summarize_ribbon(all_data, "Car_Seq_Dir_Air_Cap"), "Total DACCS Sequestration")

# Combine right-side ribbon plots vertically with tighter spacing
p_summary <- plot_grid(
  cdr_plot, ccs_plot, dac_plot,
  ncol = 1,
  align = "v",
  rel_heights = c(1, 1, 1),  # or adjust if needed
  labels = NULL
)

# === Final Patchwork Combination ===
final_combined <- plot_grid(
  final_combined,
  ncol = 1,
  rel_heights = c(0.07, 1)
)
print(final_combined)

# Save
ggsave("CDR_combo_grid_and_summary_combined.png", plot = final_combined,
       width = 22, height = 14, dpi = 300)
