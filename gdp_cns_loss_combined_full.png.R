library(gdxrrw)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

# Initialize GAMS
igdx("/Library/Frameworks/GAMS.framework/Versions/49/Resources")

# --- Step 1: Read Function ---
read_variable <- function(gdx_path, emission_label, vars) {
  rgdx.param(gdx_path, "IAMC_template") %>%
    filter(i2 == "World", i3 %in% vars) %>%
    rename(Scenario = i1, Country = i2, Parameter = i3, Year = i4, Value = value) %>%
    mutate(
      Year = as.numeric(as.character(Year)),
      Emission_Level = emission_label
    )
}

# --- Step 2: Read Data ---
vars_needed <- c("Pol_Cos_GDP_Los_rat", "Pol_Cos_GDP_Los",
                 "Pol_Cos_Cns_Los_rat", "Pol_Cos_Cns_Los")

high_data  <- read_variable("/Users/suku/Downloads/Results/High/feas/gdx/global_17_merged.gdx", "High", vars_needed)
med_data   <- read_variable("/Users/suku/Downloads/Results/Med/feas/gdx/global_17_merged.gdx", "Med", vars_needed)
low_data   <- read_variable("/Users/suku/Downloads/Results/Low/feas/gdx/global_17_merged.gdx", "Low", vars_needed)
vlow_data  <- read_variable("/Users/suku/Downloads/Results/vLow/feas/gdx/global_17_merged.gdx", "vLow", vars_needed)

all_data <- bind_rows(high_data, med_data, low_data, vlow_data) %>%
  filter(!grepl("BaU", Scenario, ignore.case = TRUE))

# --- Step 3: Detect Combo
detect_combo <- function(scenario_name) {
  if (grepl("BaU", scenario_name, ignore.case = TRUE)) return("BAU")
  has_D <- grepl("DACCS", scenario_name, ignore.case = TRUE)
  has_B <- grepl("BECCS", scenario_name, ignore.case = TRUE)
  has_A <- grepl("Affor|Afforestation", scenario_name, ignore.case = TRUE)
  has_O <- grepl("Other", scenario_name, ignore.case = TRUE)
  parts <- c()
  if (has_A) parts <- c(parts, "A")
  if (has_B) parts <- c(parts, "B")
  if (has_D) parts <- c(parts, "D")
  if (has_O) parts <- c(parts, "O")
  if (length(parts) == 0) return("None") else return(paste(parts, collapse = "+"))
}

all_data <- all_data %>%
  mutate(Combo = sapply(Scenario, detect_combo))

# --- Step 4: Summary Ribbon and Full Combo Extract ---
summarize_ribbon <- function(df, param_name) {
  df %>%
    filter(Parameter == param_name) %>%
    group_by(Year, Emission_Level) %>%
    summarise(
      ymin = quantile(Value, 0.10, na.rm = TRUE),
      ymax = quantile(Value, 0.90, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(Year >= 2030)
}

get_fullcombo_line <- function(df, param_name) {
  df %>%
    filter(Parameter == param_name, Combo == "A+B+D+O") %>%
    filter(Year >= 2030)
}

# --- Step 5: Color Scheme ---
scenario_colors <- c(
  "High" = "#3C36D9FF",
  "Med"  = "#F1C40F",
  "Low"  = "#25B8D9FF",
  "vLow" = "#E74C3C"
)

# --- Step 6: Plot Function ---
plot_metric_panel <- function(df, param_rate, param_total, title_text, y_label_text) {
  ribbon_data <- summarize_ribbon(df, param_rate)
  fullcombo_data <- get_fullcombo_line(df, param_rate)
  
  ribbon_data$Emission_Level <- factor(ribbon_data$Emission_Level, levels = c("High", "Med", "Low", "vLow"))
  fullcombo_data$Emission_Level <- factor(fullcombo_data$Emission_Level, levels = c("High", "Med", "Low", "vLow"))
  
  ggplot(ribbon_data, aes(x = Year)) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = Emission_Level), alpha = 0.3) +
    geom_line(data = fullcombo_data, aes(y = Value, color = Emission_Level, group = Emission_Level),
              linewidth = 1.3, linetype = "dashed") +
    scale_fill_manual(values = scenario_colors) +
    scale_color_manual(values = scenario_colors) +
    scale_x_continuous(breaks = seq(2030, 2100, 10)) +
    labs(
      title = title_text,
      y = y_label_text,
      x = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 12, hjust = -0.15),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.8)
    )
}

# --- Step 7: Create Panels ---
gdp_panel <- plot_metric_panel(
  all_data,
  "Pol_Cos_GDP_Los_rat",
  "Pol_Cos_GDP_Los",
  expression(bold("(a) GDP Loss")),
  "Loss Rate (%)"
)

cns_panel <- plot_metric_panel(
  all_data,
  "Pol_Cos_Cns_Los_rat",
  "Pol_Cos_Cns_Los",
  expression(bold("(b) Consumption Loss")),
  "Loss Rate (%)"
)

# --- Step 8: Combine and Save ---
final_plot <- (gdp_panel | plot_spacer() | cns_panel) +
  plot_layout(guides = "collect", widths = c(1, 0.1, 1)) &  # adjust space width here
  theme(legend.position = "right", legend.title = element_blank())

ggsave("gdp_cns_loss_combined.png", plot = final_plot, width = 14, height = 10, dpi = 300)
print(final_plot)

