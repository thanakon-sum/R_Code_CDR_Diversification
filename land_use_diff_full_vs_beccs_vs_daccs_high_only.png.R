library(gdxrrw)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

# --- Step 1: Setup ---
igdx("/Library/Frameworks/GAMS.framework/Versions/49/Resources")

read_LandUse <- function(gdx_path, emission_label) {
  rgdx.param(gdx_path, "IAMC_template") %>%
    filter(i2 == "World", i3 %in% c("Lan_Cov_Bui_Are", "Lan_Cov_Cro_Ene_Cro",
                                    "Lan_Cov_Frs_Frs", "Lan_Cov_Oth_Nat_Lan",
                                    "Lan_Cov_Pst", "Lan_Cov_Cro_Non_Ene_Cro")) %>%
    rename(Scenario = i1, Country = i2, Parameter = i3, Year = i4, Value = value) %>%
    mutate(Year = as.numeric(as.character(Year)), Emission_Level = emission_label)
}

# --- Step 2: Load data (High only) ---
high_data <- read_LandUse("/Users/suku/Downloads/Results/High/feas/gdx/global_17_merged.gdx", "High")

# --- Step 3: Clean and detect combos ---
land_all <- high_data %>%
  mutate(Parameter = recode(Parameter,
                            "Lan_Cov_Bui_Are"         = "Built Area",
                            "Lan_Cov_Cro_Ene_Cro"     = "Energy Crops",
                            "Lan_Cov_Frs_Frs"         = "Forest",
                            "Lan_Cov_Oth_Nat_Lan"     = "Other Natural Land",
                            "Lan_Cov_Pst"             = "Pasture",
                            "Lan_Cov_Cro_Non_Ene_Cro" = "Food Crops"
  ))

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

# --- Step 3: Clean and detect combos ---
land_all <- high_data %>%
  mutate(Parameter = recode(Parameter,
                            "Lan_Cov_Bui_Are"         = "Built Area",
                            "Lan_Cov_Cro_Ene_Cro"     = "Energy Crops",
                            "Lan_Cov_Frs_Frs"         = "Forest",
                            "Lan_Cov_Oth_Nat_Lan"     = "Other Natural Land",
                            "Lan_Cov_Pst"             = "Pasture",
                            "Lan_Cov_Cro_Non_Ene_Cro" = "Food Crops"
  ))

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

land_all <- land_all %>%
  mutate(Combo = sapply(Scenario, detect_combo)) %>%
  filter(Combo %in% c("BAU", "A+B+D+O", "B", "D"))

# --- Step 4: Summarize & compute differences ---
land_filtered <- land_all %>%
  filter(Year %% 5 == 0, Year >= 2030, Year <= 2100) %>%
  select(Scenario, Year, Parameter, Value, Combo)

land_summarized <- land_filtered %>%
  group_by(Year, Parameter, Combo) %>%
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")

get_diff <- function(df, combo_name, label) {
  df %>%
    filter(Combo %in% c("BAU", combo_name)) %>%
    pivot_wider(names_from = Combo, values_from = Value, id_cols = c(Year, Parameter)) %>%
    filter(!is.na(`BAU`) & !is.na(.data[[combo_name]])) %>%
    mutate(Diff = .data[[combo_name]] - BAU, Comparison = label)
}

diff_full  <- get_diff(land_summarized, "A+B+D+O", "Full Combo")
diff_beccs <- get_diff(land_summarized, "B", "BECCS Only")
diff_daccs <- get_diff(land_summarized, "D", "DACCS Only")

diff_from_bau <- bind_rows(diff_full, diff_beccs, diff_daccs)

# --- Step 5: Plotting ---
land_colors <- c(
  "Energy Crops"       = "#32CD32",
  "Food Crops"         = "#4682B4",
  "Pasture"            = "#D8BFD8",
  "Forest"             = "#006400",
  "Other Natural Land" = "#20B2AA",
  "Built Area"         = "#A9A9A9"
)

plot_diff <- function(input_data, label) {
  input_data_filtered <- input_data %>%
    filter(Comparison == label) %>%
    mutate(Parameter = factor(Parameter, levels = names(land_colors)))
  
  ggplot(input_data_filtered, aes(x = Year, y = Diff, fill = Parameter)) +
    geom_col(position = "stack", width = 4.5) +
    scale_fill_manual(values = land_colors, drop = FALSE) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(2030, 2100, 10)) +
    scale_y_continuous(limits = c(-1200, 600), breaks = seq(-1200, 600, 200)) +
    labs(
      title = paste0("(", substr(label, 1, 1), ") ", label),
      x = NULL,
      y = "Change in Land Use (Mha)",
      fill = NULL
    ) +
    guides(fill = guide_legend(nrow = 1)) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0),
      axis.title.y = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 11),
      panel.border = element_rect(color = "black", fill = NA),
      legend.position = "bottom",
      legend.box = "horizontal"
    )
}

# --- Step 6: Combine & Save ---
p_full   <- plot_diff(diff_from_bau, "Full Combo")
p_beccs  <- plot_diff(diff_from_bau, "BECCS Only")
p_daccs  <- plot_diff(diff_from_bau, "DACCS Only")

final_plot <- (p_full | p_beccs | p_daccs) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("land_use_diff_full_vs_beccs_vs_daccs_high_only.png", plot = final_plot, width = 18, height = 6, dpi = 300)
print(final_plot)