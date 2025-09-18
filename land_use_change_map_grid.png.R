library(gdxrrw)
library(dplyr)
library(ggplot2)
library(readr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyr)
library(patchwork)

# --- Step 1: GAMS setup ---
igdx("/Library/Frameworks/GAMS.framework/Versions/49/Resources")

# --- Step 2: Define target variables and read function ---
land_vars <- c("Lan_Cov_Cro_Ene_Cro", "Lan_Cov_Cro_Non_Ene_Cro", 
               "Lan_Cov_Pst", "Lan_Cov_Frs_Frs", 
               "Lan_Cov_Oth_Nat_Lan", "Lan_Cov_Bui_Are")

read_land_use <- function(gdx_path, emission_label) {
  rgdx.param(gdx_path, "IAMC_template") %>%
    filter(i2 %in% c("JPN", "CHN", "IND", "XSE", "XSA", "XOC", "CAN", "USA", "BRA",
                     "XLM", "XE25", "XER", "TUR", "CIS", "XME", "XNF", "XAF"),
           i3 %in% land_vars) %>%
    rename(Scenario = i1, Region = i2, Parameter = i3, Year = i4, Value = value) %>%
    mutate(Year = as.numeric(as.character(Year)), Emission_Level = emission_label)
}


# --- Step 3: Load data ---
high_data <- read_land_use("/Users/suku/Downloads/Results/High/feas/gdx/global_17_merged.gdx", "High")

# --- Step 4: Filter for Full CDR combo (2030 & 2100) ---
full_combo_data <- high_data %>%
  filter(grepl("DACCS", Scenario, ignore.case = TRUE),
         grepl("BECCS", Scenario, ignore.case = TRUE),
         grepl("Affor", Scenario, ignore.case = TRUE),
         grepl("Other", Scenario, ignore.case = TRUE),
         Year %in% c(2030, 2100))
# --- Step 5: Reshape and calculate changes ---
change_summary <- full_combo_data %>%
  pivot_wider(names_from = Year, values_from = Value) %>%
  mutate(
    Abs_Change = `2100` - `2030`,
    Perc_Change = (`2100` - `2030`) / `2030` * 100
  ) %>%
  select(Region, Parameter, Abs_Change, Perc_Change)

# --- Step 6: Map setup ---
world_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(-iso_a3) %>%
  rename(iso_a3 = adm0_a3) %>%
  mutate(iso_a3 = toupper(iso_a3))

region_assignment <- read_csv("/Users/suku/Downloads/aim_region_mapping.csv") %>%
  mutate(iso_a3 = toupper(iso_a3))

map_base <- world_map %>%
  left_join(region_assignment, by = "iso_a3")

# --- Step 7: Define parameter names ---
land_label_map <- c(
  "Lan_Cov_Cro_Ene_Cro"     = "Energy Crops",
  "Lan_Cov_Cro_Non_Ene_Cro" = "Food Crops",
  "Lan_Cov_Pst"             = "Pasture",
  "Lan_Cov_Frs_Frs"         = "Forest",
  "Lan_Cov_Oth_Nat_Lan"     = "Other Natural Land",
  "Lan_Cov_Bui_Are"         = "Built Area"
)

# --- Define color map ---
land_colors <- c(
  "Energy Crops"       = "#32CD32",
  "Food Crops"         = "#4682B4",
  "Pasture"            = "#D8BFD8",
  "Forest"             = "#006400",
  "Other Natural Land" = "#20B2AA",
  "Built Area"         = "#A9A9A9"
)

# --- Updated plot function: show % difference using specific color for each ---
plot_land_change <- function(param_code, letter_index) {
  param_name <- land_label_map[[param_code]]
  color_hex <- land_colors[[param_name]]
  label_prefix <- paste0("(", letters[letter_index+8], ") ")
  
  plot_data <- map_base %>%
    left_join(change_summary %>% filter(Parameter == param_code), by = "Region")
  
  ggplot(plot_data) +
    geom_sf(aes(fill = Perc_Change), color = "grey", size = 0.1) +
    scale_fill_gradient(
      name = "%",
      low = scales::alpha("white", 0.5),
      high = color_hex,
      na.value = "grey90"
    ) +
    labs(title = paste0(label_prefix, param_name)) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 10, hjust = 0),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4),
      legend.position = c(0, 0.05),
      legend.justification = c(0, 0),
      legend.text = element_text(size = 11),
      legend.background = element_blank(),
      panel.grid.major = element_blank(),           # Remove major grid lines
      panel.grid.minor = element_blank()            # Remove minor grid lines
    )
}

# --- Step 9: Generate plots ---
# --- Step 9: Generate plots ---
param_codes <- names(land_label_map)
map_plots <- lapply(seq_along(param_codes), function(i) {
  plot_land_change(param_codes[i], i)
})

# --- Step 10: Fill up to 6 plots (2x3 grid)
while (length(map_plots) < 6) {
  map_plots[[length(map_plots) + 1]] <- ggplot() + theme_void()
}

# Arrange in 2 rows × 3 columns using patchwork
final_map_grid <- wrap_plots(map_plots, ncol = 2)

# --- Display the result ---
print(final_map_grid)

# --- Step 11: Save ---
ggsave("land_use_change_map_grid.png", plot = final_map_grid, width = 18, height = 12, dpi = 300)