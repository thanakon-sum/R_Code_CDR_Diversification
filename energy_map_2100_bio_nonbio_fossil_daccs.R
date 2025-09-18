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

# --- Step 2: Define target variables ---
energy_vars <- c("Prm_Ene_Bio", "Prm_Ene_NonBioRen", "Prm_Ene_Fos_w_CCS", "Sec_Ene_Inp_Tot_DAC")

read_energy_map <- function(gdx_path) {
  rgdx.param(gdx_path, "IAMC_template") %>%
    filter(i2 %in% c("JPN", "CHN", "IND", "XSE", "XSA", "XOC", "CAN", "USA", "BRA",
                     "XLM", "XE25", "XER", "TUR", "CIS", "XME", "XNF", "XAF"),
           i3 %in% energy_vars) %>%
    rename(Scenario = i1, Region = i2, Parameter = i3, Year = i4, Value = value) %>%
    mutate(Year = as.numeric(as.character(Year)))
}

# --- Step 3: Load data for vLow ---
vlow_data <- read_energy_map("/Users/suku/Downloads/Results/vLow/feas/gdx/global_17_merged.gdx")

# --- Step 4: Filter Full Combo and Year 2100 ---
target_data <- vlow_data %>%
  filter(grepl("DACCS", Scenario, ignore.case = TRUE),
         grepl("BECCS", Scenario, ignore.case = TRUE),
         grepl("Affor", Scenario, ignore.case = TRUE),
         grepl("Other", Scenario, ignore.case = TRUE),
         Year == 2100)

# --- Step 5: Aggregate by region ---
map_values <- target_data %>%
  group_by(Region, Parameter) %>%
  summarise(Value_2100 = sum(Value, na.rm = TRUE), .groups = "drop")

# --- Step 6: Prepare mapping data ---
world_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(-iso_a3) %>%
  rename(iso_a3 = adm0_a3) %>%
  mutate(iso_a3 = toupper(iso_a3))

region_assignment <- read_csv("/Users/suku/Downloads/aim_region_mapping.csv") %>%
  mutate(iso_a3 = toupper(iso_a3))

map_base <- world_map %>%
  left_join(region_assignment, by = "iso_a3")

# --- Step 7: Labels and Colors ---
param_labels <- c(
  "Prm_Ene_Bio" = "Bioenergy",
  "Prm_Ene_NonBioRen" = "Non-Biomass Renewables",
  "Prm_Ene_Fos_w_CCS" = "Fossil + CCS",
  "Sec_Ene_Inp_Tot_DAC" = "DACCS"
)

energy_colors <- c(
  "Bioenergy"              = "#1b9e77",
  "Non-Biomass Renewables" = "#66a61e",
  "Fossil + CCS"           = "#7570b3",
  "DACCS"                  = "#b15928"
)

# --- Step 8: Plot function ---
plot_energy_map <- function(param_code, label_letter) {
  param_name <- param_labels[[param_code]]
  fill_color <- energy_colors[[param_name]]
  label_prefix <- paste0("(", letters[label_letter+7], ") ")
  
  plot_data <- map_base %>%
    left_join(map_values %>% filter(Parameter == param_code), by = "Region")
  
  ggplot(plot_data) +
    geom_sf(aes(fill = Value_2100), color = "grey", size = 0.1) +
    scale_fill_gradient(
      name = "EJ",
      low = scales::alpha("white", 0.4),
      high = fill_color,
      na.value = "grey90"
    ) +
    labs(title = paste0(label_prefix, param_name)) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4),
      legend.position = c(0, 0.05),
      legend.justification = c(0, 0),
      legend.text = element_text(size = 11),
      legend.background = element_blank(),
      panel.grid = element_blank()
    )
}

# --- Step 9: Generate plots in 2x2 grid ---
param_list <- names(param_labels)
energy_maps <- lapply(seq_along(param_list), function(i) {
  plot_energy_map(param_list[i], i)
})

final_energy_map_grid <- wrap_plots(energy_maps, ncol = 2)
print(final_energy_map_grid)

# --- Step 10: Save ---
ggsave("energy_map_2100_bio_nonbio_fossil_daccs.png", plot = final_energy_map_grid, width = 16, height = 10, dpi = 300)