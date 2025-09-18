library(gdxrrw)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

# --- Step 1: Setup ---
igdx("/Library/Frameworks/GAMS.framework/Versions/49/Resources")

# --- Step 2: Read Energy Data (vLow only) ---
read_energy_component <- function(gdx_path) {
  rgdx.param(gdx_path, "IAMC_template") %>%
    filter(i2 == "World",
           i3 %in% c("Prm_Ene_Bio_w_CCS","Prm_Ene_Bio_wo_CCS", "Prm_Ene_Fos_w_CCS", "Prm_Ene_Fos_wo_CCS", "Prm_Ene_NonBioRen", "Prm_Ene_Nuc", "Prm_Ene_Oil",
                     "Fin_Ene_Ele", "Fin_Ene_Gas", "Fin_Ene_Geo", "Fin_Ene_Heat", "Fin_Ene_Hyd", "Fin_Ene_Liq", "Fin_Ene_Solar", "Fin_Ene_Solids",
                     "Sec_Ene_Inp_Tot_DAC")) %>%
    rename(Scenario = i1, Region = i2, Parameter = i3, Year = i4, Value = value) %>%
    mutate(Year = as.numeric(as.character(Year)))
}

vlow_data <- read_energy_component("/Users/suku/Downloads/Results/vLow/feas/gdx/global_17_merged.gdx")

# --- Step 3: Detect BAU and Combos ---
detect_combo <- function(scenario_name) {
  if (grepl("BaU", scenario_name, ignore.case = TRUE)) return("BAU")
  has_D <- grepl("DACCS", scenario_name, ignore.case = TRUE)
  has_B <- grepl("BECCS", scenario_name, ignore.case = TRUE)
  has_A <- grepl("Affor", scenario_name, ignore.case = TRUE)
  has_O <- grepl("Other", scenario_name, ignore.case = TRUE)
  
  if (has_D & has_B & has_A & has_O) return("Full Combo")
  if (!has_D & has_B & has_A & has_O) return("No DACCS")
  if (has_D & !has_B & has_A & has_O) return("No BECCS")
  return("Other")
}

vlow_data <- vlow_data %>%
  mutate(Combo = sapply(Scenario, detect_combo)) %>%
  filter(Combo %in% c("BAU", "No DACCS", "No BECCS", "Full Combo"),
         Year >= 2010,
         Year %% 5 == 0)

# --- Step 4: Clean Labels ---
param_labels <- c(
  "Prm_Ene_Bio_w_CCS" = "Biomass w/ CCS",
  "Prm_Ene_Bio_wo_CCS" = "Biomass w/o CCS",
  "Prm_Ene_Fos_w_CCS" = "Fossil w/ CCS",
  "Prm_Ene_Fos_wo_CCS" = "Fossil w/o CCS",
  "Prm_Ene_NonBioRen" = "Non-Bio Renewables",
  "Prm_Ene_Nuc" = "Nuclear",
  "Prm_Ene_Oil" = "Oil",
  "Fin_Ene_Ele" = "Electricity",
  "Fin_Ene_Gas" = "Gas",
  "Fin_Ene_Geo" = "Geothermal",
  "Fin_Ene_Heat" = "Heat",
  "Fin_Ene_Hyd" = "Hydrogen",
  "Fin_Ene_Liq" = "Liquids",
  "Fin_Ene_Solar" = "Solar",
  "Fin_Ene_Solids" = "Solids",
  "Sec_Ene_Inp_Tot_DAC" = "DACCS"
)

vlow_data <- vlow_data %>%
  mutate(Parameter = recode(Parameter, !!!param_labels))

# Custom colors for Primary Energy
primary_colors <- c(
  "Biomass w/ CCS"     = "#1f78b4",  # same as Electricity
  "Biomass w/o CCS"    = "#33a02c",  # same as Gas
  "Fossil w/ CCS"      = "#fb9a99",  # same as Geothermal
  "Fossil w/o CCS"     = "#e31a1c",  # same as Heat
  "Non-Bio Renewables" = "#fdbf6f",  # same as Hydrogen
  "Nuclear"            = "#ff7f00",  # same as Liquids
  "Oil"                = "#cab2d6"   # same as Solar
)

# Custom colors for Final Energy
final_colors <- c(
  "Electricity" = "#a6cee3",
  "Gas"         = "#b2df8a",
  "Geothermal"  = "#fdbfbc",
  "Heat"        = "#fb8072",
  "Hydrogen"    = "#fee08b",
  "Liquids"     = "#fdb462",
  "Solar"       = "#d9d6ea",
  "Solids"      = "#bcb0d3",
  "DACCS"       = "green"
)

# --- Step 5: Plot Function ---
plot_energy_stack <- function(df, title_text, color_palette) {
  ggplot(df, aes(x = Year, y = Value, fill = Parameter)) +
    geom_col(position = "stack", width = 4.5) +
    facet_wrap(~ Combo, nrow = 1) +
    scale_fill_manual(values = color_palette, name = NULL) +
    labs(
      title = title_text,
      x = NULL,
      y = "EJ/yr"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = -0.05, size = 12),
      strip.text = element_text(face = "bold", size = 12),
      legend.position = "right",
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.grid.major = element_blank(),           # Remove major grid lines
      panel.grid.minor = element_blank()            # Remove minor grid lines
    )
}

# --- Step 6: Plot Primary and Final Energy ---
primary_vars <- c("Biomass w/ CCS", "Biomass w/o CCS", "Fossil w/ CCS", "Fossil w/o CCS", "Non-Bio Renewables", "Nuclear", "Oil")
final_vars <- c("Electricity", "Gas", "Geothermal", "Heat", "Hydrogen", "Liquids", "Solar", "Solids", "DACCS")

primary_plot <- plot_energy_stack(
  vlow_data %>% filter(Parameter %in% primary_vars),
  "(e) Primary Energy",
  primary_colors
)

final_plot <- plot_energy_stack(
  vlow_data %>% filter(Parameter %in% final_vars),
  "(f) Final Energy",
  final_colors
)

# --- Step 7: Combine and Save ---
final_figure <- (primary_plot / final_plot) +
  plot_layout(ncol = 1, guides = "collect")

ggsave("vlow_primary_final_energy_bau_nodaccs_nobeccs.png", plot = final_figure, width = 14, height = 9, dpi = 300)
print(final_figure)