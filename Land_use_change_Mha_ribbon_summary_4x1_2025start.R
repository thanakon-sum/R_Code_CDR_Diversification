library(gdxrrw)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

# Step 1: Setup GAMS
igdx("/Library/Frameworks/GAMS.framework/Versions/44/Resources")

# Step 2: Load Function
read_land_use <- function(gdx_path, emission_label, vars) {
  rgdx.param(gdx_path, "IAMC_template") %>%
    filter(i2 == "World", i3 %in% vars) %>%
    rename(Scenario = i1, Country = i2, Parameter = i3, Year = i4, Value = value) %>%
    mutate(
      Year = as.numeric(as.character(Year)),
      Emission_Level = emission_label
    )
}

# Step 3: Define Parameters
land_vars <- c("Lan_Cov_Cro_Ene_Cro", "Lan_Cov_Cro_Non_Ene_Cro",
               "Lan_Cov_Oth_Nat_Lan", "Lan_Cov_Pst")

# # Step 4: Load Data (BaU is in all files)
high_data  <- read_land_use("/Users/suku/Downloads/Results/High/feas/gdx/global_17_merged.gdx", "High", land_vars)
med_data   <- read_land_use("/Users/suku/Downloads/Results/Med/feas/gdx/global_17_merged.gdx", "Med", land_vars)
low_data   <- read_land_use("/Users/suku/Downloads/Results/Low/feas/gdx/global_17_merged.gdx", "Low", land_vars)
vlow_data  <- read_land_use("/Users/suku/Downloads/Results/vLow/feas/gdx/global_17_merged.gdx", "vLow", land_vars)

land_all <- bind_rows(high_data, med_data, low_data, vlow_data)

# Extract and label BaU
bau_data <- land_all %>%
  filter(grepl("BaU", Scenario, ignore.case = TRUE)) %>%
  mutate(Emission_Level = "BaU")

# Remove duplicates and combine
non_bau <- land_all %>%
  filter(!grepl("BaU", Scenario, ignore.case = TRUE))

land_all <- bind_rows(non_bau, bau_data)

# Step 5: Clean Names & Factor Levels
land_all <- land_all %>%
  mutate(Parameter = recode(Parameter,
                            "Lan_Cov_Cro_Ene_Cro"     = "Energy Crops",
                            "Lan_Cov_Cro_Non_Ene_Cro" = "Food Crops",
                            "Lan_Cov_Oth_Nat_Lan"     = "Other Natural Land",
                            "Lan_Cov_Pst"             = "Pasture"
  ))

land_all$Emission_Level <- factor(land_all$Emission_Level,
                                  levels = c("BaU", "High", "Med", "Low", "vLow"),
                                  ordered = TRUE)

scenario_colors <- c(
  "BaU"  = "black",
  "High" = "#3C36D9FF",
  "Med"  = "#F1C40F",
  "Low"  = "#25B8D9FF",
  "vLow" = "#E74C3C"
)

# Step 6: Plot Function (ribbon version)
plot_land_use <- function(param, index) {
  ribbon_df <- land_all %>%
    filter(Parameter == param, Year >= 2025) %>%
    group_by(Year, Emission_Level) %>%
    summarise(
      ymin = quantile(Value, 0.10, na.rm = TRUE),
      ymax = quantile(Value, 0.90, na.rm = TRUE),
      ymed = quantile(Value, 0.50, na.rm = TRUE),
      .groups = "drop"
    )
  
  label_prefix <- paste0("(", letters[index], ") ")  # Generate (a), (b), etc.
  
  ggplot(ribbon_df, aes(x = Year)) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = Emission_Level), alpha = 0.3) +
    geom_line(
      data = ribbon_df %>% filter(Emission_Level != "BaU"),
      aes(y = ymed, color = Emission_Level),
      linewidth = 1.2, linetype = "dashed"
    ) +
    geom_line(
      data = ribbon_df %>% filter(Emission_Level == "BaU"),
      aes(y = ymed, color = Emission_Level),
      linewidth = 1.2, linetype = "solid"
    )+
    scale_fill_manual(values = scenario_colors) +
    scale_color_manual(values = scenario_colors) +
    scale_x_continuous(breaks = seq(2025, 2100, 10), limits = c(2025, 2100)) +
    labs(
      title = paste0(label_prefix, param),
      y = "Mha",
      x = NULL,
      fill = NULL,
      color = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = -0.15, size = 12),
      panel.border = element_blank(),
      axis.line.x = element_line(color = "black", linewidth = 0.8),
      axis.line.y = element_line(color = "black", linewidth = 0.8),
      axis.title.y = element_text(size = 12),
      axis.text = element_text(size = 11),
      axis.text.x = element_text(size = 11, angle = 90, vjust = 0.5, hjust = 0),
      axis.text.y = element_text(size = 11),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"  # ✅ Hides the legend
    )
}

# Step 7: Generate plots
land_parameters <- unique(land_all$Parameter)
land_plots <- lapply(seq_along(land_parameters), function(i) {
  plot_land_use(land_parameters[i], i)
})

# Step 8: Combine plots in 4x1 grid (horizontal layout)
final_land_plot <- wrap_plots(land_plots, nrow = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")

final_land_plot <- final_land_plot +
  plot_annotation(
    title = NULL,
    theme = theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0)
    )
  )

print(final_land_plot)

# Step 9: Save Output
ggsave("Land_use_change_Mha_ribbon_summary_4x1_2025start.png", plot = final_land_plot, width = 22, height = 6, dpi = 300)
