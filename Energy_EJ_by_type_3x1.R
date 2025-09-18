library(gdxrrw)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

# Initialize GAMS
igdx("/Library/Frameworks/GAMS.framework/Versions/49/Resources")

# Step 1: Read Function
read_variable <- function(gdx_path, emission_label, vars) {
  rgdx.param(gdx_path, "IAMC_template") %>%
    filter(i2 == "World", i3 %in% vars) %>%
    rename(Scenario = i1, Country = i2, Parameter = i3, Year = i4, Value = value) %>%
    mutate(
      Year = as.numeric(as.character(Year)),
      Emission_Level = case_when(
        grepl("BaU", Scenario, ignore.case = TRUE) ~ "BaU",
        TRUE ~ emission_label
      )
    )
}

# Step 2: Variable names
target_vars <- c("Prm_Ene", "Fin_Ene", "Prc_Fin_Ene_Res_and_Com_Res_Ele", 
                 "Prc_Sec_Ene_Liq_Bio")

# --- Step 3: Load Data Per Scenario (auto-detect BaU inside files) ---
high_data  <- read_variable("/Users/suku/Downloads/Results/High/feas/gdx/global_17_merged.gdx", "High", target_vars)
med_data   <- read_variable("/Users/suku/Downloads/Results/Med/feas/gdx/global_17_merged.gdx", "Med", target_vars)
low_data   <- read_variable("/Users/suku/Downloads/Results/Low/feas/gdx/global_17_merged.gdx", "Low", target_vars)
vlow_data  <- read_variable("/Users/suku/Downloads/Results/vLow/feas/gdx/global_17_merged.gdx", "vLow", target_vars)
#

# No need for separate bau_data
all_data <- bind_rows(high_data, med_data, low_data, vlow_data)

# Step 4: Summarize for ribbon plot
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
    filter(Year >= 2005)
}

# Step 5: Colors
scenario_colors <- c(
  "BaU"  = "black",
  "High" = "#3C36D9FF",
  "Med"  = "#F1C40F",
  "Low"  = "#25B8D9FF",
  "vLow" = "#E74C3C"
)

fill_colors <- scenario_colors[names(scenario_colors) != "BaU"]

# --- Step 6: Enhanced Plot Function ---
plot_ribbon <- function(df, title_text, raw_data, show_y_axis = TRUE, border_sides = "full") {
  df$Emission_Level <- factor(df$Emission_Level, levels = c("BaU", "High", "Med", "Low", "vLow"))
  
  bau_line <- raw_data %>%
    filter(Emission_Level == "BaU") %>%
    group_by(Year) %>%
    summarise(ymed = mean(Value, na.rm = TRUE), .groups = "drop") %>%
    mutate(Emission_Level = "BaU")
  
  # Border control
  border_style <- switch(
    border_sides,
    "full"   = element_rect(color = "black", fill = NA, linewidth = 0.8),
    "left_bottom" = element_blank(),  # we'll simulate via axis lines
    "bottom_only" = element_blank(),
    element_blank()
  )
  
  ggplot(df, aes(x = Year)) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = Emission_Level), alpha = 0.3) +
    
    # Dashed lines for scenario median (excluding BaU)
    geom_line(
      data = df %>% filter(Emission_Level != "BaU"),
      aes(y = ymed, color = Emission_Level, group = Emission_Level),
      linewidth = 1.2, linetype = "dashed"
    ) +
    
    # Solid line for BaU
    geom_line(
      data = bau_line,
      aes(x = Year, y = ymed),
      color = "black", linewidth = 1.2
    ) +
    geom_vline(xintercept = 2025, linetype = "dashed", color = "pink", linewidth = 1) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    scale_color_manual(values = scenario_colors, name = "") +
    scale_fill_manual(values = scenario_colors, name = "") +
    guides(
      fill  = guide_legend(ncol = 1),  # vertical legend
      color = guide_legend(ncol = 1)
    ) +
    labs(
      title = title_text,
      x = NULL,
      y = if (show_y_axis) "EJ/yr" else NULL
    ) +
    scale_x_continuous(breaks = seq(2010, 2100, 10)) +
    coord_cartesian(ylim = c(0, 900)) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = -0.3),
      axis.title.y = if (show_y_axis) element_text(size = 12) else element_blank(),
      axis.text.y  = if (show_y_axis) element_text( size = 12) else element_blank(),
      axis.text.x  = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 1, face = "plain"),
      legend.position = NULL,
      legend.box = "horizontal",
      panel.border = element_blank(),  # remove full border
      axis.line.y = if (border_sides == "left_bottom" & show_y_axis) element_line(color = "black", linewidth = 0.5) else element_blank(),
      panel.grid = element_blank()
    )
}

# --- Step 7: Apply Styling Per Panel ---

# LEFT (a): Primary Energy
prim_plot <- plot_ribbon(
  summarize_ribbon(all_data, "Prm_Ene"),
  "(a) Primary Energy",
  all_data %>% filter(Parameter == "Prm_Ene"),
  show_y_axis = TRUE,
  border_sides = "left_bottom"
)

# MIDDLE (b): Final Energy
fin_plot <- plot_ribbon(
  summarize_ribbon(all_data, "Fin_Ene"),
  "(b) Final Energy",
  all_data %>% filter(Parameter == "Fin_Ene"),
  show_y_axis = FALSE,
  border_sides = "bottom_only"
)

# RIGHT (c): Residential + Commercial Electricity Price
elec_price_plot <- plot_ribbon(
  summarize_ribbon(all_data, "Prc_Fin_Ene_Res_and_Com_Res_Ele"),
  "(c) Electricity Price",
  all_data %>% filter(Parameter == "Prc_Fin_Ene_Res_and_Com_Res_Ele"),
  show_y_axis = TRUE,
  border_sides = "left_bottom"
) +
  labs(y = "USD2010/MWh") +
  coord_cartesian(ylim = c(0, 60))

# (d) Biofuel Price
biofuel_price_plot <- plot_ribbon(
  summarize_ribbon(all_data, "Prc_Sec_Ene_Liq_Bio"),
  "(d) Biofuel Price",
  all_data %>% filter(Parameter == "Prc_Sec_Ene_Liq_Bio"),
  show_y_axis = TRUE,
  border_sides = "left_bottom"
) +
  labs(y = "USD2010/GJ") +
  coord_cartesian(ylim = c(0, 60))


# --- Step 8–9: Combine & Save ---
final_plot <- (prim_plot | fin_plot | elec_price_plot | biofuel_price_plot) +
  plot_layout(guides = "collect") &
  theme(
    axis.text.x = element_text(size = 12, face = "plain", angle = 90, vjust = 0.5, hjust = 1),
    legend.position = NULL,
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = NULL
  )
print(final_plot)

ggsave("Energy_EJ_by_type_3x1.png", plot = final_plot, width = 14, height = 5, dpi = 300)