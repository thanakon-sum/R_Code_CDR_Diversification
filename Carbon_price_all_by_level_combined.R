library(gdxrrw)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

# Initialize GAMS
igdx("/Library/Frameworks/GAMS.framework/Versions/49/Resources")

# --- Step 1: Load and Prepare Data ---
read_carbon_price <- function(gdx_path, emission_label) {
  rgdx.param(gdx_path, "IAMC_template") %>%
    filter(i2 == "World", i3 == "Prc_Car") %>%
    rename(Scenario = i1, Country = i2, Parameter = i3, Year = i4, Value = value) %>%
    mutate(Year = as.numeric(as.character(Year)), Emission_Level = emission_label)
}

# Load GDX files
# Read from each GDX file
# high_data <- read_carbon_price("/Users/suku/Downloads/Results/High/feas/gdx/global_17_merged.gdx", "High")
# med_data  <- read_carbon_price("/Users/suku/Downloads/Results/Med/feas/gdx/global_17_merged.gdx", "Med")
# low_data  <- read_carbon_price("/Users/suku/Downloads/Results/Low/feas/gdx/global_17_merged.gdx", "Low")
# vlow_data <- read_carbon_price("/Users/suku/Downloads/Results/vLow/feas/gdx/global_17_merged.gdx", "vLow")

# --- Step 1: Define Combo Map and Color Palette ---
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

# Use names directly as labels (no numeric prefix)
combo_labels <- names(combo_map)

# Fixed color palette with 12 unique colors (names match combo labels)
combo_colors <- setNames(
  c("#205080FF", "#4890C0FF", "#98E0F0FF", "#505020FF",
    "#B0A870FF", "#F85048FF", "#D0C090FF", "#808040FF",
    "#B02820FF", "#F8C0B8FF", "#D0D0D0FF", "#801010FF"),
  combo_labels
)

# --- Step 2: Merge and Annotate Combo Info ---
carbon_price_all <- bind_rows(high_data, med_data, low_data, vlow_data)

detect_combo <- function(s) {
  has_D <- grepl("DACCS", s, ignore.case = TRUE)
  has_B <- grepl("BECCS", s, ignore.case = TRUE)
  has_A <- grepl("Affor|Afforestation", s, ignore.case = TRUE)
  has_O <- grepl("Other", s, ignore.case = TRUE)
  
  parts <- c()
  if (has_A) parts <- c(parts, "A")
  if (has_B) parts <- c(parts, "B")
  if (has_D) parts <- c(parts, "D")
  if (has_O) parts <- c(parts, "O")
  
  combo_key <- paste(parts, collapse = "+")
  if (combo_key %in% combo_labels) {
    return(combo_key)
  } else {
    return(NA)  # Drop undefined combos
  }
}

carbon_price_all <- carbon_price_all %>%
  mutate(Combo = sapply(Scenario, detect_combo)) %>%
  filter(!is.na(Combo))

# --- Step 3: Make plots for each emission level ---
plot_list <- list()

for (level in c("High", "Med", "Low", "vLow")) {
  
  filtered_data <- carbon_price_all %>% filter(Emission_Level == level)
  
  # 🔥 NEW: Calculate ranking within this emission level
  max_price_combo_order <- filtered_data %>%
    group_by(Combo) %>%
    summarise(MeanPrice = mean(Value, na.rm = TRUE)) %>%
    arrange(desc(MeanPrice)) %>%
    pull(Combo)
  
  filtered_data <- filtered_data %>%
    mutate(Combo = factor(Combo, levels = max_price_combo_order))
  
  # Line Plot
  line_plot <- ggplot(filtered_data, aes(x = Year, y = Value, color = Combo, group = Scenario)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = combo_colors, name = "CDR Combo") +
    scale_x_continuous(breaks = seq(2010, 2100, 10), expand = c(0,0)) +   
    labs(
      title = bquote(bold(.(paste0("(", letters[which(c("High", "Med", "Low", "vLow") == level) + 4], ")")) ~ .(level))),
      y = expression("USD"[2010]/"tCO"[2]), x = NULL
    )+
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 12, hjust = -0.25),
      axis.text.x = element_text(angle = 90, size = 12),
      axis.title.y = element_text(size=10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black")
    )
  
  # Box Plot
  box_plot <- ggplot(filtered_data, aes(x = Combo, y = Value, fill = Combo)) +
    geom_boxplot(outlier.shape = 1, alpha = 0.7, color = "black") +
    scale_fill_manual(values = combo_colors, guide = "none") +
    labs(
      title = NULL,
      x = NULL, y = "USD2010/tCO2"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank()
    )
  
  # Combine horizontally
  row_plot <- line_plot | box_plot
  plot_list[[level]] <- row_plot
}

# --- Step 4: Combine all rows (4 rows: High, Med, Low, vLow) ---
final_combined <- wrap_plots(plot_list, ncol = 2)

# Display
print(final_combined)

# Save
ggsave("carbon_price_line_and_box_ranked_max_each_level.png", plot = final_combined, width = 16, height = 18, dpi = 300)