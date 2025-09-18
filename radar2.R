# Load necessary libraries
library(dplyr)
library(tidyr)
library(fmsb)          # for radarchart
library(tibble)        # for column_to_rownames

# Step 1: Normalize indicator values within each emission level
normalize <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

summary_df_normalized <- summary_df %>%
  filter(Combo != "BAU") %>%
  mutate(
    Equity = -Equity,       # reverse equity so higher = worse
    FoodCrop = -FoodCrop    # reverse FoodCrop so less expansion = better
  ) %>%
  group_by(Emission_Level) %>%
  mutate(across(CDR:DACCS, normalize)) %>%
  ungroup()

# Define consistent color palette (fixed across emission levels)
combo_colors <- c(
  "A+B+D+O" = "black",
  "A+D+O"   = "#002AFFFF",
  "A+B"     = "#1965FFFF",
  "A+D"     = "#3299FFFF",
  "A+B+O"   = "#65CCFFFF",
  "B"       = "#99EDFFFF",
  "B+D"     = "#FF2A00FF",
  "B+O"     = "#FFFFCCFF",
  "D"       = "#FFEE99FF",
  "D+O"     = "#FFCC65FF",
  "B+D+O"   = "#FF9932FF",
  "A+B+D"   = "#CCFFFFFF"
)

combo_list <- names(combo_colors)

# Step 2: Helper function to prepare data for radar chart
prepare_radar_data <- function(df) {
  df <- df %>%
    complete(Combo = combo_list, fill = list(
      CDR = 0, CCS = 0, PolicyCost = 0, Equity = 0,
      LandDisp = 0, FoodCrop = 0, Fossil = 0, DACCS = 0
    )) %>%
    select(Combo, CDR, CCS, PolicyCost, Equity, LandDisp, FoodCrop, Fossil, DACCS) %>%
    column_to_rownames("Combo") %>%
    .[combo_list, ]  # reorder rows to match combo_list
  
  max_row <- rep(1, ncol(df))
  min_row <- rep(0, ncol(df))
  radar_df <- rbind(max_row, min_row, df)
  
  colnames(radar_df) <- c(
    "CDR", "CCS", "PolicyCost", "GenInequity", 
    "LandDisp", "FoodCropRed", "Fossil", "DACCS"
  )
  
  return(radar_df)
}

# Step 3: Plotting function using fixed color palette
plot_radar <- function(radar_df, title_text = "", combo_labels = NULL, combos) {
  color_lines <- combo_colors[combos]
  line_widths <- ifelse(combos == "A+B+D+O", 3, 2)
  fill_colors <- ifelse(combos == "A+B+D+O", rgb(0, 0, 0, alpha = 0.1), NA)
  
  radarchart(radar_df,
             axistype = 1,
             pcol = color_lines,
             pfcol = fill_colors,
             plwd = line_widths,
             plty = 1,
             cglcol = "grey", cglty = 1,
             cglwd = 0.8,
             caxislabels = seq(0, 1, 0.2),
             axislabcol = "black",
             calcex = 0.7,
             vlcex = 0.8,
             title = title_text,
             cex.main = 0.8
  )
  
  # if (!is.null(combo_labels)) {
  #   legend(x = -2, y = -0.1, legend = combo_labels, col = color_lines,
  #          lty = 0.5, lwd = line_widths, cex = 0.1, bty = "n", xpd = TRUE)
  }


# Step 4: Generate radar charts by emission level
em_levels <- c("High", "Med", "Low", "vLow")

# Plot to screen in a 2x2 layout
par(mfrow = c(2, 2))  # Show 4 plots in the RStudio Plots panel

for (em in em_levels) {
  df_em <- summary_df_normalized %>% filter(Emission_Level == em)
  radar_df <- prepare_radar_data(df_em)
  combos <- rownames(radar_df)[-c(1,2)]  # skip max and min rows
  plot_radar(radar_df, title_text = em, combo_labels = combos, combos = combos)
}

# Export normalized summary to CSV
write.csv(summary_df_normalized, "normalized_indicator_values.csv", row.names = FALSE)
