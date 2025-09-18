# Load libraries
library(readxl)
library(tidyverse)
library(ggplot2)

# Step 1: Load Excel
df <- read_excel("/Users/suku/Downloads/ssp.xlsx", sheet = 1) %>%
  rename(Scenario = 1)

# Step 2: Extract SSP and Raw scenario string
df <- df %>%
  mutate(
    SSP = str_extract(Scenario, "SSP\\d"),
    Raw = str_remove(Scenario, "^SSP\\d_"),
    Feasible = TRUE
  )

# Step 3: Translate emission level and CDR combo (only use valid 16 combos)
extract_combo_label <- function(raw) {
  level <- case_when(
    str_detect(raw, "LBvhigh") ~ "vHigh",
    str_detect(raw, "LBhigh")  ~ "High",
    str_detect(raw, "LBmed")   ~ "Med",
    str_detect(raw, "LBlow")   ~ "Low",
    str_detect(raw, "LBvlow")  ~ "vLow",
    TRUE ~ NA_character_
  )
  
  # Detect presence of CDR techs
  techs <- c()
  if (str_detect(raw, "Afforest")) techs <- c(techs, "A")
  if (str_detect(raw, "BECCS"))    techs <- c(techs, "B")
  if (str_detect(raw, "DACCS"))    techs <- c(techs, "D")
  if (str_detect(raw, "Other"))    techs <- c(techs, "O")
  
  if (length(techs) == 0 && str_detect(raw, "NoCC")) {
    combo <- "No CDR"
  } else if (length(techs) == 1) {
    combo <- paste0(techs, " only")
  } else {
    combo <- paste(sort(techs), collapse = "+")
  }
  
  return(paste(level, combo, sep = "_"))
}

# Step 4: Apply combo label
df <- df %>%
  mutate(ComboLabel = sapply(Raw, extract_combo_label))

# Step 5: Define valid combos and emission levels
valid_combos <- c(
  "A+B+D+O", "A+B+D", "A+B+O", "A+D+O", "B+D+O", "A+B", "A+D", "A+O",
  "B+D", "B+O", "D+O", "A only", "B only", "D only", "O only", "No CDR"
)
emission_levels <- c("vHigh", "High", "Med", "Low", "vLow")  # desired top-down order

# Create combo labels with vHigh first (top)
combo_labels_all <- expand_grid(
  Level = emission_levels,
  Combo = valid_combos
) %>%
  mutate(ComboLabel = paste(Level, Combo, sep = "_")) %>%
  pull(ComboLabel)

# Step 6: Create full matrix: 80 combos × 5 SSPs
ssp_list <- paste0("SSP", 1:5)
full_matrix <- expand_grid(SSP = ssp_list, ComboLabel = combo_labels_all)

# Add label column = Level_Combo (same as y-axis label)
feasibility_matrix <- feasibility_matrix %>%
  mutate(LabelText = ComboLabel)

# Plot with labels inside tiles
p <- ggplot(feasibility_matrix, aes(x = SSP, y = ComboLabel)) +
  geom_tile(aes(fill = Feasible), color = "white") +
  geom_text(aes(label = LabelText), color = "white", size = 2.5, fontface = "bold") +
  scale_fill_manual(
    values = c("TRUE" = "#1b9e77", "FALSE" = "grey"),
    labels = c("Infeasible","Feasible"),
    name = "Feasibility"
  ) +
  scale_y_discrete(limits = rev(levels(feasibility_matrix$ComboLabel))) +
  labs(
    x = NULL,
    y = NULL,
    title = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 8),
    legend.position = "right",
    legend.title = NULL,
    legend.text = element_text(size = 12),
    legend.key.width = unit(1, "cm")
  )

# Print plot to view
print(p)

# Save
ggsave("feasibility_matrix_with_labels.png", plot = p, width = 10, height = 12, dpi = 300)
write.csv(feasibility_matrix, "feasibility_matrix.csv", row.names = FALSE)

