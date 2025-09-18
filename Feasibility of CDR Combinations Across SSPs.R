# Load required libraries
library(readxl)
library(tidyverse)
library(ggplot2)

# Step 1: Load the Excel file
df <- read_excel("/Users/suku/Downloads/ssp.xlsx", sheet = 1)

# Step 2: Rename and extract SSP and raw combo name
df <- df %>%
  rename(Scenario = 1) %>%
  mutate(
    SSP = str_extract(Scenario, "SSP\\d"),
    RawCombo = str_remove(Scenario, "^SSP\\d_"),
    Feasible = TRUE
  )

# Step 3: Define a function to rename combos like "LBhigh_DACCS_BECCS_Afforest_Other_NoCC_No" to "High_A+B+D+O"
translate_combo <- function(x) {
  level <- case_when(
    str_detect(x, "LBhigh") ~ "High",
    str_detect(x, "LBmed") ~ "Med",
    str_detect(x, "LBlow") ~ "Low",
    str_detect(x, "LBvlow") ~ "vLow",
    TRUE ~ "Unknown"
  )
  
  cdr_codes <- c(
    A = "Afforest",
    B = "BECCS",
    D = "DACCS",
    O = "Other"
  )
  
  cdr_present <- names(cdr_codes)[sapply(cdr_codes, function(term) str_detect(x, term))]
  combo_code <- paste(cdr_present, collapse = "+")
  
  label <- paste(level, combo_code, sep = "_")
  return(label)
}

# Step 4: Apply renaming function
df <- df %>%
  mutate(Combo = sapply(RawCombo, translate_combo))

# Step 5: Generate all possible SSP × CDR Combo combinations
ssp_list <- paste0("SSP", 1:5)
combo_list <- unique(df$Combo)

full_matrix <- expand_grid(
  SSP = ssp_list,
  Combo = combo_list
) %>%
  left_join(df, by = c("SSP", "Combo")) %>%
  mutate(Feasible = ifelse(is.na(Feasible), FALSE, Feasible))

# Step 6: Plot the feasibility matrix
ggplot(full_matrix, aes(x = SSP, y = fct_rev(factor(Combo)))) +
  geom_tile(aes(fill = Feasible), color = "white") +
  scale_fill_manual(
    values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02"),
    labels = c("Feasible", "Infeasible")
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Feasibility",
    title = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 6))