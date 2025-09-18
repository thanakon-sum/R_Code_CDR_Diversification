library(gdxrrw)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(forcats)

# --- Step 1: Setup GAMS ---
igdx("/Library/Frameworks/GAMS.framework/Versions/44/Resources")

# --- Step 2: GDX Reader Function for cumulative Car_Seq_CCS ---
read_CCS_cumulative <- function(gdx_path, emission_label) {
  rgdx.param(gdx_path, "IAMC_template") %>%
    filter(i2 == "World", i3 == "Car_Seq_CCS") %>%
    rename(Scenario = i1, Country = i2, Parameter = i3, Year = i4, Value = value) %>%
    mutate(Year = as.numeric(as.character(Year)),
           Emission_Level = emission_label)
}

# --- Step 3: Load data from all emission levels ---
paths <- list(
  "High"  = "/Users/suku/Downloads/Results/High/feas/gdx/global_17_merged.gdx",
  "Med"   = "/Users/suku/Downloads/Results/Med/feas/gdx/global_17_merged.gdx",
  "Low"   = "/Users/suku/Downloads/Results/Low/feas/gdx/global_17_merged.gdx",
  "vLow"  = "/Users/suku/Downloads/Results/vLow/feas/gdx/global_17_merged.gdx"
)

ccs_all <- bind_rows(lapply(names(paths), function(label) {
  read_CCS_cumulative(paths[[label]], label)
}))

# --- Step 4: Detect Combo Function ---
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

# --- Step 5: Filter and tag ---
valid_combos <- c("A+B+D+O", "A+B", "A+D", "B+D", "B+O", "D+O", "B", "D", "A", "O",
                  "A+B+D", "A+B+O", "A+D+O", "B+D+O")

ccs_all <- ccs_all %>%
  mutate(Combo = sapply(Scenario, detect_combo)) %>%
  filter(Combo %in% valid_combos, Year >= 2025, Year <= 2100)

# --- Step 6: Aggregate cumulative CCS (2025–2100), convert MtCO₂ → GtCO₂ ---
ccs_summary <- ccs_all %>%
  group_by(Scenario, Combo, Emission_Level) %>%
  summarise(Cumulative_CCS = sum(Value) / 1000, .groups = "drop")  # GtCO₂

# --- Step 7: Factor levels for Combo and Emission Level ---
combo_order <- ccs_summary %>%
  group_by(Combo) %>%
  summarise(Max_CCS = max(Cumulative_CCS), .groups = "drop") %>%
  arrange(desc(Max_CCS)) %>%
  pull(Combo)

ccs_summary <- ccs_summary %>%
  mutate(
    Combo = factor(Combo, levels = combo_order),
    Emission_Level = factor(Emission_Level, levels = c("High", "Med", "Low", "vLow"))
  )

# --- Step 8: Create boxplot (x = Emission Level, fill = Combo) ---
# --- Step 8: Create boxplot with combo-colored dots and emission-level boxes ---
boxplot_ccs <- ggplot(ccs_summary, aes(x = Emission_Level, y = Cumulative_CCS)) +
  geom_boxplot(outlier.shape = NA, width = 0.4, fill = "white", color = "black") +
  geom_jitter(aes(color = Combo), width = 0.2, size = 2.5, alpha = 0.9) +
  labs(
    title = "Cumulative CCS (2025–2100) across Emission Levels and CDR Combinations",
    x = NULL,
    y = expression("GtCO"[2]*""),
    color = "CDR Combination"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = -0.1),
    legend.position = "right",
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.line = element_line(color = "black", linewidth = 0.8),
    panel.grid = element_blank()
  )

# --- Step 9: Save the plot ---
ggsave("cumulative_ccs_boxplot_combos_across_emissions.png", boxplot_ccs, width = 12, height = 8, dpi = 300)

# Show the plot
print(boxplot_ccs)

