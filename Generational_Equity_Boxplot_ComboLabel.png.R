library(gdxrrw)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(forcats)

# --- Step 1: Setup GAMS ---
igdx("/Library/Frameworks/GAMS.framework/Versions/49/Resources")

# --- Step 2: Define GDX paths ---
paths <- list(
  "High"  = "/Users/suku/Downloads/Results/High/feas/gdx/global_17_merged.gdx",
  "Med"   = "/Users/suku/Downloads/Results/Med/feas/gdx/global_17_merged.gdx",
  "Low"   = "/Users/suku/Downloads/Results/Low/feas/gdx/global_17_merged.gdx",
  "vLow"  = "/Users/suku/Downloads/Results/vLow/feas/gdx/global_17_merged.gdx"
)

# --- Step 3: Combo detection ---
detect_combo <- function(s) {
  if (grepl("BaU", s, ignore.case = TRUE)) return("BAU")
  parts <- c()
  if (grepl("Affor", s, ignore.case = TRUE)) parts <- c(parts, "A")
  if (grepl("BECCS", s, ignore.case = TRUE)) parts <- c(parts, "B")
  if (grepl("DACCS", s, ignore.case = TRUE)) parts <- c(parts, "D")
  if (grepl("Other", s, ignore.case = TRUE)) parts <- c(parts, "O")
  if (length(parts) == 0) return("None") else return(paste(parts, collapse = "+"))
}

# --- Step 4: Read policy cost data ---
read_policycost <- function(i3_code) {
  bind_rows(lapply(names(paths), function(em_label) {
    rgdx.param(paths[[em_label]], "IAMC_template") %>%
      filter(i2 == "World", i3 == i3_code) %>%
      rename(Scenario = i1, Year = i4, Value = value) %>%
      mutate(Year = as.numeric(as.character(Year)),
             Emission_Level = em_label,
             Combo = sapply(Scenario, detect_combo))
  }))
}

# --- Step 5: Process data for generational equity ---
consumption <- read_policycost("Pol_Cos_Cns_Los_NPV_5pc") %>%
  filter(Combo != "BAU", Year >= 2020, Year <= 2100)

generation_df <- consumption %>%
  mutate(Period = ifelse(Year <= 2060, "Current", "Future")) %>%
  group_by(Combo, Emission_Level, Period) %>%
  summarise(Sum = sum(Value), .groups = "drop") %>%
  pivot_wider(names_from = Period, values_from = Sum) %>%
  mutate(
    Relative_Diff = (Current - Future) / Future,
    Emission_Level = factor(Emission_Level, levels = c("High", "Med", "Low", "vLow"))
  )

# --- Step 6: Define color palette using real combo names ---
combo_levels <- c(
  "B", "D", "A+B", "B+O", "D+O", "B+D", "A+D", 
  "B+D+O", "A+D+O", "A+B+O", "A+B+D", "A+B+D+O"
)

combo_colors <- c(
  "B"         = "#205080FF",
  "D"         = "#4890C0FF",
  "A+B"       = "#98E0F0FF",
  "B+O"       = "#505020FF",
  "D+O"       = "#B0A870FF",
  "B+D"       = "#F85048FF",
  "A+D"       = "#D0C090FF",
  "B+D+O"     = "#808040FF",
  "A+D+O"     = "#B02820FF",
  "A+B+O"     = "#F8C0B8FF",
  "A+B+D"     = "#D0D0D0FF",
  "A+B+D+O"   = "#801010FF"
)

# --- Step 7: Filter and assign color labels ---
generation_df <- generation_df %>%
  filter(Combo %in% combo_levels) %>%
  mutate(Combo = factor(Combo, levels = combo_levels))

# --- Step 8: Plot ---
gg_equity_box <- ggplot(generation_df, aes(x = Emission_Level, y = Relative_Diff)) +
  geom_boxplot(
    alpha = 0.2, width = 0.6, color = "gray60",
    outlier.shape = NA
  ) +
  geom_text(
    aes(label = Combo, color = Combo),
    position = position_jitter(width = 0.25, height = 0),
    size = 3.8,
    show.legend = FALSE
  ) +
  scale_color_manual(values = combo_colors) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Generational Equity from Discounted Consumption Loss (5%)",
    x = NULL,
    y = "Relative Difference [(Current - Future)/Future]"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = -0.1),
    legend.position = "none",
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.line = element_line(color = "black", linewidth = 0.8),
    panel.grid = element_blank()
  )

# --- Step 9: Save and display ---
ggsave("Generational_Equity_Boxplot_ComboLabel.png", plot = gg_equity_box, width = 12, height = 7, dpi = 300)
print(gg_equity_box)

