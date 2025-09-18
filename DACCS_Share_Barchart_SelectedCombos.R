# --- REQUIRED LIBRARIES ---
library(gdxrrw)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggtext)
library(patchwork)
library(scales)
library(tidyr)

# --- GAMS INIT ---
igdx("/Library/Frameworks/GAMS.framework/Versions/49/Resources")

# --- COMBO DETECTION (only 8 combos) ---
selected_combos <- c("D", "A+D", "B+D", "D+O", "A+B+D", "A+D+O", "B+D+O", "A+B+D+O")

combo_map <- setNames(seq_along(selected_combos), selected_combos)
combo_label_map <- setNames(selected_combos, as.character(seq_along(selected_combos)))

get_combo_label <- function(scn) {
  has <- function(x) grepl(x, scn, ignore.case = TRUE)
  used <- c()
  if (has("BECCS"))  used <- c(used, "B")
  if (has("DACCS"))  used <- c(used, "D")
  if (has("Affor"))  used <- c(used, "A")
  if (has("Other"))  used <- c(used, "O")
  if (length(used) == 0) return(NA_character_)
  key <- paste0(sort(used), collapse = "+")
  if (key %in% selected_combos) key else NA_character_
}

# --- DATA LOAD ---
# vars <- c("Sec_Ene", "Sec_Ene_Inp_Tot_DAC")
# high_data <- read_variable("/Users/suku/Downloads/Results/High/feas/gdx/global_17_merged.gdx", "High", vars)
# med_data  <- read_variable("/Users/suku/Downloads/Results/Med/feas/gdx/global_17_merged.gdx", "Med", vars)
# low_data  <- read_variable("/Users/suku/Downloads/Results/Low/feas/gdx/global_17_merged.gdx", "Low", vars)
# vlow_data <- read_variable("/Users/suku/Downloads/Results/vLow/feas/gdx/global_17_merged.gdx", "vLow", vars)

all_data <- bind_rows(high_data, med_data, low_data, vlow_data) %>%
  mutate(ComboLabel = sapply(Scenario, get_combo_label)) %>%
  filter(!is.na(ComboLabel), Year >= 2040, Year <= 2100)

# --- CALCULATE CUMULATIVE DACCS SHARE ---
share_data <- all_data %>%
  group_by(Scenario, Parameter, Emission_Level, ComboLabel) %>%
  summarise(Cumulative = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Parameter, values_from = Cumulative) %>%
  mutate(Share = Sec_Ene_Inp_Tot_DAC / Sec_Ene) %>%
  complete(
    Emission_Level = c("High", "Med", "Low", "vLow"),
    ComboLabel = selected_combos,
    fill = list(Share = NA)
  ) %>%
  mutate(
    ComboLabel = factor(ComboLabel, levels = selected_combos),
    Emission_Level = factor(Emission_Level, levels = c("High", "Med", "Low", "vLow"))
  )

# --- MANUAL COLOR PALETTE FOR 8 COMBOS ---
combo_colors <- setNames(RColorBrewer::brewer.pal(8, "Set2"), selected_combos)

# --- PLOT ---
plot_daccs_share_bar <- ggplot(share_data, aes(x = Emission_Level, y = Share, fill = ComboLabel)) +
  geom_col(
    position = position_dodge(preserve = "single", width = 0.9),
    width = 0.65
  ) +
  scale_fill_manual(
    values = combo_colors,
    guide = guide_legend(override.aes = list(shape = 21, size = 6)),
    name = NULL
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "(g) Cumulative DACCS Share in Total Secondary Energy (2040–2100)",
    x = NULL,
    y = "DACCS Share (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = -0.1),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    legend.title = element_blank(),
    legend.key.size = unit(0.1, "cm"),  
    legend.text = element_text(size = 10),
    legend.position = "right",
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.2)
  )

# --- SAVE AND DISPLAY ---
ggsave("DACCS_Share_Barchart_SelectedCombos.png", plot = plot_daccs_share_bar, width = 10, height = 6, dpi = 300)
print(plot_daccs_share_bar)