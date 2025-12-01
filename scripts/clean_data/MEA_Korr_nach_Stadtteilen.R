library(readxl)
library(tidyverse)
library(ggpubr)
library(ggplot2)

export_be <- read_excel("data/raw/export_be.xlsx")
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")

export_ar <- read_excel("data/raw/export_ar.xlsx")
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")

all_districts_and_city <- be_sheet %>%
  filter(
    Indikator == "Durchschnittsalter Mütter erstgebärend",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    mean_age = `Basiswert 1` / `Basiswert 2` 
  )

Sozialversicherungspflichtig_Beschäftigte_anteil_frau <- ar_sheet %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(
    anteil = 100 * `Basiswert 1` / `Basiswert 2`
  )

korrelations_daten <- inner_join(
  all_districts_and_city, 
  Sozialversicherungspflichtig_Beschäftigte_anteil_frau, 
  by = c("Raumbezug", "Jahr")
)

korrelations_daten_clean <- korrelations_daten %>%
  filter(Raumbezug != "Stadt München")

mea_gesamt_sw <- ggplot(korrelations_daten_clean, aes(x = mean_age, y = anteil)) +
  geom_point(size = 1.3, color = "grey", alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1) +
  stat_cor(label.x.npc = "left", label.y.npc = "top") + # Spearmans Rho als Standard
  labs(
    title = "Korrelationskoeffizient gesamt (alle Jahre und Stadtteile) zwischen Erstgeburtsalter und Frauenbeschäftigung",
    x = "Durchschnittsalter Mütter erstgebärend",
    y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)"
  ) +
  theme_minimal()

mea_gesamt_sw
saveRDS(mea_gesamt_sw, "results/figures/MEA/mea_gesamt_sw.rds")

# ----------------------------------------------------------------------------------
mea_korr_nach_stadtteilen <- ggplot(korrelations_daten_clean, aes(x = mean_age, y = anteil)) +
  geom_point(aes(color = Raumbezug), 
             size = 1.3,     
             alpha = 0.7) +   
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1) +
  stat_cor(
    label.x.npc = "left",   
    label.y.npc = "top",    
    color = "black",
    size = 5
  ) + 
  # X- und Y-Limits angepasst für die Altersdaten
  coord_cartesian(xlim = c(29, 34), ylim = c(48, 68)) + 
  labs(
    title = "Korrelationskoeffizient nach Stadtteilen zwischen Erstgeburtsalter und Frauenbeschäftigung",
    x = "Durchschnittsalter Mütter erstgebärend",
    y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)",
    color = "Stadtteile" 
  ) +
  guides(color = guide_legend(ncol = 1)) + 
  theme_minimal()

mea_korr_nach_stadtteilen
saveRDS(mea_korr_nach_stadtteilen, "results/figures/MEA/mea_korr_nach_stadtteilen_point.rds")


# -----------------------------------------------------------------------
plot_data_final <- korrelations_daten_clean %>%
  group_by(Raumbezug) %>%
  mutate(
    r = cor(mean_age, anteil, use = "complete.obs"),
    r_label = sprintf("(R=%.2f)", r),
    Raumbezug_Label = paste(Raumbezug, r_label, sep = " ") # Stadtteil und R-Wert für die Legende
  ) %>%
  ungroup()

all_labels <- sort(unique(plot_data_final$Raumbezug_Label))
all_colors <- scales::hue_pal()(length(all_labels))
stadtteil_palette <- all_colors
names(stadtteil_palette) <- all_labels

mea_korr_nach_stadtteilen_point_line <- ggplot(plot_data_final, aes(x = mean_age, y = anteil)) +
  geom_smooth(aes(color = Raumbezug_Label, group = Raumbezug_Label),
              method = "lm", se = FALSE, linewidth = 1.1, alpha = 0.8) +
  geom_point(aes(color = Raumbezug_Label), size = 1.1, alpha = 0.5) +
  geom_smooth(aes(group = 1), method = "lm", color = "black", linewidth = 1.1, se = FALSE) +
  scale_color_manual(values = stadtteil_palette) +
  labs(
    title = "Korrelationskoeffizient nach Stadtteilen zwischen Erstgeburtsalter und Frauenbeschäftigung",
    x = "Durchschnittsalter Mütter erstgebärend",
    y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)",
    color = "Stadtteile" 
  ) +
  coord_cartesian(xlim = c(29, 34), ylim = c(48, 68)) +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 7),
    legend.key.height = unit(0.4, "cm")
  ) +
  guides(color = guide_legend(ncol = 1))

mea_korr_nach_stadtteilen_point_line
saveRDS(mea_korr_nach_stadtteilen_point_line, "results/figures/MEA/mea_korr_nach_stadtteilen_point_line.rds")

# ----------------------------------------------------------------------
r_werte_check <- korrelations_daten_clean %>%
  group_by(Raumbezug) %>%
  summarise(
    r_wert = cor(mean_age, anteil)
  ) %>%
  mutate(
    is_negative = r_wert < 0,
    Raumbezug_Label = paste0(Raumbezug, " (R=", round(r_wert, 2), ")")
  )

plot_data_highlight <- korrelations_daten_clean %>%
  left_join(r_werte_check, by = "Raumbezug")

ggplot(plot_data_highlight, aes(x = mean_age, y = anteil)) +
  geom_smooth(
    data = subset(plot_data_highlight, is_negative == FALSE),
    aes(group = Raumbezug), 
    method = "lm", 
    se = FALSE, 
    color = "grey85", 
    linewidth = 0.8,
    alpha = 0.5
  ) +
  geom_smooth(
    data = subset(plot_data_highlight, is_negative == TRUE),
    aes(color = Raumbezug_Label, group = Raumbezug), 
    method = "lm", 
    se = FALSE, 
    linewidth = 1.1, 
    alpha = 1
  ) +
  geom_smooth(
    aes(group = 1), 
    method = "lm", 
    color = "black", 
    linewidth = 1.1, 
    se = FALSE
  ) +
  scale_color_manual(values = stadtteil_palette) +
  labs(
    title = "non Simpson's Paradox",
    subtitle = "Grau = Positiver Zusammenhang (R ≥ 0), Bunt = Negativer Zusammenhang (R < 0)",
    x = "Durchschnittsalter Mütter erstgebärend",
    y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)",
    color = "Stadtteile mit R < 0" 
  ) +
  coord_cartesian(xlim = c(29, 34), ylim = c(48, 68)) +
  theme_minimal() +
  theme(legend.position = "right")

# -------------------------------------------------------------------------
plot_data_colored <- korrelations_daten_clean %>%
  group_by(Raumbezug) %>% 
  mutate(
    r_wert = cor(mean_age, anteil),
    trend_richtung = ifelse(r_wert >= 0, "positiv", "negativ") 
  ) %>%
  ungroup() 

ggplot(plot_data_colored, aes(x = mean_age, y = anteil)) +
  geom_smooth(
    aes(
      color = trend_richtung,  
      group = Raumbezug            
    ), 
    method = "lm", 
    se = FALSE, 
    linewidth = 1.1,    
    alpha = 0.6          
  ) +
  geom_smooth(aes(group = 1), method = "lm", color = "black", linewidth = 1.1, se = FALSE) +
  scale_color_manual(values = c(
    "positiv" = "red", 
    "negativ" = "blue"  
  )) +
  labs(
    title = "non Simpson's Paradox",
    subtitle = "Rot = positiver Zusammenhang (R ≥ 0), Blau = negativer Zusammenhang (R < 0)",
    x = "Durchschnittsalter Mütter erstgebärend",
    y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)"
  ) +
  coord_cartesian(xlim = c(29, 34), ylim = c(48, 68)) +
  theme_minimal() +
  theme(legend.position = "none")

