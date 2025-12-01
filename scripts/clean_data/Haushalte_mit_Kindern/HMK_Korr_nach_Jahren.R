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
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    hmk = 100 * `Basiswert 1` / `Basiswert 2`
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


hmk_korr_gesamt_sw <- ggplot(korrelations_daten_clean, aes(x = hmk, y = anteil)) +
  geom_point(size = 1.3, color = "grey", alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1) +
  stat_cor(label.x.npc = "left", label.y.npc = "top") + 
  labs(
    title = "Korrelationskoeffizient gesamt (alle Jahre und Stadtteile) zwischen Haushalte mit Kindern und Frauenbeschäftigung",
    x = "Anteil Haushalte mit Kindern (%)",
    y = "Anteil Frauenbeschäftigung (%)"
  ) +
  theme_minimal()

hmk_korr_gesamt_sw
saveRDS(hmk_korr_gesamt_sw, "results/figures/Haushalt_mit_Kindern/hmk_korr_gesamt_sw.rds")


# ----------------------------------------------------------------------------------
hmk_korr_nach_jahr_color <- ggplot(korrelations_daten_clean, aes(x = hmk, y = anteil)) +
  geom_point(aes(color = factor(Jahr)), 
             size = 1.3,   
             alpha = 0.7) +   
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1) +
  stat_cor(
    label.x.npc = "left",   
    label.y.npc = "top",    
    color = "black",
    size = 5
  ) + 
  coord_cartesian(xlim = c(8, 28), ylim = c(48, 68)) +
  labs(
    title = "Korrelationskoeffizient nach Jahren zwischen Haushalte mit Kindern und Frauenbeschäftigung",
    x = "Anteil Haushalte mit Kindern (%)",
    y = "Anteil Frauenbeschäftigung (%)",
    color = "Jahr"
  ) +
  guides(color = guide_legend(ncol = 1)) + 
  theme_minimal()

hmk_korr_nach_jahr_color
saveRDS(hmk_korr_nach_jahr_color, "results/figures/Haushalt_mit_Kindern/hmk_korr_point_nach_jahr_color.rds")

# -----------------------------------------------------------------------
plot_data_final_year <- korrelations_daten_clean %>%
  group_by(Jahr) %>% 
  mutate(
    r = cor(hmk, anteil, use = "complete.obs"),
    r_label = sprintf("(R=%.2f)", r),
    Jahr_Label = paste(Jahr, r_label, sep = " ")
  ) %>%
  ungroup()

all_labels_year <- sort(unique(plot_data_final_year$Jahr_Label))
all_colors_year <- scales::hue_pal()(length(all_labels_year))
jahr_palette <- all_colors_year
names(jahr_palette) <- all_labels_year

hmk_point_line_nach_jahr_color <- ggplot(plot_data_final_year, aes(x = hmk, y = anteil)) +
  geom_smooth(aes(color = Jahr_Label, group = Jahr_Label),
              method = "lm", se = FALSE, linewidth = 1.1, alpha = 0.8) +
  geom_point(aes(color = Jahr_Label), size = 1.1, alpha = 0.5) +
  geom_smooth(aes(group = 1), method = "lm", color = "black", linewidth = 1.1, se = FALSE) +
  scale_color_manual(values = jahr_palette) +
  coord_cartesian(xlim = c(8, 28), ylim = c(48, 68)) +
  labs(
    title = "Korrelationskoeffizient nach Jahren zwischen Haushalte mit Kindern und Frauenbeschäftigung",
    x = "Anteil Haushalte mit Kindern (%)",
    y = "Anteil Frauenbeschäftigung (%)",
    color = "Jahr"
  ) +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 7),
    legend.key.height = unit(0.4, "cm")
  ) +
  guides(color = guide_legend(ncol = 1))

hmk_point_line_nach_jahr_color
saveRDS(hmk_point_line_nach_jahr_color, "results/figures/Haushalt_mit_Kindern/hmk_korr_point_line_nach_jahr_color.rds")

# ----------------------------------------------------------------------------
r_werte_check_year <- korrelations_daten_clean %>%
  group_by(Jahr) %>% 
  summarise(
    r_wert = cor(hmk, anteil)
  ) %>%
  mutate(
    is_negative = r_wert < 0,
    Jahr_Label = paste0(Jahr, " (R=", round(r_wert, 2), ")")
  )

plot_data_highlight_year <- korrelations_daten_clean %>%
  left_join(r_werte_check_year, by = "Jahr") 

hmk_non_simpson_nach_jahr <- ggplot(plot_data_highlight_year, aes(x = hmk, y = anteil)) +
  geom_smooth(
    data = subset(plot_data_highlight_year, is_negative == FALSE),
    aes(group = Jahr), 
    method = "lm", 
    se = FALSE, 
    color = "grey85", 
    linewidth = 0.8,
    alpha = 0.5
  ) +
  geom_smooth(
    data = subset(plot_data_highlight_year, is_negative == TRUE),
    aes(color = Jahr_Label, group = Jahr), 
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
  scale_color_manual(values = jahr_palette) +
  labs(
    title = "non Simpson's Paradox (nach Jahr)",
    subtitle = "Grau = Positiver Zusammenhang (R ≥ 0), Bunt = Negativer Zusammenhang (R < 0)",
    x = "Anteil Haushalte mit Kindern (%)",
    y = "Anteil Frauenbeschäftigung (%)",
    color = "Jahr mit R < 0" 
  ) +
  coord_cartesian(xlim = c(8, 28), ylim = c(48, 68)) +
  theme_minimal() +
  theme(legend.position = "right")

hmk_non_simpson_nach_jahr
saveRDS(hmk_non_simpson_nach_jahr, "results/figures/Haushalt_mit_Kindern/hmk_non_simpson_nach_jahr.rds")

# ------------------------------------------------------------------------------
plot_data_colored_year <- korrelations_daten_clean %>%
  group_by(Jahr) %>%  
  mutate(
    r_wert = cor(hmk, anteil),
    trend_richtung = ifelse(r_wert >= 0, "positiv", "negativ") 
  ) %>%
  ungroup() 

hmk_non_simpson_nach_jahr_blau_rot <- ggplot(plot_data_colored_year, aes(x = hmk, y = anteil)) +
  geom_smooth(
    aes(
      color = trend_richtung, 
      group = Jahr            
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
    title = "non Simpson's Paradox (nach Jahr)",
    subtitle = "Rot = Positiver Zusammenhang (R ≥ 0), Blau = Negativer Zusammenhang (R < 0)",
    x = "Anteil Haushalte mit Kindern (%)",
    y = "Anteil sozialversicherungspflichtig Beschäftigter Frauen (%)"
  ) +
  coord_cartesian(xlim = c(8, 28)) +
  theme_minimal() +
  theme(legend.position = "none")

hmk_non_simpson_nach_jahr_blau_rot
saveRDS(hmk_non_simpson_nach_jahr_blau_rot, "results/figures/Haushalt_mit_Kindern/hmk_non_simpson_nach_jahr_blau_rot.rds")
