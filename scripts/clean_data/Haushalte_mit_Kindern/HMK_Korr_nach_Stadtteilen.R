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

hmk_korr_gesamt_point_sw <- ggplot(korrelations_daten_clean, aes(x = hmk, y = anteil)) +
  geom_point(size = 1.3, color = "grey", alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1) +
  stat_cor(label.x.npc = "left", label.y.npc = "top") + 
  labs(
    title = "Korrelationskoeffizient gesamt (alle Jahre und Städte) zwischen Haushalte mit Kindern und Frauenbeschäftigung",
    x = "Anteil Haushalte mit Kindern (%)",
    y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)"
  ) +
  theme_minimal()

hmk_korr_gesamt_point_sw
saveRDS(hmk_korr_gesamt_point_sw, "results/figures/Haushalt_mit_Kindern/hmk_korr_gesamt_point_sw.rds")

#----------------------------------------------------------------------------------
hmk_korr_ponit_nach_stadtteile_color <- ggplot(korrelations_daten_clean, aes(x = hmk, y = anteil)) +
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
  coord_cartesian(xlim = c(8, 28), ylim = c(48, 68)) +
  labs(
    title = "Korrelationskoeffizient nach Stadtteilen zwischen Haushalte mit Kindern und Frauenbeschäftigung",
    x = "Anteil Haushalte mit Kindern (%)",
    y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)",
    color = "Stadtteile" 
  ) +
  guides(color = guide_legend(ncol = 1)) + 
  theme_minimal()

hmk_korr_ponit_nach_stadtteile_color
saveRDS(hmk_korr_ponit_nach_stadtteile_color, "results/figures/Haushalt_mit_Kindern/hmk_korr_ponit_nach_stadtteile_color.rds")


#-----------------------------------------------------------------------
korrelations_daten <- inner_join(
  all_districts_and_city, 
  Sozialversicherungspflichtig_Beschäftigte_anteil_frau, 
  by = c("Raumbezug", "Jahr")
)

korrelations_daten_clean <- korrelations_daten %>%
  filter(Raumbezug != "Stadt München")



plot_data_final <- korrelations_daten_clean %>%
  group_by(Raumbezug) %>%
  mutate(
    r = cor(hmk, anteil, use = "complete.obs"),
    r_label = sprintf("(R=%.2f)", r),
    Raumbezug_Label = paste(Raumbezug, r_label, sep = " ")
  ) %>%
  ungroup()

all_labels <- sort(unique(plot_data_final$Raumbezug_Label))
all_colors <- scales::hue_pal()(length(all_labels))
stadtteil_palette <- all_colors
names(stadtteil_palette) <- all_labels

hmk_korr_point_line_nach_stadtteile_color <- ggplot(plot_data_final, aes(x = hmk, y = anteil)) +
  geom_smooth(aes(color = Raumbezug_Label, group = Raumbezug_Label),
              method = "lm", se = FALSE, linewidth = 1.1, alpha = 0.8) +
  geom_point(aes(color = Raumbezug_Label), size = 1.1, alpha = 0.5) +
  geom_smooth(aes(group = 1), method = "lm", color = "black", linewidth = 1.1, se = FALSE) +
  scale_color_manual(values = stadtteil_palette) +
  labs(
    title = "Korrelationskoeffizient nach Stadtteilen zwischen Haushalte mit Kindern und Frauenbeschäftigung",
    x = "Anteil Haushalte mit Kindern (%)",
    y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)",
    color = "Stadtteile" 
  ) +
  coord_cartesian(xlim = c(8, 28), ylim = c(48, 68)) +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 7),
    legend.key.height = unit(0.4, "cm")
  ) +
  guides(color = guide_legend(ncol = 1))

hmk_korr_point_line_nach_stadtteile_color

saveRDS(hmk_korr_point_line_nach_stadtteile_color, "results/figures/Haushalt_mit_Kindern/hmk_korr_point_line_nach_stadtteile_color.rds")


#----------------------------------------------------------------------------
r_werte_check <- korrelations_daten_clean %>%
  group_by(Raumbezug) %>%
  summarise(
    r_wert = cor(hmk, anteil)
  ) %>%
  mutate(
    is_negative = r_wert < 0,
    Raumbezug_Label = paste0(Raumbezug, " (R=", round(r_wert, 2), ")")
  )

plot_data_highlight <- korrelations_daten_clean %>%
  left_join(r_werte_check, by = "Raumbezug")

hmk_simpson_stadtteile_kleiner_0 <- ggplot(plot_data_highlight, aes(x = hmk, y = anteil)) +
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
    title = "Simpson's Paradox",
    subtitle = "Grau = Positiver Zusammenhang, Bunt = Negativer Zusammenhang",
    x = "Anteil Haushalte mit Kindern (%)",
    y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)",
    color = "Stadtteile mit R < 0" 
  ) +
  coord_cartesian(xlim = c(8, 28), ylim = c(48, 68)) +
  theme_minimal() +
  theme(legend.position = "right")

hmk_simpson_stadtteile_kleiner_0
saveRDS(hmk_simpson_stadtteile_kleiner_0, "results/figures/Haushalt_mit_Kindern/hmk_simpson_stadtteile_kleiner_0.rds")

#------------------------------------------------------------------------------
plot_data_colored <- korrelations_daten_clean %>%
  group_by(Raumbezug) %>%
  mutate(
    r_wert = cor(hmk, anteil),
    trend_richtung = ifelse(r_wert >= 0, "positiv", "negativ")
  ) %>%
  ungroup() 

hmk_simpson_blau_rot <- ggplot(plot_data_colored, aes(x = hmk, y = anteil)) +
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
    title = "Simpson's Paradox",
    subtitle = "Rot = positiver Zusammenhang (R > 0), Blau = negative Zusammenhang (R < 0)",
    x = "Anteil Haushalte mit Kindern (%)",
    y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)"
  ) +
  coord_cartesian(xlim = c(8, 28), ylim = c(48, 68)) +
  theme_minimal() +
  theme(legend.position = "none")

hmk_simpson_blau_rot
saveRDS(hmk_simpson_blau_rot, "results/figures/Haushalt_mit_Kindern/hmk_simpson_blau_rot.rds")
