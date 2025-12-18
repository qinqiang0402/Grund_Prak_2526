library(readxl)
library(tidyverse)
library(ggpubr)
library(ggplot2)


# Einlesen der Rohdaten aus den Excel-Dateien
export_be <- read_excel("data/raw/export_be.xlsx")
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")

export_ar <- read_excel("data/raw/export_ar.xlsx")
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")

# Datensatz 1: Durchschnittsalter Mütter erstgebärend
all_districts_and_city <- be_sheet %>%
  filter(
    Indikator == "Durchschnittsalter Mütter erstgebärend",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    mean_age = `Basiswert 1` / `Basiswert 2` 
  )


# Datensatz 2: Frauenbeschäftigung
Sozialversicherungspflichtig_Beschäftigte_anteil_frau <- ar_sheet %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(
    anteil = 100 * `Basiswert 1` / `Basiswert 2`
  )


# Zusammenfügen der beiden Datensätze über Stadtteil und Jahr
korrelations_daten <- inner_join(
  all_districts_and_city, 
  Sozialversicherungspflichtig_Beschäftigte_anteil_frau, 
  by = c("Raumbezug", "Jahr")
)


# Filtern: "Stadt München" entfernen
korrelations_daten_clean <- korrelations_daten %>%
  filter(Raumbezug != "Stadt München")


# Plot 1: gesamter Zusammenhang(25 Stadtteile und 10 Jahre)
# Einfacher Scatterplot mit einer einzigen Regressionsgeraden für alle Daten
ggplot(korrelations_daten_clean, aes(x = mean_age, y = anteil)) +
  geom_point(size = 1.3, color = "grey", alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1) +
  stat_cor(label.x.npc = "left", label.y.npc = "top") + 
  labs(
    title = "Korrelationskoeffizient gesamt (alle Jahre und Stadtteile) zwischen Erstgeburtsalter und Frauenbeschäftigung",
    x = "Durchschnittsalter Mütter erstgebärend",
    y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)"
  ) +
  theme_minimal()

#-----------------------------------------------------------------------------------------------
#Plot 2: Färbung nach Jahren
mea_korr_jahr <- ggplot(korrelations_daten_clean, aes(x = mean_age, y = anteil)) +
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
  coord_cartesian(xlim = c(29, 34), ylim = c(48, 68)) + 
  labs(
    title = "Korrelationskoeffizient nach Jahren zwischen Erstgeburtsalter und Frauenbeschäftigung",
    x = "Durchschnittsalter Mütter erstgebärend",
    y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)",
    color = "Jahr" 
  ) +
  guides(color = guide_legend(ncol = 1)) + 
  theme_minimal()

# Plot anzeigen und speichern
mea_korr_jahr
saveRDS(mea_korr_jahr, "results/figures/MEA/mea_korr_jahr_point.rds")

# -----------------------------------------------------------------------
# Plot 3:Korrelation nach Jahren
plot_data_final_year <- korrelations_daten_clean %>%
  group_by(Jahr) %>% 
  mutate(
    r = cor(mean_age, anteil, use = "complete.obs"),
    r_label = sprintf("(R=%.2f)", r),
    Jahr_Label = paste(Jahr, r_label, sep = " ")
  ) %>%
  ungroup()


all_labels_year <- sort(unique(plot_data_final_year$Jahr_Label))
all_colors_year <- scales::hue_pal()(length(all_labels_year))
jahr_palette <- all_colors_year
names(jahr_palette) <- all_labels_year

mea_korr_jahr_point_line <- ggplot(plot_data_final_year, aes(x = mean_age, y = anteil)) +
  geom_smooth(aes(color = Jahr_Label, group = Jahr_Label),
              method = "lm", se = FALSE, linewidth = 1.1, alpha = 0.8) +
  geom_point(aes(color = Jahr_Label), size = 1.1, alpha = 0.5) +
  geom_smooth(aes(group = 1), method = "lm", color = "black", linewidth = 1.1, se = FALSE) +
  scale_color_manual(values = jahr_palette) +
  labs(
    title = "Korrelationskoeffizient nach Jahren zwischen Erstgeburtsalter und Frauenbeschäftigung",
    x = "Durchschnittsalter Mütter erstgebärend",
    y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)",
    color = "Jahr"
  ) +
  coord_cartesian(xlim = c(29, 34), ylim = c(48, 68)) +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 7),
    legend.key.height = unit(0.4, "cm")
  ) +
  guides(color = guide_legend(ncol = 1))


# Plot anzeigen und speichern
mea_korr_jahr_point_line
saveRDS(mea_korr_jahr_point_line, "results/figures/MEA/mea_korr_jahr_point_line.rds")
