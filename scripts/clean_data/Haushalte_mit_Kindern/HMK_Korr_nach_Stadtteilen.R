library(readxl)
library(tidyverse)
library(ggpubr)
library(ggplot2)

# Einlesen der Rohdaten aus den Excel-Dateien
export_be <- read_excel("data/raw/export_be.xlsx")
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")

export_ar <- read_excel("data/raw/export_ar.xlsx")
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")


# Datensatz 1: Haushalte mit Kindern (hmk)
# Wir filtern "Haushalte mit Kindern" und berechnen den Anteil in %
all_districts_and_city <- be_sheet %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    hmk = 100 * `Basiswert 1` / `Basiswert 2`
  )

# Datensatz 2: Frauenbeschäftigung
# Wir filtern die weibliche sozialversicherungspflichtig Beschäftigten (Frauenbeschäftigung) und berechnen den Anteil in %
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

# Entfernen des Aggregats "Stadt München", um nur einzelne Stadtteile zu analysieren
korrelations_daten_clean <- korrelations_daten %>%
  filter(Raumbezug != "Stadt München")

# Plot 1: gesamter Zusammenhang(25 Stadtteile und 13 Jahre)
# Einfacher Scatterplot mit einer einzigen Regressionsgeraden für alle Daten
hmk_korr_gesamt_point_sw <- ggplot(korrelations_daten_clean, aes(x = hmk, y = anteil)) +
  geom_point(size = 1.4, color = "grey70", alpha = 0.7) + 
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1.2) +
  labs(
    x = "Haushalte mit Kindern (%)",
    y = "Frauenbeschäftigung (%)" 
  ) +
  coord_cartesian(xlim = c(8, 28), ylim = c(48, 68)) +
  theme_bw() + 
  theme(
    axis.title = element_text(size = 28, face = "bold"),
    axis.text = element_text(size = 20, color = "black"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

# Plot anzeigen und speichern
hmk_korr_gesamt_point_sw
saveRDS(hmk_korr_gesamt_point_sw, "results/figures/Haushalt_mit_Kindern/hmk_korr_gesamt_point_sw.rds")


#-----------------------------------------------------------------------
# Plot 2: Korrelation nach Stadtteilen(Simpson's Paradox)
# Vorbereitung: Berechnung des Korrelationskoeffizienten pro Stadtteil
plot_data_final <- korrelations_daten_clean %>%
  group_by(Raumbezug) %>%
  mutate(
    r = cor(hmk, anteil, use = "complete.obs"),
    r_label = sprintf("(R=%.2f)", r),
    Raumbezug_Label = paste(Raumbezug, r_label, sep = " ")
  ) %>%
  ungroup()

# Plot mit individuellen Regressionslinien pro Stadtteil (grau) + gesamte Linie (schwarz)
hmk_korr_point_line_nach_stadtteile_color <- ggplot(plot_data_final, aes(x = hmk, y = anteil)) +
  geom_smooth(aes(group = Raumbezug_Label), 
              method = "lm", se = FALSE, 
              color = "grey60", linewidth = 1, alpha = 0.7) +
  geom_point(color = "grey70", size = 1.4, alpha = 0.7) +
  geom_smooth(aes(group = 1), method = "lm", color = "black", linewidth = 1.2, se = FALSE) +
  labs(
    x = "Haushalte mit Kindern (%)",
    y = "Frauenbeschäftigung (%)"
  ) +
  coord_cartesian(xlim = c(8, 28), ylim = c(48, 68)) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 28, face = "bold"), 
    axis.text = element_text(size = 20, color = "black"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

# Plot anzeigen und speichern
hmk_korr_point_line_nach_stadtteile_color
saveRDS(hmk_korr_point_line_nach_stadtteile_color, "results/figures/Haushalt_mit_Kindern/hmk_korr_point_line_nach_stadtteile_color.rds")

