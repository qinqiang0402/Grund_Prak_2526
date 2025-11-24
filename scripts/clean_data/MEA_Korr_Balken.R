# 1. Bibliotheken laden
library(readxl)
library(tidyverse) # enthält dplyr, ggplot2, etc.
library(ggpubr)
library(broom)

# 2. Daten einlesen
# (Stelle sicher, dass die Pfade zu deinen Dateien stimmen)
export_be <- read_excel("data/raw/export_be.xlsx")
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")

export_ar <- read_excel("data/raw/export_ar.xlsx")
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")

# 3. Datenbereinigung: Mütteralter
all_districts_and_city <- be_sheet %>%
  filter(
    Indikator == "Durchschnittsalter Mütter erstgebärend",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    mean_age = `Basiswert 1` / `Basiswert 2`
  )

# 4. Datenbereinigung: Frauenanteil am Arbeitsmarkt
Sozialversicherungspflichtig_Beschäftigte_anteil_frau <- ar_sheet %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(
    anteil = 100 * `Basiswert 1` / `Basiswert 2`
  )

# 5. Daten zusammenfügen (Join)
korrelations_daten <- inner_join(
  all_districts_and_city, 
  Sozialversicherungspflichtig_Beschäftigte_anteil_frau, 
  by = c("Raumbezug", "Jahr")
)

# 6. Spearman-Korrelation berechnen
korrelations_stats <- korrelations_daten %>%
  group_by(Raumbezug) %>%
  summarize(
    test_stats = list(broom::tidy(cor.test(mean_age, anteil, method = "spearman", exact = FALSE))),
    .groups = "drop"
  ) %>%
  unnest(test_stats) %>%
  mutate(bezirk_nr = substr(Raumbezug, 1, 2))

# 7. Visualisierung
ggplot(korrelations_stats, 
       aes(x = reorder(Raumbezug, estimate), y = estimate)) +
  geom_segment(aes(xend = reorder(Raumbezug, estimate), yend = 0), color = "grey") +
  geom_point(aes(color = estimate), size = 3) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  coord_flip() +
  labs(
    title = "Zusammenhang: Mütteralter (Erstgeburt) & Frauen-Beschäftigtenanteil",
    subtitle = "Berechnet mittels Spearman-Rangkorrelation",
    x = "Stadtteile (sortiert nach R-Wert)",
    y = "Spearman Korrelationskoeffizient", 
    color = "R-Wert"
  ) +
  theme_minimal()
