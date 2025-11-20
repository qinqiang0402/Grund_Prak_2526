library(readxl)

export_be <- read_excel("data/raw/export_be.xlsx") 
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
export_ar <- read_excel("data/raw/export_ar.xlsx") 
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")

library(tidyverse)


hk_bezirke <- be_sheet %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt",
    Raumbezug != "Stadt München" 
  ) %>%
  mutate(
    genau_indikator = 100 * `Basiswert 1` / `Basiswert 2`
  ) %>%
  select(Jahr, Raumbezug, genau_indikator)

hk_durchschnitt_pro_jahr <- hk_bezirke %>%
  group_by(Jahr) %>%
  summarise(
    Jahres_Durchschnitt = mean(genau_indikator, na.rm = TRUE)
  )

hk_ueber_durchschnitt <- hk_bezirke %>%
  left_join(hk_durchschnitt_pro_jahr, by = "Jahr") %>%
  filter(genau_indikator >= Jahres_Durchschnitt) %>%
  select(Jahr, Raumbezug, genau_indikator, Jahres_Durchschnitt)


hk_unter_durchschnitt <- hk_bezirke %>%
  left_join(hk_durchschnitt_pro_jahr, by = "Jahr") %>%
  filter(genau_indikator < Jahres_Durchschnitt) %>%
  select(Jahr, Raumbezug, genau_indikator, Jahres_Durchschnitt)

sv_anteil <- ar_sheet %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich",
    Raumbezug != "Stadt München"
  ) %>%
  select(Jahr, Raumbezug, `Basiswert 1`, `Basiswert 2`) 


ergebnis_a_b <- hk_ueber_durchschnitt %>%
  inner_join(sv_anteil, by = c("Jahr", "Raumbezug")) %>%
  group_by(Jahr) %>%
  summarise(
    A = 100 * sum(`Basiswert 1`) / sum(`Basiswert 2`),
    B = 100 * (1 - sum(`Basiswert 1`) / sum(`Basiswert 2`))
  )


ergebnis_c_d <- hk_unter_durchschnitt %>%
  inner_join(sv_anteil, by = c("Jahr", "Raumbezug")) %>%
  group_by(Jahr) %>%
  summarise(
    C = 100 * sum(`Basiswert 1`) / sum(`Basiswert 2`),
    D = 100 * (1 - sum(`Basiswert 1`) / sum(`Basiswert 2`))
  )


finales_vergleichs_ergebnis <- ergebnis_a_b %>%
  left_join(ergebnis_c_d, by = "Jahr")


odds_ratio_tabelle <- finales_vergleichs_ergebnis %>%
  mutate(
    Odds_Ratio_hkue_hkun_sv = (A * D) / (B * C),
    Odds_Ratio_hkun_hkue_sv = (C * B) / (A * D)
  ) %>%
  select(Jahr, Odds_Ratio_hkue_hkun_sv, Odds_Ratio_hkun_hkue_sv)


library(ggplot2)

ggplot(data = odds_ratio_tabelle, aes(x = Jahr, y = Odds_Ratio_hkun_hkue_sv)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", linewidth = 1) +
  geom_line(color = "#2E86C1", linewidth = 1.2) + 
  geom_point(color = "#1B4F72", size = 3) +
  labs(
    title = "Entwicklung der Odds Ratio",
    x = "Jahr",
    y = "Odds Ratio "
  ) +
  scale_x_continuous(breaks = unique(odds_ratio_tabelle$Jahr)) + 
  scale_y_continuous(limits = c(1.0, 1.20)) + 
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
