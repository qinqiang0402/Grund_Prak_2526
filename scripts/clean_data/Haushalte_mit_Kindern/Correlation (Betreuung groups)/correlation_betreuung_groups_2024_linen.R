library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)


# Einlesen der Bevölkerungsdaten (0–2 Jahre)


be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")

be_0bis2 <- be_sheet %>% 
  filter(
    Indikator == "Altersgruppen",
    Ausprägung == "bis 2 Jahre"
  ) %>%
  select(Indikator, Ausprägung, Jahr, Raumbezug, `Basiswert 1`)


# Einlesen der Kinderbetreuungsdaten (0–2 Jahre)


ki_sheet <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

ki_0bis2 <- ki_sheet %>% 
  filter(
    Indikator == "Altersgruppen",
    Ausprägung == "bis 2 Jahre"
  ) %>% 
  select(Indikator, Ausprägung, Jahr, Raumbezug, `Basiswert 1`)


# Umbenennen der Variablen


be_0bis2 <- be_0bis2 %>%
  rename(kinder_total = `Basiswert 1`)

ki_0bis2 <- ki_0bis2 %>%
  rename(kinder_betreut = `Basiswert 1`)


# Zusammenführen der Bevölkerung und Betreuungszahlen


df_betreut <- left_join(
  be_0bis2,
  ki_0bis2,
  by = c("Jahr", "Raumbezug", "Indikator", "Ausprägung")
)


# Berechnung: unbetreute Kinder und Betreuungsanteile


df_betreut <- df_betreut %>%
  mutate(
    kinder_unbetreut = kinder_total - kinder_betreut,
    anteil_unbetreut = kinder_unbetreut / kinder_total * 100,
    anteil_betreut = kinder_betreut / kinder_total * 100
  )


# Filter für das Jahr 2024


df_2024 <- df_betreut %>%
  filter(Jahr == 2024)


# Einlesen: Beschäftigungsdaten (Anteil weiblich)


ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")

df_emp <- ar_sheet %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(
    Jahr = as.numeric(Jahr),
    emp_female_pct = 100 * `Basiswert 1` / `Basiswert 2`
  ) %>%
  select(Jahr, Raumbezug, emp_female_pct)


# Einlesen: Haushalte mit Kindern


df_households <- be_sheet %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    Jahr = as.numeric(Jahr),
    households_pct = 100 * `Basiswert 1` / `Basiswert 2`
  ) %>%
  select(Jahr, Raumbezug, households_pct)


# Zusammenführen der Beschäftigung + Haushalte


df_merged <- df_emp %>%
  inner_join(df_households,
             by = c("Jahr", "Raumbezug"))


# Hinzufügen der Betreuungsquote (0–2 Jahre, Jahr 2024)

df_2024 <- df_merged %>%
  inner_join(
    df_betreut %>%
      filter(Jahr == 2024) %>%
      select(Raumbezug, anteil_betreut),
    by = "Raumbezug"
  ) %>%
  filter(
    Jahr == 2024,
    Raumbezug != "Stadt München"   
  )



# Bildung von Quantilsgruppen (Niedrig / Mittel / Hoch)


quantiles_2024 <- quantile(df_2024$anteil_betreut,
                           probs = c(0, 1/3, 2/3, 1),
                           na.rm = TRUE)

df_2024 <- df_2024 %>%
  mutate(
    betreuung_group = cut(
      anteil_betreut,
      breaks = quantiles_2024,
      labels = c("Niedrig", "Mittel", "Hoch"),
      include.lowest = TRUE
    )
  )


# Korrelationsberechnung für Gesamt, Niedrig, Hoch


cor_total <- cor(df_2024$emp_female_pct,
                 df_2024$households_pct,
                 use = "complete.obs",
                 method = "pearson")

cor_low <- df_2024 %>%
  filter(betreuung_group == "Niedrig") %>%
  summarise(cor = cor(emp_female_pct, households_pct,
                      use="complete.obs", method="pearson")) %>%
  pull(cor)

cor_high <- df_2024 %>%
  filter(betreuung_group == "Hoch") %>%
  summarise(cor = cor(emp_female_pct, households_pct,
                      use="complete.obs", method="pearson")) %>%
  pull(cor)


# Vorbereitung der Grafikdaten


df_bar <- tibble(
  group = c("Gesamt", "Niedrige Betreuung", "Hohe Betreuung"),
  corr = c(cor_total, cor_low, cor_high)
)



# Vorbereitung für eine Legendengruppe:
# Wir erzeugen ein Mapping mit einer künstlichen Variable "gruppe"
df_lines <- bind_rows(
  df_2024 %>% mutate(gruppe = "Gesamt"),
  df_2024 %>% filter(betreuung_group == "Hoch") %>% mutate(gruppe = "Hohe Betreuung"),
  df_2024 %>% filter(betreuung_group == "Niedrig") %>% mutate(gruppe = "Niedrige Betreuung")
)

# Farben definieren (gleicher Farbton: tiefrot / hellrot + schwarz)
farben <- c(
  "Gesamt" = "black",
  "Hohe Betreuung" = "#e66101",     
  "Niedrige Betreuung" = "#fdb863"  
)


# --- Grafik 1: Nur Gesamtlinie, keine Gruppen, keine Legende ---

m_effekt_single <- ggplot(df_2024, 
                          aes(x = emp_female_pct, y = households_pct)) +
  geom_point(color = "grey50", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1.2) +
  labs(
    x = "Frauenbeschäftigung (%)",
    y = "Haushalte mit Kindern (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

m_effekt_single

saveRDS(m_effekt_single, "results/figures/m_effekt/m_effekt_00_singleline.rds")


# --- Grafik 2: Linien + farbcodierte Punkte nach Gruppe ---

# Punkte mit Gruppenfarbe (nur Hoch & Niedrig)
df_points_grouped <- df_2024 %>%
  filter(betreuung_group %in% c("Hoch", "Niedrig")) %>%
  mutate(gruppe = case_when(
    betreuung_group == "Hoch" ~ "Hohe Betreuung",
    betreuung_group == "Niedrig" ~ "Niedrige Betreuung"
  ))

# Gesamt-Punkte (grau)
df_points_gesamt <- df_2024 %>%
  filter(betreuung_group == "Mittel" | is.na(betreuung_group)) %>% 
  mutate(gruppe = "Gesamt")


m_effekt_colored_points <- ggplot() +
  
  # --- 1) Gesamt-Punkte (grau) ---
  geom_point(
    data = df_points_gesamt,
    aes(x = emp_female_pct, y = households_pct),
    color = "grey50",
    alpha = 0.6
  ) +
  
  # --- 2) Hoch + Niedrig Punkte---
  geom_point(
    data = df_points_grouped,
    aes(x = emp_female_pct, y = households_pct, color = gruppe),
    alpha = 0.8
  ) +
  
  # --- 3) Linien  ---
  geom_smooth(
    data = df_lines,
    aes(x = emp_female_pct, y = households_pct, color = gruppe),
    method = "lm",
    se = FALSE,
    size = 1.2
  ) +
  
  scale_color_manual(
    values = farben,
    name = "Linien:"
  ) +
  
  labs(
    x = "Frauenbeschäftigung (%)",
    y = "Haushalte mit Kindern (%)"
  ) +
  theme_minimal(base_size = 14)

m_effekt_colored_points

saveRDS(m_effekt_colored_points,
        "results/figures/m_effekt/m_effekt_02_colored_points.rds")
