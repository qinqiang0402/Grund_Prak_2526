library(readxl)
library(dplyr)
library(tidyr)

# 1. Daten laden
ar_raw <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")
be_raw <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
ki_raw <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

col_names <- c("Indikator", "Ausprägung", "Jahr", "Raumbezug", "Indikatorwert")

# 2. Daten extrahieren MIT QUELLE
ar_tab <- ar_raw %>%
  select(1:5) %>%
  mutate(across(everything(), as.character)) %>%
  setNames(col_names) %>%
  mutate(Quelle = "Arbeitsmarkt")

# Bevölkerung: Nur Haushalte mit Kindern
be_tab_haushalte <- be_raw %>%
  select(1:5) %>%
  mutate(across(everything(), as.character)) %>%
  setNames(col_names) %>%
  filter(grepl("Haushalte mit Kindern", Indikator, ignore.case = TRUE)) %>%
  mutate(Quelle = "Bevölkerung")

# Bevölkerung: Altersgruppen
be_tab_altersgruppen <- be_raw %>%
  select(1:5) %>%
  mutate(across(everything(), as.character)) %>%
  setNames(col_names) %>%
  filter(grepl("Altersgruppen", Indikator, ignore.case = TRUE)) %>%
  mutate(Quelle = "Bevölkerung")

# Kinderbetreuung: Altersgruppen
ki_tab_altersgruppen <- ki_raw %>%
  select(1:5) %>%
  mutate(across(everything(), as.character)) %>%
  setNames(col_names) %>%
  filter(grepl("Altersgruppen", Indikator, ignore.case = TRUE)) %>%
  mutate(Quelle = "Kinderbetreuung")

# 3. Alle Daten kombinieren
data <- bind_rows(
  ar_tab,
  be_tab_haushalte,
  be_tab_altersgruppen,
  ki_tab_altersgruppen
)

# 4. Selection Tabelle (ERWEITERT mit spezifischen Raumbezügen)
selection <- tribble(
  ~Indikator,                                           ~Ausprägung, ~Jahr,  ~Raumbezug, ~Quelle,
  "Sozialversicherungspflichtig Beschäftigte - Anteil",                         "weiblich",   "2024","Stadt München", "Arbeitsmarkt",
  "Sozialversicherungspflichtig Beschäftigte - Anteil",                         "weiblich",   "2010","06 Sendling", "Arbeitsmarkt",
  "Haushalte mit Kindern",                              "deutsch",    "2024","13 Bogenhausen", "Bevölkerung",
  "Haushalte mit Kindern",                              "deutsch",    "2012","03 Maxvorstadt", "Bevölkerung",
  "Altersgruppen",                                      "bis 2 Jahre", "2024","10 Moosach", "Bevölkerung",
  "Altersgruppen",                                      "bis 2 Jahre", "2000","20 Hadern", "Bevölkerung",
  "Altersgruppen",                                      "bis 2 Jahre", "2024","06 Sendling", "Kinderbetreuung",
  "Altersgruppen",                                      "bis 2 Jahre", "2007","25 Laim", "Kinderbetreuung"
)

# 5. Daten filtern
df_data <- data %>%
  semi_join(selection,
            by = c("Indikator", "Ausprägung", "Jahr", "Raumbezug", "Quelle"))

# 6. In RICHTIGER REIHENFOLGE anordnen (wie im Screenshot)
# Zuerst nach Gruppen sortieren, dann innerhalb der Gruppe
df_data <- data %>%
  semi_join(selection,
            by = c("Indikator", "Ausprägung", "Jahr", "Raumbezug", "Quelle"))

# 6. Jetzt bauen wir die Tabelle MANUELL in der richtigen Reihenfolge auf
# Das ist der einfachste Weg, um die exakte Struktur zu erhalten


# Schritt 1: Teile die Daten in separate DataFrames ---------------------

df_beschaeftigt <- df_data %>%
  filter(Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil") %>%
  arrange(Jahr)  # 2010 zuerst, dann 2024

df_haushalte <- df_data %>%
  filter(Indikator == "Haushalte mit Kindern") %>%
  arrange(Jahr)  # 2012 zuerst, dann 2024

df_altersgruppen_bev <- df_data %>%
  filter(Indikator == "Altersgruppen", Quelle == "Bevölkerung") %>%
  arrange(Jahr)  # 2000 zuerst, dann 2024

df_altersgruppen_ki <- df_data %>%
  filter(Indikator == "Altersgruppen", Quelle == "Kinderbetreuung") %>%
  arrange(Jahr)  # 2007 zuerst, dann 2024


# Schritt 2: Finale Tabelle OHNE Themenbereich-Zeilen -------------------

df_data_final <- bind_rows(
  df_beschaeftigt,       # 1–2: Beschäftigte
  df_haushalte,          # 3–4: Haushalte mit Kindern
  df_altersgruppen_bev,  # 5–6: Be_Altersgruppen (gleich umbenennen)
  df_altersgruppen_ki    # 7–8: Ki_Altersgruppen
)


# Schritt 3: Altersgruppen umbenennen + Quelle später wegwerfen --------

df_data_final <- df_data_final %>%
  mutate(
    Indikator = case_when(
      Indikator == "Altersgruppen" & Quelle == "Bevölkerung"     ~ "Be_Altersgruppen",
      Indikator == "Altersgruppen" & Quelle == "Kinderbetreuung" ~ "Ki_Altersgruppen",
      TRUE ~ Indikator
    )
  ) %>%
  select(-Quelle) %>%                              # Quelle jetzt löschen
  mutate(across(everything(), ~ ifelse(is.na(.), "", .))) %>%  # NA -> ""
  mutate(Index = row_number(), .before = Indikator)  %>%   # 先生成 Index
  select(-Index)

#_________________________
df_data_final <- df_data_final %>%
  mutate(
    Indikator = ifelse(
      Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
      "Anteil Frauenbeschäftigung",
      Indikator
    )
  )
#_______________________



# Anzeigen & speichern wie
View(df_data_final)
saveRDS(df_data_final, "df_data_qiang.rds")




# 10. Als CSV speichern (für Excel)
write.csv(df_data_final, "df_data.csv", row.names = FALSE, na = "")