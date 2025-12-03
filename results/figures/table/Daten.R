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
  "Sozialversicherungspflichtig Beschäftigte - Anteil", "weiblich",   "2024","Stadt München", "Arbeitsmarkt",
  "Sozialversicherungspflichtig Beschäftigte - Anteil", "weiblich",   "2010","01 Altstadt - Lehel", "Arbeitsmarkt",
  "Haushalte mit Kindern",                              "deutsch",    "2024","02 Ludwigsvorstadt - Isarvorstadt", "Bevölkerung",
  "Haushalte mit Kindern",                              "deutsch",    "2012","03 Maxvorstadt", "Bevölkerung",
  "Altersgruppen",                                      "bis 2 Jahre", "2024","22 Aubing - Lochhausen - Langwied", "Bevölkerung",
  "Altersgruppen",                                      "bis 2 Jahre", "2000","23 Allach - Untermenzing", "Bevölkerung",
  "Altersgruppen",                                      "bis 2 Jahre", "2024","24 Feldmoching - Hasenbergl", "Kinderbetreuung",
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

# Schritt 1: Teile die Daten in separate DataFrames
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

# Schritt 2: Baue die finale Tabelle in EXAKTER Reihenfolge
df_data_final <- bind_rows(
  # 1. Sozialversicherungspflichtig Beschäftigte
  df_beschaeftigt,
  
  # 2. Thematische Zeile: Arbeitsmarkt
  data.frame(
    Indikator = "(Themenbereich Arbeitsmarkt)",
    Ausprägung = "", 
    Jahr = "", 
    Raumbezug = "", 
    Indikatorwert = "",
    Quelle = "",
    stringsAsFactors = FALSE
  ),
  
  # 3. Haushalte mit Kindern
  df_haushalte,
  
  # 4. Altersgruppen aus Bevölkerung (direkt nach Haushalten)
  df_altersgruppen_bev,
  
  # 5. Thematische Zeile: Bevölkerung (NACH Altersgruppen aus Bevölkerung!)
  data.frame(
    Indikator = "(Themenbereich Bevölkerung)",
    Ausprägung = "", 
    Jahr = "", 
    Raumbezug = "", 
    Indikatorwert = "",
    Quelle = "",
    stringsAsFactors = FALSE
  ),
  
  # 6. Altersgruppen aus Kinderbetreuung
  df_altersgruppen_ki,
  
  # 7. Thematische Zeile: Kinderbetreuung (NACH Altersgruppen aus Kinderbetreuung!)
  data.frame(
    Indikator = "(Themenbereich Kinderbetreuung)",
    Ausprägung = "", 
    Jahr = "", 
    Raumbezug = "", 
    Indikatorwert = "",
    Quelle = "",
    stringsAsFactors = FALSE
  )
)

# Schritt 3: Entferne die Quelle-Spalte (wenn nicht benötigt)
df_data_final <- df_data_final %>%
  select(-Quelle) %>%
  # Ersetze NA mit leeren Strings
  mutate(across(everything(), ~ifelse(is.na(.), "", .)))

# Schritt 4: Index-Spalte hinzufügen (optional)
df_data_final <- df_data_final %>%
  mutate(Index = row_number(), .before = Indikator)

# 9. Anzeigen und speichern
View(df_data_final)
saveRDS(df_data_final, "df_data.rds")

# 10. Als CSV speichern (für Excel)
write.csv(df_data_final, "df_data.csv", row.names = FALSE, na = "")