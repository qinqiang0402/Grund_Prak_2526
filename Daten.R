library(readxl)
library(dplyr)
library(tibble)
library(tidyr)

ar_raw <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")
be_raw <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
ki_raw <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

col_names <- c("Indikator",
                "Ausprägung",
                "Jahr",
                "Raumbezug",
                "Indikatorwert",
                "Basiswert1",
                "Basiswert2",
                "Name_Basiswert1",
                "Name_Basiswert2")

# Aus jedem Sheet die Spalten A–I nehmen (1:9) und umbenennen
ar_tab <- ar_raw %>%
  select(1:9) %>%
  mutate(across(everything(), as.character)) %>%
  setNames(col_names)

be_tab <- be_raw %>%
  select(1:9) %>%
  mutate(across(everything(), as.character)) %>%
  setNames(col_names)

ki_tab <- ki_raw %>%
  select(1:9) %>%
  mutate(across(everything(), as.character)) %>%
  setNames(col_names)

data <- bind_rows(ar_tab, be_tab, ki_tab)


selection <- tribble(
  ~Indikator,                                           ~Ausprägung, ~Jahr,  ~Raumbezug,
  "Sozialversicherungspflichtig Beschäftigte - Anteil", "weiblich",   "2024","Stadt München",
  "Sozialversicherungspflichtig Beschäftigte - Anteil", "weiblich",   "2000","01 Altstadt - Lehel",
  "Haushalte mit Kindern",                              "deutsch",    "2024","02 Ludwigsvorstadt - Isarvorstadt",
  "Haushalte mit Kindern",                              "deutsch",    "2012","03 Maxvorstadt",
  "Betreuungsangebot",                                  "insgesamt",  "2024","24 Feldmoching - Hasenbergl",
  "Betreuungsangebot",                                  "insgesamt",  "2007","25 Laim"
)

selection %>%
  filter(Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
         Jahr == "2000")

df_data <- data %>%
  semi_join(selection,
            by = c("Indikator", "Ausprägung", "Jahr", "Raumbezug")) %>%
  arrange(
    factor(
      Indikator,
      levels = c("Sozialversicherungspflichtig Beschäftigte - Anteil",
                 "Haushalte mit Kindern",
                 "Betreuungsangebot")
    ),
    Jahr,
    Raumbezug
  ) %>%
  mutate(across(where(is.character), ~replace_na(., "")))

# add descriptive rows "(Themenbereich...)" under each indicator group
sv_row <- max(which(df_data$Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil"))
hk_row <- max(which(df_data$Indikator == "Haushalte mit Kindern"))
betr_row <- max(which(df_data$Indikator == "Betreuungsangebot"))
# # Insert thematic rows
df_data <- df_data %>%
  add_row(
    Indikator = "(Themenbereich Arbeitsmarkt)",
    Ausprägung = "", Jahr = "", Raumbezug = "", Indikatorwert = "",
    Basiswert1 = "", Basiswert2 = "", Name_Basiswert1 = "", Name_Basiswert2 = "",
    .after = sv_row
  )
# Update positions since we added a row
hk_row <- hk_row + 1
betr_row <- betr_row + 1

df_data <- df_data %>%
  add_row(
    Indikator = "(Themenbereich Bevölkerung)",
    Ausprägung = "", Jahr = "", Raumbezug = "", Indikatorwert = "",
    Basiswert1 = "", Basiswert2 = "", Name_Basiswert1 = "", Name_Basiswert2 = "",
    .after = hk_row
  )

# Update position again
betr_row <- betr_row + 1

df_data <- df_data %>%
  add_row(
    Indikator = "(Themenbereich Betreuung)",
    Ausprägung = "", Jahr = "", Raumbezug = "", Indikatorwert = "",
    Basiswert1 = "", Basiswert2 = "", Name_Basiswert1 = "", Name_Basiswert2 = "",
    .after = betr_row
  )
view(df_data)
saveRDS(df_data, "df_data.rds")
