library(readxl)
library(dplyr)
library(tidyr)

# 1. load data
ar_raw <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")
be_raw <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
ki_raw <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

col_names <- c("Indikator", "Ausprägung", "Jahr", "Raumbezug", "Indikatorwert")

# 2. extract data
ar_tab <- ar_raw %>%
  select(1:5) %>%
  mutate(across(everything(), as.character)) %>%
  setNames(col_names) %>%
  mutate(Quelle = "Arbeitsmarkt")

# Households with children data for table
be_tab_household <- be_raw %>%
  select(1:5) %>%
  mutate(across(everything(), as.character)) %>%
  setNames(col_names) %>%
  filter(grepl("Haushalte mit Kindern", Indikator, ignore.case = TRUE)) %>%
  mutate(Quelle = "Bevölkerung")

# 3. Childcare data: percentage of children in care
ki_tab <- be_raw %>%
  filter(
    Indikator == "Altersgruppen",
    Ausprägung == "bis 2 Jahre"
  ) %>%
  select(Jahr, Raumbezug, kinder_total = `Basiswert 1`) %>%
  left_join(
    ki_raw %>%
      filter(
        Indikator == "Altersgruppen",
        Ausprägung == "bis 2 Jahre"
      ) %>%
      select(Jahr, Raumbezug, kinder_betreut = `Basiswert 1`),
    by = c("Jahr", "Raumbezug")
  ) %>%
  mutate(
    across(c(kinder_total, kinder_betreut), as.numeric),
    kinder_unbetreut = kinder_total - kinder_betreut,
    anteil_betreut   = 100 * kinder_betreut / kinder_total,
    anteil_unbetreut = 100 - anteil_betreut
  ) %>%
  filter(
    (Jahr == "2007" & Raumbezug == "25 Laim") |
      (Jahr == "2024" & Raumbezug == "10 Moosach")
  )

# 4. table selection (female employment, Households with children)
selection <- tribble(
  ~Indikator,                                           ~Ausprägung, ~Jahr,  ~Raumbezug, ~Quelle,
  "Sozialversicherungspflichtig Beschäftigte - Anteil", "weiblich",   "2024","Stadt München", "Arbeitsmarkt",
  "Sozialversicherungspflichtig Beschäftigte - Anteil", "weiblich",   "2000","13 Bogenhausen", "Arbeitsmarkt",
  "Haushalte mit Kindern",                              "insgesamt",  "2024","06 Sendling", "Bevölkerung",
  "Haushalte mit Kindern",                              "insgesamt",  "2012","03 Maxvorstadt", "Bevölkerung"
)

# 5. data for first four rows
df_data <- bind_rows(ar_tab, be_tab_household) %>%
  semi_join(selection,
            by = c("Indikator", "Ausprägung", "Jahr", "Raumbezug", "Quelle")) %>%
  select(-Quelle) %>%
  mutate(
    Indikator = ifelse(
      Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
      "Frauenbeschäftigung",
      Indikator
    ),
    # 手动设定分组顺序：先 Anteil，再 Haushalt
    Indikator = factor(
      Indikator,
      levels = c("Frauenbeschäftigung", "Haushalte mit Kindern")
    )
  ) %>%
  arrange(Indikator, Jahr)   # 先按指标，再按年份升序
df_data
# 6. devide into seperate dataframes
df_employment <- df_data %>%
  filter(Indikator == "Frauenbeschäftigung") %>%
  arrange(Jahr)

df_household <- df_data %>%
  filter(Indikator == "Haushalte mit Kindern") %>%
  arrange(Jahr)

# 7. Dataframe Childcare
df_childcare <- ki_tab %>%
  mutate(
    Indikator = "Kinderbetreuung",
    Ausprägung = "bis 2 Jahre",
    Jahr = as.character(Jahr),
    Raumbezug = as.character(Raumbezug),
    Indikatorwert = as.character(round(anteil_betreut, 1))
  ) %>%
  select(Indikator, Ausprägung, Jahr, Raumbezug, Indikatorwert) %>%
  arrange(Jahr) 

# 8. final table with six rows
df_data_final <- bind_rows(
  df_employment,
  df_household,
  df_childcare
)
# remove NAs
df_data_final <- df_data_final %>%
  mutate(across(everything(), ~ ifelse(is.na(.) | . == "NA", "", .)))

# results
df_employment
df_data
View(df_data_final)
saveRDS(df_data_final, "results/figures/table/table_data_03.rds")
saveRDS(df_employment, "results/figures/table/table_data_01.rds")
saveRDS(df_data, "results/figures/table/table_data_02.rds")
