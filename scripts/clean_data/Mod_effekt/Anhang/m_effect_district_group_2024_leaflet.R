library(leaflet)
library(dplyr)
library(sf)
library(tidyverse)
library(readxl)
library(htmltools)

# Load geometry
munich_map <- st_read("results/geo/bezirk_map.gpkg", quiet = TRUE) |>
  st_transform(4326)

# Load tables
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")
ki_sheet <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

# Households with children
hh_data_long <- be_sheet %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(
    anteil_kinder = 100 * `Basiswert 1` / `Basiswert 2`,
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(Jahr, bezirksnummer, anteil_kinder)

# Female employment
ar_data_long <- ar_sheet %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(
    anteil = 100 * `Basiswert 1` / `Basiswert 2`,
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(Jahr, bezirksnummer, anteil)

# Combine variables
combined_data_all <- hh_data_long %>%
  full_join(ar_data_long, by = c("Jahr", "bezirksnummer"))

# Join with geometry
final_sf_data_long <- munich_map %>%
  left_join(
    combined_data_all,
    by = c("sb_nummer" = "bezirksnummer"),
    relationship = "many-to-many"
  ) %>%
  mutate(Jahr = as.numeric(Jahr)) %>%
  filter(!is.na(Jahr)) %>%
  sf::st_as_sf()

# Filter 2024
data_2024 <- final_sf_data_long %>%
  filter(Jahr == 2024) %>%
  dplyr::select(
    sb_nummer,
    name,
    anteil_kinder,
    anteil
  ) %>%
  sf::st_as_sf()

# City means (2024)
mean_HaKi_city <- be_sheet %>%
  filter(
    Jahr == 2024,
    Raumbezug == "Stadt München",
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(anteil_kinder = 100 * `Basiswert 1` / `Basiswert 2`) %>%
  pull(anteil_kinder)

mean_FE_city <- ar_sheet %>%
  filter(
    Jahr == 2024,
    Raumbezug == "Stadt München",
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(anteil = 100 * `Basiswert 1` / `Basiswert 2`) %>%
  pull(anteil)

# Classification
data_2024 <- data_2024 %>%
  mutate(
    gruppe = dplyr::case_when(
      anteil_kinder > mean_HaKi_city & anteil < mean_FE_city ~
        "Haushalte mit Kindern hoch\nund Frauenbeschäftigung niedrig",
      TRUE ~ "Andere"
    ),
    color = dplyr::case_when(
      gruppe == "Haushalte mit Kindern hoch\nund Frauenbeschäftigung niedrig" ~ "#e75480",
      TRUE ~ "#d9d9d9"
    )
  )

# Leaflet map
m_effekt_04 <- leaflet(data_2024) %>%
  addTiles(
    urlTemplate = "https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png",
    attribution = '&copy; CartoDB'
  ) %>%
  addPolygons(
    fillColor = ~color,
    color = "white",
    weight = 1,
    fillOpacity = 0.8,
    label = ~paste0(
      "<b>", name, "</b><br/>",
      "Haushalte mit Kindern: ", round(anteil_kinder, 1), "%<br/>",
      "Frauenbeschäftigung: ", round(anteil, 1), "%<br/>",
      "<b>Gruppe:</b> ", gruppe
    ) %>% lapply(htmltools::HTML),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "grey85",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    colors = c("#e75480", "#d9d9d9"),
    labels = c(
      "Haushalte mit Kindern hoch\nund Frauenbeschäftigung niedrig",
      "Andere"
    ),
    title = "Kategorien (2024)",
    position = "bottomright"
  )

m_effekt_04
saveRDS(m_effekt_04, "results/figures/m_effekt/m_effekt_04.rds")
