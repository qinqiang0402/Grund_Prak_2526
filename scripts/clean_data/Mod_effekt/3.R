library(leaflet)
library(dplyr)
library(sf)
library(tidyverse)
library(sf)
library(readxl)
library(htmltools)

# === Load Geometry ===
geojson_url <- "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gsm_wfs:vablock_stadtbezirke_opendata&outputFormat=application/json"
munich_map <- st_read(geojson_url, quiet = TRUE) %>% st_transform(4326)

# === Load tables ===
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")
ki_sheet <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

# === Variable processing ===

# 1. Haushalte mit Kindern
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

# 2. Frauenbeschäftigung
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

# === Combine all variables ===
combined_data_all <- hh_data_long %>%
  full_join(ar_data_long, by = c("Jahr", "bezirksnummer"))

# === Attach geometry ===
final_sf_data_long <- munich_map %>%
  left_join(combined_data_all, by = c("sb_nummer" = "bezirksnummer")) %>%
  filter(!is.na(Jahr))

# ================================================================
# 1. 过滤 2024 年数据
# ================================================================
data_2024 <- final_sf_data_long %>%
  filter(Jahr == 2024) %>%
  select(
    sb_nummer, name, geometry,
    anteil_kinder,        # HaKi (%)
    anteil                # Frauenbeschäftigung (%)
  )

# ================================================================
# 2. Stadt München 市平均值 (HaKi & Frauenbeschäftigung)
# ================================================================

# HaKi 来自 be_sheet
mean_HaKi_city <- be_sheet %>%
  filter(
    Jahr == 2024,
    Raumbezug == "Stadt München",
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(anteil_kinder = 100 * `Basiswert 1` / `Basiswert 2`) %>%
  pull(anteil_kinder)

# Frauenbeschäftigung 来自 ar_sheet
mean_FE_city <- ar_sheet %>%
  filter(
    Jahr == 2024,
    Raumbezug == "Stadt München",
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(anteil = 100 * `Basiswert 1` / `Basiswert 2`) %>%
  pull(anteil)

cat("City mean HaKi 2024 =", mean_HaKi_city, "\n")
cat("City mean FE   2024 =", mean_FE_city, "\n")


# ================================================================
# 3. 分类：A = HaKi 高 + 就业低
# ================================================================
data_2024 <- data_2024 %>%
  mutate(
    gruppe = case_when(
      anteil_kinder > mean_HaKi_city & anteil < mean_FE_city ~
        "hohe Haushalte mit Kindern + niedrige Beschäftigung",
      TRUE ~ "Andere"
    ),
    color = case_when(
      gruppe == "hohe Haushalte mit Kindern + niedrige Beschäftigung" ~ "#e75480",  # 红色
      TRUE ~ "#d9d9d9"  # 灰色（HEX）
    )
  )


# ================================================================
# 4. Leaflet 地图
# ================================================================
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
    labels = c("hohe Haushalte mit Kindern + niedrige Beschäftigung", "Andere"),
    title = "Kategorien (2024)",
    position = "bottomright"
  )

m_effekt_04
saveRDS(m_effekt_04, "results/figures/m_effekt/m_effekt_04.rds")
