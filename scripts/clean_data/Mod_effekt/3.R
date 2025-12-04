library(leaflet)
library(dplyr)
library(sf)
library(tidyverse)
library(sf)
library(readxl)
library(htmltools)

# === Load Geometry ===
munich_map <- st_read("results/geo/bezirk_map.gpkg", quiet = TRUE) |>
  st_transform(4326)

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
# === Attach geometry ===
# 这里不需要在 join 之后立刻 select，先把所有列都保留
final_sf_data_long <- munich_map %>%
  left_join(
    combined_data_all,
    by = c("sb_nummer" = "bezirksnummer"),
    relationship = "many-to-many"   # 消除 warning，用于一对多 / 多对多
  ) %>%
  # Jahr 可能是字符，统一转成 numeric 比较安全
  mutate(Jahr = as.numeric(Jahr)) %>%
  filter(!is.na(Jahr))

# ================================================================
# 1. 过滤 2024 年数据
#    注意：这里 **不要** 手动选 geometry 列，sf 会自动保留
# ================================================================
data_2024 <- final_sf_data_long %>%
  filter(Jahr == 2024) %>%
  dplyr::select(
    sb_nummer,
    name,
    anteil_kinder,   # HaKi (%)
    anteil           # Frauenbeschäftigung (%)
    # 几何列会自动保留，无需写 geometry
  )

# 保险起见，强制成 sf（通常不需要，但加上也没坏处）
data_2024 <- sf::st_as_sf(data_2024)

# ================================================================
# 3. 分类：A = HaKi 高 + 就业低
# ================================================================
data_2024 <- data_2024 %>%
  mutate(
    gruppe = dplyr::case_when(
      anteil_kinder > mean_HaKi_city & anteil < mean_FE_city ~
        "hohe Haushalte mit Kindern + niedrige Beschäftigung",
      TRUE ~ "Andere"
    ),
    color = dplyr::case_when(
      gruppe == "hohe Haushalte mit Kindern + niedrige Beschäftigung" ~ "#e75480",  # 红
      TRUE ~ "#d9d9d9"  # 灰
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

