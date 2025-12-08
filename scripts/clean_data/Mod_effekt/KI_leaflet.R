library(tidyverse)
library(sf)
library(leaflet)
library(readxl)
library(htmltools)
library(htmlwidgets)  # ⭐ 为了使用 prependContent()

# =================================================================
# 1. Datenvorbereitung
# =================================================================

# 1.1. Geometrie laden
munich_map <- st_read("results/geo/bezirk_map.gpkg", quiet = TRUE) |>
  st_transform(4326)

# 1.2. Daten laden
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
ki_sheet <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

# =================================================================
# 2. Kinderbetreuung 0–2 Jahre berechnen (nur 2024)
# =================================================================

df_total <- be_sheet %>%
  filter(
    Jahr == 2024,
    Indikator == "Altersgruppen",
    Ausprägung == "bis 2 Jahre",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(bezirksnummer, kinder_total = `Basiswert 1`)

df_betreut <- ki_sheet %>%
  filter(
    Jahr == 2024,
    Indikator == "Altersgruppen",
    Ausprägung == "bis 2 Jahre",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(bezirksnummer, kinder_betreut = `Basiswert 1`)

betreuung_2024 <- df_total %>%
  left_join(df_betreut, by = "bezirksnummer") %>%
  mutate(
    anteil_betreuung = 100 * kinder_betreut / kinder_total
  )

# =================================================================
# 3. Join mit Geodaten
# =================================================================

final_sf <- munich_map %>%
  left_join(betreuung_2024, by = c("sb_nummer" = "bezirksnummer")) %>%
  filter(!is.na(anteil_betreuung))

# =================================================================
# 4. Farbpalette + Popup
# =================================================================

pal <- colorNumeric(
  palette = c("#fff5eb", "#7f2704"),   # ⭐ 浅 → 深红
  domain  = c(0, 100)
)

popup_content <- paste0(
  "<b>Stadtteil:</b> ", final_sf$name, "<br/>",
  "<b>Kinderbetreuung:</b> ", round(final_sf$anteil_betreuung, 1), " %"
)

# =================================================================
# 5. Leaflet-Karte erzeugen 并保存为对象
# =================================================================

ki_leaflet_2024 <- leaflet(final_sf, options = leafletOptions(minZoom = 10, maxZoom = 14)) %>%
  addTiles(
    urlTemplate = "https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png",
    attribution = '&copy; CartoDB'
  ) %>%
  setView(lng = 11.5761, lat = 48.1372, zoom = 10) %>%
  addPolygons(
    fillColor = ~pal(anteil_betreuung),
    weight = 1.5,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    label = lapply(popup_content, HTML),
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = pal,
    values = c(0, 100),
    opacity = 0.7,
    title = "Kinderbetreuung (%)",
    position = "bottomright",
    className = "small-legend"   # ⭐ 用 CSS 缩小 legend
  ) %>%
  htmlwidgets::prependContent(
    tags$style(HTML("
      .small-legend {
        font-size: 10px !important;
        line-height: 10px !important;
        padding: 3px !important;
      }
      .small-legend .leaflet-control-legend-scale {
        height: 6px !important;
      }
    "))
  )

# 看一下效果（在 R 里）
ki_leaflet_2024

# =================================================================
# 6. 保存为 RDS
# =================================================================

saveRDS(
  ki_leaflet_2024,
  file = "results/figures/m_effekt/m_effekt_03.rds"
)
