library(tidyverse)
library(sf)
library(leaflet)
library(readxl)
library(htmltools)
library(htmlwidgets)  # required for prependContent()

# Data preparation

# Load geometry
munich_map <- st_read("results/geo/bezirk_map.gpkg", quiet = TRUE) |>
  st_transform(4326)

# Load tables
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
ki_sheet <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

# Childcare (age 0–2), 2024 only

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

# Join with geometry
final_sf <- munich_map %>%
  left_join(betreuung_2024, by = c("sb_nummer" = "bezirksnummer")) %>%
  filter(!is.na(anteil_betreuung))

# Color palette and popup content
pal <- colorNumeric(
  palette = c("#fff5eb", "#7f2704"),
  domain  = c(0, 100)
)

popup_content <- paste0(
  "<b>Stadtteil:</b> ", final_sf$name, "<br/>",
  "<b>Kinderbetreuung:</b> ", round(final_sf$anteil_betreuung, 1), " %"
)

# Create leaflet map and store as object
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
    className = "small-legend"
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

# Preview map
ki_leaflet_2024

# Save as RDS
saveRDS(
  ki_leaflet_2024,
  file = "results/figures/m_effekt/m_effekt_03.rds"
)
