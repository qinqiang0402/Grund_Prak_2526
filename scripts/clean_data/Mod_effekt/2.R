library(tidyverse)
library(sf)
library(leaflet)
library(readxl)
library(htmltools)


# =================================================================
# 1. Datenvorbereitung
# =================================================================

# 1.1. Geometrie laden
geojson_url <- "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gsm_wfs:vablock_stadtbezirke_opendata&outputFormat=application/json"
munich_map <- st_read(geojson_url, quiet = TRUE) %>% 
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

MIN_BETR <- min(final_sf$anteil_betreuung, na.rm = TRUE)
MAX_BETR <- max(final_sf$anteil_betreuung, na.rm = TRUE)

# =================================================================
# 4. Leaflet-Karte (ohne Shiny)
# =================================================================

pal <- colorNumeric(
  palette = "Purples",
  domain = c(0, 100)
)

popup_content <- paste0(
  "<b>Stadtteil:</b> ", final_sf$name, "<br/>",
  "<b>Kinderbetreuung:</b> ", round(final_sf$anteil_betreuung, 1), " %"
)

m_effekt_03 <- leaflet(final_sf, options = leafletOptions(minZoom = 10, maxZoom = 14)) %>%
  addTiles(
    urlTemplate = "https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png",
    attribution = '&copy; CartoDB'
  )%>%
  setView(lng = 11.5761, lat = 48.1372, zoom = 10) %>%
  addPolygons(
    fillColor = ~pal(anteil_betreuung),
    weight = 1.5,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    label = lapply(popup_content, HTML),
    highlightOptions = highlightOptions(
      weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = pal,
    values = c(0, 100), 
    opacity = 0.7,
    title = "Kinderbetreuung (%)",
    position = "bottomright"
  )

m_effekt_03

saveRDS(m_effekt_03, "results/figures/m_effekt/m_effekt_03.rds")
