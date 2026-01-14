# ============================================================
# Script: plot_childcare_leaflet_2024.R
# Purpose: Generate an interactive Leaflet map for 
#          Childcare rates (0-2 years) in 2024.
# Returns: A Leaflet object
# ============================================================

library(tidyverse)
library(sf)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(stringr)

# ------------------------------------------------------------
# Main Function
# ------------------------------------------------------------
generate_childcare_leaflet_2024 <- function(be, ki, munich_map_sf) {
  
  # 1. Prepare Population Data (Total children 0-2)
  df_total <- be %>%
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
  
  # 2. Prepare Childcare Data (Children in care 0-2)
  df_betreut <- ki %>%
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
  
  # 3. Calculate Rate
  betreuung_2024 <- df_total %>%
    left_join(df_betreut, by = "bezirksnummer") %>%
    mutate(
      anteil_betreuung = 100 * kinder_betreut / kinder_total
    )
  
  # 4. Join with Geometry AND Transform to WGS84 (Critical for Leaflet)
  final_sf <- munich_map_sf %>%
    st_transform(4326) %>%
    mutate(sb_string = sprintf("%02d", as.numeric(sb_nummer))) %>%
    left_join(betreuung_2024, by = c("sb_string" = "bezirksnummer")) %>%
    filter(!is.na(anteil_betreuung))
  
  # 5. Define Palette and Popups
  pal <- colorNumeric(
    palette = c("#fff5eb", "#7f2704"),
    domain  = c(0, 100)
  )
  
  popup_content <- paste0(
    "<b>Stadtteil:</b> ", final_sf$name, "<br/>",
    "<b>Kinderbetreuung:</b> ", round(final_sf$anteil_betreuung, 1), " %"
  )
  
  # 6. Create Leaflet Map
  leaflet_map <- leaflet(final_sf, options = leafletOptions(minZoom = 10, maxZoom = 14)) %>%
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
    # Inject Custom CSS for the Legend
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
  
  return(leaflet_map)
}