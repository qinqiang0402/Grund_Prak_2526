# ============================================================
# Script: plot_target_group_leaflet.R
# Purpose: Generate an interactive Leaflet map identifying districts
#          with High "Haushalte mit Kindern" AND Low "Frauenbeschäftigung"
#          (Compared to the city average of 2024)
# Returns: A Leaflet object
# ============================================================

library(tidyverse)
library(sf)
library(leaflet)
library(htmltools)
library(stringr)

# ------------------------------------------------------------
# Main Function
# ------------------------------------------------------------
generate_target_group_leaflet <- function(ar, be, munich_map_sf) {
  
  # 1. Helper to clean and extract Bezirk Number
  process_bezirk <- function(df, col_name = "Raumbezug") {
    df %>%
      mutate(
        bezirksnummer = sprintf("%02d", as.numeric(str_extract(.data[[col_name]], "^\\d+")))
      ) %>%
      filter(!is.na(bezirksnummer))
  }
  
  # 2. Process Households with Children
  hh_data <- be %>%
    filter(
      Indikator == "Haushalte mit Kindern",
      Ausprägung == "insgesamt",
      Raumbezug != "Stadt München"
    ) %>%
    process_bezirk() %>%
    mutate(
      anteil_kinder = 100 * `Basiswert 1` / `Basiswert 2`
    ) %>%
    select(Jahr, bezirksnummer, anteil_kinder)
  
  # 3. Process Female Employment
  ar_data <- ar %>%
    filter(
      Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
      Ausprägung == "weiblich",
      Raumbezug != "Stadt München"
    ) %>%
    process_bezirk() %>%
    mutate(
      anteil = 100 * `Basiswert 1` / `Basiswert 2`
    ) %>%
    select(Jahr, bezirksnummer, anteil)
  
  # 4. Combine Variables
  combined_data <- hh_data %>%
    full_join(ar_data, by = c("Jahr", "bezirksnummer")) %>%
    mutate(Jahr = as.numeric(Jahr))
  
  # 5. Join with Geometry & Transform to WGS84
  final_sf <- munich_map_sf %>%
    st_transform(4326) %>%
    mutate(sb_string = sprintf("%02d", as.numeric(sb_nummer))) %>%
    left_join(combined_data, by = c("sb_string" = "bezirksnummer")) %>%
    filter(Jahr == 2024)
  
  # 6. Calculate City Thresholds (2024)
  mean_haki <- be %>%
    filter(
      Jahr == 2024, 
      Raumbezug == "Stadt München", 
      Indikator == "Haushalte mit Kindern", 
      Ausprägung == "insgesamt"
    ) %>%
    mutate(val = 100 * `Basiswert 1` / `Basiswert 2`) %>% 
    pull(val)
  
  mean_fe <- ar %>%
    filter(
      Jahr == 2024, 
      Raumbezug == "Stadt München", 
      Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil", 
      Ausprägung == "weiblich"
    ) %>%
    mutate(val = 100 * `Basiswert 1` / `Basiswert 2`) %>% 
    pull(val)
  
  # 7. Classification Logic
  final_sf <- final_sf %>%
    mutate(
      gruppe = dplyr::case_when(
        anteil_kinder > mean_haki & anteil < mean_fe ~ 
          "Haushalte mit Kindern hoch\nund Frauenbeschäftigung niedrig",
        TRUE ~ "Andere"
      ),
      color = dplyr::case_when(
        gruppe == "Haushalte mit Kindern hoch\nund Frauenbeschäftigung niedrig" ~ "#e75480",
        TRUE ~ "#d9d9d9"
      )
    )
  
  # 8. Generate Leaflet Map
  map <- leaflet(final_sf, options = leafletOptions(minZoom = 10, maxZoom = 14)) %>%
    addTiles(
      urlTemplate = "https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png",
      attribution = '&copy; CartoDB'
    ) %>%
    setView(lng = 11.5761, lat = 48.1372, zoom = 10) %>%
    addPolygons(
      fillColor = ~color,
      color = "white",
      weight = 1,
      fillOpacity = 0.8,
      label = ~paste0(
        "<b>", name, "</b><br/>",
        "Haushalte mit Kindern: ", round(anteil_kinder, 1), "%<br/>",
        "Frauenbeschäftigung: ", round(anteil, 1), "%<br/>",
        "<b>Kategorie:</b> ", gruppe
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
  
  return(map)
}