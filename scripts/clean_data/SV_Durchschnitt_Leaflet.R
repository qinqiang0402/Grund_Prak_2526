library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(readxl)

geojson_url <- "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gsm_wfs:vablock_stadtbezirke_opendata&outputFormat=application/json"
munich_map <- st_read(geojson_url, quiet = TRUE) %>% 
  st_transform(4326) 

ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")

ar_data_long <- ar_sheet %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(
    Value = 100 * `Basiswert 1` / `Basiswert 2`, 
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(Jahr, bezirksnummer, Value) %>% 
  filter(!is.na(bezirksnummer))

final_sf_data_long <- munich_map %>%
  left_join(ar_data_long, by = c("sb_nummer" = "bezirksnummer")) %>%
  filter(!is.na(Jahr))




## 1.Only 2024 data
data_2024 <- final_sf_data_long %>%
  filter(Jahr == 2024)

## 2. 2024 mean Frauenbeschäftigungsquote (munich)
munich_2024 <- ar_sheet %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich",
    Raumbezug == "Stadt München",
    Jahr == 2024
  ) %>%
  mutate(Value = 100 * `Basiswert 1` / `Basiswert 2`) %>%
  pull(Value)

mean_2024 <- munich_2024

## 3. Abweichung berechnen
data_2024 <- data_2024 %>%
  mutate(
    Value_dev = Value - mean_2024  
  )

## 4. max als Grenze
max_abs <- max(abs(data_2024$Value_dev), na.rm = TRUE)

## 5. Color
pal_div <- colorNumeric(
  palette = "PuOr",
  domain = c(-max_abs, max_abs),
  reverse = TRUE
)



## 6. popup 
popup_content_2024 <- paste0(
  "<b>Stadtbezirk:</b> ", data_2024[["name"]], "<br/>",
  "<b>Frauenbeschäftigungsquote 2024:</b> ",
  round(data_2024$Value, 1), " %", "<br/>",
  "<b>München-Durchschnitt 2024:</b> ",
  round(mean_2024, 1), " %", "<br/>",
  "<b>Abweichung vom München-Durchschnitt:</b> ",
  sprintf("%+.1f%%", data_2024$Value_dev)
)


## 7. leaflet
leaflet(data_2024, options = leafletOptions(minZoom = 10, maxZoom = 14)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = 11.5761, lat = 48.1372, zoom = 10) %>%
  addPolygons(
    fillColor = ~pal_div(Value_dev),
    weight = 1.5,
    opacity = 1,
    color = "white",
    dashArray = "1",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
    ),
    label = lapply(popup_content_2024, HTML)
  ) %>%
  addLegend(
    pal = pal_div,
    values = c(-max_abs, max_abs),
    opacity = 0.7,
    title = HTML("Abweichung von der<br>Frauenbeschäftigungsquote 2024<br>(München = 0)"),
    labFormat = labelFormat(suffix = " % ", digits = 1),
    position = "bottomright"
  )
