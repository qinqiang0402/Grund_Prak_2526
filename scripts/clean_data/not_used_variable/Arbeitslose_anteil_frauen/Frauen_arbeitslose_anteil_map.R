install.packages(c("tidyverse", "readxl", "sf", "leaflet"))
library(tidyverse)
library(readxl)
library(sf)
library(leaflet)
library(htmltools)
library(dplyr)
library(ggplot2)

install.packages("readxl")
library(readxl)

export_ar <- read_excel("data/raw/export_ar.xlsx")
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")

geojson_url <- "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gsm_wfs:vablock_stadtbezirke_opendata&outputFormat=application/json"
munich_map <- st_read(geojson_url)

arbeitslos_anteil_frau <- ar_sheet %>%
  filter(
    Indikator == "Arbeitslose - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(
    anteil = 100 * `Basiswert 1` / `Basiswert 2`
  )

average_arbeitslos_anteil_frau <- arbeitslos_anteil_frau %>%
  group_by(Raumbezug) %>%
  summarise(arbeitslos_anteil_frau = mean(anteil, na.rm = TRUE))

average_arbeitslos_anteil_frau_cleaned <- average_arbeitslos_anteil_frau %>%
  mutate(
    bezirksnummer = as.numeric(str_extract(Raumbezug, "^\\d+"))
  ) %>%
  filter(!is.na(bezirksnummer))

average_arbeitslos_anteil_frau_formatted <- average_arbeitslos_anteil_frau_cleaned %>%
  mutate(
    bezirksnummer = sprintf("%02d", bezirksnummer)
  )

merged_average_arbeitslos_anteil_frau_avg <- merge(
  munich_map,
  average_arbeitslos_anteil_frau_formatted,
  by.x = "sb_nummer", 
  by.y = "bezirksnummer",     
  all.x = TRUE                
)


merged_karte_mit_links <- merged_average_arbeitslos_anteil_frau_avg %>%
  mutate(
    WikiURL = paste0("https://de.wikipedia.org/wiki/", name)
  )


merged_karte_transformiert <- merged_karte_mit_links %>%
  st_transform(crs = 4326)

pal <- colorNumeric(
  palette = "Blues", 
  domain = merged_karte_transformiert$arbeitslos_anteil_frau,
  na.color = "#bdbdbd"
)


karte <- leaflet(data = merged_karte_transformiert) %>%
  
  addTiles() %>%
  
  addPolygons(
    fillColor = ~pal(arbeitslos_anteil_frau), 
    fillOpacity = 0.7,
    weight = 2,
    color = "white",
    dashArray = "3",
    
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    
    popup = ~paste(
      "<b>Bezirk:</b>", name, "<br>",
      "<b> Frauen-Arbeitslosenanteil:</b>", round(arbeitslos_anteil_frau, 2), "%<br><br>", 
      "<a href='", WikiURL, "' target='_blank'>",
      #"Auf Wikipedia nachschlagen (DE)",
      "</a>"
    ) %>% lapply(HTML)
  ) %>%
  
  addLegend(
    pal = pal, 
    values = ~arbeitslos_anteil_frau,
    opacity = 0.7,
    title = " Frauen-Arbeitslose-Anteil (%)",
    position = "bottomright"
  )


karte


#-----------------------------------------------------
#INTERAKTIVE KARTE OHNE HINTERGRUND ---
merged_karte_transformiert <- merged_karte_mit_links %>%
  st_transform(crs = 4326)


pal <- colorNumeric(
  palette = "Blues",
  domain = merged_karte_transformiert$arbeitslos_anteil_frau,
  na.color = "#bdbdbd"
)

bounds <- st_bbox(merged_karte_transformiert)

karte <- leaflet(data = merged_karte_transformiert) %>%
  fitBounds(
    lng1 = unname(bounds["xmin"]),
    lat1 = unname(bounds["ymin"]),
    lng2 = unname(bounds["xmax"]),
    lat2 = unname(bounds["ymax"]),
    options = list(padding = c(10, 10))
  ) %>%
  
  addPolygons(
    fillColor = ~pal(arbeitslos_anteil_frau),
    fillOpacity = 0.7,
    weight = 2,
    color = "white", 
    dashArray = "3",
    
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    
    popup = ~paste(
      "<b>Bezirk:</b>", name, "<br>",
      "<b> Frauen-Arbeitslosenanteil:</b>", round(arbeitslos_anteil_frau, 2), "%<br><br>", 
      "<a href='", WikiURL, "' target='_blank'>",
      #"Auf Wikipedia nachschlagen (DE)",
      "</a>"
    ) %>% lapply(HTML)
  ) %>%
  
  addLegend(
    pal = pal,
    values = ~arbeitslos_anteil_frau,
    opacity = 0.7,
    title = " Frauen-Arbeitslose-Anteil (%)",
    position = "bottomright"
  )

karte


