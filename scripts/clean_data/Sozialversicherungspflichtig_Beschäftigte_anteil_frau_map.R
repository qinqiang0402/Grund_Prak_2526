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

Sozialversicherungspflichtig_Beschäftigte_anteil_frau <- ar_sheet %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(
    anteil = 100 * `Basiswert 1` / `Basiswert 2`
  )

average_Sozialversicherungspflichtig_Beschäftigte_anteil_frau <- Sozialversicherungspflichtig_Beschäftigte_anteil_frau %>%
  group_by(Raumbezug) %>%
  summarise(Sozialversicherungspflichtig_Beschäftigte_anteil_frau = mean(anteil, na.rm = TRUE))

average_Sozialversicherungspflichtig_Beschäftigte_anteil_frau_cleaned <- average_Sozialversicherungspflichtig_Beschäftigte_anteil_frau %>%
  mutate(
    bezirksnummer = as.numeric(str_extract(Raumbezug, "^\\d+"))
  ) %>%
  filter(!is.na(bezirksnummer))

average_Sozialversicherungspflichtig_Beschäftigte_anteil_frau_formatted <- average_Sozialversicherungspflichtig_Beschäftigte_anteil_frau_cleaned %>%
  mutate(
    bezirksnummer = sprintf("%02d", bezirksnummer)
  )

merged_average_Sozialversicherungspflichtig_Beschäftigte_anteil_frau_avg <- merge(
  munich_map,
  average_Sozialversicherungspflichtig_Beschäftigte_anteil_frau_formatted,
  by.x = "sb_nummer", 
  by.y = "bezirksnummer",     
  all.x = TRUE                
)


merged_karte_mit_links <- merged_average_Sozialversicherungspflichtig_Beschäftigte_anteil_frau_avg %>%
  mutate(
    WikiURL = paste0("https://de.wikipedia.org/wiki/", name)
  )


#------------------------------------------------------------------------------------------------
merged_karte_transformiert <- merged_karte_mit_links %>%
  st_transform(crs = 4326)

pal <- colorNumeric(
  palette = "Blues", 
  domain = merged_karte_transformiert$Sozialversicherungspflichtig_Beschäftigte_anteil_frau,
  na.color = "#bdbdbd"
)


karte <- leaflet(data = merged_karte_transformiert) %>%
  
  addTiles() %>%
  
  addPolygons(
    fillColor = ~pal(Sozialversicherungspflichtig_Beschäftigte_anteil_frau), 
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
      "<b> Sozialversicherungspflichtig_Beschäftigte_anteil_frau:</b>", round(Sozialversicherungspflichtig_Beschäftigte_anteil_frau, 2), "%<br><br>", 
      "<a href='", WikiURL, "' target='_blank'>",
      #"Auf Wikipedia nachschlagen (DE)",
      "</a>"
    ) %>% lapply(HTML)
  ) %>%
  
  addLegend(
    pal = pal, 
    values = ~Sozialversicherungspflichtig_Beschäftigte_anteil_frau,
    opacity = 0.7,
    title = " Sozialversicherungspflichtig_Beschäftigte_anteil_frau (%)",
    position = "bottomright"
  )


karte

