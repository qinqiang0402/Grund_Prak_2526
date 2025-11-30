library(sf)
library(dplyr)
library(stringr)
library(leaflet)
library(htmltools)
library(readxl)
library(tidyverse)

ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")

df_emp <- ar_sheet %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(
    Jahr = as.numeric(Jahr),
    emp_female_pct = 100 * `Basiswert 1` / `Basiswert 2`
  ) %>%
  select(Jahr, Raumbezug, emp_female_pct)


be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")

df_households <- be_sheet %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    Jahr = as.numeric(Jahr),
    households_pct = 100 * `Basiswert 1` / `Basiswert 2`,
  ) %>%
  select(Jahr, Raumbezug, households_pct)

df_merged <- df_emp %>%
  inner_join(df_households,
             by = c("Jahr", "Raumbezug"))



geojson_url <- "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gsm_wfs:vablock_stadtbezirke_opendata&outputFormat=application/json"
munich_map <- st_read(geojson_url)



df_corr_by_district <- df_merged %>%
  group_by(Raumbezug) %>%
  summarise(
    corr = cor(emp_female_pct, households_pct, use = "complete.obs")
  ) %>%
  mutate(
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  filter(!is.na(bezirksnummer)) %>%
  select(bezirksnummer, corr)


create_indicator_map <- function(
    geo_data,
    data_table,
    value_col,
    name_col = "name",
    title = "Indicator",
    palette = "RdBu",       
    na_color = "#cccccc",
    show_basemap = TRUE
) {
  
  merged_data <- geo_data %>%
    left_join(data_table, by = c("sb_nummer" = "bezirksnummer")) %>%
    mutate(
      value = .data[[value_col]],
      WikiURL = paste0("https://de.wikipedia.org/wiki/", .data[[name_col]])
    ) %>%
    st_transform(crs = 4326)
  
  
  pal <- colorNumeric(
    palette = palette,
    domain = merged_data$value,
    na.color = na_color,
    reverse = TRUE   
  )
  
  map <- leaflet(merged_data)
  
  
  if (show_basemap) {
    map <- map %>% addProviderTiles("CartoDB.Positron")
  } else {
    bounds <- st_bbox(merged_data)
    map <- map %>% fitBounds(
      bounds["xmin"], bounds["ymin"],
      bounds["xmax"], bounds["ymax"]
    )
  }
  
  
  map <- map %>%
    addPolygons(
      fillColor   = ~pal(value),
      fillOpacity = 0.85,
      weight      = 0.7,
      color       = "white",
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#555",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      popup = ~paste0(
        "<b>Bezirk:</b> ", get(name_col), "<br>",
        "<b>", title, ":</b> ", round(value, 3), "<br><br>",
        "<a href='", WikiURL, "' target='_blank'>Wikipedia</a>"
      ) %>% lapply(HTML)
    ) %>%
    addLegend(
      pal = pal,
      values = ~value,
      opacity = 0.85,
      title = title,
      position = "bottomright"
    )
  
  return(map)
}


karte_corr <- create_indicator_map(
  geo_data   = munich_map,
  data_table = df_corr_by_district,
  value_col  = "corr",
  title      = "Korrelation: Frauenbeschäftigung<br>vs. Haushalte mit Kindern",
  palette    = "RdBu",    
  show_basemap = TRUE      
)

karte_corr

saveRDS(karte_corr, "results/figures/Haushalt_mit_Kindern/HMK_karte_korr.rds")
