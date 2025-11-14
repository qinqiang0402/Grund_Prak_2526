library(dplyr)
library(stringr)
library(readxl)

be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")

df_households_yearly <- be_sheet %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    year = as.numeric(Jahr),  
    households_with_children_pct = 100 * `Basiswert 1` / `Basiswert 2`,
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  filter(!is.na(bezirksnummer)) %>%
  select(year, bezirksnummer, households_with_children_pct)

head(df_households_yearly)

create_indicator_map <- function(
    geo_data,
    data_table,
    value_col,
    name_col = "name",
    title = "Indicator (%)",
    palette = "YlGnBu",      
    na_color = "#bdbdbd",
    show_basemap = TRUE
) {
  library(dplyr)
  library(sf)
  library(leaflet)
  library(htmltools)
  
  # merge spatial and indicator data
  merged_data <- geo_data %>%
    left_join(data_table, by = c("sb_nummer" = "bezirksnummer")) %>%
    mutate(
      value = .data[[value_col]],
      WikiURL = paste0("https://de.wikipedia.org/wiki/", .data[[name_col]])
    ) %>%
    st_transform(crs = 4326)
  
  # define color palette
  pal <- colorNumeric(
    palette = palette,
    domain = merged_data$value,
    na.color = na_color
  )
  
  # initialize leaflet map
  map <- leaflet(data = merged_data)
  
  # Basemap
  if (show_basemap) {
    map <- map %>% addProviderTiles(providers$CartoDB.Positron)
  } else {
    bounds <- st_bbox(merged_data)
    map <- map %>% fitBounds(
      lng1 = unname(bounds["xmin"]),
      lat1 = unname(bounds["ymin"]),
      lng2 = unname(bounds["xmax"]),
      lat2 = unname(bounds["ymax"]),
      options = list(padding = c(10, 10))
    )
  }
  
  # add polygons and legend
  map <- map %>%
    addPolygons(
      fillColor = ~pal(value),
      fillOpacity = 0.75,       # ✔ 更透明，但仍清晰
      weight = 1.5,
      color = "white",
      dashArray = "3",
      highlightOptions = highlightOptions(
        weight = 3,
        color = "#444",
        dashArray = "",
        fillOpacity = 0.8,
        bringToFront = TRUE
      ),
      popup = ~paste0(
        "<b>District:</b> ", get(name_col), "<br>",
        "<b>", title, ":</b> ", round(value, 2), "%<br><br>",
        "<a href='", WikiURL, "' target='_blank'>Wikipedia</a>"
      ) %>% lapply(HTML)
    ) %>%
    addLegend(
      pal = pal,
      values = ~value,
      opacity = 0.8,
      title = title,
      position = "bottomright"
    )
  
  return(map)
}




geojson_url <- "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gsm_wfs:vablock_stadtbezirke_opendata&outputFormat=application/json"
munich_map <- st_read(geojson_url)


years <- sort(unique(df_households_yearly$year))


maps_by_year <- list()

for (yr in years) {
  df_single_year <- df_households_yearly %>%
    filter(year == yr)
  
  map_obj <- create_indicator_map(
    geo_data = munich_map,
    data_table = df_single_year,
    value_col = "households_with_children_pct",
    title = paste0(
      "Households with Children (%)<br>",
      "<span style='font-size:90%;'>Year ", yr, "</span>"
    ),
    palette = "Blues",   
    show_basemap = TRUE
  )
  
  
  maps_by_year[[as.character(yr)]] <- map_obj
}


maps_by_year[["2015"]] # choose year
 
