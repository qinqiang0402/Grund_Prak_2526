create_indicator_map <- function(
    geo_data,
    data_table,
    value_col,
    name_col = "name",
    title = "Indicator (%)",
    palette = "Blues",
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
  
  # if no basemap, fit to district bounds
  if (show_basemap) {
    map <- map %>% addTiles()
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
      popup = ~paste0(
        "<b>District:</b> ", get(name_col), "<br>",
        "<b>", title, ":</b> ", round(value, 2), "%<br><br>",
        "<a href='", WikiURL, "' target='_blank'>Wikipedia</a>"
      ) %>% lapply(HTML)
    ) %>%
    addLegend(
      pal = pal,
      values = ~value,
      opacity = 0.7,
      title = title,
      position = "bottomright"
    )
  
  return(map)
}



# Start to create our map
library(readxl)
export_be <- read_excel("data/raw/export_be.xlsx")
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")

library(dplyr)
library(stringr)

# Create df_households for indicator "Haushalte mit Kindern"
df_households <- be_sheet %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    # calculate the share of households with children in percent
    households_with_children_pct = 100 * `Basiswert 1` / `Basiswert 2`
  ) %>%
  group_by(Raumbezug) %>%
  summarize(
    households_with_children_pct = mean(households_with_children_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # extract district number and format as two digits (e.g. 01, 02, ...)
    bezirksnummer = as.numeric(str_extract(Raumbezug, "^\\d+")),
    bezirksnummer = sprintf("%02d", bezirksnummer)
  ) %>%
  filter(!is.na(bezirksnummer)) %>%
  select(bezirksnummer, households_with_children_pct)

# Preview the result
head(df_households)

# with basemap
geojson_url <- "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gsm_wfs:vablock_stadtbezirke_opendata&outputFormat=application/json"
munich_map <- st_read(geojson_url)

karte_households <- create_indicator_map(
  geo_data   = munich_map,
  data_table = df_households,
  value_col  = "households_with_children_pct",
  title      = "Households with Children (%) — Average 2012–2024",
  palette    = "Greens",
  show_basemap = TRUE    # setting here
)

karte_households


# without basemap
karte_households_nobase <- create_indicator_map(
  geo_data   = munich_map,
  data_table = df_households,
  value_col  = "households_with_children_pct",
  title      = "Households with Children (%)<br><span style='font-size:90%;'>Average 2012–2024</span>",
  palette    = "Greens",
  show_basemap = FALSE   # setting here
)

karte_households_nobase

