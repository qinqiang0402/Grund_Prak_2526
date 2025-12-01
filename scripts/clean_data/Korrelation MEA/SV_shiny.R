# app.R

library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(readxl)

geojson_url <- "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gsm_wfs:vablock_stadtbezirke_opendata&outputFormat=application/json"
munich_map <- st_read(geojson_url, quiet = TRUE) %>% 
  st_transform(4326) 


ar_sheet <- read_excel("../../data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")

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

MAP_NAME_COLUMN <- "name" 
MIN_YEAR <- min(final_sf_data_long$Jahr, na.rm = TRUE)
MAX_YEAR <- max(final_sf_data_long$Jahr, na.rm = TRUE)

GLOBAL_MIN <- min(final_sf_data_long$Value, na.rm = TRUE)
GLOBAL_MAX <- max(final_sf_data_long$Value, na.rm = TRUE)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .main-title { font-size: 24px; font-weight: bold; margin-top: 10px; text-align: center; }
      .time-label { font-size: 18px; font-weight: bold; text-align: center; margin-bottom: 10px; }
    "))
  ),
  
  # Titel fest angepasst
  div(class = "main-title", "Frauenbeschäftigungsquote"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "time_slider",
        label = div(class = "time-label", textOutput("year_display")),
        min = MIN_YEAR,
        max = MAX_YEAR,
        value = MIN_YEAR, 
        step = 1, 
        sep = "",
        animate = animationOptions(interval = 1500, loop = FALSE) 
      ),
      width = 3
    ),
    
    mainPanel(
      leafletOutput("map_output", height = "700px"),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  
  output$year_display <- renderText({
    paste0("Jahr: ", input$time_slider)
  })
  
  filtered_sf_data <- reactive({
    final_sf_data_long %>%
      filter(Jahr == input$time_slider)
  })
  
  output$map_output <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 10, maxZoom = 14)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 11.5761, lat = 48.1372, zoom = 10)
  })
  
  observe({
    current_sf <- filtered_sf_data()
    
    color_min <- GLOBAL_MIN
    color_max <- GLOBAL_MAX
    unit <- " %"
    decimal_places <- 1
    var_label <- "Frauenbeschäftigungsquote" 
    
    pal <- colorNumeric(
      palette = "Blues", 
      domain = c(color_min, color_max)
    )
    
    popup_content <- paste0(
      "<b>Stadtbezirk:</b> ", current_sf[[MAP_NAME_COLUMN]], "<br/>", 
      "<b>", var_label, ":</b> ", round(current_sf$Value, decimal_places), unit
    )
    
    leafletProxy("map_output", data = current_sf) %>%
      clearShapes() %>% 
      clearControls() %>% 
      
      addPolygons(
        fillColor = ~pal(Value),
        weight = 1.5,
        opacity = 1,
        color = "white",
        dashArray = "1",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
        ),
        label = lapply(popup_content, HTML) 
      ) %>% 
      
      addLegend(
        pal = pal, 
        values = c(color_min, color_max),
        opacity = 0.7, 
        title = var_label,
        labFormat = labelFormat(suffix = unit, digits = decimal_places),
        position = "bottomright"
      )
  })
}

shinyApp(ui = ui, server = server)