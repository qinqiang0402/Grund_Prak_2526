# install.packages("shiny", "tidyverse", "sf", "leaflet", "readxl")
library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(readxl)
# library(RColorBrewer) 
geojson_url <- "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gsm_wfs:vablock_stadtbezirke_opendata&outputFormat=application/json"
munich_map <- st_read(geojson_url, quiet = TRUE) %>% 
  st_transform(4326) 

export_be <- read_excel("../../data/raw/export_be.xlsx") 
be_sheet <- read_excel("../../data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
export_ar <- read_excel("../../data/raw/export_ar.xlsx") 
ar_sheet <- read_excel("../../data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")



be_data_long <- be_sheet %>%
  filter(
    Indikator == "Durchschnittsalter Mütter erstgebärend",
    Ausprägung == "insgesamt",
    Raumbezug != "Stadt München" 
  ) %>%
  mutate(
    age_mean = `Basiswert 1` / `Basiswert 2`,
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(Jahr, bezirksnummer, age_mean) %>%
  filter(!is.na(bezirksnummer))


ar_data_long <- ar_sheet %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich",
    Raumbezug != "Stadt München" 
  ) %>%
  mutate(
    anteil = 100 * `Basiswert 1` / `Basiswert 2`, 
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(Jahr, bezirksnummer, anteil) %>%
  filter(!is.na(bezirksnummer))


# 1.5. Zusammenfuehrung der Daten und des SF-Objekts
combined_data_all <- full_join(be_data_long, ar_data_long, by = c("Jahr", "bezirksnummer"))

final_sf_data_long <- munich_map %>%
  left_join(combined_data_all, by = c("sb_nummer" = "bezirksnummer")) %>%
  filter(!is.na(Jahr)) 


MAP_NAME_COLUMN <- "name" 

MIN_YEAR <- min(final_sf_data_long$Jahr, na.rm = TRUE)
MAX_YEAR <- max(final_sf_data_long$Jahr, na.rm = TRUE)

GLOBAL_MIN_AGE <- min(final_sf_data_long$age_mean, na.rm = TRUE)
GLOBAL_MAX_AGE <- max(final_sf_data_long$age_mean, na.rm = TRUE)
GLOBAL_MIN_ANTEIL <- min(final_sf_data_long$anteil, na.rm = TRUE)
GLOBAL_MAX_ANTEIL <- max(final_sf_data_long$anteil, na.rm = TRUE)


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Styling fuer akademische/nüchterne Darstellung */
      .main-title { font-size: 28px; font-weight: bold; margin-top: 15px; text-align: center; color: #333; }
      .map-title { font-size: 18px; font-weight: 600; text-align: center; margin-top: 15px; margin-bottom: 5px; color: #555; }
      .time-label { font-size: 18px; font-weight: bold; text-align: center; margin-top: 15px; margin-bottom: 5px; }
      .irs-bar { background: #4a90e2; border-top: 1px solid #4a90e2; border-bottom: 1px solid #4a90e2; }
      .leaflet-container { border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); }
    "))
  ),
  
  div(class = "main-title", "Durchschnittsalter Mütter erstgebärend und Anteil weiblicher Sozialversicherungspflichtig Beschäftigte"),
  
  fluidRow(
    column(6,
           div(class = "map-title", "Karte 1: Durchschnittsalter Mütter erstgebärend"),
           leafletOutput("map_output_1", height = "650px")
    ),
    column(6,
           div(class = "map-title", "Karte 2: Anteil weiblicher Sozialversicherungspflichtig Beschäftigte"),
           leafletOutput("map_output_2", height = "650px") 
    )
  ),
  fluidRow(
    column(12,
           tags$hr(style = "margin-top: 20px;"),
           div(class = "time-label", textOutput("year_display")),
           sliderInput(
             inputId = "time_slider",
             label = NULL,
             min = MIN_YEAR,
             max = MAX_YEAR,
             value = MIN_YEAR, 
             step = 1, 
             sep = "",
             animate = animationOptions(interval = 1500, loop = FALSE), 
             width = "100%" 
           )
    )
  )
)


server <- function(input, output, session) {
  output$year_display <- renderText({
    paste0("Aktuelles Jahr: ", input$time_slider)
  })
  
  filtered_sf_data <- reactive({
    final_sf_data_long %>%
      filter(Jahr == input$time_slider)
  })
  
  l_map_base <- leaflet(options = leafletOptions(minZoom = 10, maxZoom = 14)) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% 
    setView(lng = 11.5761, lat = 48.1372, zoom = 10) 
  
  output$map_output_1 <- renderLeaflet({ l_map_base })
  output$map_output_2 <- renderLeaflet({ l_map_base })
  
  observe({
    current_sf <- filtered_sf_data()
    
    var_id <- "age_mean"
    var_label <- "Durchschnittsalter Mütter erstgebärend"
    color_min <- GLOBAL_MIN_AGE
    color_max <- GLOBAL_MAX_AGE
    unit <- " Jahre"
    decimal_places <- 2
    pal <- colorNumeric(
      palette = "Blues", 
      domain = c(color_min, color_max)
    )
    
    map_data <- current_sf %>%
      select(sb_nummer, !!sym(MAP_NAME_COLUMN), geometry, Value = !!sym(var_id))
    
    popup_content <- paste0(
      "<b>Stadtbezirk:</b> ", map_data[[MAP_NAME_COLUMN]], "<br/>", 
      "<b>", var_label, " (", input$time_slider, "):</b> ", round(map_data$Value, decimal_places), unit
    )
    
    leafletProxy("map_output_1", data = map_data) %>%
      clearShapes() %>% 
      clearControls() %>% 
      addPolygons(
        fillColor = ~pal(Value),
        weight = 1.5, opacity = 1, color = "white", dashArray = "1", fillOpacity = 0.7,
        highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
        label = lapply(popup_content, HTML) 
      ) %>% 
      addLegend(
        pal = pal, 
        values = c(color_min, color_max),
        opacity = 0.7, 
        title = var_label,
        labFormat = labelFormat(suffix = unit, digits = decimal_places),
        position = "bottomleft" 
      )
  })
  
  
  observe({
    current_sf <- filtered_sf_data()
    var_id <- "anteil"
    var_label <- "Anteil weiblicher Sozialversicherungspflichtig Beschäftigte"
    color_min <- GLOBAL_MIN_ANTEIL
    color_max <- GLOBAL_MAX_ANTEIL
    unit <- " %"
    decimal_places <- 1
    
    pal <- colorNumeric(
      palette = "Reds", 
      domain = c(color_min, color_max)
    )
    
    map_data <- current_sf %>%
      select(sb_nummer, !!sym(MAP_NAME_COLUMN), geometry, Value = !!sym(var_id))
    
    popup_content <- paste0(
      "<b>Stadtbezirk:</b> ", map_data[[MAP_NAME_COLUMN]], "<br/>", 
      "<b>", var_label, " (", input$time_slider, "):</b> ", round(map_data$Value, decimal_places), unit
    )
    
    leafletProxy("map_output_2", data = map_data) %>%
      clearShapes() %>% 
      clearControls() %>% 
      addPolygons(
        fillColor = ~pal(Value),
        weight = 1.5, opacity = 1, color = "white", dashArray = "1", fillOpacity = 0.7,
        highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
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
