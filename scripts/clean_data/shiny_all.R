library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(readxl)

# =================================================================
# 1. Datenvorbereitung
# =================================================================

# 1.1. Geometrie laden
geojson_url <- "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gsm_wfs:vablock_stadtbezirke_opendata&outputFormat=application/json"
munich_map <- st_read(geojson_url, quiet = TRUE) %>% 
  st_transform(4326)

# 1.2. Daten laden
# Pfade zu deinen Excel-Dateien
be_sheet <- read_excel("../../data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
ar_sheet <- read_excel("../../data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")
ki_sheet <- read_excel("../../data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG") 

# --- 1.3. Variablen Aufbereitung ---

# 1.3.1 Var 1: Erstgeburtsalter (Soll später PINK werden)
be_data_long <- be_sheet %>%
  filter(Indikator == "Durchschnittsalter Mütter erstgebärend", 
         Ausprägung == "insgesamt", 
         Raumbezug != "Stadt München") %>%
  mutate(
    age_mean = `Basiswert 1` / `Basiswert 2`,
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(Jahr, bezirksnummer, age_mean) %>% 
  filter(!is.na(bezirksnummer))

# 1.3.2 Var 2: SV-Beschäftigte Frauen
ar_data_long <- ar_sheet %>%
  filter(Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil", 
         Ausprägung == "weiblich", 
         Raumbezug != "Stadt München") %>%
  mutate(
    anteil = 100 * `Basiswert 1` / `Basiswert 2`,
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(Jahr, bezirksnummer, anteil) %>% 
  filter(!is.na(bezirksnummer))

# 1.3.3 Var 3: Haushalte mit Kindern
hh_data_long <- be_sheet %>%
  filter(Indikator == "Haushalte mit Kindern", 
         Ausprägung == "insgesamt", 
         Raumbezug != "Stadt München") %>%
  mutate(
    anteil_kinder = 100 * `Basiswert 1` / `Basiswert 2`,
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(Jahr, bezirksnummer, anteil_kinder) %>% 
  filter(!is.na(bezirksnummer))

# 1.3.4 Var 4: Kinderbetreuungsquote (FIXED LOGIC)
# Schritt A: Nenner (Alle Kinder 0-2) aus BEVÖLKERUNG
df_total <- be_sheet %>%
  filter(Indikator == "Altersgruppen", 
         Ausprägung == "bis 2 Jahre", 
         Raumbezug != "Stadt München") %>%
  select(Jahr, Raumbezug, kinder_total = `Basiswert 1`)

# Schritt B: Zähler (Betreute Kinder) aus KINDERBETREUUNG
df_betreut_raw <- ki_sheet %>%
  filter(Indikator == "Altersgruppen", 
         Ausprägung == "bis 2 Jahre", 
         Raumbezug != "Stadt München") %>%
  select(Jahr, Raumbezug, kinder_betreut = `Basiswert 1`)

# Schritt C: Join und Berechnung
betreuung_data_long <- df_total %>%
  left_join(df_betreut_raw, by = c("Jahr", "Raumbezug")) %>%
  mutate(
    # Berechnung der Quote in Prozent
    anteil_betreuung = 100 * kinder_betreut / kinder_total,
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(Jahr, bezirksnummer, anteil_betreuung) %>%
  filter(!is.na(bezirksnummer))


# 1.3.5 Var 5: Geburtenrate (OHNE FAKTOR 1000)
gr_data_long <- be_sheet %>%
  filter(
    Indikator == "Allgemeine Geburtenrate",
    Ausprägung == "insgesamt",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(
    new_baby = 100 * `Basiswert 1` / `Basiswert 2`, 
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(Jahr, bezirksnummer, new_baby) %>%
  filter(!is.na(bezirksnummer))


combined_data_all <- be_data_long %>%
  full_join(ar_data_long, by = c("Jahr", "bezirksnummer")) %>%
  full_join(hh_data_long, by = c("Jahr", "bezirksnummer")) %>%
  full_join(betreuung_data_long, by = c("Jahr", "bezirksnummer")) %>%
  full_join(gr_data_long, by = c("Jahr", "bezirksnummer"))

final_sf_data_long <- munich_map %>%
  left_join(combined_data_all, by = c("sb_nummer" = "bezirksnummer")) %>%
  filter(!is.na(Jahr))

# --- 2. Konstanten & Globale Werte ---

MAP_NAME_COLUMN <- "name" 
MIN_YEAR <- min(final_sf_data_long$Jahr, na.rm = TRUE)
MAX_YEAR <- max(final_sf_data_long$Jahr, na.rm = TRUE)

variable_choices <- c(
  "Muttererstgeburtsalter" = "age_mean",
  "Anteil Frauenbeschäftigung (%)" = "anteil",
  "Anteil Haushalte mit Kindern (%)" = "anteil_kinder",
  "Betreuungsquote Kinder 0-2 Jahre (%)" = "anteil_betreuung",
  "Geburtenrate (%)" = "new_baby"
)

# Globale Min/Max für konsistente Farben
GLOBAL_MIN_AGE <- min(final_sf_data_long$age_mean, na.rm = TRUE); GLOBAL_MAX_AGE <- max(final_sf_data_long$age_mean, na.rm = TRUE)
GLOBAL_MIN_ANTEIL <- min(final_sf_data_long$anteil, na.rm = TRUE); GLOBAL_MAX_ANTEIL <- max(final_sf_data_long$anteil, na.rm = TRUE)
GLOBAL_MIN_KINDER <- min(final_sf_data_long$anteil_kinder, na.rm = TRUE); GLOBAL_MAX_KINDER <- max(final_sf_data_long$anteil_kinder, na.rm = TRUE)
GLOBAL_MIN_BETREUUNG <- min(final_sf_data_long$anteil_betreuung, na.rm = TRUE); GLOBAL_MAX_BETREUUNG <- max(final_sf_data_long$anteil_betreuung, na.rm = TRUE)
GLOBAL_MIN_BABY <- min(final_sf_data_long$new_baby, na.rm = TRUE); GLOBAL_MAX_BABY <- max(final_sf_data_long$new_baby, na.rm = TRUE)

# =================================================================
# 3. UI
# =================================================================
ui <- fluidPage(
  tags$head(tags$style(HTML(".main-title { font-size: 24px; font-weight: bold; margin-top: 10px; text-align: center; }"))),
  div(class = "main-title", "Variable nach Münchener Stadtteilen"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable_select", "Indikator wählen:", choices = variable_choices, selected = "age_mean"),
      sliderInput("time_slider", "Jahr:", min = MIN_YEAR, max = MAX_YEAR, value = MAX_YEAR, step = 1, sep = "", animate = TRUE),
      width = 3
    ),
    mainPanel(leafletOutput("map_output", height = "700px"), width = 9)
  )
)

# =================================================================
# 4. Server
# =================================================================
server <- function(input, output, session) {
  
  output$year_display <- renderText({ paste0("Jahr: ", input$time_slider) })
  
  filtered_sf_data <- reactive({
    final_sf_data_long %>% 
      filter(Jahr == input$time_slider) %>%
      select(sb_nummer, !!sym(MAP_NAME_COLUMN), geometry, Value = !!sym(input$variable_select))
  })
  
  output$map_output <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 10, maxZoom = 14)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 11.5761, lat = 48.1372, zoom = 10)
  })
  
  observe({
    current_sf <- filtered_sf_data()
    var_id <- input$variable_select
    var_label <- names(variable_choices)[variable_choices == var_id]
    
    # --- Konfiguration der Variablen ---
    if (var_id == "age_mean") {
      # Mütter -> PINK ("RdPu")
      c_min <- GLOBAL_MIN_AGE; c_max <- GLOBAL_MAX_AGE
      unit <- " Jahre"; dec <- 2
      pal_name <- "RdPu" 
      
    } else if (var_id == "anteil") {
      # Arbeit -> BLAU
      c_min <- GLOBAL_MIN_ANTEIL; c_max <- GLOBAL_MAX_ANTEIL
      unit <- " %"; dec <- 1
      pal_name <- "Blues"
      
    } else if (var_id == "anteil_kinder") {
      # HH Kinder -> GRÜN
      c_min <- GLOBAL_MIN_KINDER; c_max <- GLOBAL_MAX_KINDER
      unit <- " %"; dec <- 1
      pal_name <- "Greens"
      
    } else if (var_id == "anteil_betreuung") {
      # Betreuung -> ORANGE (Zur Unterscheidung)
      c_min <- GLOBAL_MIN_BETREUUNG; c_max <- GLOBAL_MAX_BETREUUNG
      unit <- " %"; dec <- 1
      pal_name <- "Purples"
      
    } else { 
      # Geburtenrate -> ROT
      # Achtung: Werte sind sehr klein (z.B. 0.01), daher 4 Dezimalstellen
      c_min <- GLOBAL_MIN_BABY; c_max <- GLOBAL_MAX_BABY
      unit <- ""; dec <- 4 
      pal_name <- "Reds" 
    }
    
    pal <- colorNumeric(palette = pal_name, domain = c(c_min, c_max))
    
    popup_content <- paste0(
      "<b>Bezirk:</b> ", current_sf[[MAP_NAME_COLUMN]], "<br/>", 
      "<b>", var_label, ":</b> ", round(current_sf$Value, dec), unit
    )
    
    leafletProxy("map_output", data = current_sf) %>%
      clearShapes() %>% clearControls() %>% 
      addPolygons(
        fillColor = ~pal(Value), weight = 1.5, opacity = 1, color = "white", dashArray = "1", fillOpacity = 0.7,
        label = lapply(popup_content, HTML),
        highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE)
      ) %>% 
      addLegend(
        pal = pal, values = c(c_min, c_max), opacity = 0.7, title = var_label,
        labFormat = labelFormat(suffix = unit, digits = dec), position = "bottomright"
      )
  })
}

shinyApp(ui = ui, server = server)
