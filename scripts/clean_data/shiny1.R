# app.R

# =================================================================
# 1. Bibliotheken und Datenvorbereitung (Globale Umgebung)
# =================================================================
# Installation und Laden der notwendigen Pakete
# install.packages("shiny", "tidyverse", "sf", "leaflet", "readxl")
library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(readxl)


# --- Deine Datenladungs- und Vorbereitungsschritte ---

# 1.1. Laden der Geometriedaten (Stadtbezirke München)
geojson_url <- "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gsm_wfs:vablock_stadtbezirke_opendata&outputFormat=application/json"
munich_map <- st_read(geojson_url, quiet = TRUE) %>% 
  st_transform(4326) # WGS84-Projektion fuer Leaflet


# 1.2. Laden der Statistikdaten (Voraussetzung: Dateien im Pfad data/raw/)

export_be <- read_excel("../../data/raw/export_be.xlsx") 
be_sheet <- read_excel("../../data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
export_ar <- read_excel("../../data/raw/export_ar.xlsx") 
ar_sheet <- read_excel("../../data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")


# 1.3. Bereinigung und Aufbereitung der Bevoelkerungsdaten (Erstgeburtsalter)
be_data_long <- be_sheet %>%
  filter(
    Indikator == "Durchschnittsalter Mütter erstgebärend",
    Ausprägung == "insgesamt",
    Raumbezug != "Stadt München" # Ausschluss der Gesamtsumme
  ) %>%
  mutate(
    age_mean = `Basiswert 1` / `Basiswert 2`, # Berechnung des Durchschnittsalters
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(Jahr, bezirksnummer, age_mean) %>%
  filter(!is.na(bezirksnummer))


# 1.4. Bereinigung und Aufbereitung der Arbeitsmarktdaten (SV-Quote Frauen)
ar_data_long <- ar_sheet %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich",
    Raumbezug != "Stadt München" # Ausschluss der Gesamtsumme
  ) %>%
  mutate(
    anteil = 100 * `Basiswert 1` / `Basiswert 2`, # Berechnung des prozentualen Anteils
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(Jahr, bezirksnummer, anteil) %>%
  filter(!is.na(bezirksnummer))



# 1.5. Zusammenfuehrung der Daten und des SF-Objekts
combined_data_all <- full_join(be_data_long, ar_data_long, by = c("Jahr", "bezirksnummer"))

final_sf_data_long <- munich_map %>%
  left_join(combined_data_all, by = c("sb_nummer" = "bezirksnummer")) %>%
  filter(!is.na(Jahr)) # Entfernt Bezirke/Jahre ohne Daten


# --- 2. Definition von Konstanten und Variablen-Optionen ---

# Annahme: Der Spaltenname fuer den Bezirksnamen in munich_map (bitte pruefen und anpassen!)
MAP_NAME_COLUMN <- "name" 

MIN_YEAR <- min(final_sf_data_long$Jahr, na.rm = TRUE)
MAX_YEAR <- max(final_sf_data_long$Jahr, na.rm = TRUE)

variable_choices <- c(
  "Durchschnittsalter Mütter bei Erstgeburt (Jahre)" = "age_mean",
  "Anteil Sozialversicherungspflichtig Beschäftigter (Frauen in %)" = "anteil"
)


# Berechnung des globalen Farbbereichs (Domain) fuer konsistente Darstellung
GLOBAL_MIN_AGE <- min(final_sf_data_long$age_mean, na.rm = TRUE)
GLOBAL_MAX_AGE <- max(final_sf_data_long$age_mean, na.rm = TRUE)
GLOBAL_MIN_ANTEIL <- min(final_sf_data_long$anteil, na.rm = TRUE)
GLOBAL_MAX_ANTEIL <- max(final_sf_data_long$anteil, na.rm = TRUE)

# =================================================================
# 3. Benutzeroberflaeche (UI)
# =================================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Styling fuer akademische/nüchterne Darstellung */
      .main-title { font-size: 24px; font-weight: bold; margin-top: 10px; text-align: center; }
      .time-label { font-size: 18px; font-weight: bold; text-align: center; margin-bottom: 10px; }
    "))
  ),
  
  div(class = "main-title", "Anteil weiblicher Sozialversicherungspflichtig Beschäftigte in München"),
  
  sidebarLayout(
    # Linke Seite: Eingabeparameter (Kontroll-Panel)
    sidebarPanel(
      # Variablenauswahl
      selectInput(
        inputId = "variable_select",
        label = "Indikator zur Analyse wählen:",
        choices = variable_choices,
        selected = "age_mean"
      ),
      
      # Zeitschieberegler (Slider)
      sliderInput(
        inputId = "time_slider",
        label = div(class = "time-label", textOutput("year_display")),
        min = MIN_YEAR,
        max = MAX_YEAR,
        value = MIN_YEAR, 
        step = 1, 
        sep = "",
        # Option zur automatischen Wiedergabe (Animation)
        animate = animationOptions(interval = 1500, loop = FALSE) 
      ),
      
      width = 3
    ),
    
    # Rechte Seite: Kartenausgabe
    mainPanel(
      leafletOutput("map_output", height = "700px"),
      width = 9
    )
  )
)


# =================================================================
# 4. Server-Logik
# =================================================================

server <- function(input, output, session) {
  
  # Ausgabe des aktuell gewaehlten Jahres
  output$year_display <- renderText({
    paste0("Jahr: ", input$time_slider)
  })
  
  # 4.1. Reaktive Datenfilterung
  # Diese Funktion reagiert auf Änderungen bei input$time_slider und input$variable_select
  filtered_sf_data <- reactive({
    
    # Filterung nach dem aktuellen Jahr
    data_filtered <- final_sf_data_long %>%
      filter(Jahr == input$time_slider)
    
    # Dynamische Auswahl der Variablen und Umbenennung zu 'Value'
    data_filtered %>%
      select(sb_nummer, !!sym(MAP_NAME_COLUMN), geometry, Value = !!sym(input$variable_select))
  })
  
  # 4.2. Initiales Rendern der Karte (Basis-Karte)
  
  output$map_output <- renderLeaflet({
    
    # 1. Erstelle die Basis-Karte (Die setView-Aktion wird explizit an die Karte gebunden)
    l_map <- leaflet(options = leafletOptions(minZoom = 10, maxZoom = 14)) %>%
      addProviderTiles(providers$CartoDB.Positron) # Saubere Basiskarte
    
    # 2. Fuehre setView aus
    l_map <- l_map %>%
      setView(lng = 11.5761, lat = 48.1372, zoom = 10) # Zentrum Muenchen
    
    # 3. Gib die fertige Basiskarte zurueck
    return(l_map)
  })
  

  
  # 4.3. Dynamisches Karten-Update (Observer)
  # Aktualisiert die Farbgebung und Legende bei jeder Nutzerinteraktion
  observe({
    # Abrufen des reaktiven Datensatzes
    current_sf <- filtered_sf_data()
    
    # Abrufen des Indikators und seiner globalen Grenzen
    var_id <- input$variable_select
    var_label <- names(variable_choices)[variable_choices == var_id]
    
    # Bestimmung der Farbskala-Parameter (Domain und Einheit)
    if (var_id == "age_mean") {
      color_min <- GLOBAL_MIN_AGE
      color_max <- GLOBAL_MAX_AGE
      unit <- " Jahre"
      decimal_places <- 2
    } else { # var_id == "anteil"
      color_min <- GLOBAL_MIN_ANTEIL
      color_max <- GLOBAL_MAX_ANTEIL
      unit <- " %"
      decimal_places <- 1
    }
    
    # Erstellung der Farbpalette (Blautöne fuer Choroplethenkarte)
    pal <- colorNumeric(
      palette = "Blues", 
      domain = c(color_min, color_max) # Nutzung der globalen Domain fuer Konsistenz
    )
    
    # Inhalt der Pop-up-Labels (Mouseover)
    popup_content <- paste0(
      "<b>Stadtbezirk:</b> ", current_sf[[MAP_NAME_COLUMN]], "<br/>", 
      "<b>", var_label, " (", input$time_slider, "):</b> ", round(current_sf$Value, decimal_places), unit
    )
    
    # Verwendung von leafletProxy() fuer effizientes Update
    leafletProxy("map_output", data = current_sf) %>%
      clearShapes() %>% # Entfernt die alten Polygone
      clearControls() %>% # Entfernt die alte Legende
      
      # Hinzufuegen der neuen Polygone
      addPolygons(
        fillColor = ~pal(Value), # Farbzuweisung basierend auf 'Value'
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
      
      # Hinzufuegen der Legende
      addLegend(
        pal = pal, 
        values = c(color_min, color_max),
        opacity = 0.7, 
        title = var_label,
        labFormat = labelFormat(suffix = unit, digits = decimal_places), # Anzeige von Einheit und Praezision
        position = "bottomright"
      )
  })
}

# 5. Starten der Shiny App
shinyApp(ui = ui, server = server)

# app.R (Ausschnitt aus Sektion 4.2: Initiales Karten-Rendering)


