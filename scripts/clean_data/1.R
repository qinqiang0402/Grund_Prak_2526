library(readxl)
library(sf)
library(shiny)
library(leaflet)
library(dplyr)
library(stringr)
library(htmltools)

# Load Data
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = 2)
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
ki_sheet <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

geojson_url <- "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gsm_wfs:vablock_stadtbezirke_opendata&outputFormat=application/json"
munich_map <- st_read(geojson_url)

# Employment
df_employment_yearly <- ar_sheet %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    year = Jahr,
    employment_pct = 100 * `Basiswert 1` / `Basiswert 2`,
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(year, bezirksnummer, employment_pct)

# Households with Children
df_households_yearly <- be_sheet %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    year = Jahr,
    households_with_children_pct = 100 * `Basiswert 1` / `Basiswert 2`,
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(year, bezirksnummer, households_with_children_pct)

# First-Birth Age
df_firstbirth_age_yearly <- be_sheet %>%
  filter(
    Indikator == "Durchschnittsalter Mütter erstgebärend",
    Ausprägung == "insgesamt",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(
    year = Jahr,
    firstbirth_age = `Basiswert 1` / `Basiswert 2`,
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(year, bezirksnummer, firstbirth_age) %>%
  filter(!is.na(bezirksnummer))

# Kinderbetreuung 0–2 Jahre
df_betreuung_yearly <- ki_sheet %>%
  filter(
    Indikator == "Altersgruppen",
    Ausprägung == "bis 2 Jahre"
  ) %>%
  mutate(
    year = Jahr,
    betreuung_pct = 100 * `Basiswert 1` / `Basiswert 2`,
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(year, bezirksnummer, betreuung_pct)

# Indicator list
indicator_list <- list(
  employment = df_employment_yearly,
  households = df_households_yearly,
  firstbirth = df_firstbirth_age_yearly,
  betreuung = df_betreuung_yearly
)

# GLOBAL MIN / MAX
GLOBAL_MIN_EMP <- min(df_employment_yearly[[3]], na.rm = TRUE)
GLOBAL_MAX_EMP <- max(df_employment_yearly[[3]], na.rm = TRUE)

GLOBAL_MIN_HH <- min(df_households_yearly[[3]], na.rm = TRUE)
GLOBAL_MAX_HH <- max(df_households_yearly[[3]], na.rm = TRUE)

GLOBAL_MIN_FB <- min(df_firstbirth_age_yearly[[3]], na.rm = TRUE)
GLOBAL_MAX_FB <- max(df_firstbirth_age_yearly[[3]], na.rm = TRUE)

GLOBAL_MIN_BT <- min(df_betreuung_yearly[[3]], na.rm = TRUE)
GLOBAL_MAX_BT <- max(df_betreuung_yearly[[3]], na.rm = TRUE)

# Mapping Function
create_indicator_map <- function(
    geo_data,
    data_table,
    value_col,
    global_min,
    global_max,
    name_col = "name",
    title = "Indikator",
    palette = "Blues"
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
    domain = c(global_min, global_max)
  )
  
  leaflet(data = merged_data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      fillColor = ~pal(value),
      fillOpacity = 0.85,
      weight = 1.5,
      color = "white",
      dashArray = "1",
      popup = ~paste0(
        "<b>Bezirk:</b> ", get(name_col), "<br>",
        "<b>", title, ":</b> ", round(value, 2), "<br><br>",
        "<a href='", WikiURL, "' target='_blank'>Wikipedia</a>"
      ) %>% lapply(HTML)
    ) %>%
    addLegend(
      pal = pal,
      values = seq(global_min, global_max, length.out = 5),
      title = title,
      position = "bottomright"
    )
}

# UI
ui <- fluidPage(
  
  titlePanel("Sozialindikatoren München – Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      h4("Vergleichsvariable auswählen (Variable 2)"),
      selectInput(
        "var2",
        "Variable:",
        choices = c(
          "Haushalte mit Kindern (%)" = "households",
          "Durchschnittsalter erstgebärender Mütter" = "firstbirth",
          "Kinderbetreuung 0–2 Jahre (%)" = "betreuung"
        ),
        selected = "households"
      ),
      
      hr(),
      h5("Jahr auswählen"),
      sliderInput(
        "year",
        label = NULL,
        min = min(df_employment_yearly$year),
        max = max(df_employment_yearly$year),
        value = min(df_employment_yearly$year),
        step = 1,
        animate = animationOptions(interval = 1200, loop = TRUE)
      ),
      
      hr(),
      textOutput("var_info")
    ),
    
    mainPanel(
      fluidRow(
        column(
          width = 6,
          h4("Beschäftigungsquote (Variable 1)"),
          leafletOutput("map_var1", height = 600)
        ),
        column(
          width = 6,
          h4("Ausgewählte Variable (Variable 2)"),
          leafletOutput("map_var2", height = 600)
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  var2_year_range <- reactive({
    df <- indicator_list[[input$var2]]
    range(df$year, na.rm = TRUE)
  })
  
  observeEvent(input$var2, {
    rng <- var2_year_range()
    updateSliderInput(session, "year",
                      min = rng[1], max = rng[2], value = rng[1])
  })
  
  # Var1 (Employment)
  output$map_var1 <- renderLeaflet({
    df <- indicator_list$employment %>% filter(year == input$year)
    
    create_indicator_map(
      munich_map,
      df,
      value_col = names(df)[3],
      global_min = GLOBAL_MIN_EMP,
      global_max = GLOBAL_MAX_EMP,
      title = paste0("Beschäftigungsquote (%) — ", input$year),
      palette = "Blues"
    )
  })
  
  # Var2
  output$map_var2 <- renderLeaflet({
    df <- indicator_list[[input$var2]] %>% filter(year == input$year)
    
    if (input$var2 == "households") {
      gmin <- GLOBAL_MIN_HH; gmax <- GLOBAL_MAX_HH; pal <- "Greens"
      title_text <- "Haushalte mit Kindern (%)"
    } else if (input$var2 == "firstbirth") {
      gmin <- GLOBAL_MIN_FB; gmax <- GLOBAL_MAX_FB; pal <- "Purples"
      title_text <- "Durchschnittsalter erstgebärender Mütter"
    } else {
      gmin <- GLOBAL_MIN_BT; gmax <- GLOBAL_MAX_BT; pal <- "OrRd"
      title_text <- "Kinderbetreuung 0–2 Jahre (%)"
    }
    
    create_indicator_map(
      munich_map,
      df,
      value_col = names(df)[3],
      global_min = gmin,
      global_max = gmax,
      title = paste0(title_text, " — ", input$year),
      palette = pal
    )
  })
  
  output$var_info <- renderText({
    paste("Ausgewählte Variable:", input$var2)
  })
}

shinyApp(ui, server)

