library(shiny)
library(dplyr)
library(sf)
library(tidyverse)
library(readxl)
library(htmltools)
library(leaflet)

# ---------------------------------------------------------------
# 0. Load FULL DATA (一次性预处理)
# ---------------------------------------------------------------

munich_map <- st_read("results/geo/bezirk_map.gpkg", quiet = TRUE) |>
  st_transform(4326)

be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")
ki_sheet <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

# Haushalte mit Kindern
hh_long <- be_sheet %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+"))),
    anteil_kinder = 100 * `Basiswert 1` / `Basiswert 2`
  ) %>%
  select(Jahr, bezirksnummer, anteil_kinder)

# Frauenbeschäftigung
emp_long <- ar_sheet %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+"))),
    anteil = 100 * `Basiswert 1` / `Basiswert 2`
  ) %>%
  select(Jahr, bezirksnummer, anteil)

# Kinderbetreuung bis 2 Jahre
total_long <- be_sheet %>%
  filter(
    Indikator == "Altersgruppen",
    Ausprägung == "bis 2 Jahre",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))) %>%
  select(Jahr, bezirksnummer, kinder_total = `Basiswert 1`)

betreut_long <- ki_sheet %>%
  filter(
    Indikator == "Altersgruppen",
    Ausprägung == "bis 2 Jahre",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))) %>%
  select(Jahr, bezirksnummer, kinder_betreut = `Basiswert 1`)

betreuung_long <- total_long %>%
  left_join(betreut_long, by = c("Jahr", "bezirksnummer")) %>%
  mutate(anteil_betreuung = 100 * kinder_betreut / kinder_total)

# Merge ALL
full_data <- hh_long %>%
  left_join(emp_long, by = c("Jahr", "bezirksnummer")) %>%
  left_join(betreuung_long, by = c("Jahr", "bezirksnummer")) %>%
  left_join(munich_map, by = c("bezirksnummer" = "sb_nummer")) %>%
  st_as_sf()


# ---------------------------------------------------------------
# UI
# ---------------------------------------------------------------
ui <- fluidPage(
  
  # --- 顶部控制区 ---
  fluidRow(
    column(4,
           sliderInput("year", "Jahr:", 
                       min = 2012, max = 2024, value = 2024, step = 1, sep = "")
    ),
    column(2,
           actionButton("play", "▶ Play", width = "120px")
    )
  ),
  
  hr(),
  
  # --- 两张地图并排 ---
  fluidRow(
    column(6,
           h4("Haushalte mit Kindern + Beschäftigung"),
           leafletOutput("map_left", height = 450)
    ),
    column(6,
           h4("Kinderbetreuung (0–2 Jahre)"),
           leafletOutput("map_right", height = 450)
    )
  )
)


# ---------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------
server <- function(input, output, session) {
  
  # -------------------------
  # 自动播放状态
  # -------------------------
  playing <- reactiveVal(FALSE)
  
  observeEvent(input$play, {
    playing(!playing())
    updateActionButton(session, "play", 
                       label = ifelse(playing(), "⏸ Pause", "▶ Play"))
  })
  
  observe({
    req(playing())
    invalidateLater(1000)
    
    yr <- isolate(input$year)
    updateSliderInput(session, "year",
                      value = ifelse(yr >= 2024, 2012, yr + 1))
  })
  
  
  # -------------------------
  # 每年计算 München 均值（HK & FE）
  # -------------------------
  city_means <- reactive({
    tibble(
      mean_HaKi = be_sheet %>%
        filter(Jahr == input$year,
               Raumbezug == "Stadt München",
               Indikator == "Haushalte mit Kindern",
               Ausprägung == "insgesamt") %>%
        mutate(val = 100 * `Basiswert 1` / `Basiswert 2`) %>%
        pull(val),
      
      mean_FE = ar_sheet %>%
        filter(Jahr == input$year,
               Raumbezug == "Stadt München",
               Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
               Ausprägung == "weiblich") %>%
        mutate(val = 100 * `Basiswert 1` / `Basiswert 2`) %>%
        pull(val)
    )
  })
  
  
  # -------------------------
  # 按年份过滤数据
  # -------------------------
  df_year <- reactive({
    full_data %>% filter(Jahr == input$year)
  })
  
  
  # -------------------------------------------------
  # 左图：HK hoch + FE niedrig 染粉色
  # -------------------------------------------------
  output$map_left <- renderLeaflet({
    
    d <- df_year()
    m <- city_means()
    
    d <- d %>%
      mutate(
        gruppe = case_when(
          anteil_kinder > m$mean_HaKi & anteil < m$mean_FE ~
            "HK hoch + Beschäftigung niedrig",
          TRUE ~ "Andere"
        ),
        color = ifelse(gruppe == "HK hoch + Beschäftigung niedrig",
                       "#e75480", "#d9d9d9")
      )
    
    leaflet(d) %>%
      addTiles(urlTemplate = "https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>%
      addPolygons(
        fillColor = ~color, color = "white", weight = 1,
        fillOpacity = 0.8,
        label = ~paste0("<b>", name, "</b><br>",
                        "Haushalte mit Kindern: ", round(anteil_kinder,1),"%<br>",
                        "Frauenbeschäftigung: ", round(anteil,1),"%") %>% lapply(HTML)
      )
  })
  
  
  # -------------------------------------------------
  # 右图：Kinderbetreuung Purples 色阶
  # -------------------------------------------------
  output$map_right <- renderLeaflet({
    
    d <- df_year()
    
    pal <- colorNumeric("Purples", domain = c(0, 100))
    
    leaflet(d) %>%
      addTiles(urlTemplate = "https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>%
      addPolygons(
        fillColor = ~pal(anteil_betreuung),
        color = "white", weight = 1,
        fillOpacity = 0.8,
        label = ~paste0("<b>", name, "</b><br>",
                        "Betreuung 0–2: ", round(anteil_betreuung,1),"%") %>% lapply(HTML)
      ) %>%
      addLegend(pal = pal, values = c(0, 100),
                title = "Betreuung (%)",
                position = "bottomright")
  })
  
}

shinyApp(ui, server)
