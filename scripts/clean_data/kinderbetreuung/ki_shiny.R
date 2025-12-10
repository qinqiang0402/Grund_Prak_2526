<!--
  
  ## Kinderbetreuung
  
  ```{r}
#| context: server

# ---- Play / Pause 状态（第二套）----
playing_2 <- reactiveVal(FALSE)

# 点击按钮切换播放状态 + 修改按钮文字
observeEvent(input$play_pause_2, {
  if (playing_2()) {
    playing_2(FALSE)
    updateActionButton(session, "play_pause_2", label = "▶ Play")
  } else {
    playing_2(TRUE)
    updateActionButton(session, "play_pause_2", label = "⏸ Pause")
  }
})

# 自动推进年份（只有在 playing_2() == TRUE 时才会动）
observe({
  req(playing_2())                # 如果没在播放，下面不执行
  invalidateLater(1000, session)  # 播放速度：1000ms 一步
  
  current   <- isolate(input$time_slider_2)
  next_year <- if (current >= MAX_YEAR) MIN_YEAR else current + 1L
  
  updateSliderInput(session, "time_slider_2", value = next_year)
})

# 1. Reaktive gefilterte Daten (immer aktuelles Jahr + gewählte Variable)
filtered_sf_data_2 <- reactive({
  final_sf_data_long %>% 
    filter(Jahr == input$time_slider_2) %>%
    select(
      sb_nummer,
      !!sym(MAP_NAME_COLUMN),
      geometry,
      Value = !!sym(input$variable_select_2)
    )
})

# 2. Grundkarte (wird einmal initialisiert)
output$map_output_2 <- renderLeaflet({
  leaflet(options = leafletOptions(minZoom = 10, maxZoom = 14)) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%   # 黑白底图
    setView(lng = 11.5761, lat = 48.1372, zoom = 10)
})

# 3. Karte bei Änderung von Slider / Variable neu einfärben
observe({
  current_sf <- filtered_sf_data_2()
  var_id     <- input$variable_select_2
  var_label  <- names(variable_choices)[variable_choices == var_id]
  
  # ---- Farbbereich & Einheit je nach Variable ----
  if (var_id == "age_mean") {
    c_min <- GLOBAL_MIN_AGE;       c_max <- GLOBAL_MAX_AGE
    unit  <- " Jahre";             dec   <- 2
    pal_name <- "RdPu"
    
  } else if (var_id == "anteil") {
    c_min <- GLOBAL_MIN_ANTEIL;    c_max <- GLOBAL_MAX_ANTEIL
    unit  <- " %";                 dec   <- 1
    pal_name <- "Blues"
    
  } else if (var_id == "anteil_kinder") {
    c_min <- GLOBAL_MIN_KINDER;    c_max <- GLOBAL_MAX_KINDER
    unit  <- " %";                 dec   <- 1
    pal_name <- "Greens"
    
  } else if (var_id == "anteil_betreuung") {
    c_min <- GLOBAL_MIN_BETREUUNG; c_max <- GLOBAL_MAX_BETREUUNG
    unit  <- " %";                 dec   <- 1
    pal_name <- "Reds"
    
  } else {
    c_min <- GLOBAL_MIN_BABY;      c_max <- GLOBAL_MAX_BABY
    unit  <- "";                   dec   <- 4
    pal_name <- ""
  }
  
  pal <- colorNumeric(palette = pal_name, domain = c(c_min, c_max))
  
  popup_content <- paste0(
    "<b>Stadtteil:</b> ", current_sf[[MAP_NAME_COLUMN]], "<br/>",
    "<b>", var_label, ":</b> ", round(current_sf$Value, dec), unit
  )
  
  leafletProxy("map_output_2", data = current_sf) %>%
    clearShapes() %>%
    clearControls() %>%
    addPolygons(
      fillColor = ~pal(Value),
      weight = 1.5,
      opacity = 1,
      color = "white",
      dashArray = "1",
      fillOpacity = 0.7,
      label = lapply(popup_content, HTML),
      highlightOptions = highlightOptions(
        weight = 3,
        color  = "#666",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) %>%
    addLegend(
      pal      = pal,
      values   = c(c_min, c_max),
      opacity  = 0.7,
      title    = var_label,
      labFormat= labelFormat(suffix = unit, digits = dec),
      position = "bottomright"
    ) %>%
    # ✅ 右上角年份（第二套用 time_slider_2）
    addControl(
      html = sprintf(
        "<div style='font-size:26px;
                     font-weight:bold;
                     background:rgba(255,255,255,0.8);
                     padding:4px 8px;
                     border-radius:4px;
                     box-shadow:0 0 3px rgba(0,0,0,0.3);'>
           Jahr: %s
         </div>",
        input$time_slider_2
      ),
      position = "topright"
    )
})

```

```{r}
#| panel: sidebar
# 这里是 Shiny 的 UI 控件（侧边栏）
selectInput(
  "variable_select_2",
  "",
  choices  = variable_choices,
  selected = "anteil_betreuung"    # 默认先看 Kinderbetreuung
)

sliderInput(
  "time_slider_2",
  "Jahr",
  min     = 2007,
  max     = 2024,
  value   = 2007,
  step    = 1,
  sep     = "",
  animate = FALSE
)

actionButton(
  "play_pause_2",
  "▶ Play",
  width = "100%"
)
```

```{r}
#| panel: fill
# 主区域放地图
leafletOutput("map_output_2", height = "550px")
```

-->