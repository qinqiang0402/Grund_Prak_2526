library(shiny)
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)

# ---------------------------------------------------------
# Datenvorbereitung
# ---------------------------------------------------------

# Bevölkerungsdaten (0–2 Jahre)
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")

be_0bis2 <- be_sheet %>% 
  filter(
    Indikator == "Altersgruppen",
    Ausprägung == "bis 2 Jahre"
  ) %>%
  select(Indikator, Ausprägung, Jahr, Raumbezug, `Basiswert 1`) %>%
  rename(kinder_total = `Basiswert 1`)

# Kinderbetreuungsdaten (0–2 Jahre)
ki_sheet <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

ki_0bis2 <- ki_sheet %>% 
  filter(
    Indikator == "Altersgruppen",
    Ausprägung == "bis 2 Jahre"
  ) %>% 
  select(Indikator, Ausprägung, Jahr, Raumbezug, `Basiswert 1`) %>%
  rename(kinder_betreut = `Basiswert 1`)

# Zusammenführen von Bevölkerung + betreuten Kindern
df_betreut <- left_join(
  be_0bis2, ki_0bis2,
  by = c("Jahr", "Raumbezug", "Indikator", "Ausprägung")
) %>%
  mutate(
    kinder_unbetreut   = kinder_total - kinder_betreut,
    anteil_unbetreut   = kinder_unbetreut / kinder_total * 100,
    anteil_betreut     = kinder_betreut / kinder_total * 100
  )

# Beschäftigungsdaten (Anteil weiblich)
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

# Haushalte mit Kindern
df_households <- be_sheet %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    Jahr = as.numeric(Jahr),
    households_pct = 100 * `Basiswert 1` / `Basiswert 2`
  ) %>%
  select(Jahr, Raumbezug, households_pct)

# Merge Beschäftigte + Haushalte
df_merged <- df_emp %>%
  inner_join(df_households, by = c("Jahr", "Raumbezug"))



# ---------------------------------------------------------
# Shiny UI
# ---------------------------------------------------------

ui <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "year",
        "Jahr:",
        min     = min(df_merged$Jahr),
        max     = max(df_merged$Jahr),
        value   = max(df_merged$Jahr),
        step    = 1,
        sep     = "",
        animate = animationOptions(interval = 1200, loop = TRUE)
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)



# ---------------------------------------------------------
# Shiny Server
# ---------------------------------------------------------

server <- function(input, output) {
  
  # Jahresdatensatz erstellen

  df_year <- reactive({
    df_merged %>%
      filter(
        Jahr == input$year,
        Raumbezug != "Stadt München"   
      ) %>%
      inner_join(
        df_betreut %>%
          filter(Jahr == input$year) %>%
          select(Raumbezug, anteil_betreut),
        by = "Raumbezug"
      )
  })
  
  # Quantile berechnen + Betreuungsgruppen zuordnen
  df_year_grouped <- reactive({
    d  <- df_year()
    qs <- quantile(
      d$anteil_betreut,
      probs = c(0, 1/3, 2/3, 1),
      na.rm = TRUE
    )
    
    d %>%
      mutate(
        betreuung_group = cut(
          anteil_betreut,
          breaks = qs,
          labels = c("Niedrig", "Mittel", "Hoch"),
          include.lowest = TRUE
        )
      )
  })
  
  # Farben für Gruppen
  farben <- c(
    "Gesamt"             = "black",
    "Hohe Betreuung"     = "#e66101",
    "Niedrige Betreuung" = "#fdb863"
  )
  
  
  # ---------------------------------------------------------
  # Plot-Ausgabe
  # ---------------------------------------------------------
  output$plot <- renderPlot({
    
    df <- df_year_grouped()
    
    # Linien-Datensatz (3 Linien)
    df_lines <- bind_rows(
      df %>% mutate(gruppe = "Gesamt"),
      df %>% filter(betreuung_group == "Hoch")    %>% mutate(gruppe = "Hohe Betreuung"),
      df %>% filter(betreuung_group == "Niedrig") %>% mutate(gruppe = "Niedrige Betreuung")
    )
    
    # Punkte für Hoch + Niedrig (farbig)
    df_points_grouped <- df %>%
      filter(betreuung_group %in% c("Hoch", "Niedrig")) %>%
      mutate(
        gruppe = case_when(
          betreuung_group == "Hoch"    ~ "Hohe Betreuung",
          betreuung_group == "Niedrig" ~ "Niedrige Betreuung"
        )
      )
    
    # Punkte für Gesamt (grau)
    df_points_gesamt <- df %>%
      filter(betreuung_group == "Mittel" | is.na(betreuung_group)) %>%
      mutate(gruppe = "Gesamt")
    
    
    ggplot() +
      
      # Punkte: Gesamt (grau)
      geom_point(
        data  = df_points_gesamt,
        aes(x = emp_female_pct, y = households_pct),
        color = "grey50",
        alpha = 0.6
      ) +
      
      # Punkte: Hoch / Niedrig (farbig)
      geom_point(
        data  = df_points_grouped,
        aes(x = emp_female_pct, y = households_pct, color = gruppe),
        alpha = 0.8
      ) +
      
      # Regressionslinien
      geom_smooth(
        data  = df_lines,
        aes(x = emp_female_pct, y = households_pct, color = gruppe),
        method = "lm",
        se     = FALSE,
        size   = 1.2
      ) +
      
      scale_color_manual(values = farben, name = "Linien:") +
      
      labs(
        x = "Frauenbeschäftigung (%)",
        y = "Haushalte mit Kindern (%)"
      ) +
      
      annotate(
        "text", x = -Inf, y = Inf,
        label = paste("Jahr:", input$year),
        hjust  = -0.1,
        vjust  =  1.5,
        size   =  5
      ) +
      
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)
