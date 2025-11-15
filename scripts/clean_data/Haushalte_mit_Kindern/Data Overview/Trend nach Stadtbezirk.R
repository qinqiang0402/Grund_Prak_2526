library(ggplot2)
library(dplyr)
library(readxl)

data <- read_excel("data/raw/export_be.xlsx", sheet = 2)

df_trend <- data %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    Indikatorwert = as.numeric(gsub(",", ".", as.character(Indikatorwert))),
    color_group = case_when(
      Raumbezug == "06 Sendling" ~ "highlight_red",
      Raumbezug == "01 Altstadt - Lehel" ~ "highlight_blue",
      Raumbezug == "Stadt München" ~ "munich",
      TRUE ~ "other"
    )
  )

# ---- Farben & Linienbreite ----
cols <- c(
  "highlight_red"  = "#D55E00",
  "highlight_blue" = "#0072B2",
  "munich"         = "black",
  "other"          = "grey75"
)

sizes <- c(
  "highlight_red"  = 1.3,
  "highlight_blue" = 1.3,
  "munich"         = 1.5,
  "other"          = 0.5
)

# ---- Zeichnen ----
ggplot(df_trend, aes(
  x = Jahr,
  y = Indikatorwert,
  group = Raumbezug,
  color = color_group,
  size = color_group
)) +
  geom_line(alpha = 0.9) +
  scale_color_manual(values = cols) +
  scale_size_manual(values = sizes) +
  labs(
    title = "Haushalte mit Kindern – Trend nach Stadtbezirk",
    subtitle = "06 Sendling (Rot), 01 Altstadt-Lehel (Blau), Stadt München (Schwarz)",
    x = "Jahr",
    y = "Anteil (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    legend.position = "none"
  )
