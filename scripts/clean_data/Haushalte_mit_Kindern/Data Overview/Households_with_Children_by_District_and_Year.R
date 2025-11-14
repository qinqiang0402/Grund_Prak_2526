library(ggplot2)
library(dplyr)
library(readxl)
library(viridis)

data <- read_excel("data/raw/export_be.xlsx", sheet = 2)

# Filter for households with children (total), excluding Munich city
df_trend <- data %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(
    Indikatorwert = as.numeric(gsub(",", ".", as.character(Indikatorwert)))
  )

# Plot: Heatmap of share of households with children by district and year
ggplot(df_trend, aes(
  x = Jahr,
  y = reorder(Raumbezug, -Indikatorwert),
  fill = Indikatorwert
)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Share (%)"
  ) +
  labs(
    title = "Share of German Households with Children by District and Year",
    subtitle = "Color scale: Viridis (Plasma option)",
    x = "Year",
    y = "City District"
  ) +
  theme_minimal(base_size = 11) +  # smaller base font
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

