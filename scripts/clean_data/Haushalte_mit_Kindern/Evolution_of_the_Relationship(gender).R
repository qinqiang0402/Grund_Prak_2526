library(readxl)
library(tidyverse)
library(ggrepel)
library(ggpmisc)

# Load unemployment data
data_ar <- read_excel("data/raw/export_ar.xlsx", sheet = 2)

data_ar_clean <- data_ar %>%
  select(Indikator, Ausprägung, Jahr, Raumbezug, Indikatorwert) %>%
  # Keep both male and female unemployment rates, all years
  filter(
    Indikator == "Arbeitslose - Anteil",
    Ausprägung %in% c("weiblich", "männlich")
  ) %>%
  # Convert decimal commas to numeric
  mutate(Indikatorwert = as.numeric(str_replace(Indikatorwert, ",", "."))) %>%
  # Pivot wider: male/female unemployment in separate columns
  pivot_wider(
    names_from = Ausprägung,
    values_from = Indikatorwert,
    names_prefix = "arbeitslos_"
  ) %>%
  # Compute gender difference (male − female)
  mutate(geschlechter_diff = arbeitslos_männlich - arbeitslos_weiblich) %>%
  # Exclude Munich total
  filter(Raumbezug != "Stadt München")

# Load household-with-children data
data_be <- read_excel("data/raw/export_be.xlsx", sheet = 2)

data_be_clean <- data_be %>%
  select(Indikator, Ausprägung, Jahr, Raumbezug, Indikatorwert) %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(Indikatorwert = as.numeric(str_replace(Indikatorwert, ",", "."))) %>%
  filter(Raumbezug != "Stadt München")

# Merge datasets
df <- inner_join(data_ar_clean, data_be_clean, by = c("Jahr", "Raumbezug"))

# Plot
ggplot(df, aes(x = Indikatorwert, y = geschlechter_diff, group = Jahr)) +
  # Grey transparent scatter points
  geom_point(size = 2, alpha = 0.25, color = "grey60") +
  # Linear trend lines for each year (colored by year)
  geom_smooth(
    aes(color = Jahr),
    method = "lm",
    se = FALSE,
    linewidth = 1.1,
    alpha = 0.9
  ) +
  # Viridis color gradient (Plasma option)
  scale_color_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Year"
  ) +
  # Axes
  scale_x_continuous(limits = c(5, 30), breaks = seq(5, 30, 5)) +
  scale_y_continuous(limits = c(-0.1, 1.3), breaks = seq(0, 1.2, 0.2)) +
  # Labels and captions
  labs(
    title = "Evolution of the Relationship (2012–2024)",
    subtitle = "Link between households with children and gender gap in unemployment rates",
    x = "Share of households with children (%)",
    y = "Difference in unemployment rate (male − female, percentage points)\n(positive values = higher male unemployment)",
    caption = "Data source: City of Munich · Own calculation"
  ) +
  # Clean theme, smaller font
  theme_minimal(base_size = 11, base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey30"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "grey25"),
    legend.position = "bottom",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8)
  )+
  scale_color_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Year",
    breaks = seq(2012, 2024, 2),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 10,
      barheight = 0.6
    )
  )

