# =========================================================
# Visualization: Female Unemployment Rate vs. Households with Children
# Data source: City of Munich (district level, 2024)
# =========================================================

library(readxl)
library(tidyverse)
library(ggrepel)

# --- 1. Import and clean unemployment data ---
data_ar <- read_excel("data/raw/export_ar.xlsx", sheet = 2)

data_ar_clean <- data_ar %>%
  select(Indikator, Ausprägung, Jahr, Raumbezug, Indikatorwert) %>%
  # Keep female unemployment rate for 2024
  filter(
    Indikator == "Arbeitslose - Anteil",
    Ausprägung == "weiblich",
    Jahr == 2024
  ) %>%
  # Convert decimal commas to numeric format
  mutate(Indikatorwert = as.numeric(str_replace(Indikatorwert, ",", "."))) %>%
  # Rename variable for clarity
  rename(arbeitslos_weiblich = Indikatorwert) %>%
  # Exclude total city-level observation
  filter(Raumbezug != "Stadt München")


# --- 2. Import and clean household data ---
data_be <- read_excel("data/raw/export_be.xlsx", sheet = 2)

data_be_clean <- data_be %>%
  select(Indikator, Ausprägung, Jahr, Raumbezug, Indikatorwert) %>%
  # Keep overall households with children for 2024
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt",
    Jahr == 2024
  ) %>%
  # Convert decimal commas to numeric format
  mutate(Indikatorwert = as.numeric(str_replace(Indikatorwert, ",", "."))) %>%
  # Rename variable for clarity
  rename(haushalt_kinder = Indikatorwert) %>%
  # Exclude total city-level observation
  filter(Raumbezug != "Stadt München")


# --- 3. Merge both datasets by year and district ---
df <- inner_join(data_ar_clean, data_be_clean, by = c("Jahr", "Raumbezug"))


# --- 4. Visualization ---
ggplot(df, aes(x = haushalt_kinder, y = arbeitslos_weiblich, label = Raumbezug)) +
  
  # Plot points (semi-transparent blue)
  geom_point(size = 3, color = "#2C7BB6", alpha = 0.8) +
  
  # Add regression line with confidence interval
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#D55E00",
    fill = "grey80",
    linetype = "dashed",
    linewidth = 0.8,
    alpha = 0.3
  ) +
  
  # Add district labels with leader lines
  geom_text_repel(
    size = 2.8,
    color = "grey10",
    segment.color = "grey70",
    segment.size = 0.3,
    segment.alpha = 0.6,
    box.padding = 0.35,
    point.padding = 0.2,
    max.overlaps = 50,
    force = 4,
    min.segment.length = 0
  ) +
  
  # Define axis scales and tick marks
  scale_x_continuous(limits = c(5, 30), breaks = seq(5, 30, 5)) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, 1)) +
  
  # Add titles, labels, and caption
  labs(
    title = "Relationship Between Households with Children and Female Unemployment Rate (2024)",
    subtitle = "Level of analysis: Munich city districts (excluding total city)",
    x = "Share of households with children (%)",
    y = "Female unemployment rate (%)",
    caption = "Data source: City of Munich · Own calculation"
  ) +
  
  # Apply clean minimal theme
  theme_minimal(base_size = 11, base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, lineheight = 1.1),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey30"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "grey25"),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 8, color = "grey40", hjust = 1),
    plot.margin = margin(10, 20, 10, 10)
  ) +
  coord_cartesian(expand = TRUE)

