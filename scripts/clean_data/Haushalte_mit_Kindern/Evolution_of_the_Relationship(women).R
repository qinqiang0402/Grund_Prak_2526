# =========================================================
# Visualization: Evolution of the Relationship between
# Households with Children and Female Unemployment Rate (2012–2024)
# Data source: City of Munich (district level)
# =========================================================

library(readxl)
library(tidyverse)
library(ggrepel)
library(ggpmisc)

# --- 1. Load and clean unemployment data ---
data_ar <- read_excel("data/raw/export_ar.xlsx", sheet = 2)

data_ar_clean <- data_ar %>%
  select(Indikator, Ausprägung, Jahr, Raumbezug, Indikatorwert) %>%
  # Keep only female unemployment rate for all years
  filter(
    Indikator == "Arbeitslose - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  # Convert decimal commas to numeric format
  mutate(Indikatorwert = as.numeric(str_replace(Indikatorwert, ",", "."))) %>%
  # Rename for clarity
  rename(arbeitslos_weiblich = Indikatorwert) %>%
  # Exclude total city-level observation
  filter(Raumbezug != "Stadt München")


# --- 2. Load and clean household-with-children data ---
data_be <- read_excel("data/raw/export_be.xlsx", sheet = 2)

data_be_clean <- data_be %>%
  select(Indikator, Ausprägung, Jahr, Raumbezug, Indikatorwert) %>%
  # Keep overall households with children for all years
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  # Convert decimal commas to numeric format
  mutate(Indikatorwert = as.numeric(str_replace(Indikatorwert, ",", "."))) %>%
  # Rename for clarity
  rename(haushalt_kinder = Indikatorwert) %>%
  # Exclude total city-level observation
  filter(Raumbezug != "Stadt München")


# --- 3. Merge datasets by year and district ---
df <- inner_join(data_ar_clean, data_be_clean, by = c("Jahr", "Raumbezug"))


# --- 4. Compute correlation coefficients per year ---
cor_data <- df %>%
  group_by(Jahr) %>%
  summarize(
    r = cor(haushalt_kinder, arbeitslos_weiblich, use = "complete.obs")
  )


# --- 5. Visualization ---
ggplot(df, aes(x = haushalt_kinder, y = arbeitslos_weiblich, group = Jahr)) +
  
  # Scatter points
  geom_point(size = 2, alpha = 0.25, color = "grey60") +
  
  # Regression lines for each year
  geom_smooth(
    aes(color = Jahr),
    method = "lm",
    se = FALSE,
    linewidth = 1.1,
    alpha = 0.9
  ) +
  
  # Add correlation coefficient labels
  geom_text(
    data = cor_data,
    aes(
      x = 28,                 # X position of label (adjust as needed)
      y = 7.5 - (Jahr - 2012) * 0.4,  # Slightly offset labels vertically by year
      label = paste0("r = ", round(r, 2))
    ),
    color = "black",
    size = 3.2,
    hjust = 1,
    fontface = "bold"
  ) +
  
  # Apply Viridis color palette
  scale_color_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Year"
  ) +
  
  # Define axes
  scale_x_continuous(limits = c(5, 30), breaks = seq(5, 30, 5)) +
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 1)) +
  
  # Titles and labels
  labs(
    title = "Evolution of the Relationship (2012–2024)",
    subtitle = "Link between households with children and female unemployment rate",
    x = "Share of households with children (%)",
    y = "Female unemployment rate (%)",
    caption = "Data source: City of Munich · Own calculation"
  ) +
  
  # Theme and legend
  theme_minimal(base_size = 11, base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey30"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "grey25"),
    legend.position = "bottom",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8)
  ) +
  
  # Custom legend layout
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

