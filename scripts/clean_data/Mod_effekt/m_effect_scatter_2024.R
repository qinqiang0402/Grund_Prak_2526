library(leaflet)
library(dplyr)
library(sf)
library(tidyverse)
library(readxl)
library(htmltools)

# Load geometry
munich_map <- st_read("results/geo/bezirk_map.gpkg", quiet = TRUE) |>
  st_transform(4326)

# Load tables
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")
ki_sheet <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

# Variable processing

# Households with children
hh_data_long <- be_sheet %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(
    anteil_kinder = 100 * `Basiswert 1` / `Basiswert 2`,
    bezirksnummer = sprintf("%02d", as.numeric(stringr::str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(Jahr, bezirksnummer, anteil_kinder)

# Female employment
ar_data_long <- ar_sheet %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(
    anteil = 100 * `Basiswert 1` / `Basiswert 2`,
    bezirksnummer = sprintf("%02d", as.numeric(stringr::str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(Jahr, bezirksnummer, anteil)

# Combine variables
combined_data_all <- hh_data_long %>%
  full_join(ar_data_long, by = c("Jahr", "bezirksnummer"))

# Attach geometry
final_sf_data_long <- munich_map %>%
  left_join(
    combined_data_all,
    by = c("sb_nummer" = "bezirksnummer"),
    relationship = "many-to-many"
  ) %>%
  mutate(Jahr = as.numeric(Jahr)) %>%
  filter(!is.na(Jahr))

# Ensure sf object
final_sf_data_long <- sf::st_as_sf(final_sf_data_long)

# Filter 2024 data
data_2024 <- final_sf_data_long %>%
  filter(Jahr == 2024) %>%
  dplyr::select(
    sb_nummer,
    name,
    anteil_kinder,
    anteil
  )

# Keep sf structure
data_2024 <- sf::st_as_sf(data_2024)

# City means for 2024

# Households with children (city level)
mean_HaKi_city <- be_sheet %>%
  filter(
    Jahr == 2024,
    Raumbezug == "Stadt München",
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(anteil_kinder = 100 * `Basiswert 1` / `Basiswert 2`) %>%
  pull(anteil_kinder)

# Female employment (city level)
mean_FE_city <- ar_sheet %>%
  filter(
    Jahr == 2024,
    Raumbezug == "Stadt München",
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(anteil = 100 * `Basiswert 1` / `Basiswert 2`) %>%
  pull(anteil)

cat("City mean HaKi 2024 =", mean_HaKi_city, "\n")
cat("City mean FE   2024 =", mean_FE_city, "\n")

# Classification: high HaKi + low employment
data_2024 <- data_2024 %>%
  mutate(
    gruppe = dplyr::case_when(
      anteil_kinder > mean_HaKi_city & anteil < mean_FE_city ~
        "hohe Haushalte mit Kindern + niedrige Beschäftigung",
      TRUE ~ "Andere"
    ),
    color = dplyr::case_when(
      gruppe == "hohe Haushalte mit Kindern + niedrige Beschäftigung" ~ "#e75480",
      TRUE ~ "#d9d9d9"
    )
  )


# Scatter plot
library(ggplot2)

h_cut <- mean_HaKi_city
f_cut <- mean_FE_city

# Final legend labels
data_2024_plot <- data_2024 %>%
  mutate(
    gruppe_lab = if_else(
      gruppe == "hohe Haushalte mit Kindern + niedrige Beschäftigung",
      "Haushalte mit Kindern hoch\nund Frauenbeschäftigung niedrig",
      "Andere"
    )
  )

m_effekt_2.5 <- ggplot(data_2024_plot,
                       aes(x = anteil_kinder, y = anteil, color = gruppe_lab)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_vline(xintercept = h_cut, linetype = "dashed", color = "black") +
  geom_hline(yintercept = f_cut, linetype = "dashed", color = "black") +
  scale_color_manual(
    values = c(
      "Haushalte mit Kindern hoch\nund Frauenbeschäftigung niedrig" = "#e75480",
      "Andere"                                                      = "#bdbdbd"
    ),
    breaks = c(
      "Haushalte mit Kindern hoch\nund Frauenbeschäftigung niedrig",
      "Andere"
    )
  ) +
  labs(
    x = "Haushalte mit Kindern (%)",
    y = "Frauenbeschäftigung (%)",
    color = ""
  ) +
  theme_bw(base_size = 13) +
  theme(
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 16),
    legend.position = "bottom",
    legend.text   = element_text(size = 18),
    legend.title  = element_text(size = 18)
  )

m_effekt_2.5
saveRDS(m_effekt_2.5, "results/figures/m_effekt/m_effekt_2.5.rds")
