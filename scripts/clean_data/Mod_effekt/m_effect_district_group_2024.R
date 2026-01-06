# Set locale and encoding
if (.Platform$OS.type == "windows") {
  Sys.setlocale(category = "LC_ALL", locale = "English_United States.utf8")
} else {
  Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")
}
options(encoding = "UTF-8")

# Load libraries
library(dplyr)
library(sf)
library(tidyverse)
library(readxl)
library(ggplot2)
library(stringr) 

# Load and prepare geometry
munich_map <- st_read("results/geo/bezirk_map.gpkg", quiet = TRUE) |>
  st_transform(4326) |>
  mutate(
    name = enc2utf8(name),
    name = iconv(name, from = "", to = "UTF-8", sub = ""),
    name = gsub("[[:cntrl:]]", "", name)
  )

# Load data tables
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = 2)
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")
ki_sheet <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

# Households with children
hh_data_long <- be_sheet %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(
    Raumbezug = enc2utf8(Raumbezug),
    Raumbezug = iconv(Raumbezug, from = "", to = "UTF-8", sub = ""),
    Raumbezug = gsub("[[:cntrl:]]", "", Raumbezug),
    bezirksnummer = substr(Raumbezug, 1, 2),
    bezirksnummer = str_replace_all(bezirksnummer, "\\D", ""),
    bezirksnummer = as.numeric(bezirksnummer),
    bezirksnummer = sprintf("%02d", bezirksnummer),
    anteil_kinder = 100 * `Basiswert 1` / `Basiswert 2`
  ) %>%
  filter(!is.na(bezirksnummer)) %>%
  select(Jahr, bezirksnummer, anteil_kinder)

# Female employment
ar_data_long <- ar_sheet %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(
    Raumbezug = enc2utf8(Raumbezug),
    Raumbezug = iconv(Raumbezug, from = "", to = "UTF-8", sub = ""),
    Raumbezug = gsub("[[:cntrl:]]", "", Raumbezug),
    bezirksnummer = substr(Raumbezug, 1, 2),
    bezirksnummer = str_replace_all(bezirksnummer, "\\D", ""),
    bezirksnummer = as.numeric(bezirksnummer),
    bezirksnummer = sprintf("%02d", bezirksnummer),
    anteil = 100 * `Basiswert 1` / `Basiswert 2`
  ) %>%
  filter(!is.na(bezirksnummer)) %>%
  select(Jahr, bezirksnummer, anteil)

# Combine variables
combined_data_all <- hh_data_long %>%
  full_join(ar_data_long, by = c("Jahr", "bezirksnummer")) %>%
  mutate(Jahr = as.numeric(Jahr))

# Join with geometry
final_sf_data_long <- munich_map %>%
  left_join(
    combined_data_all,
    by = c("sb_nummer" = "bezirksnummer"),
    relationship = "many-to-many"
  ) %>%
  filter(!is.na(Jahr))

# Filter 2024 data
data_2024 <- final_sf_data_long %>%
  filter(Jahr == 2024) %>%
  select(
    sb_nummer,
    name,
    anteil_kinder,
    anteil
  ) %>%
  sf::st_as_sf()

# City means (district average)
mean_HaKi_city <- mean(data_2024$anteil_kinder, na.rm = TRUE)
mean_FE_city <- mean(data_2024$anteil, na.rm = TRUE)

# Classification
data_2024 <- data_2024 %>%
  mutate(
    gruppe = dplyr::case_when(
      anteil_kinder > mean_HaKi_city & anteil < mean_FE_city ~
        "hohe Haushalte mit Kindern + niedrige Beschäftigung",
      TRUE ~ "Andere"
    )
  )

# Prepare labels for plotting
data_2024_plot <- data_2024 %>%
  mutate(
    gruppe_lab = if_else(
      gruppe == "hohe Haushalte mit Kindern + niedrige Beschäftigung",
      "Haushalte mit Kindern hoch\nund Frauenbeschäftigung niedrig",
      "Andere"
    )
  )

# Map plot
m_effekt_04_plot <- ggplot(data_2024_plot) +
  geom_sf(
    aes(fill = gruppe_lab),
    color = "white",
    size  = 0.4,
    alpha = 0.8
  ) +
  scale_fill_manual(
    values = c(
      "Haushalte mit Kindern hoch\nund Frauenbeschäftigung niedrig" = "#e75480",
      "Andere"                                                      = "#d9d9d9"
    ),
    breaks = c(
      "Haushalte mit Kindern hoch\nund Frauenbeschäftigung niedrig",
      "Andere"
    )
  ) +
  labs(fill = NULL) +
  theme_void(base_size = 14) +
  theme(
    legend.position   = "bottom",
    legend.text       = element_text(size = 27),
    legend.key.size   = unit(1, "cm"),
    legend.background = element_blank(),
    plot.caption      = element_blank()
  )

m_effekt_04_plot

# Save plot
saveRDS(m_effekt_04_plot, "results/figures/m_effekt/m_effekt_04_plot.rds")
