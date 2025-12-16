# 02_emp_map_2015.R
# Single figure: Bezirke map of Frauenbeschäftigung (female employment share) for one year (here: 2015)

library(tidyverse)
library(readxl)
library(sf)
library(stringr)
library(grid)

# ---------------- 0. Parameter: which year to plot ----------------
target_year <- 2015

# ---------------- 1. Helper: extract a 2-digit Bezirk code from "Raumbezug" ----------------
add_sb <- function(x, var = "Raumbezug", new = "sb") {
  x %>%
    mutate(
      !!new := str_pad(
        str_extract(.data[[var]], "^[0-9]+"),
        width = 2,
        pad   = "0"
      )
    )
}

# ---------------- 2. Read Excel and compute Frauenbeschäftigung (female share) ----------------
ar <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")

# Female share of sozialversicherungspflichtig Beschäftigte (as percent)
sozial_anteil_weiblich <- ar %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(
    sozial_weiblich = 100 * `Basiswert 1` / `Basiswert 2` # convert to percent
  ) %>%
  select(Jahr, Raumbezug, sozial_weiblich) %>%
  add_sb() %>%
  filter(
    !is.na(sb),
    Jahr == target_year
  )

# ---------------- 3. Read Munich district geometry + create Bezirk code ----------------
# IMPORTANT: If your GeoJSON column is not 'sb_nummer', adjust it below.
munich_map <- st_read("results/geo/bezirk_map.json", quiet = TRUE) %>%
  mutate(
    sb = str_pad(as.character(sb_nummer), 2, pad = "0")
  )

# ---------------- 4. Join target-year data to the map ----------------
map_emp_year <- munich_map %>%
  left_join(
    sozial_anteil_weiblich %>% select(sb, sozial_weiblich),
    by = "sb"
  )

# ---------------- 5. Plot: same style as the Kinderbetreuung map, different color scale ----------------
emp_map_year <- ggplot(map_emp_year) +
  geom_sf(
    aes(fill = sozial_weiblich),
    color = "white",
    size  = 0.2
  ) +
  scale_fill_gradient(
    name   = "Frauenbeschäftigung (%)",
    low    = "#f7fbff", # low values
    high   = "#08306b", # high values
    # You can adjust limits/breaks as needed; keep them consistent across years if possible.
    limits = c(50, 62),
    breaks = c(50, 56, 62),
    guide  = guide_colorbar(
      title.position = "top",
      title.hjust    = 0.5,
      barwidth       = unit(8, "cm"),
      barheight      = unit(0.8, "cm")
    )
  ) +
  labs(
    title    = "",
    subtitle = ""
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position    = "bottom",
    legend.title.align = 0.5,
    legend.title       = element_text(size = 26, face = "bold"),
    legend.text        = element_text(size = 26),
    plot.title         = element_text(face = "bold", size = 26, hjust = 0.5)
  )

emp_map_year

# ---------------- 6. Save RDS ----------------
saveRDS(emp_map_year, "results/figures/NEW_Kinderbetreuung/map_sv_2015.rds")
