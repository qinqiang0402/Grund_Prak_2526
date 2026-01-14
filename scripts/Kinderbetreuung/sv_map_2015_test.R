# ============================================================
# Script: plot_emp_map.R
# Purpose: Generate the map of Frauenbeschäftigung (Female Employment)
#          for a specific year (Default: 2015)
# Returns: A ggplot object (geom_sf)
# ============================================================

library(tidyverse)
library(sf)
library(stringr)
library(grid)

# ------------------------------------------------------------
# Helper: create a 2-digit district code (sb)
# ------------------------------------------------------------
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

# ------------------------------------------------------------
# Main Mapping Function
# ------------------------------------------------------------
# Arguments:
# - ar: ARBEITSMARKT dataframe
# - munich_map_sf: The SF object (geo data) for Munich districts
# - target_year: Integer, the year to filter (default 2015)
generate_emp_map <- function(ar, munich_map_sf, target_year = 2015) {
  
  # 1. Process Female Employment Data
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
    filter(!is.na(sb))
  
  # 2. Filter for specific year
  emp_year <- sozial_anteil_weiblich %>%
    filter(Jahr == target_year)
  
  # 3. Prepare Map Data
  # Ensure the map has the 'sb' column for joining
  map_emp_year <- munich_map_sf %>%
    mutate(
      sb = str_pad(as.character(sb_nummer), 2, pad = "0")
    ) %>%
    left_join(
      emp_year %>% select(sb, sozial_weiblich),
      by = "sb"
    )
  
  # 4. Generate Plot
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
      legend.position      = "bottom",
      legend.title.align   = 0.5,
      legend.title         = element_text(size = 26, face = "bold"),
      legend.text          = element_text(size = 26),
      plot.title           = element_text(face = "bold", size = 26, hjust = 0.5)
    )
  
  return(emp_map_year)
}