# ============================================================
# Script: plot_ki_map.R
# Purpose: Generate the map of Kinderbetreuung (Childcare coverage)
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
# - be: BEVÖLKERUNG dataframe
# - ki: KINDERBETREUUNG dataframe
# - munich_map_sf: The SF object (geo data) for Munich districts
# - target_year: Integer, the year to filter (default 2015)
generate_ki_map <- function(be, ki, munich_map_sf, target_year = 2015) {
  
  # 1. Process Childcare Data
  df_betreut <- be %>%
    filter(
      Indikator == "Altersgruppen",
      Ausprägung == "bis 2 Jahre"
    ) %>%
    select(Jahr, Raumbezug, kinder_total = `Basiswert 1`) %>%
    left_join(
      ki %>%
        filter(
          Indikator == "Altersgruppen",
          Ausprägung == "bis 2 Jahre"
        ) %>%
        select(Jahr, Raumbezug, kinder_betreut = `Basiswert 1`),
      by = c("Jahr", "Raumbezug")
    ) %>%
    mutate(
      anteil_betreut = 100 * kinder_betreut / kinder_total
    ) %>%
    add_sb() %>%
    filter(!is.na(sb))
  
  # 2. Filter for specific year
  ki_year <- df_betreut %>%
    filter(Jahr == target_year)
  
  # 3. Prepare Map Data
  # Ensure the map has the 'sb' column for joining
  # (Assumes munich_map_sf has 'sb_nummer')
  map_year <- munich_map_sf %>%
    mutate(
      sb = str_pad(as.character(sb_nummer), 2, pad = "0")
    ) %>%
    left_join(
      ki_year %>% select(sb, anteil_betreut),
      by = "sb"
    )
  
  # 4. Generate Plot
  ki_map_year <- ggplot(map_year) +
    geom_sf(
      aes(fill = anteil_betreut),
      color = "white",
      size  = 0.2
    ) +
    scale_fill_gradient(
      name   = "Kinderbetreuung (%)",
      low    = "#fff5eb",
      high   = "#7f2704",
      limits = c(10, 55),
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
  
  return(ki_map_year)
}