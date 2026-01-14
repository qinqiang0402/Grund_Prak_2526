# ============================================================
# Script: plot_childcare_map_2024.R
# Purpose: Generate a static map of childcare rates (0-2 years)
#          for the year 2024 using ggplot2.
# Returns: A ggplot object
# ============================================================

library(tidyverse)
library(sf)
library(stringr)

# ------------------------------------------------------------
# Main Function
# ------------------------------------------------------------
generate_childcare_map_2024 <- function(be, ki, munich_map_sf) {
  
  # 1. Prepare Population Data (Total children 0-2)
  df_total <- be %>%
    filter(
      Jahr == 2024,
      Indikator == "Altersgruppen",
      Ausprägung == "bis 2 Jahre",
      Raumbezug != "Stadt München"
    ) %>%
    mutate(
      bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
    ) %>%
    select(bezirksnummer, kinder_total = `Basiswert 1`)
  
  # 2. Prepare Childcare Data (Children in care 0-2)
  df_betreut <- ki %>%
    filter(
      Jahr == 2024,
      Indikator == "Altersgruppen",
      Ausprägung == "bis 2 Jahre",
      Raumbezug != "Stadt München"
    ) %>%
    mutate(
      bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
    ) %>%
    select(bezirksnummer, kinder_betreut = `Basiswert 1`)
  
  # 3. Calculate Rate
  betreuung_2024 <- df_total %>%
    left_join(df_betreut, by = "bezirksnummer") %>%
    mutate(
      anteil_betreut = 100 * kinder_betreut / kinder_total
    )
  
  # 4. Join with Geometry
  # Ensure map is in WGS84 (consistent with other plots)
  final_sf <- munich_map_sf %>%
    st_transform(4326) %>%
    # Ensure sb_nummer matches format of bezirksnummer
    mutate(sb_string = sprintf("%02d", as.numeric(sb_nummer))) %>%
    left_join(betreuung_2024, by = c("sb_string" = "bezirksnummer")) %>%
    filter(!is.na(anteil_betreut))
  
  # 5. Generate Plot
  p <- ggplot(final_sf) +
    geom_sf(
      aes(fill = anteil_betreut),
      color = "white",
      size  = 0.4
    ) +
    scale_fill_gradient(
      low    = "#fff5eb",
      high   = "#cd6155",
      limits = c(0, 100),
      name   = "Kinderbetreuung (%)"
    ) +
    guides(
      fill = guide_colorbar(
        title.position = "top",
        barwidth       = 25,
        barheight      = 2
      )
    ) +
    theme_void(base_size = 14) +
    theme(
      legend.position      = "bottom",
      legend.title.align   = 0.5,
      legend.title         = element_text(size = 28, face = "bold"),
      legend.text          = element_text(size = 26),
      plot.title           = element_text(face = "bold", size = 26, hjust = 0.5)
    )
  
  return(p)
}