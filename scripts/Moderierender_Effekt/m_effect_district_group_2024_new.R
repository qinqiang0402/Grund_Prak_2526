# ============================================================
# Script: plot_target_group_map.R
# Purpose: Generate a map identifying districts with:
#          High "Haushalte mit Kindern" AND Low "Frauenbeschäftigung"
#          (Compared to the city average of 2024)
# Returns: A ggplot object
# ============================================================

library(tidyverse)
library(sf)
library(stringr)

# ------------------------------------------------------------
# Main Function
# ------------------------------------------------------------
generate_target_group_map <- function(ar, be, munich_map_sf) {
  
  # 1. Helper to clean and extract Bezirk Number
  # (Includes encoding fixes from your original script)
  process_bezirk <- function(df, col_name = "Raumbezug") {
    df %>%
      mutate(
        !!col_name := enc2utf8(.data[[col_name]]),
        !!col_name := iconv(.data[[col_name]], from = "", to = "UTF-8", sub = ""),
        !!col_name := gsub("[[:cntrl:]]", "", .data[[col_name]]),
        bezirksnummer = substr(.data[[col_name]], 1, 2),
        bezirksnummer = str_replace_all(bezirksnummer, "\\D", ""),
        bezirksnummer = as.numeric(bezirksnummer),
        bezirksnummer = sprintf("%02d", bezirksnummer)
      ) %>%
      filter(!is.na(bezirksnummer))
  }
  
  # 2. Process Households with Children (Haushalte mit Kindern)
  hh_data_long <- be %>%
    filter(
      Indikator == "Haushalte mit Kindern",
      Ausprägung == "insgesamt",
      Raumbezug != "Stadt München"
    ) %>%
    process_bezirk() %>%
    mutate(
      anteil_kinder = 100 * `Basiswert 1` / `Basiswert 2`
    ) %>%
    select(Jahr, bezirksnummer, anteil_kinder)
  
  # 3. Process Female Employment (Frauenbeschäftigung)
  ar_data_long <- ar %>%
    filter(
      Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
      Ausprägung == "weiblich",
      Raumbezug != "Stadt München"
    ) %>%
    process_bezirk() %>%
    mutate(
      anteil = 100 * `Basiswert 1` / `Basiswert 2`
    ) %>%
    select(Jahr, bezirksnummer, anteil)
  
  # 4. Combine Variables
  combined_data_all <- hh_data_long %>%
    full_join(ar_data_long, by = c("Jahr", "bezirksnummer")) %>%
    mutate(Jahr = as.numeric(Jahr))
  
  # 5. Join with Geometry
  # Ensure map also has clean text if needed, though usually st_read handles it well.
  # We assume munich_map_sf has 'sb_nummer' or similar.
  
  # Prepare map data (fix encoding on map names just in case)
  munich_map_clean <- munich_map_sf %>%
    mutate(
      name = enc2utf8(name),
      name = iconv(name, from = "", to = "UTF-8", sub = ""),
      name = gsub("[[:cntrl:]]", "", name),
      sb_string = sprintf("%02d", as.numeric(sb_nummer))
    )
  
  final_sf_data_long <- munich_map_clean %>%
    left_join(
      combined_data_all,
      by = c("sb_string" = "bezirksnummer")
    ) %>%
    filter(!is.na(Jahr))
  
  # 6. Filter 2024 Data
  data_2024 <- final_sf_data_long %>%
    filter(Jahr == 2024)
  
  # 7. Calculate Thresholds (Mean of districts)
  mean_HaKi_city <- mean(data_2024$anteil_kinder, na.rm = TRUE)
  mean_FE_city   <- mean(data_2024$anteil,        na.rm = TRUE)
  
  # 8. Classification Logic
  data_2024 <- data_2024 %>%
    mutate(
      gruppe = dplyr::case_when(
        anteil_kinder > mean_HaKi_city & anteil < mean_FE_city ~
          "hohe Haushalte mit Kindern + niedrige Beschäftigung",
        TRUE ~ "Andere"
      )
    )
  
  # 9. Prepare Labels for Plotting
  target_label <- "Haushalte mit Kindern hoch\nund Frauenbeschäftigung niedrig"
  
  data_2024_plot <- data_2024 %>%
    mutate(
      gruppe_lab = if_else(
        gruppe == "hohe Haushalte mit Kindern + niedrige Beschäftigung",
        target_label,
        "Andere"
      )
    )
  
  # 10. Generate Plot
  p <- ggplot(data_2024_plot) +
    geom_sf(
      aes(fill = gruppe_lab),
      color = "white",
      size  = 0.4,
      alpha = 0.8
    ) +
    scale_fill_manual(
      values = setNames(c("#e75480", "#d9d9d9"), c(target_label, "Andere")),
      breaks = c(target_label, "Andere")
    ) +
    labs(fill = NULL) +
    theme_void(base_size = 14) +
    theme(
      legend.position   = "bottom",
      legend.text       = element_text(size = 20), # Adjusted size for readability
      legend.key.size   = unit(1, "cm"),
      legend.background = element_blank(),
      plot.caption      = element_blank()
    )
  
  return(p)
}