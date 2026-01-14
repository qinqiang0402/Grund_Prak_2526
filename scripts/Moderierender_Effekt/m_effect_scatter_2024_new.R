# ============================================================
# Script: plot_target_group_scatter.R
# Purpose: Generate a scatter plot identifying districts with:
#          High "Haushalte mit Kindern" AND Low "Frauenbeschäftigung"
#          (Compared to the city average of 2024)
# Returns: A ggplot object
# ============================================================

library(tidyverse)
library(stringr)

# ------------------------------------------------------------
# Main Function
# ------------------------------------------------------------
generate_target_group_scatter <- function(ar, be) {
  
  # 1. Helper to clean and extract Bezirk Number
  process_bezirk <- function(df, col_name = "Raumbezug") {
    df %>%
      mutate(
        bezirksnummer = sprintf("%02d", as.numeric(str_extract(.data[[col_name]], "^\\d+")))
      ) %>%
      filter(!is.na(bezirksnummer))
  }
  
  # 2. Process Households with Children
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
  
  # 3. Process Female Employment
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
  
  # 5. Filter 2024 Data
  data_2024 <- combined_data_all %>%
    filter(Jahr == 2024)
  
  # 6. Calculate City Means (2024)
  # Households (City Level)
  mean_HaKi_city <- be %>%
    filter(
      Jahr == 2024,
      Raumbezug == "Stadt München",
      Indikator == "Haushalte mit Kindern",
      Ausprägung == "insgesamt"
    ) %>%
    mutate(val = 100 * `Basiswert 1` / `Basiswert 2`) %>%
    pull(val)
  
  # Employment (City Level)
  mean_FE_city <- ar %>%
    filter(
      Jahr == 2024,
      Raumbezug == "Stadt München",
      Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
      Ausprägung == "weiblich"
    ) %>%
    mutate(val = 100 * `Basiswert 1` / `Basiswert 2`) %>%
    pull(val)
  
  # 7. Classification Logic
  data_2024 <- data_2024 %>%
    mutate(
      gruppe = dplyr::case_when(
        anteil_kinder > mean_HaKi_city & anteil < mean_FE_city ~
          "hohe Haushalte mit Kindern + niedrige Beschäftigung",
        TRUE ~ "Andere"
      )
    )
  
  # 8. Prepare Labels for Plotting
  target_label <- "Haushalte mit Kindern hoch\nund Frauenbeschäftigung niedrig"
  
  data_2024_plot <- data_2024 %>%
    mutate(
      gruppe_lab = if_else(
        gruppe == "hohe Haushalte mit Kindern + niedrige Beschäftigung",
        target_label,
        "Andere"
      )
    )
  
  # 9. Generate Scatter Plot
  p <- ggplot(data_2024_plot, aes(x = anteil_kinder, y = anteil, color = gruppe_lab)) +
    geom_point(size = 4, alpha = 0.8) +
    
    # Reference Lines (City Averages)
    geom_vline(xintercept = mean_HaKi_city, linetype = "dashed", color = "black") +
    geom_hline(yintercept = mean_FE_city,   linetype = "dashed", color = "black") +
    
    scale_color_manual(
      values = setNames(c("#e75480", "#bdbdbd"), c(target_label, "Andere")),
      breaks = c(target_label, "Andere")
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
      legend.text     = element_text(size = 18),
      legend.title    = element_text(size = 18)
    )
  
  return(p)
}