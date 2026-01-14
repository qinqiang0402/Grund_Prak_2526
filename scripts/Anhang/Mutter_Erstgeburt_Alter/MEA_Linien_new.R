# ============================================================
# Script: plot_mea_trend.R
# Purpose: Generate a line plot showing the trend of average age
#          of mothers at first birth (MEA) over time.
#          Highlights: Munich Average, Max District, Min District.
# Returns: A ggplot object
# ============================================================

library(tidyverse)
library(readxl)

# ------------------------------------------------------------
# Main Function
# ------------------------------------------------------------
generate_mea_trend_plot <- function(be) {
  
  # 1. Process MEA Data
  all_districts_and_city <- be %>%
    filter(
      Indikator == "Durchschnittsalter Mütter erstgebärend",
      Ausprägung == "insgesamt"
    ) %>%
    mutate(
      mean_age = `Basiswert 1` / `Basiswert 2`
    )
  
  # 2. Identify Max/Min Districts (Average over all years)
  district_averages <- all_districts_and_city %>%
    filter(Raumbezug != "Stadt München") %>%
    group_by(Raumbezug) %>%
    summarise(
      avg_age_overall = mean(mean_age, na.rm = TRUE)
    )
  
  max_district <- district_averages %>%
    slice_max(order_by = avg_age_overall, n = 1) %>%
    pull(Raumbezug)
  
  min_district <- district_averages %>%
    slice_min(order_by = avg_age_overall, n = 1) %>%
    pull(Raumbezug)
  
  # 3. Prepare Data for Plotting (Highlighting Logic)
  data_for_plot <- all_districts_and_city %>%
    mutate(
      highlight_status = case_when(
        Raumbezug == "Stadt München" ~ "München avg", 
        Raumbezug == max_district ~ "Max (Durchschnitt)",
        Raumbezug == min_district ~ "Min (Durchschnitt)",
        TRUE ~ "Normal" 
      )
    )
  
  # 4. Generate Plot
  p <- ggplot(data = data_for_plot, mapping = aes(x = Jahr, y = mean_age)) +
    # Background lines (Normal districts)
    geom_line(data = . %>% filter(highlight_status == "Normal"), 
              aes(group = Raumbezug), 
              color = "grey80", 
              linewidth = 0.5) +
    # Highlighted lines (Max, Min, Munich)
    geom_line(data = . %>% filter(highlight_status != "Normal"), 
              aes(color = highlight_status, group = Raumbezug), 
              linewidth = 1.2) +
    scale_color_manual(
      name = "Legende:", 
      values = c(
        "München avg"        = "black",               
        "Max (Durchschnitt)" = "#e41a1c",        
        "Min (Durchschnitt)" = "#377eb8"        
      ),
      labels = c(
        "München avg"        = "Gesamtdurchschnitt Stadt München",
        "Max (Durchschnitt)" = paste("Höchster Durchschnitt (Stadtteil):", max_district),
        "Min (Durchschnitt)" = paste("Niedrigster Durchschnitt (Stadtteil):", min_district)
      )
    ) +
    scale_x_continuous(breaks = seq(min(data_for_plot$Jahr), max(data_for_plot$Jahr), by = 1)) +
    theme_minimal() +
    theme(
      legend.position = "bottom", 
      legend.justification = "left"
    ) +
    labs(
      title = "Durchschnittsalter von Müttern bei Erstgeburt in München",
      subtitle = "Vergleich der Stadtteile mit dem gesamtstädtischen Durchschnitt",
      x = "Jahr",
      y = "Durchschnittliches Alter (Jahre)"
    )
  
  return(p)
}