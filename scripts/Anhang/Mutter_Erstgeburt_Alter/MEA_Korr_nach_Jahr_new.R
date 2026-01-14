# ============================================================
# Script: plot_mea_year_correlation.R
# Purpose: Analyze correlation between "Mean Age of Mothers (MEA)"
#          and "Female Employment" over years.
# Returns: A list containing 2 ggplot objects:
#          1. $point_plot (Points colored by year + global line)
#          2. $line_plot  (Regression lines per year)
# ============================================================

library(tidyverse)
library(ggpubr)
library(scales)

# ------------------------------------------------------------
# Main Function
# ------------------------------------------------------------
generate_mea_year_plots <- function(ar, be) {
  
  # 1. Process MEA (Durchschnittsalter Mütter erstgebärend)
  df_mea <- be %>%
    filter(
      Indikator == "Durchschnittsalter Mütter erstgebärend",
      Ausprägung == "insgesamt"
    ) %>%
    mutate(
      mean_age = `Basiswert 1` / `Basiswert 2` 
    ) %>%
    select(Jahr, Raumbezug, mean_age)
  
  # 2. Process Employment (Frauenbeschäftigung)
  df_emp <- ar %>%
    filter(
      Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
      Ausprägung == "weiblich"
    ) %>%
    mutate(
      anteil = 100 * `Basiswert 1` / `Basiswert 2`
    ) %>%
    select(Jahr, Raumbezug, anteil)
  
  # 3. Join and Clean
  korrelations_daten <- inner_join(
    df_mea, 
    df_emp, 
    by = c("Raumbezug", "Jahr")
  )
  
  korrelations_daten_clean <- korrelations_daten %>%
    filter(Raumbezug != "Stadt München")
  
  # ----------------------------------------------------------
  # Plot 1: Färbung nach Jahren (Points colored by Year)
  # ----------------------------------------------------------
  mea_korr_jahr <- ggplot(korrelations_daten_clean, aes(x = mean_age, y = anteil)) +
    geom_point(aes(color = factor(Jahr)), 
               size = 1.3,      
               alpha = 0.7) +    
    geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1) +
    stat_cor(
      label.x.npc = "left",    
      label.y.npc = "top",     
      color = "black",
      size = 5
    ) + 
    coord_cartesian(xlim = c(29, 34), ylim = c(48, 68)) + 
    labs(
      title = "Korrelationskoeffizient nach Jahren (Punkte)",
      x = "Durchschnittsalter Mütter erstgebärend",
      y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)",
      color = "Jahr" 
    ) +
    guides(color = guide_legend(ncol = 1)) + 
    theme_minimal()
  
  # ----------------------------------------------------------
  # Plot 2: Korrelation nach Jahren (Regression Lines per Year)
  # ----------------------------------------------------------
  # Prepare data with labels
  plot_data_final_year <- korrelations_daten_clean %>%
    group_by(Jahr) %>% 
    mutate(
      r = cor(mean_age, anteil, use = "complete.obs"),
      r_label = sprintf("(R=%.2f)", r),
      Jahr_Label = paste(Jahr, r_label, sep = " ")
    ) %>%
    ungroup()
  
  # Create custom palette based on unique labels
  all_labels_year <- sort(unique(plot_data_final_year$Jahr_Label))
  all_colors_year <- scales::hue_pal()(length(all_labels_year))
  jahr_palette <- all_colors_year
  names(jahr_palette) <- all_labels_year
  
  mea_korr_jahr_point_line <- ggplot(plot_data_final_year, aes(x = mean_age, y = anteil)) +
    geom_smooth(aes(color = Jahr_Label, group = Jahr_Label),
                method = "lm", se = FALSE, linewidth = 1.1, alpha = 0.8) +
    geom_point(aes(color = Jahr_Label), size = 1.1, alpha = 0.5) +
    geom_smooth(aes(group = 1), method = "lm", color = "black", linewidth = 1.1, se = FALSE) +
    scale_color_manual(values = jahr_palette) +
    labs(
      title = "Korrelationskoeffizient nach Jahren (Linien)",
      x = "Durchschnittsalter Mütter erstgebärend",
      y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)",
      color = "Jahr"
    ) +
    coord_cartesian(xlim = c(29, 34), ylim = c(48, 68)) +
    theme_minimal() +
    theme(
      legend.text = element_text(size = 7),
      legend.key.height = unit(0.4, "cm")
    ) +
    guides(color = guide_legend(ncol = 1))
  
  # ----------------------------------------------------------
  # Return List
  # ----------------------------------------------------------
  return(list(
    point_plot = mea_korr_jahr,
    line_plot  = mea_korr_jahr_point_line
  ))
}