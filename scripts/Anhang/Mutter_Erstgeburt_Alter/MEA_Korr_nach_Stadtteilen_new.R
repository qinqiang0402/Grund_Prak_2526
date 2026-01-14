# ============================================================
# Script: plot_mea_district_correlation.R
# Purpose: Analyze correlation between "Mean Age of Mothers (MEA)"
#          and "Female Employment" (Overall & by District).
# Returns: A list containing 3 ggplot objects:
#          1. $gesamt      (Overall scatter with one regression line)
#          2. $point_plot  (Scatter colored by district)
#          3. $line_plot   (Regression lines per district)
# ============================================================

library(tidyverse)
library(ggpubr)
library(scales)

# ------------------------------------------------------------
# Main Function
# ------------------------------------------------------------
generate_mea_district_plots <- function(ar, be) {
  
  # 1. Process MEA (Average age of mothers at first birth)
  df_mea <- be %>%
    filter(
      Indikator == "Durchschnittsalter Mütter erstgebärend",
      Ausprägung == "insgesamt"
    ) %>%
    mutate(
      mean_age = `Basiswert 1` / `Basiswert 2` 
    ) %>%
    select(Jahr, Raumbezug, mean_age)
  
  # 2. Process Employment (Female share)
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
  # Plot 1: Overall Correlation (Gesamt)
  # ----------------------------------------------------------
  p_gesamt <- ggplot(korrelations_daten_clean, aes(x = mean_age, y = anteil)) +
    geom_point(size = 1.3, color = "grey", alpha = 0.7) +
    geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1) +
    stat_cor(label.x.npc = "left", label.y.npc = "top") + 
    labs(
      title = "Korrelation: Erstgeburtsalter vs. Frauenbeschäftigung (Gesamt)",
      x = "Durchschnittsalter Mütter erstgebärend",
      y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)"
    ) +
    theme_minimal()
  
  # ----------------------------------------------------------
  # Plot 2: Colored by District (Points only)
  # ----------------------------------------------------------
  p_point <- ggplot(korrelations_daten_clean, aes(x = mean_age, y = anteil)) +
    geom_point(aes(color = Raumbezug), 
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
      title = "Korrelation nach Stadtteilen (Punkte)",
      x = "Durchschnittsalter Mütter erstgebärend",
      y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)",
      color = "Stadtteile" 
    ) +
    guides(color = guide_legend(ncol = 1)) + 
    theme_minimal()
  
  # ----------------------------------------------------------
  # Plot 3: Correlation by District (Regression Lines)
  # ----------------------------------------------------------
  # Calculate R per district for labeling
  plot_data_final <- korrelations_daten_clean %>%
    group_by(Raumbezug) %>%
    mutate(
      r = cor(mean_age, anteil, use = "complete.obs"),
      r_label = sprintf("(R=%.2f)", r),
      Raumbezug_Label = paste(Raumbezug, r_label, sep = " ")
    ) %>%
    ungroup()
  
  # Create custom palette based on labels
  all_labels <- sort(unique(plot_data_final$Raumbezug_Label))
  all_colors <- scales::hue_pal()(length(all_labels))
  stadtteil_palette <- all_colors
  names(stadtteil_palette) <- all_labels
  
  p_line <- ggplot(plot_data_final, aes(x = mean_age, y = anteil)) +
    geom_smooth(aes(color = Raumbezug_Label, group = Raumbezug_Label),
                method = "lm", se = FALSE, linewidth = 1.1, alpha = 0.8) +
    geom_point(aes(color = Raumbezug_Label), size = 1.1, alpha = 0.5) +
    geom_smooth(aes(group = 1), method = "lm", color = "black", linewidth = 1.1, se = FALSE) +
    scale_color_manual(values = stadtteil_palette) +
    labs(
      title = "Korrelation nach Stadtteilen (Linien)",
      x = "Durchschnittsalter Mütter erstgebärend",
      y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)",
      color = "Stadtteile" 
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
    gesamt     = p_gesamt,
    point_plot = p_point,
    line_plot  = p_line
  ))
}