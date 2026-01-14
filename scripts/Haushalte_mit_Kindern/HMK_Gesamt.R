# ============================================================
# Script: plot_hmk.R
# Purpose: Generate ALL plots for "Haushalte mit Kindern"
# Returns: A list containing 3 ggplot objects:
#          1. $gesamt      (Simple scatter)
#          2. $jahr        (Green gradient by year)
#          3. $stadtteile  (Simpson's Paradox by district)
# ============================================================

library(tidyverse)
library(scales)
library(ggpubr)

# ------------------------------------------------------------
# Main Function
# ------------------------------------------------------------
generate_hmk_plots <- function(ar, be) {
  
  # 1. Process "Haushalte mit Kindern" (hmk)
  df_hmk <- be %>%
    filter(
      Indikator == "Haushalte mit Kindern",
      Ausprägung == "insgesamt"
    ) %>%
    mutate(
      hmk = 100 * `Basiswert 1` / `Basiswert 2`
    ) %>%
    select(Jahr, Raumbezug, hmk)
  
  # 2. Process "Frauenbeschäftigung" (anteil)
  df_emp <- ar %>%
    filter(
      Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
      Ausprägung == "weiblich"
    ) %>%
    mutate(
      anteil = 100 * `Basiswert 1` / `Basiswert 2`
    ) %>%
    select(Jahr, Raumbezug, anteil)
  
  # 3. Join Datasets
  korrelations_daten <- inner_join(
    df_hmk, 
    df_emp, 
    by = c("Raumbezug", "Jahr")
  )
  
  # 4. Clean Data
  korrelations_daten_clean <- korrelations_daten %>%
    filter(Raumbezug != "Stadt München")
  
  # ----------------------------------------------------------
  # Plot 1: Overall Correlation (Gesamt)
  # ----------------------------------------------------------
  p_gesamt <- ggplot(korrelations_daten_clean, aes(x = hmk, y = anteil)) +
    geom_point(size = 1.4, color = "grey70", alpha = 0.7) + 
    geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1.2) +
    labs(
      x = "Haushalte mit Kindern (%)",
      y = "Frauenbeschäftigung (%)" 
    ) +
    coord_cartesian(xlim = c(8, 28), ylim = c(48, 68)) +
    theme_bw() + 
    theme(
      axis.title = element_text(size = 28, face = "bold"),
      axis.text = element_text(size = 20, color = "black"),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    )
  
  # ----------------------------------------------------------
  # Plot 2: Correlation by Year (Grüner Verlauf) - 您的新图
  # ----------------------------------------------------------
  p_jahr <- ggplot(korrelations_daten_clean, aes(x = hmk, y = anteil)) +
    geom_smooth(aes(color = Jahr, group = Jahr),
                method = "lm", se = FALSE, linewidth = 1.2, alpha = 0.7) +
    geom_point(aes(color = Jahr), size = 1.4, alpha = 0.7) +
    geom_smooth(aes(group = 1), method = "lm", color = "black", linewidth = 1.2, se = FALSE) +
    scale_color_gradient(
      low = "#e5f5e0", 
      high = "#238b45",
      breaks = scales::pretty_breaks(n = 5) 
    ) +
    coord_cartesian(xlim = c(8, 28), ylim = c(48, 68)) +
    labs(
      x = "Haushalte mit Kindern (%)",
      y = "Frauenbeschäftigung (%)",
      color = "Jahr"
    ) +
    theme_bw(base_size = 13) +
    theme(
      axis.title.x = element_text(size = 18, face = "bold"),
      axis.title.y = element_text(size = 18, face = "bold"),
      axis.text.x  = element_text(size = 16),
      axis.text.y  = element_text(size = 16),
      legend.text   = element_text(size = 16),
      legend.title  = element_text(size = 16)
    ) +
    guides(color = guide_colorbar(barwidth = 0.8, barheight = 8))
  
  # ----------------------------------------------------------
  # Plot 3: Simpson's Paradox (Nach Stadtteilen)
  # ----------------------------------------------------------
  plot_data_final <- korrelations_daten_clean %>%
    group_by(Raumbezug) %>%
    mutate(
      r = cor(hmk, anteil, use = "complete.obs"),
      r_label = sprintf("(R=%.2f)", r),
      Raumbezug_Label = paste(Raumbezug, r_label, sep = " ")
    ) %>%
    ungroup()
  
  p_stadtteile <- ggplot(plot_data_final, aes(x = hmk, y = anteil)) +
    geom_smooth(aes(group = Raumbezug_Label), 
                method = "lm", se = FALSE, 
                color = "grey60", linewidth = 1, alpha = 0.7) +
    geom_point(color = "grey70", size = 1.4, alpha = 0.7) +
    geom_smooth(aes(group = 1), method = "lm", color = "black", linewidth = 1.2, se = FALSE) +
    labs(
      x = "Haushalte mit Kindern (%)",
      y = "Frauenbeschäftigung (%)"
    ) +
    coord_cartesian(xlim = c(8, 28), ylim = c(48, 68)) +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.title = element_text(size = 28, face = "bold"), 
      axis.text = element_text(size = 20, color = "black"),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    )
  
  # ----------------------------------------------------------
  # Return List (3 Elements)
  # ----------------------------------------------------------
  # 这里定义了列表的 Key，确保名字互不冲突
  return(list(
    gesamt     = p_gesamt,      # 总体
    jahr       = p_jahr,        # 年份 (绿色)
    stadtteile = p_stadtteile   # 城区 (Simpson)
  ))
}