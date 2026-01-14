# ============================================================
# Script: plot_ki_correlation.R
# Purpose: Generate correlation plots for Kinderbetreuung vs. Frauenbeschäftigung
# Returns: A list of ggplot objects (gesamt, stadtteile)
# ============================================================

library(tidyverse)
library(stringr)
library(ggpubr)
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
# Main Function: Generate correlation plots
# ------------------------------------------------------------
# Input arguments:
# - ar: Dataframe from ARBEITSMARKT
# - be: Dataframe from BEVÖLKERUNG
# - ki: Dataframe from KINDERBETREUUNG
generate_ki_corr_plots <- function(ar, be, ki) {
  
  # 1. Process Female Employment (0~1)
  sozial_anteil_weiblich <- ar %>%
    filter(
      Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
      Ausprägung == "weiblich"
    ) %>%
    mutate(sozial_weiblich = `Basiswert 1` / `Basiswert 2`) %>%
    select(Jahr, Raumbezug, sozial_weiblich)
  
  # 2. Process Childcare (0–2 years)
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
      kinder_unbetreut = kinder_total - kinder_betreut,
      anteil_betreut   = 100 * kinder_betreut / kinder_total,
      anteil_unbetreut = 100 - anteil_betreut
    )
  
  # 3. Clean District Codes
  df_betreut_bezirk <- df_betreut %>%
    add_sb() %>%
    filter(!is.na(sb))
  
  # 4. Merge Data
  df_merge <- df_betreut_bezirk %>%
    filter(Jahr >= 2007, Jahr <= 2024) %>%
    select(Jahr, sb, Raumbezug, anteil_betreut) %>%
    left_join(
      sozial_anteil_weiblich %>%
        add_sb() %>%
        filter(Jahr >= 2007, Jahr <= 2024) %>%
        select(Jahr, sb, sozial_weiblich),
      by = c("Jahr", "sb")
    ) %>%
    arrange(sb, Jahr)
  
  # 5. Build Correlation Dataset
  korrelations_daten_clean <- df_merge %>%
    mutate(
      hmk    = anteil_betreut,        # x-axis: childcare coverage (%)
      anteil = 100 * sozial_weiblich  # y-axis: female employment (%)
    ) %>%
    filter(
      Raumbezug != "Stadt München",
      Jahr >= 2007,
      Jahr <= 2024
    ) %>%
    select(Jahr, Raumbezug, hmk, anteil)
  
  # 6. Define Shared Theme and Axes
  x_breaks <- pretty(korrelations_daten_clean$hmk,    n = 5)
  y_breaks <- pretty(korrelations_daten_clean$anteil, n = 5)
  
  x_limits <- range(x_breaks)
  y_limits <- range(y_breaks)
  
  base_theme_corr <- theme_bw(base_size = 13) +
    theme(
      axis.title.x = element_text(size = 20, face = "bold"),
      axis.title.y = element_text(size = 20, face = "bold"),
      axis.text.x  = element_text(size = 16),
      axis.text.y  = element_text(size = 16)
    )
  
  # 7. Generate Plot 1: Overall Correlation (Gesamt)
  plot_gesamt <- ggplot(korrelations_daten_clean, aes(x = hmk, y = anteil)) +
    geom_point(size = 1.3, color = "grey85", alpha = 0.7) +
    geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1.2) +
    scale_x_continuous(
      name   = "Kinderbetreuung (%)",
      limits = x_limits,
      breaks = x_breaks
    ) +
    scale_y_continuous(
      name   = "Frauenbeschäftigung (%)",
      limits = y_limits,
      breaks = y_breaks
    ) +
    base_theme_corr
  
  # 8. Generate Plot 2: By-District Correlation (Stadtteile)
  korr_stadtteile_data <- korrelations_daten_clean %>%
    group_by(Raumbezug) %>%
    filter(n() >= 2) %>%
    ungroup()
  
  plot_stadtteile <- ggplot(korr_stadtteile_data, aes(x = hmk, y = anteil)) +
    geom_point(color = "grey90", size = 1.3, alpha = 0.7) +
    geom_smooth(
      aes(group = Raumbezug),
      method    = "lm",
      se        = FALSE,
      color     = "grey65",
      linewidth = 0.9,
      alpha     = 0.9
    ) +
    geom_smooth(
      aes(group = 1),
      method    = "lm",
      se        = FALSE,
      color     = "black",
      linewidth = 1.2
    ) +
    scale_x_continuous(
      name   = "Kinderbetreuung (%)",
      limits = x_limits,
      breaks = x_breaks
    ) +
    scale_y_continuous(
      name   = "Frauenbeschäftigung (%)",
      limits = y_limits,
      breaks = y_breaks
    ) +
    base_theme_corr
  
  # Return a list containing both plots
  return(list(
    gesamt     = plot_gesamt,
    stadtteile = plot_stadtteile
  ))
}