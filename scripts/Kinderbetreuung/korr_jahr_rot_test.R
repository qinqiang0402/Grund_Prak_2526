# ============================================================
# Script: plot_ki_year_trend.R
# Purpose: Generate the yearly correlation lines (Red Gradient)
#          for Kinderbetreuung vs. Frauenbeschäftigung
# Returns: A ggplot object
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
        str_extract(as.character(.data[[var]]), "^[0-9]+"),
        width = 2,
        pad   = "0"
      )
    )
}

# ------------------------------------------------------------
# Main Function
# ------------------------------------------------------------
generate_ki_year_trend <- function(ar, be, ki) {
  
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
    add_sb(var = "Raumbezug", new = "sb") %>%
    filter(!is.na(sb))
  
  # 4. Merge Data (2007-2024)
  df_merge <- df_betreut_bezirk %>%
    filter(Jahr >= 2007, Jahr <= 2024) %>%
    select(Jahr, sb, Raumbezug, anteil_betreut) %>%
    left_join(
      sozial_anteil_weiblich %>%
        add_sb(var = "Raumbezug", new = "sb") %>%
        filter(Jahr >= 2007, Jahr <= 2024) %>%
        select(Jahr, sb, sozial_weiblich),
      by = c("Jahr", "sb")
    ) %>%
    arrange(sb, Jahr)
  
  # 5. Build Analysis Dataset
  korrelations_daten_clean <- df_merge %>%
    mutate(
      hmk    = anteil_betreut,        # x: childcare
      anteil = 100 * sozial_weiblich  # y: employment
    ) %>%
    filter(
      Raumbezug != "Stadt München",
      Jahr >= 2007,
      Jahr <= 2024
    ) %>%
    select(Jahr, Raumbezug, hmk, anteil)
  
  # 6. Prepare Plotting Data (Numeric Year for Gradient)
  plot_data_final_year <- korrelations_daten_clean %>%
    mutate(
      Jahr       = as.integer(Jahr),
      Jahr_Label = factor(Jahr),
      Jahr_num   = as.numeric(Jahr)
    ) %>%
    arrange(Jahr)
  
  # 7. Define Axis Breaks & Theme
  x_breaks <- pretty(korrelations_daten_clean$hmk,    n = 5)
  y_breaks <- pretty(korrelations_daten_clean$anteil, n = 5)
  x_limits <- range(x_breaks)
  y_limits <- range(y_breaks)
  
  base_theme_corr <- theme_bw(base_size = 13) +
    theme(
      axis.title.x = element_text(size = 18, face = "bold"),
      axis.title.y = element_text(size = 18, face = "bold"),
      axis.text.x  = element_text(size = 14),
      axis.text.y  = element_text(size = 14)
    )
  
  # 8. Generate Plot
  p <- ggplot(plot_data_final_year, aes(x = hmk, y = anteil)) +
    # Scatter points
    geom_point(color = "grey90", size = 1.2, alpha = 0.5) +
    
    # Yearly regression lines (colored)
    geom_smooth(
      aes(color = Jahr_num, group = Jahr_Label),
      method    = "lm",
      se        = FALSE,
      linewidth = 1.1,
      alpha     = 0.85
    ) +
    
    # Global regression line (black)
    geom_smooth(
      aes(group = 1),
      method    = "lm",
      se        = FALSE,
      linewidth = 1.2,
      color     = "black"
    ) +
    
    # Scales
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
    
    # Color Gradient (Red)
    scale_color_gradient(
      name   = "Jahr",
      limits = c(min(plot_data_final_year$Jahr), max(plot_data_final_year$Jahr)),
      breaks = c(2007, 2011, 2015, 2019, 2024),
      labels = c("2007", "2011", "2015", "2019", "2024"),
      low    = "#fee5e5",
      high   = "#990000",
      guide  = guide_colorbar(
        title.position = "top",
        barheight      = unit(6, "cm"),
        barwidth       = unit(0.4, "cm")
      )
    ) +
    
    # Theme application
    base_theme_corr +
    theme(
      legend.title = element_text(size = 16, face = "bold"),
      legend.text  = element_text(size = 14)
    )
  
  return(p)
}