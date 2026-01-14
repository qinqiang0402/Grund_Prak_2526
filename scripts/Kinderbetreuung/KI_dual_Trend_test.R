# ============================================================
# Script: plot_ki_dual_trend.R
# Purpose: Generate the dual-axis trend plot dynamically
# Returns: A ggplot object
# ============================================================

library(tidyverse)
library(stringr)
library(grid)

# ------------------------------------------------------------
# Helper Function: Extract district code
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
# Main Plotting Function
# ------------------------------------------------------------
# Input arguments:
# - ar: Dataframe from ARBEITSMARKT sheet
# - be: Dataframe from BEVÖLKERUNG sheet
# - ki: Dataframe from KINDERBETREUUNG sheet
generate_ki_dual_trend <- function(ar, be, ki) {
  
  # 1. Process Frauenbeschäftigung (female share, 0~1)
  sozial_anteil_weiblich <- ar %>%
    filter(
      Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
      Ausprägung == "weiblich"
    ) %>%
    mutate(sozial_weiblich = `Basiswert 1` / `Basiswert 2`) %>%
    select(Jahr, Raumbezug, sozial_weiblich)
  
  # 2. Process Kinderbetreuung age 0–2 (percentage)
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
    )
  
  # 3. Clean and Merge
  df_betreut_bezirk <- df_betreut %>%
    add_sb() %>%
    filter(!is.na(sb))
  
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
  
  # 4. Aggregate to Time Series (Mean across districts)
  ts_dual <- df_merge %>%
    group_by(Jahr) %>%
    summarise(
      frauen_mean = mean(sozial_weiblich * 100, na.rm = TRUE), 
      kinder_mean = mean(anteil_betreut,       na.rm = TRUE),
      .groups = "drop"
    )
  
  # 5. Compute Scaling Parameters (Dynamic calculation)
  left_min  <- min(ts_dual$frauen_mean, na.rm = TRUE)
  left_max  <- max(ts_dual$frauen_mean, na.rm = TRUE)
  right_min <- min(ts_dual$kinder_mean, na.rm = TRUE)
  right_max <- max(ts_dual$kinder_mean, na.rm = TRUE)
  
  scale_factor <- (left_max - left_min) / (right_max - right_min)
  
  ts_dual <- ts_dual %>%
    mutate(
      kinder_scaled = (kinder_mean - right_min) * scale_factor + left_min
    )
  
  breaks_left <- pretty(c(left_min, left_max), n = 5)
  breaks_right <- c(20, 25, 30, 35, 40)
  
  # Critical Fix for "Environment Error": 
  # Evaluating the transformation formula inside the function scope creates a fresh environment
  sec_trans <- eval(substitute(
    ~ (. - L) / S + R,
    list(L = left_min, S = scale_factor, R = right_min)
  ))
  
  # 6. Generate Plot
  p <- ggplot(ts_dual, aes(x = Jahr)) +
    # Left axis: Frauenbeschäftigung (blue)
    geom_line(aes(y = frauen_mean, color = "Frauenbeschäftigung"), size = 1.2) +
    geom_point(aes(y = frauen_mean, color = "Frauenbeschäftigung"), size = 2) +
    # Right axis: Kinderbetreuung (orange)
    geom_line(aes(y = kinder_scaled, color = "Kinderbetreuung"), size = 1.2) +
    geom_point(aes(y = kinder_scaled, color = "Kinderbetreuung"), size = 2) +
    scale_y_continuous(
      name   = "Frauenbeschäftigung (%)",
      limits = c(left_min, left_max),
      breaks = breaks_left,
      sec.axis = sec_axis(
        trans  = sec_trans,
        name   = "Kinderbetreuung (%)",
        breaks = breaks_right
      )
    ) +
    scale_color_manual(
      name   = NULL,
      values = c(
        "Frauenbeschäftigung" = "#0072B2", 
        "Kinderbetreuung"     = "#D55E00"
      )
    ) +
    scale_x_continuous(
      breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021, 2024)
    ) +
    labs(x = "", title = "") +
    theme_bw(base_size = 13) +
    theme(
      legend.position      = "top",
      axis.title.y.left  = element_text(size = 20, face = "bold"),
      axis.title.y.right = element_text(size = 20, face = "bold"),
      axis.text.x        = element_text(size = 16),
      axis.text.y.left   = element_text(size = 16),
      axis.text.y.right  = element_text(size = 16),
      legend.text        = element_text(size = 18),
      legend.key.size    = unit(1.2, "cm")
    )
  
  return(p)
}

