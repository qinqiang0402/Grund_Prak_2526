# ============================================================
# Script: plot_birthrate_correlation.R
# Purpose: Analyze correlation between Birthrate and Female Employment
# Returns: A list containing 2 ggplot objects:
#          1. $stadtteile (Bar chart: Correlation by district)
#          2. $jahr       (Line chart: Correlation over time)
# ============================================================

library(tidyverse)
library(stringr)

# ------------------------------------------------------------
# Main Function
# ------------------------------------------------------------
generate_birthrate_correlation_plots <- function(ar, be) {
  
  # 1. Clean and Prepare Labor Market Data (Employment)
  # Cleaning Indikatorwert: "50,5 %" -> 50.5
  labormarket <- ar %>%
    mutate(
      Indikatorwert = as.character(Indikatorwert), # Ensure it's string first
      Indikatorwert = gsub(",", ".", Indikatorwert),
      Indikatorwert = gsub("%", "", Indikatorwert),
      Indikatorwert = trimws(Indikatorwert),
      Indikatorwert = as.numeric(Indikatorwert)
    )
  
  employment_female <- labormarket %>%
    filter(
      Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
      Ausprägung == "weiblich"
    ) %>%
    select(Jahr, Raumbezug, employment_female = Indikatorwert) %>%
    distinct(Raumbezug, Jahr, .keep_all = TRUE)
  
  # 2. Clean and Prepare Population Data (Birthrate)
  population <- be %>%
    mutate(
      Indikatorwert = as.character(Indikatorwert),
      Indikatorwert = gsub(",", ".", Indikatorwert),
      Indikatorwert = gsub("%", "", Indikatorwert),
      Indikatorwert = trimws(Indikatorwert),
      Indikatorwert = as.numeric(Indikatorwert)
    )
  
  birthrate <- population %>%
    filter(Indikator == "Allgemeine Geburtenrate") %>%
    mutate(Indikatorwert = Indikatorwert / 10) %>% # Keep your original logic
    select(Jahr, Raumbezug, birthrate = Indikatorwert) %>%
    distinct(Raumbezug, Jahr, .keep_all = TRUE)
  
  # 3. Join Data (Create data_wide)
  data_wide <- inner_join(employment_female, birthrate, by = c("Jahr", "Raumbezug"))
  
  # ----------------------------------------------------------
  # Plot 1: Spatial Dimension (Correlation by District)
  # ----------------------------------------------------------
  cor_district <- data_wide %>% 
    group_by(Raumbezug) %>%
    summarise(
      spearman_rho = cor(birthrate, employment_female, method = "spearman", use = "complete.obs"),
      n = n()
    ) %>%
    arrange(desc(spearman_rho))
  
  p_stadtteile <- ggplot(cor_district, 
                         aes(x = reorder(Raumbezug, spearman_rho), y = spearman_rho,
                             fill = ifelse(spearman_rho >= 0, "Positiv", "Negativ"))) +
    geom_col() +
    scale_fill_manual(
      name = "Korrelation",
      values = c("Positiv" = "steelblue", "Negativ" = "firebrick3")
    ) +
    coord_flip() + 
    geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
    labs(
      title = "Korrelation: Geburtenrate vs. Frauenbeschäftigung",
      subtitle = "Bezirke Münchens (Durchschnitt 2000–2024)",
      x = "Stadtbezirk",
      y = "Spearman ρ"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      axis.text.y = element_text(size = 10)
    )
  
  # ----------------------------------------------------------
  # Plot 2: Time Dimension (Correlation by Year)
  # ----------------------------------------------------------
  cor_year <- data_wide %>%
    filter(Raumbezug != "Stadt München") %>%
    group_by(Jahr) %>%
    summarise(
      spearman_rho = cor(birthrate, employment_female, 
                         method = "spearman", use = "complete.obs")
    )
  
  p_jahr <- ggplot(cor_year, aes(x = Jahr, y = spearman_rho)) +
    geom_line(color = "darkblue", linewidth = 1) +
    geom_point(color = "darkblue", size = 2) + # Added points for better visibility
    labs(
      title = "Entwicklung der Korrelation",
      subtitle = "Geburtenrate vs. Frauenbeschäftigung (2000-2024)",
      x = "Jahr",
      y = "Spearman ρ"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      panel.grid.minor = element_blank()
    )
  
  # ----------------------------------------------------------
  # Return List
  # ----------------------------------------------------------
  return(list(
    stadtteile = p_stadtteile,
    trend_jahr = p_jahr
  ))
}