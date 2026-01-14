# ============================================================
# Script: plot_birthrate_trend_districts.R
# Purpose: Generate a line plot showing birthrate trends for:
#          - All districts (grey background lines)
#          - Highest average district (Red)
#          - Lowest average district (Blue)
#          - Munich city average (Black)
# Returns: A ggplot object
# ============================================================

library(tidyverse)
library(stringr)

# ------------------------------------------------------------
# Main Function
# ------------------------------------------------------------
# Arguments:
# - be: BEVÖLKERUNG dataframe
generate_birthrate_district_trend <- function(be) {
  
  # 1. Clean and Prepare Population Data (Birthrate)
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
    mutate(Indikatorwert = Indikatorwert / 10) %>% # Keep original logic
    select(Jahr, Raumbezug, birthrate = Indikatorwert) %>%
    distinct(Raumbezug, Jahr, .keep_all = TRUE)
  
  # 2. Calculate Averages to Identify High/Low Districts
  birthrate_average <- birthrate %>%
    group_by(Raumbezug) %>%
    mutate(mean_birthrate = mean(birthrate, na.rm = TRUE)) %>%
    ungroup()
  
  # Find district with Lowest Average (excluding "Stadt München")
  lowest_birth_district <- birthrate_average %>%
    filter(Raumbezug != "Stadt München") %>%
    group_by(Raumbezug) %>%
    summarise(mean_birthrate = mean(birthrate, na.rm = TRUE)) %>%
    slice_min(mean_birthrate, n = 1) %>%
    pull(Raumbezug)
  
  # Find district with Highest Average (excluding "Stadt München")
  highest_birth_district <- birthrate_average %>%
    filter(Raumbezug != "Stadt München") %>%
    group_by(Raumbezug) %>%
    summarise(mean_birthrate = mean(birthrate, na.rm = TRUE)) %>%
    slice_max(mean_birthrate, n = 1) %>%
    pull(Raumbezug)
  
  # 3. Categorize Data for Plotting
  line_birthrate_data <- birthrate %>%
    mutate(
      line_birthrate_group = case_when(
        Raumbezug == "Stadt München" ~ "Stadt München", 
        Raumbezug == highest_birth_district ~ "highest",
        Raumbezug == lowest_birth_district ~ "lowest",
        TRUE ~ "rest"
      )
    )
  
  # 4. Generate Plot
  p <- ggplot() +
    # Background lines (Rest)
    geom_line(
      data = line_birthrate_data %>% filter(line_birthrate_group == "rest"),
      aes(x = Jahr, y = birthrate, group = Raumbezug), 
      color = "grey80", 
      linewidth = 0.5
    ) +
    # Highlighted lines (High, Low, Munich)
    geom_line(
      data = line_birthrate_data %>% filter(line_birthrate_group != "rest"), 
      aes(x = Jahr, y = birthrate, color = line_birthrate_group, group = Raumbezug), 
      linewidth = 1.2
    ) +
    scale_color_manual(
      name = "Legende",
      values = c(
        "Stadt München" = "black",
        "highest"       = "red",
        "lowest"        = "blue"
      ),
      labels = c(
        "Stadt München" = "Gesamtdurchschnitt Münchens",
        "highest"       = paste(highest_birth_district, "(Bezirk mit höchstem Durchschnitt)"),
        "lowest"        = paste(lowest_birth_district, "(Bezirk mit niedrigstem Durchschnitt)")
      )
    ) +
    labs(
      title = "Entwicklung der Geburtenrate in Stadtbezirken Münchens (2000-2024)",
      x = "Jahr",
      y = "Anteil (%)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 14, hjust = 0.5),
      legend.position  = "right",
      legend.title     = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    ) +
    guides(
      color = guide_legend(
        nrow = 3,      
        byrow = TRUE    
      )
    )
  
  return(p)
}