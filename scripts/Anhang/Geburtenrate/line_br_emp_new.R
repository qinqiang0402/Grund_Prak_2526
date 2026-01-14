# ============================================================
# Script: plot_birthrate_trend.R
# Purpose: Generate a dual-axis trend plot for Birthrate vs. 
#          Female Employment for a specific district (Default: Munich).
# Returns: A ggplot object
# ============================================================

library(tidyverse)
library(stringr)

# ------------------------------------------------------------
# Main Function
# ------------------------------------------------------------
# Arguments:
# - ar: ARBEITSMARKT dataframe
# - be: BEVÖLKERUNG dataframe
# - input_district: Name or number of the district to plot (default "Stadt München")
generate_birthrate_trend <- function(ar, be, input_district = "Stadt München") {
  
  # 1. Clean Labor Market Data
  labormarket <- ar %>%
    mutate(
      Indikatorwert = as.character(Indikatorwert),
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
  
  # 2. Clean Population Data
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
    mutate(Indikatorwert = Indikatorwert / 10) %>%
    select(Jahr, Raumbezug, birthrate = Indikatorwert) %>%
    distinct(Raumbezug, Jahr, .keep_all = TRUE)
  
  # 3. Join Data
  data_wide <- birthrate %>%
    left_join(employment_female, by = c("Jahr", "Raumbezug"))
  
  # 4. District Selection Logic
  districts <- unique(data_wide$Raumbezug)
  district_name <- character(0)
  
  if (is.numeric(input_district)) {
    number <- sprintf("%02d", input_district)
    district_name <- districts[str_detect(districts, paste0("^", number, " "))]
  } else {
    input_str <- as.character(input_district)
    if (input_str %in% districts) {
      district_name <- input_str
    } else {
      district_name <- districts[str_detect(districts, regex(input_str, ignore_case = TRUE))]
    }
  }
  
  if (length(district_name) == 0) stop("No district found. Please check input.")
  if (length(district_name) > 1) {
    message("ℹ More than one district found. Plotting first match: ", district_name[1])
    district_name <- district_name[1]
  }
  
  # 5. Filter Data for Selected District
  df <- data_wide %>%
    filter(Raumbezug == district_name) %>%
    drop_na(birthrate, employment_female)
  
  # 6. Calculate Scaling for Dual Axis
  range_emp   <- range(df$employment_female, na.rm = TRUE)
  range_birth <- range(df$birthrate, na.rm = TRUE)
  
  # Handle cases with zero variance to avoid division by zero
  if (diff(range_birth) == 0) scale_factor <- 1 else scale_factor <- diff(range_emp) / diff(range_birth)
  
  # Fix for cases where ranges might be identical or zero, ensuring plot still renders
  if (is.infinite(scale_factor) || is.na(scale_factor)) scale_factor <- 1
  
  df <- df %>%
    mutate(birthrate_scaled = birthrate * scale_factor)
  
  # 7. Generate Plot
  p <- ggplot(df, aes(x = Jahr)) +
    # Female Employment (Left Axis)
    geom_line(aes(y = employment_female, color = "Beschäftigungsrate"), linewidth = 1.1) +
    geom_point(aes(y = employment_female, color = "Beschäftigungsrate"), size = 1.8) +
    
    # Birthrate (Right Axis, Scaled)
    geom_line(aes(y = birthrate_scaled, color = "Allgemeine Geburtenrate"), linewidth = 1.1) +
    geom_point(aes(y = birthrate_scaled, color = "Allgemeine Geburtenrate"), size = 1.8) +
    
    scale_y_continuous(
      name = "Beschäftigungsrate (%)",
      sec.axis = sec_axis(
        # We need to construct the transform carefully to avoid environment issues
        # Using a simple multiplication inverse for the second axis labels
        trans = ~ . / scale_factor,
        name = "Allgemeine Geburtenrate"
      )
    ) +
    scale_x_continuous(breaks = seq(min(df$Jahr), max(df$Jahr), by = 2)) +
    
    scale_color_manual(
      name   = "Indikator",
      values = c("Allgemeine Geburtenrate" = "#E74C3C", 
                 "Beschäftigungsrate"      = "#3498DB")
    ) +
    
    labs(
      title    = "Allgemeine Geburtenrate und weibliche SV-pflichtige Beschäftigungsrate",
      subtitle = district_name,
      x = "Jahr"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", hjust = 0.5, size = 16),
      plot.subtitle    = element_text(hjust = 0.5),
      axis.title.y.right = element_text(margin = margin(l = 8)),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold")
    )
  
  return(p)
}