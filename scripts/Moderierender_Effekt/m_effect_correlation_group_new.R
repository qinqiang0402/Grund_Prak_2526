# ============================================================
# Script: plot_moderation_effect.R
# Purpose: Analyze the moderating effect of childcare on the 
#          correlation between households with children and 
#          female employment.
# Returns: A ggplot object showing correlation trends by group.
# ============================================================

library(tidyverse)
library(readxl)

# ------------------------------------------------------------
# Main Function
# ------------------------------------------------------------
generate_moderation_plot <- function(ar, be, ki) {
  
  # 1. Prepare Employment Data (Frauenbeschäftigung)
  df_emp <- ar %>%
    filter(
      Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
      Ausprägung == "weiblich"
    ) %>%
    mutate(
      Jahr = as.numeric(Jahr),
      emp_female_pct = 100 * `Basiswert 1` / `Basiswert 2`
    ) %>%
    select(Jahr, Raumbezug, emp_female_pct)
  
  # 2. Prepare Households Data (Haushalte mit Kindern)
  df_households <- be %>%
    filter(
      Indikator == "Haushalte mit Kindern",
      Ausprägung == "insgesamt"
    ) %>%
    mutate(
      Jahr = as.numeric(Jahr),
      households_pct = 100 * `Basiswert 1` / `Basiswert 2`
    ) %>%
    select(Jahr, Raumbezug, households_pct)
  
  # 3. Prepare Childcare Data (Kinderbetreuung 0-2)
  # 3a. Betreute Kinder (aus KI Sheet)
  ki_0bis2 <- ki %>% 
    filter(
      Indikator == "Altersgruppen",
      Ausprägung == "bis 2 Jahre"
    ) %>% 
    select(Jahr, Raumbezug, `Basiswert 1`) %>%
    rename(kinder_betreut = `Basiswert 1`)
  
  # 3b. Alle Kinder (aus BE Sheet)
  be_0bis2 <- be %>% 
    filter(
      Indikator == "Altersgruppen",
      Ausprägung == "bis 2 Jahre"
    ) %>% 
    select(Jahr, Raumbezug, `Basiswert 1`) %>%
    rename(kinder_total = `Basiswert 1`)
  
  # 3c. Quote berechnen
  df_betreut <- left_join(
    be_0bis2, ki_0bis2,
    by = c("Jahr", "Raumbezug")
  ) %>%
    mutate(
      anteil_betreut = 100 * kinder_betreut / kinder_total
    ) %>%
    select(Jahr, Raumbezug, anteil_betreut)
  
  # 4. Join All Datasets
  df_all <- df_emp %>%
    inner_join(df_households, by = c("Jahr", "Raumbezug")) %>%
    inner_join(df_betreut,    by = c("Jahr", "Raumbezug")) %>%
    filter(Raumbezug != "Stadt München")
  
  # 5. Calculate Correlations per Year and Group
  years <- sort(unique(df_all$Jahr))
  
  corr_df <- map_df(years, function(y) {
    
    df_y <- df_all %>% filter(Jahr == y)
    
    # Quantile für Gruppeneinteilung (Drittel)
    qu <- quantile(df_y$anteil_betreut, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
    
    df_y <- df_y %>%
      mutate(
        betreuung_group = cut(
          anteil_betreut,
          breaks = qu,
          labels = c("Niedrig", "Mittel", "Hoch"),
          include.lowest = TRUE
        )
      )
    
    # Korrelationen berechnen
    tibble(
      Jahr = y,
      
      Mittel = cor(
        df_y %>% filter(betreuung_group == "Mittel") %>% pull(emp_female_pct),
        df_y %>% filter(betreuung_group == "Mittel") %>% pull(households_pct),
        use = "complete.obs"
      ),
      
      Niedrig = cor(
        df_y %>% filter(betreuung_group == "Niedrig") %>% pull(emp_female_pct),
        df_y %>% filter(betreuung_group == "Niedrig") %>% pull(households_pct),
        use = "complete.obs"
      ),
      
      Hoch = cor(
        df_y %>% filter(betreuung_group == "Hoch") %>% pull(emp_female_pct),
        df_y %>% filter(betreuung_group == "Hoch") %>% pull(households_pct),
        use = "complete.obs"
      )
    )
  })
  
  # 6. Reshape for Plotting
  corr_long <- corr_df %>%
    pivot_longer(
      cols = c("Mittel", "Niedrig", "Hoch"),
      names_to = "Group",
      values_to = "Correlation"
    )
  
  # 7. Generate Plot
  kor_group <- ggplot(corr_long, aes(x = Jahr, y = Correlation, color = Group)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = c(
      "Mittel"  = "#D97400",
      "Niedrig" = "#FFB84D",
      "Hoch"    = "#8C3F00"
    )) +
    theme_minimal(base_size = 14) +
    labs(
      x = "Jahr",
      y = "Korrelationskoeffizient",
      color = "Gruppe"
    )
  
  return(kor_group)
}