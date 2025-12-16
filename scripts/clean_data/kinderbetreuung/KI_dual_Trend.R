# ============================================================
# Build dual-axis trend plot for Kinderbetreuung & Frauenbeschäftigung
# Save as: results/figures/NEW_Kinderbetreuung/ki_dual_trend.rds
# In Quarto, you only need readRDS() + print()
# ============================================================

library(tidyverse)
library(readxl)
library(stringr)
library(grid)

# Small helper: extract a two-digit Bezirk (district) code from "Raumbezug"
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

#----------------------------------------------------------
# 1. Read raw data
#----------------------------------------------------------
ar <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")
be <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
ki <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

# Frauenbeschäftigung (female share, 0~1)
sozial_anteil_weiblich <- ar %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(sozial_weiblich = `Basiswert 1` / `Basiswert 2`) %>%
  select(Jahr, Raumbezug, sozial_weiblich)

# Kinderbetreuung age 0–2 (percentage)
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

# Keep only rows that have a valid Bezirk code
df_betreut_bezirk <- df_betreut %>%
  add_sb() %>%
  filter(!is.na(sb))

# Merge Kinderbetreuung + Frauenbeschäftigung at Bezirk × Jahr level
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

#----------------------------------------------------------
# 2. Aggregate across 25 Bezirke by year → time series
#----------------------------------------------------------
ts_dual <- df_merge %>%
  group_by(Jahr) %>%
  summarise(
    frauen_mean = mean(sozial_weiblich * 100, na.rm = TRUE), # left axis: female employment (%)
    kinder_mean = mean(anteil_betreut,       na.rm = TRUE), # right axis: childcare coverage (%)
    .groups = "drop"
  )

#----------------------------------------------------------
# 3. Compute dual-axis scaling parameters (as literal constants)
#----------------------------------------------------------
left_min  <- min(ts_dual$frauen_mean, na.rm = TRUE)
left_max  <- max(ts_dual$frauen_mean, na.rm = TRUE)
right_min <- min(ts_dual$kinder_mean, na.rm = TRUE)
right_max <- max(ts_dual$kinder_mean, na.rm = TRUE)

scale_factor <- (left_max - left_min) / (right_max - right_min)

# Map Kinderbetreuung onto the left-axis coordinate system (plotting only)
ts_dual <- ts_dual %>%
  mutate(
    kinder_scaled = (kinder_mean - right_min) * scale_factor + left_min
  )

# Left-axis breaks (automatic)
breaks_left <- pretty(c(left_min, left_max), n = 5)

# Right-axis breaks (fixed as requested)
breaks_right <- c(20, 25, 30, 35, 40)

# Key: build a formula that does not depend on external variable names
sec_trans <- eval(substitute(
  ~ (. - L) / S + R,
  list(L = left_min, S = scale_factor, R = right_min)
))
# The formula becomes something like: ~ (. - 27.3) / 1.5 + 20.1
# It contains only numeric literals (no left_min / scale_factor / right_min symbols)

#----------------------------------------------------------
# 4. Build the dual-axis plot (no longer referencing left_min etc. by name)
#----------------------------------------------------------
ki_dual_trend <- ggplot(ts_dual, aes(x = Jahr)) +
  
  # Left axis: Frauenbeschäftigung (blue)
  geom_line(aes(y = frauen_mean, color = "Frauenbeschäftigung"), size = 1.2) +
  geom_point(aes(y = frauen_mean, color = "Frauenbeschäftigung"), size = 2) +
  
  # Right axis: Kinderbetreuung (orange, scaled onto left axis)
  geom_line(aes(y = kinder_scaled, color = "Kinderbetreuung"), size = 1.2) +
  geom_point(aes(y = kinder_scaled, color = "Kinderbetreuung"), size = 2) +
  
  scale_y_continuous(
    name   = "Frauenbeschäftigung (%)",
    limits = c(left_min, left_max),
    breaks = breaks_left,
    sec.axis = sec_axis(
      trans  = sec_trans,      # formula contains only numeric literals
      name   = "Kinderbetreuung (%)",
      breaks = breaks_right
    )
  ) +
  
  scale_color_manual(
    name   = NULL,
    values = c(
      "Frauenbeschäftigung" = "#0072B2", # blue
      "Kinderbetreuung"     = "#D55E00"  # orange
    )
  ) +
  
  scale_x_continuous(
    breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021, 2024)
  ) +
  
  labs(
    x     = "",
    title = ""
  ) +
  
  theme_bw(base_size = 13) +
  theme(
    legend.position    = "top",
    axis.title.y.left  = element_text(size = 20, face = "bold"),
    axis.title.y.right = element_text(size = 20, face = "bold"),
    axis.text.x        = element_text(size = 16),
    axis.text.y.left   = element_text(size = 16),
    axis.text.y.right  = element_text(size = 16),
    legend.text        = element_text(size = 18),
    legend.key.size    = unit(1.2, "cm")
  )

#----------------------------------------------------------
# 5. Save as RDS (directly usable via readRDS() in Quarto)
#----------------------------------------------------------
saveRDS(ki_dual_trend, "results/figures/NEW_Kinderbetreuung/ki_dual_trend.rds")

# Optional: preview in this script
ki_dual_trend