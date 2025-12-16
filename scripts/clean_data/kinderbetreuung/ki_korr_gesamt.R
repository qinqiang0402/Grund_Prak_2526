library(tidyverse)
library(readxl)
library(sf)
library(stringr)
library(forcats)
library(ggpubr)
library(grid)

# ------------------------------------------------------------
# Helper: create a 2-digit district code (sb) from a text column (default: "Raumbezug")
# Purpose: unify Bezirk identifiers for joins and filtering
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
# 0. Read raw input tables (Arbeitsmarkt / Bevölkerung / Kinderbetreuung)
# ------------------------------------------------------------
ar <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")
be <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
ki <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

# ------------------------------------------------------------
# 1. Female employment share (sozialversicherungspflichtig Beschäftigte - Anteil, weiblich)
#    Output: sozial_weiblich in [0, 1]
# ------------------------------------------------------------
sozial_anteil_weiblich <- ar %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(sozial_weiblich = `Basiswert 1` / `Basiswert 2`) %>%
  select(Jahr, Raumbezug, sozial_weiblich)

# ------------------------------------------------------------
# 2. Childcare (0–2 years): total children & supervised children
#    - kinder_total: total number of children aged 0–2
#    - kinder_betreut: number of children aged 0–2 in institutional childcare
#    - anteil_betreut / anteil_unbetreut: percentages
# ------------------------------------------------------------
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

# ------------------------------------------------------------
# 3. Add district code (sb) and keep only rows with a valid code
# ------------------------------------------------------------
df_betreut_bezirk <- df_betreut %>%
  add_sb() %>%
  filter(!is.na(sb))

# Quick sanity check: if this is empty, joins will fail later
stopifnot(nrow(df_betreut_bezirk) > 0)

# ------------------------------------------------------------
# 4. Read Munich district geometry (for mapping / joining later)
#    IMPORTANT: use named arguments for var/new
# ------------------------------------------------------------
munich_map2 <- st_read("results/geo/bezirk_map.json", quiet = TRUE) %>%
  add_sb(var = "sb_nummer", new = "sb_nummer")

# ------------------------------------------------------------
# 5. Merge childcare + female employment on Bezirk × Jahr
# ------------------------------------------------------------
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

stopifnot(nrow(df_merge) > 0)

# ------------------------------------------------------------
# 6. Build correlation dataset: Kinderbetreuung (%) vs Frauenbeschäftigung (%)
# ------------------------------------------------------------
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

stopifnot(nrow(korrelations_daten_clean) > 0)

# ------------------------------------------------------------
# 7. Define shared axis ranges/breaks and a shared theme
# ------------------------------------------------------------
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

# ------------------------------------------------------------
# 8. Overall correlation (Gesamt): scatter + one global regression line
# ------------------------------------------------------------
ki_korr_gesamt_sw <- ggplot(korrelations_daten_clean, aes(x = hmk, y = anteil)) +
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

saveRDS(
  ki_korr_gesamt_sw,
  "results/figures/NEW_Kinderbetreuung/ki_point_korr_gesamt_sw.rds"
)

# ------------------------------------------------------------
# 9. By-district correlation (Stadtteile): district lines + global line
# ------------------------------------------------------------
korr_stadtteile_data <- korrelations_daten_clean %>%
  group_by(Raumbezug) %>%
  filter(n() >= 2) %>%
  ungroup()

ki_korr_stadtteile_sw <- ggplot(korr_stadtteile_data, aes(x = hmk, y = anteil)) +
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

saveRDS(
  ki_korr_stadtteile_sw,
  "results/figures/NEW_Kinderbetreuung/ki_korr_stadtteile_point_line_sw.rds"
)

# Optional: preview plots
ki_korr_gesamt_sw
ki_korr_stadtteile_sw
