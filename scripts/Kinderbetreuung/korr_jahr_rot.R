library(tidyverse)
library(readxl)
library(sf)
library(stringr)
library(forcats)
library(ggpubr)
library(grid)

# ------------------------------------------------------------
# Helper: extract a 2-digit Bezirk code from a text/ID column
# - var: input column name that contains a leading district number (e.g., "01 ...")
# - new: output column name for the standardized 2-digit code (e.g., "sb")
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
  add_sb(var = "Raumbezug", new = "sb") %>%
  filter(!is.na(sb))

# ------------------------------------------------------------
# 4. Read Munich district geometry
#    IMPORTANT:
#    - For joining, we want the map to also have a standardized column named "sb"
#    - If your geojson column is not "sb_nummer", change var=... accordingly
# ------------------------------------------------------------
munich_map2 <- st_read("results/geo/bezirk_map.json", quiet = TRUE) %>%
  add_sb(var = "sb_nummer", new = "sb")

# ------------------------------------------------------------
# 5. Merge childcare + female employment on Bezirk × Jahr
# ------------------------------------------------------------
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

# ------------------------------------------------------------
# 6. Build correlation dataset (same construction as your other correlation plots)
# ------------------------------------------------------------
korrelations_daten_clean <- df_merge %>%
  mutate(
    hmk    = anteil_betreut,        # x: childcare coverage (%)
    anteil = 100 * sozial_weiblich  # y: female employment (%)
  ) %>%
  filter(
    Raumbezug != "Stadt München",
    Jahr >= 2007,
    Jahr <= 2024
  ) %>%
  select(Jahr, Raumbezug, hmk, anteil)

# ------------------------------------------------------------
# 7. Shared axis ranges/breaks and a shared theme (identical across plots)
# ------------------------------------------------------------
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

# ------------------------------------------------------------
# 8. Prepare data for "one regression line per year"
# ------------------------------------------------------------
plot_data_final_year <- korrelations_daten_clean %>%
  mutate(
    Jahr       = as.integer(Jahr),
    Jahr_Label = factor(Jahr),
    Jahr_num   = as.numeric(Jahr)
  ) %>%
  arrange(Jahr)

# ------------------------------------------------------------
# 9. Plot: one colored regression line per year + one global black line
# ------------------------------------------------------------
ki_korr_jahr_multiline <- ggplot(plot_data_final_year, aes(x = hmk, y = anteil)) +
  geom_point(color = "grey90", size = 1.2, alpha = 0.5) +
  geom_smooth(
    aes(color = Jahr_num, group = Jahr_Label),
    method    = "lm",
    se        = FALSE,
    linewidth = 1.1,
    alpha     = 0.85
  ) +
  geom_smooth(
    aes(group = 1),
    method    = "lm",
    se        = FALSE,
    linewidth = 1.2,
    color     = "black"
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
  base_theme_corr +
  theme(
    legend.title = element_text(size = 16, face = "bold"),
    legend.text  = element_text(size = 14)
  )

ki_korr_jahr_multiline

# ------------------------------------------------------------
# 10. Save as RDS
# ------------------------------------------------------------
saveRDS(
  ki_korr_jahr_multiline,
  "results/figures/NEW_Kinderbetreuung/ki_point_line_rot.rds"
)