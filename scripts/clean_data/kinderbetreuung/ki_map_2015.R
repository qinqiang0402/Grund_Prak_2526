# 01_ki_map_2015.R
# Single figure: Bezirke map of Kinderbetreuung (0–2 years) for one year (here: 2015)
# After running, an RDS file will be created under:
# results/figures/NEW_Kinderbetreuung/map_ki_2015.rds

library(tidyverse)
library(readxl)
library(sf)
library(stringr)
library(grid)

# ---------------- 0. Parameter: which year to plot ----------------
target_year <- 2015

# ---------------- 1. Helper: extract a 2-digit Bezirk code from "Raumbezug" ----------------
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

# ---------------- 2. Read Excel + compute Kinderbetreuung ----------------
# Note: 'ar' is loaded for consistency with your pipeline, but is not used in this script.
ar <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")
be <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
ki <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

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
  ) %>%
  add_sb() %>%
  filter(!is.na(sb))

# ---------------- 3. Read Munich district geometry + create Bezirk code ----------------
# IMPORTANT: If your GeoJSON column name is not 'sb_nummer', adjust it below.
munich_map <- st_read("results/geo/bezirk_map.json", quiet = TRUE) %>%
  mutate(
    sb = str_pad(as.character(sb_nummer), 2, pad = "0")
  )

# ---------------- 4. Join 2015 data to the map ----------------
ki_year <- df_betreut %>%
  filter(Jahr == target_year)

map_year <- munich_map %>%
  left_join(
    ki_year %>% select(sb, anteil_betreut),
    by = "sb"
  )

# ---------------- 5. Plot ----------------
ki_map_year <- ggplot(map_year) +
  geom_sf(
    aes(fill = anteil_betreut),
    color = "white",
    size  = 0.2
  ) +
  scale_fill_gradient(
    name   = "Kinderbetreuung (%)",
    low    = "#fff5eb",
    high   = "#7f2704",
    limits = c(10, 55),
    guide  = guide_colorbar(
      title.position = "top",
      title.hjust    = 0.5,
      barwidth       = unit(8, "cm"),
      barheight      = unit(0.8, "cm")
    )
  ) +
  labs(
    title    = "",
    subtitle = ""
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position    = "bottom",
    legend.title.align = 0.5,
    legend.title       = element_text(size = 26, face = "bold"),
    legend.text        = element_text(size = 26),
    plot.title         = element_text(face = "bold", size = 26, hjust = 0.5)
  )

# ---------------- 6. Save RDS ----------------
ki_map_year
saveRDS(ki_map_year, "results/figures/NEW_Kinderbetreuung/map_ki_2015.rds")       