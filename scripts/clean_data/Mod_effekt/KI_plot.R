library(tidyverse)
library(sf)
library(leaflet)
library(readxl)
library(htmltools)


# =================================================================
# 1. Datenvorbereitung
# =================================================================

# 1.1. Geometrie laden
munich_map <- st_read("results/geo/bezirk_map.gpkg", quiet = TRUE) |>
  st_transform(4326)

# 1.2. Daten laden
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
ki_sheet <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

# =================================================================
# 2. Kinderbetreuung 0–2 Jahre berechnen (nur 2024)
# =================================================================

df_total <- be_sheet %>%
  filter(
    Jahr == 2024,
    Indikator == "Altersgruppen",
    Ausprägung == "bis 2 Jahre",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(bezirksnummer, kinder_total = `Basiswert 1`)

df_betreut <- ki_sheet %>%
  filter(
    Jahr == 2024,
    Indikator == "Altersgruppen",
    Ausprägung == "bis 2 Jahre",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(bezirksnummer, kinder_betreut = `Basiswert 1`)

betreuung_2024 <- df_total %>%
  left_join(df_betreut, by = "bezirksnummer") %>%
  mutate(
    anteil_betreuung = 100 * kinder_betreut / kinder_total
  )

# =================================================================
# 3. Join mit Geodaten
# =================================================================

final_sf <- munich_map %>%
  left_join(betreuung_2024, by = c("sb_nummer" = "bezirksnummer")) %>%
  filter(!is.na(anteil_betreuung))

MIN_BETR <- min(final_sf$anteil_betreuung, na.rm = TRUE)
MAX_BETR <- max(final_sf$anteil_betreuung, na.rm = TRUE)


#————————————————————————
library(ggplot2)
library(sf)
library(dplyr)

# final_sf 已存在，这里直接使用

m_effekt_03_plot <- ggplot(final_sf) +
  
  # 多边形
  geom_sf(aes(fill = anteil_betreuung),
          color = "white",
          size = 0.4) +
  
  # ⭐ 改成红色渐变（#fff5eb → #7f2704）
  scale_fill_gradient(
    low  = "#fff5eb",
    high = "#cd6155",
    limits = c(0, 100),
    name = "Kinderbetreuung (%)"
  ) +
  
  # —— Legend：标题在上、颜色条加长加粗 —— 
  guides(
    fill = guide_colorbar(
      title.position = "top",
      barwidth = 25,
      barheight = 2
    )
  ) +
  
  # 地图主题
  theme_void(base_size = 14) +
  theme(
    legend.position    = "bottom",
    legend.title.align = 0.5,
    legend.title = element_text(size = 28, face = "bold"),
    legend.text  = element_text(size = 26),
    plot.title = element_text(face = "bold", size = 26, hjust = 0.5)
  )


m_effekt_03_plot

saveRDS(m_effekt_03_plot, "results/figures/m_effekt/m_effekt_03_plot.rds")
