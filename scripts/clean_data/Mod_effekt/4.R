library(leaflet)
library(dplyr)
library(sf)
library(tidyverse)
library(sf)
library(readxl)

# === Load Geometry ===
geojson_url <- "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gsm_wfs:vablock_stadtbezirke_opendata&outputFormat=application/json"
munich_map <- st_read(geojson_url, quiet = TRUE) %>% st_transform(4326)

# === Load tables ===
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")
ki_sheet <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

# === Variable processing ===

# 1. Haushalte mit Kindern
hh_data_long <- be_sheet %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(
    anteil_kinder = 100 * `Basiswert 1` / `Basiswert 2`,
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(Jahr, bezirksnummer, anteil_kinder)

# 2. Frauenbeschäftigung
ar_data_long <- ar_sheet %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(
    anteil = 100 * `Basiswert 1` / `Basiswert 2`,
    bezirksnummer = sprintf("%02d", as.numeric(str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(Jahr, bezirksnummer, anteil)

# === Combine all variables ===
combined_data_all <- hh_data_long %>%
  full_join(ar_data_long, by = c("Jahr", "bezirksnummer"))

# === Attach geometry ===
final_sf_data_long <- munich_map %>%
  left_join(combined_data_all, by = c("sb_nummer" = "bezirksnummer")) %>%
  filter(!is.na(Jahr))

# ================================================================
# 1. 过滤 2024 年数据
# ================================================================
data_2024 <- final_sf_data_long %>%
  filter(Jahr == 2024) %>%
  select(
    sb_nummer, name, geometry,
    anteil_kinder,        # HaKi (%)
    anteil                # Frauenbeschäftigung (%)
  )

# ================================================================
# 2. Stadt München 市平均值 (HaKi & Frauenbeschäftigung)
# ================================================================

# HaKi 来自 be_sheet
mean_HaKi_city <- be_sheet %>%
  filter(
    Jahr == 2024,
    Raumbezug == "Stadt München",
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(anteil_kinder = 100 * `Basiswert 1` / `Basiswert 2`) %>%
  pull(anteil_kinder)

# Frauenbeschäftigung 来自 ar_sheet
mean_FE_city <- ar_sheet %>%
  filter(
    Jahr == 2024,
    Raumbezug == "Stadt München",
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(anteil = 100 * `Basiswert 1` / `Basiswert 2`) %>%
  pull(anteil)

cat("City mean HaKi 2024 =", mean_HaKi_city, "\n")
cat("City mean FE   2024 =", mean_FE_city, "\n")


# ================================================================
# 3. 分类：A = HaKi 高 + 就业低
# ================================================================
data_2024 <- data_2024 %>%
  mutate(
    gruppe = case_when(
      anteil_kinder > mean_HaKi_city & anteil < mean_FE_city ~
        "hohe Haushalte mit Kindern + niedrige Beschäftigung",
      TRUE ~ "Andere"
    ),
    color = case_when(
      gruppe == "hohe Haushalte mit Kindern + niedrige Beschäftigung" ~ "#e75480",  # 红色
      TRUE ~ "#d9d9d9"  # 灰色（HEX）
    )
  )

library(ggplot2)
library(dplyr)

# ---- 使用你前面定义的 data_2024（含 gruppe 和 color） ----

# 构建四象限定义线
h_cut <- mean_HaKi_city
f_cut <- mean_FE_city

ggplot(data_2024, aes(x = anteil_kinder, y = anteil)) +
  
  # 所有散点
  geom_point(aes(color = gruppe), size = 4, alpha = 0.8) +
  
  # 四象限分割线
  geom_vline(xintercept = h_cut, linetype = "dashed", color = "black") +
  geom_hline(yintercept = f_cut, linetype = "dashed", color = "black") +
  
  # 颜色定义：A = 红色，其他 = 灰色
  scale_color_manual(values = c(
    "hohe Haushalte mit Kindern + niedrige Beschäftigung" = "#e75480",
    "Andere" = "#bdbdbd"   # 更柔和的灰色
  )) +
  
  labs(
    x = "Haushalte mit Kindern (%)",
    y = "Frauenbeschäftigungsquote (%)",
    color = ""
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom"
  )

