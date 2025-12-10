library(leaflet)
library(dplyr)
library(sf)
library(tidyverse)
library(readxl)
library(htmltools)

# === Load Geometry ===
munich_map <- st_read("results/geo/bezirk_map.gpkg", quiet = TRUE) |>
  st_transform(4326)

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
    bezirksnummer = sprintf("%02d", as.numeric(stringr::str_extract(Raumbezug, "^\\d+")))
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
    bezirksnummer = sprintf("%02d", as.numeric(stringr::str_extract(Raumbezug, "^\\d+")))
  ) %>%
  select(Jahr, bezirksnummer, anteil)

# === Combine all variables ===
combined_data_all <- hh_data_long %>%
  full_join(ar_data_long, by = c("Jahr", "bezirksnummer"))

# === Attach geometry ===
final_sf_data_long <- munich_map %>%
  left_join(
    combined_data_all,
    by = c("sb_nummer" = "bezirksnummer"),
    relationship = "many-to-many"   # 消掉 many-to-many 的 warning
  ) %>%
  mutate(Jahr = as.numeric(Jahr)) %>%  # 防止 Jahr 是字符
  filter(!is.na(Jahr))

# 确保它是 sf 对象（通常 join 后还是 sf，但这里稳妥一点）
final_sf_data_long <- sf::st_as_sf(final_sf_data_long)

# ================================================================
# 1. 过滤 2024 年数据
#    注意：这里不要手动选 geometry，sf 会自动保留几何列
# ================================================================
data_2024 <- final_sf_data_long %>%
  filter(Jahr == 2024) %>%
  dplyr::select(
    sb_nummer,
    name,
    anteil_kinder,   # HaKi (%)
    anteil           # Frauenbeschäftigung (%)
    # geometry 会自动跟着保留
  )

# 再保险一次：保持 sf 属性
data_2024 <- sf::st_as_sf(data_2024)

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
    gruppe = dplyr::case_when(
      anteil_kinder > mean_HaKi_city & anteil < mean_FE_city ~
        "hohe Haushalte mit Kindern + niedrige Beschäftigung",
      TRUE ~ "Andere"
    ),
    color = dplyr::case_when(
      gruppe == "hohe Haushalte mit Kindern + niedrige Beschäftigung" ~ "#e75480",  # 红色
      TRUE ~ "#d9d9d9"  # 灰色
    )
  )


# ================================================================
# 4A. Leaflet 地图（如果你也想要）
# ================================================================

###
#m_effekt_04 <- leaflet(data_2024) %>%
#  addTiles(
#    urlTemplate = "https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png",
#    attribution = '&copy; CartoDB'
#  ) %>%
#  addPolygons(
#    fillColor = ~color,
#    color = "white",
#    weight = 1,
#    fillOpacity = 0.8,
#    label = ~paste0(
#     "<b>", name, "</b><br/>",
#      "Haushalte mit Kindern: ", round(anteil_kinder, 1), "%<br/>",
#      "Frauenbeschäftigung: ", round(anteil, 1), "%<br/>",
#      "<b>Gruppe:</b> ", gruppe
#    ) %>% lapply(htmltools::HTML),
#    highlightOptions = highlightOptions(
#      weight = 2,
#      color = "grey85",
#      fillOpacity = 0.9,
#      bringToFront = TRUE
#    )
#  ) %>%
#  addLegend(
#    colors = c("#e75480", "#d9d9d9"),
#    labels = c("hohe Haushalte mit Kindern + niedrige Beschäftigung", "Andere"),
#    title = "Kategorien (2024)",
#    position = "bottomright"
#  )
###
# 如果不需要 leaflet，可以注释掉下面两行
#m_effekt_04
#saveRDS(m_effekt_04, "results/figures/m_effekt/m_effekt_04.rds")


# ================================================================
# 4B. 散点图（你定义的 m_effekt_2.5）
# ================================================================
# ================================================================
# 4B. 散点图（最终正确版本）
# ================================================================
# ================================================================
# 4B. 散点图（最终正确版本）
# ================================================================
library(ggplot2)

h_cut <- mean_HaKi_city
f_cut <- mean_FE_city

# 换成最终两行图例标签
data_2024_plot <- data_2024 %>%
  mutate(
    gruppe_lab = if_else(
      gruppe == "hohe Haushalte mit Kindern + niedrige Beschäftigung",
      "Haushalte mit Kindern hoch\nund Frauenbeschäftigung niedrig",  # ← 你想要的两行版本
      "Andere"
    )
  )

m_effekt_2.5 <- ggplot(data_2024_plot,
                       aes(x = anteil_kinder, y = anteil, color = gruppe_lab)) +
  geom_point(size = 4, alpha = 0.8) +
  
  # 四象限线
  geom_vline(xintercept = h_cut, linetype = "dashed", color = "black") +
  geom_hline(yintercept = f_cut, linetype = "dashed", color = "black") +
  
  # 手动颜色 + 图例顺序（粉色 → 灰色）
  scale_color_manual(
    values = c(
      "Haushalte mit Kindern hoch\nund Frauenbeschäftigung niedrig" = "#e75480",  # 粉色
      "Andere"                                                      = "#bdbdbd"   # 灰色
    ),
    breaks = c(
      "Haushalte mit Kindern hoch\nund Frauenbeschäftigung niedrig",
      "Andere"
    )
  ) +
  
  labs(
    x = "Haushalte mit Kindern (%)",
    y = "Frauenbeschäftigung (%)",
    color = ""
  ) +
  
  theme_bw(base_size = 13) +
  theme(
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 16),
    legend.position = "bottom",
    legend.text   = element_text(size = 18),
    legend.title  = element_text(size = 18)
  )

m_effekt_2.5
saveRDS(m_effekt_2.5, "results/figures/m_effekt/m_effekt_2.5.rds")

