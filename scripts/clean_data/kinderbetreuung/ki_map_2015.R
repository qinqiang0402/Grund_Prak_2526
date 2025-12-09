# 01_ki_map_2015.R
# 一张图：某一年(这里是 2015 年) Kinderbetreuung 0–2 Jahre 的 Bezirk 地图
# 运行后会在 results/figures/kinderbetreuung/ 下面生成 ki_map_2015.rds

library(tidyverse)
library(readxl)
library(sf)
library(stringr)

#---------------- 0. 参数：要画哪一年 ----------------
target_year <- 2015

#---------------- 1. 帮助函数：从 Raumbezug 提取 Bezirk 编号 ----------------
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

#---------------- 2. 读 Excel、算 Kinderbetreuung ----------------
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

#---------------- 3. 读 München 地图 + 加 Bezirk 编号 ----------------
munich_map <- st_read("results/geo/bezirk_map.json", quiet = TRUE) %>%
  # 你的 json 里列名如果不是 sb_nummer，改这里
  mutate(
    sb = str_pad(as.character(sb_nummer), 2, pad = "0")
  )

#---------------- 4. 把 2015 年数据合并到地图 ----------------
ki_year <- df_betreut %>%
  filter(Jahr == target_year)

map_year <- munich_map %>%
  left_join(
    ki_year %>% select(sb, anteil_betreut),
    by = "sb"
  )

#---------------- 5. 画图 ----------------
library(grid)

ki_map_year <- ggplot(map_year) +
  geom_sf(aes(fill = anteil_betreut),
          color = "white", size = 0.2) +
  scale_fill_gradient(
    name   = "Kinderbetreuung (%)",
    low    = "#fff5eb",
    high   = "#7f2704",
    limits = c(10, 55),
    guide = guide_colorbar(
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

#---------------- 6. 保存 RDS ----------------

ki_map_year

saveRDS(ki_map_year, "results/figures/NEW_Kinderbetreuung/map_ki_2015.rds")

        