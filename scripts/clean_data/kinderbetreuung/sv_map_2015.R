# 02_emp_map_2015.R
# 一张图：某一年(这里是 2015 年) Frauenbeschäftigung (Anteil weiblich) 的 Bezirk 地图

library(tidyverse)
library(readxl)
library(sf)
library(stringr)
library(grid)

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

#---------------- 2. 读 Excel，算 Frauenbeschäftigung (Anteil weiblich) ----------------

ar <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")

# 女性 sozialversicherungspflichtig Beschäftigte Anteil
sozial_anteil_weiblich <- ar %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(
    sozial_weiblich = 100 * `Basiswert 1` / `Basiswert 2`   # 转成百分比
  ) %>%
  select(Jahr, Raumbezug, sozial_weiblich) %>%
  add_sb() %>%
  filter(!is.na(sb),
         Jahr == target_year)

#---------------- 3. 读 München 地图 + 加 Bezirk 编号 ----------------

munich_map <- st_read("results/geo/bezirk_map.json", quiet = TRUE) %>%
  mutate(
    sb = str_pad(as.character(sb_nummer), 2, pad = "0")
  )

#---------------- 4. 把 target_year 年的数据合并到地图 ----------------

map_emp_year <- munich_map %>%
  left_join(
    sozial_anteil_weiblich %>% select(sb, sozial_weiblich),
    by = "sb"
  )

#---------------- 5. 画图：风格跟 Kinderbetreuung 图完全一致，只改颜色 ----------------

emp_map_year <- ggplot(map_emp_year) +
  geom_sf(aes(fill = sozial_weiblich),
          color = "white", size = 0.2) +
  scale_fill_gradient(
    name   = "Frauenbeschäftigung (%)",
    low    = "#f7fbff",   # 你指定的低值颜色
    high   = "#08306b",   # 你指定的高值颜色
    # limits 可以按需要设，比如 c(40, 80)，也可以先不设
    limits = c(50, 62),
    breaks = c(50, 56, 62),
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


emp_map_year


saveRDS(emp_map_year, "results/figures/NEW_Kinderbetreuung/map_sv_2015.rds")
