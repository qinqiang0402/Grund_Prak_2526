library(tidyverse)
library(readxl)
library(sf)
library(stringr)
library(forcats)
library(ggpubr)
library(grid)



ar <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")
be <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
ki <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

# 女性 sozialversicherungspflichtig Beschäftigte Anteil
sozial_anteil_weiblich <- ar %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(sozial_weiblich = `Basiswert 1` / `Basiswert 2`) %>%
  select(Jahr, Raumbezug, sozial_weiblich)

# 0–2 岁儿童总数 & 被托管数
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

# Stadtbezirk编号、只保留有编号的 Bezirk
df_betreut_bezirk <- df_betreut %>%
  add_sb() %>%
  filter(!is.na(sb))

# 地图

munich_map2 <- st_read("results/geo/bezirk_map.json", quiet = TRUE)  %>% add_sb("sb_nummer", "sb_nummer")

# 合并“儿童托管 + 女性 Beschäftigung”到 Bezirk×Jahr
df_merge <- df_betreut_bezirk %>%
  filter(Jahr >= 2007, Jahr <= 2024) %>%       # ★★ 加入年份过滤 ★★
  select(Jahr, sb, Raumbezug, anteil_betreut) %>%
  left_join(
    sozial_anteil_weiblich %>%
      add_sb() %>%
      filter(Jahr >= 2007, Jahr <= 2024) %>%   # 最好这里也加一下
      select(Jahr, sb, sozial_weiblich),
    by = c("Jahr", "sb")
  ) %>%
  arrange(sb, Jahr)




range_left  <- range(ts_dual$frauen_mean, na.rm = TRUE)
breaks_left <- pretty(range_left, n = 5)



#---------------- 1. 从 df_merge 计算每年的平均女性就业率 ----------------
ts_frauen <- df_merge %>%
  group_by(Jahr) %>%
  summarise(
    frauen_mean = mean(sozial_weiblich * 100, na.rm = TRUE),  # 转成百分比
    .groups = "drop"
  )

# （⭐ 和 dual-trend 保持一致：这里不要再另外算 range/breaks；
#    直接用你上面 dual 图里算好的 range_left / breaks_left）

#---------------- 2. 单独趋势图 ----------------
p_frauen <- ggplot(ts_frauen, aes(x = Jahr)) +
  geom_line(
    aes(y = frauen_mean, color = "Frauenbeschäftigung"),
    size = 1.3
  ) +
  geom_point(
    aes(y = frauen_mean, color = "Frauenbeschäftigung"),
    size = 2.8
  ) +
  
  scale_y_continuous(
    name   = "Frauenbeschäftigung (%)",
    limits = range_left,      # ⭐ 和双轴图同一范围
    breaks = breaks_left      # ⭐ 和双轴图同一刻度
  ) +
  
  scale_x_continuous(
    breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021, 2024)
  ) +
  
  scale_color_manual(
    name   = NULL,
    values = c("Frauenbeschäftigung" = "#0072B2")
  ) +
  
  labs(
    x = "",
    title = ""
  ) +
  
  theme_bw(base_size = 13) +
  theme(
    axis.title.y    = element_text(size = 20, face = "bold"),
    axis.text.y     = element_text(size = 16),
    axis.text.x     = element_text(size = 16),
    legend.position = "top",
    legend.text     = element_text(size = 18),
    legend.key.size = unit(1.2, "cm")
  )


p_frauen


saveRDS(p_frauen, "results/figures/NEW_Kinderbetreuung/trend_sv.rds")
