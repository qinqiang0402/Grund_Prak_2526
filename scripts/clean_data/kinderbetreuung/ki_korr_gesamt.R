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



#----------------------------------------------------------
# 1. 构造相关性数据：Kinderbetreuung (%) vs. Frauenbeschäftigung (%)
#----------------------------------------------------------

#----------------------------------------------------------
# 1. 构造相关性数据：Kinderbetreuung (%) vs. Frauenbeschäftigung (%)
#----------------------------------------------------------

korrelations_daten_clean <- df_merge %>%
  mutate(
    hmk    = anteil_betreut,       # x 轴：Kinderbetreuung 0–2 Jahre (%)
    anteil = 100 * sozial_weiblich # y 轴：Frauenbeschäftigung (%)
  ) %>%
  filter(
    Raumbezug != "Stadt München",
    Jahr >= 2007,
    Jahr <= 2024
  ) %>%
  select(Jahr, Raumbezug, hmk, anteil)

#----------------------------------------------------------
# 2. 统一的坐标轴范围和刻度
#----------------------------------------------------------

# 用全部数据算一次范围和 pretty 刻度，然后两个图都用这组
x_breaks <- pretty(korrelations_daten_clean$hmk,    n = 5)
y_breaks <- pretty(korrelations_daten_clean$anteil, n = 5)

x_limits <- range(x_breaks)
y_limits <- range(y_breaks)

# 统一的 theme
base_theme_corr <- theme_bw(base_size = 13) +
  theme(
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 16)
  )

#----------------------------------------------------------
# 3. Korrelation (gesamt) —— 只有散点 + 一条黑线
#----------------------------------------------------------

ki_korr_gesamt_sw <- ggplot(korrelations_daten_clean,
                            aes(x = hmk, y = anteil)) +
  geom_point(size = 1.3, color = "grey85", alpha = 0.7) +
  geom_smooth(method = "lm", color = "black",
              se = FALSE, linewidth = 1.2) +
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

ki_korr_gesamt_sw
saveRDS(ki_korr_gesamt_sw,
        "results/figures/NEW_Kinderbetreuung/ki_point_korr_gesamt_sw.rds")

#----------------------------------------------------------
# 4. Korrelation (Stadtteile) —— 在上面基础上 + 每个 Bezirk 的灰线
#----------------------------------------------------------

korr_stadtteile_data <- korrelations_daten_clean %>%
  group_by(Raumbezug) %>%
  filter(n() >= 2) %>%
  ungroup()

ki_korr_stadtteile_sw <- ggplot(korr_stadtteile_data,
                                aes(x = hmk, y = anteil)) +
  # 底层散点（和 Gesamt 图完全一样）
  geom_point(color = "grey90", size = 1.3, alpha = 0.7) +
  
  # 每个 Stadtbezirk 的回归线（灰色）
  geom_smooth(
    aes(group = Raumbezug),
    method    = "lm",
    se        = FALSE,
    color     = "grey65",
    linewidth = 0.9,
    alpha     = 0.9
  ) +
  
  # Gesamt 回归线（黑色，和上一张一样）
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

ki_korr_stadtteile_sw
saveRDS(ki_korr_stadtteile_sw,
        "results/figures/NEW_Kinderbetreuung/ki_korr_stadtteile_point_line_sw.rds")
