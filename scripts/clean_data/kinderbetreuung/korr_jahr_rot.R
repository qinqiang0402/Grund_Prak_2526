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


library(tidyverse)
library(RColorBrewer)
library(grid)

#----------------------------------------------------------
# 1. 先用同一套数据构造 korrelations_daten_clean
#    （和前面两张相关图保持一致）
#----------------------------------------------------------
korrelations_daten_clean <- df_merge %>%
  mutate(
    hmk    = anteil_betreut,        # x: Kinderbetreuung (%)
    anteil = 100 * sozial_weiblich  # y: Frauenbeschäftigung (%)
  ) %>%
  filter(
    Raumbezug != "Stadt München",
    Jahr >= 2007,
    Jahr <= 2024
  ) %>%
  select(Jahr, Raumbezug, hmk, anteil)

#----------------------------------------------------------
# 2. 统一的坐标轴范围和刻度（和前面两张保持一模一样）
#----------------------------------------------------------
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

#----------------------------------------------------------
# 3. 构造“按年份”的画图数据
#----------------------------------------------------------
plot_data_final_year <- korrelations_daten_clean %>%
  mutate(
    Jahr       = as.integer(Jahr),
    Jahr_Label = factor(Jahr),
    Jahr_num   = as.numeric(Jahr)
  ) %>%
  arrange(Jahr)

#----------------------------------------------------------
# 4. 画图：每一年一条彩色回归线 + 黑色总体线
#----------------------------------------------------------
ki_korr_jahr_multiline <- ggplot(
  plot_data_final_year,
  aes(x = hmk, y = anteil)
) +
  # 灰色散点（底层）
  geom_point(
    color = "grey90",
    size  = 1.2,
    alpha = 0.5
  ) +
  # 每一年一条彩色回归线
  geom_smooth(
    aes(color = Jahr_num, group = Jahr_Label),
    method    = "lm",
    se        = FALSE,
    linewidth = 1.1,
    alpha     = 0.85
  ) +
  # 总体回归线（黑色）
  geom_smooth(
    aes(group = 1),
    method    = "lm",
    se        = FALSE,
    linewidth = 1.2,
    color     = "black"
  ) +
  # 统一 x / y 轴（和另外两张图完全一样）
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
  # 年份颜色渐变
  scale_color_gradient(
    name   = "Jahr",
    limits = c(min(plot_data_final_year$Jahr),
               max(plot_data_final_year$Jahr)),
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

saveRDS(ki_korr_jahr_multiline, "results/figures/NEW_Kinderbetreuung/ki_point_line_rot.rds")
