# ============================================================
# 构建 Kinderbetreuung & Frauenbeschäftigung 双轴趋势图
# 保存为：results/figures/NEW_Kinderbetreuung/ki_dual_trend.rds
# 之后在 Quarto 里只需要 readRDS() + 打印即可
# ============================================================

library(tidyverse)
library(readxl)
library(stringr)
library(grid)

# 小工具：从 "Raumbezug" 里提取两位数 Bezirk 编号
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

#----------------------------------------------------------
# 1. 读原始数据
#----------------------------------------------------------
ar <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")
be <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
ki <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

# Frauenbeschäftigung (Anteil weiblich，0~1)
sozial_anteil_weiblich <- ar %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(sozial_weiblich = `Basiswert 1` / `Basiswert 2`) %>%
  select(Jahr, Raumbezug, sozial_weiblich)

# Kinderbetreuung 0–2 Jahre（百分比）
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
    anteil_betreut = 100 * kinder_betreut / kinder_total
  )

# 只保留有 Bezirk 编号的行
df_betreut_bezirk <- df_betreut %>%
  add_sb() %>%
  filter(!is.na(sb))

# 合并 Kinderbetreuung + Frauenbeschäftigung 到 Bezirk × Jahr
df_merge <- df_betreut_bezirk %>%
  filter(Jahr >= 2007, Jahr <= 2024) %>%
  select(Jahr, sb, Raumbezug, anteil_betreut) %>%
  left_join(
    sozial_anteil_weiblich %>%
      add_sb() %>%
      filter(Jahr >= 2007, Jahr <= 2024) %>%
      select(Jahr, sb, sozial_weiblich),
    by = c("Jahr", "sb")
  ) %>%
  arrange(sb, Jahr)

#----------------------------------------------------------
# 2. 按年份取 25 个 Bezirk 的平均值 → 时间序列
#----------------------------------------------------------
ts_dual <- df_merge %>%
  group_by(Jahr) %>%
  summarise(
    frauen_mean = mean(sozial_weiblich * 100, na.rm = TRUE),  # 左轴：Frauenbeschäftigung (%)
    kinder_mean = mean(anteil_betreut,       na.rm = TRUE),  # 右轴：Kinderbetreuung (%)
    .groups = "drop"
  )

#----------------------------------------------------------
# 3. 计算双轴缩放参数（注意：全部变成“字面常数”）
#----------------------------------------------------------
left_min  <- min(ts_dual$frauen_mean, na.rm = TRUE)
left_max  <- max(ts_dual$frauen_mean, na.rm = TRUE)
right_min <- min(ts_dual$kinder_mean, na.rm = TRUE)
right_max <- max(ts_dual$kinder_mean, na.rm = TRUE)

scale_factor <- (left_max - left_min) / (right_max - right_min)

# 把 Kinderbetreuung 映射到左轴坐标系（只用于画图）
ts_dual <- ts_dual %>%
  mutate(
    kinder_scaled = (kinder_mean - right_min) * scale_factor + left_min
  )

# 左轴刻度（自动）
breaks_left  <- pretty(c(left_min, left_max), n = 5)

# 右轴刻度：按你之前的要求
breaks_right <- c(20, 25, 30, 35, 40)

# ⭐⭐ 关键：构造一个“完全不依赖外部变量名”的公式
sec_trans <- eval(substitute(
  ~ (. - L) / S + R,
  list(L = left_min, S = scale_factor, R = right_min)
))
# 此时 formula 变成类似：~ (. - 27.3) / 1.5 + 20.1
# 里面只有数字，没有 left_min / scale_factor / right_min 这些符号

#----------------------------------------------------------
# 4. 画双轨图（不再引用 left_min 这些名字）
#----------------------------------------------------------
ki_dual_trend <- ggplot(ts_dual, aes(x = Jahr)) +
  
  # 左轴：Frauenbeschäftigung（蓝色）
  geom_line(aes(y = frauen_mean, color = "Frauenbeschäftigung"), size = 1.2) +
  geom_point(aes(y = frauen_mean, color = "Frauenbeschäftigung"), size = 2) +
  
  # 右轴：Kinderbetreuung（橙色，已缩放到左轴）
  geom_line(aes(y = kinder_scaled, color = "Kinderbetreuung"), size = 1.2) +
  geom_point(aes(y = kinder_scaled, color = "Kinderbetreuung"), size = 2) +
  
  scale_y_continuous(
    name   = "Frauenbeschäftigung (%)",
    limits = c(left_min, left_max),
    breaks = breaks_left,
    sec.axis = sec_axis(
      trans  = sec_trans,           # ✅ 这里的公式里只有字面量数字
      name   = "Kinderbetreuung (%)",
      breaks = breaks_right
    )
  ) +
  
  scale_color_manual(
    name   = NULL,
    values = c(
      "Frauenbeschäftigung" = "#0072B2",  # 蓝
      "Kinderbetreuung"     = "#D55E00"   # 橙
    )
  ) +
  
  scale_x_continuous(
    breaks = c(2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021, 2024)
  ) +
  
  labs(
    x     = "",
    title = ""
  ) +
  
  theme_bw(base_size = 13) +
  theme(
    legend.position    = "top",
    axis.title.y.left  = element_text(size = 20, face = "bold"),
    axis.title.y.right = element_text(size = 20, face = "bold"),
    axis.text.x        = element_text(size = 16),
    axis.text.y.left   = element_text(size = 16),
    axis.text.y.right  = element_text(size = 16),
    legend.text        = element_text(size = 18),
    legend.key.size    = unit(1.2, "cm")
  )



#----------------------------------------------------------
# 5. 保存为 RDS（Quarto 里直接 readRDS() 使用）
#----------------------------------------------------------
saveRDS(ki_dual_trend, "results/figures/NEW_Kinderbetreuung/ki_dual_trend.rds")

# 可选：在脚本里直接查看
ki_dual_trend

