# ==============================================================================
# 全局环境配置（解决编码/系统兼容性问题）
# ==============================================================================
# 强制设置UTF-8编码（Windows/Mac/Linux通用）
if (.Platform$OS.type == "windows") {
  Sys.setlocale(category = "LC_ALL", locale = "English_United States.utf8")
} else {
  Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")
}
options(encoding = "UTF-8")

# ==============================================================================
# 加载所需包
# ==============================================================================
library(dplyr)
library(sf)
library(tidyverse)
library(readxl)
library(ggplot2)
library(stringr)  # 显式加载字符串处理包

# ==============================================================================
# 1. 读取空间数据（慕尼黑行政区地图）
# ==============================================================================
munich_map <- st_read("results/geo/bezirk_map.gpkg", quiet = TRUE) |>
  st_transform(4326) |>
  # 提前清理空间数据中的字符编码
  mutate(
    name = enc2utf8(name),
    name = iconv(name, from = "", to = "UTF-8", sub = ""),
    name = gsub("[[:cntrl:]]", "", name)
  )

# ==============================================================================
# 2. 读取Excel数据并预处理
# ==============================================================================
# 读取数据（优先用工作表序号，避免名称编码问题）
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = 2)  # BEVÖLKERUNG
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")
ki_sheet <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

# ------------------------------------------------------------------------------
# 2.1 处理「有孩子家庭占比」数据
# ------------------------------------------------------------------------------
hh_data_long <- be_sheet %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt",
    Raumbezug != "Stadt München"
  ) %>%
  # 核心修复：清理字符串 + 稳定提取行政区编号
  mutate(
    # 强制转UTF-8并清理无效字符
    Raumbezug = enc2utf8(Raumbezug),
    Raumbezug = iconv(Raumbezug, from = "", to = "UTF-8", sub = ""),
    Raumbezug = gsub("[[:cntrl:]]", "", Raumbezug),
    # 提取行政区编号（替代str_extract，更稳定）
    bezirksnummer = substr(Raumbezug, 1, 2),          # 截取前2位
    bezirksnummer = str_replace_all(bezirksnummer, "\\D", ""),  # 只保留数字
    bezirksnummer = as.numeric(bezirksnummer),
    bezirksnummer = sprintf("%02d", bezirksnummer),   # 补0成2位
    # 计算占比
    anteil_kinder = 100 * `Basiswert 1` / `Basiswert 2`
  ) %>%
  filter(!is.na(bezirksnummer)) %>%  # 过滤无效编号
  select(Jahr, bezirksnummer, anteil_kinder)

# ------------------------------------------------------------------------------
# 2.2 处理「女性就业率」数据
# ------------------------------------------------------------------------------
ar_data_long <- ar_sheet %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(
    # 同样清理字符串
    Raumbezug = enc2utf8(Raumbezug),
    Raumbezug = iconv(Raumbezug, from = "", to = "UTF-8", sub = ""),
    Raumbezug = gsub("[[:cntrl:]]", "", Raumbezug),
    # 提取行政区编号
    bezirksnummer = substr(Raumbezug, 1, 2),
    bezirksnummer = str_replace_all(bezirksnummer, "\\D", ""),
    bezirksnummer = as.numeric(bezirksnummer),
    bezirksnummer = sprintf("%02d", bezirksnummer),
    # 计算占比
    anteil = 100 * `Basiswert 1` / `Basiswert 2`
  ) %>%
  filter(!is.na(bezirksnummer)) %>%
  select(Jahr, bezirksnummer, anteil)

# ==============================================================================
# 3. 合并数据 + 关联空间几何
# ==============================================================================
# 合并两个指标数据
combined_data_all <- hh_data_long %>%
  full_join(ar_data_long, by = c("Jahr", "bezirksnummer")) %>%
  mutate(Jahr = as.numeric(Jahr))  # 统一年份为数值型

# 关联空间数据
final_sf_data_long <- munich_map %>%
  left_join(
    combined_data_all,
    by = c("sb_nummer" = "bezirksnummer"),
    relationship = "many-to-many"
  ) %>%
  filter(!is.na(Jahr))  # 过滤无年份数据的行

# ==============================================================================
# 4. 筛选2024年数据并分类
# ==============================================================================
data_2024 <- final_sf_data_long %>%
  filter(Jahr == 2024) %>%
  select(
    sb_nummer,
    name,
    anteil_kinder,   # 有孩子家庭占比 (%)
    anteil           # 女性就业率 (%)
  ) %>%
  sf::st_as_sf()  # 确保是sf对象

# 计算全市均值（用于分类）
mean_HaKi_city <- mean(data_2024$anteil_kinder, na.rm = TRUE)
mean_FE_city <- mean(data_2024$anteil, na.rm = TRUE)

# 按条件分类
data_2024 <- data_2024 %>%
  mutate(
    gruppe = dplyr::case_when(
      anteil_kinder > mean_HaKi_city & anteil < mean_FE_city ~
        "hohe Haushalte mit Kindern + niedrige Beschäftigung",
      TRUE ~ "Andere"
    )
  )

# ==============================================================================
# 5. 绘制ggplot2静态地图（最终样式调整）
library(ggplot2)
library(sf)
library(dplyr)

# 1. 图例标签（不含 “Bezirke”）
data_2024_plot <- data_2024 %>%
  mutate(
    gruppe_lab = if_else(
      gruppe == "hohe Haushalte mit Kindern + niedrige Beschäftigung",
      "Haushalte mit Kindern hoch\nund Frauenbeschäftigung niedrig",   # 粉色
      "Andere"                                                          # 灰色（不含 Bezirke）
    )
  )

# 2. 绘图
m_effekt_04_plot <- ggplot(data_2024_plot) +
  geom_sf(
    aes(fill = gruppe_lab),
    color = "white",
    size  = 0.4,
    alpha = 0.8
  ) +
  scale_fill_manual(
    values = c(
      "Haushalte mit Kindern hoch\nund Frauenbeschäftigung niedrig" = "#e75480",  # 粉色（左）
      "Andere"                                                      = "#d9d9d9"   # 灰色（右）
    ),
    breaks = c(   # 控制顺序：粉色 → 灰色
      "Haushalte mit Kindern hoch\nund Frauenbeschäftigung niedrig",
      "Andere"
    )
  ) +
  labs(fill = NULL) +
  theme_void(base_size = 14) +
  theme(
    legend.position   = "bottom",
    legend.text       = element_text(size = 12),
    legend.key.size   = unit(1, "cm"),
    legend.background = element_blank(),
    plot.caption      = element_blank()
  )

m_effekt_04_plot
