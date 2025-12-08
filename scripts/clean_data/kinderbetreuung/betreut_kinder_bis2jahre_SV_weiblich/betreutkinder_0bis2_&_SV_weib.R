library(tidyverse)
library(readxl)
library(sf)
library(stringr)
library(forcats)
library(ggpubr)
library(grid)







#──────────────────────── 0. 帮助函数 ────────────────────────

# 给 Raumbezug / sb_nummer 统一生成两位数 Bezirk-Code
add_sb <- function(x, var = "Raumbezug", new = "sb") {
  x %>%
    mutate(
      !!new := str_pad(str_extract(.data[[var]], "^[0-9]+"), 2, pad = "0")
    )
}

# 画统一风格的 Stadtbezirk 地图
# 画统一风格的 Stadtbezirk 地图
plot_bezirk_map <- function(map_df, value_col, title, subtitle,
                            legend_title, limits = NULL,
                            low_col  = "#f7fbff",   # 新增：低值颜色
                            high_col = "#08306b") { # 新增：高值颜色
  
  ggplot(map_df) +
    geom_sf(aes(fill = .data[[value_col]]),
            color = "white", linewidth = 0.25, na.rm = TRUE) +
    coord_sf(datum = NA) +
    scale_fill_gradient(
      low   = low_col,         # 用参数
      high  = high_col,        # 用参数
      name  = legend_title,
      limits = limits,
      na.value = "grey90",
      breaks   = seq(limits[1], limits[2], by = 10),
      guide = guide_colorbar(              # ⭐ 这里控制色条和标题
        title.position = "top",            # 标题在上面
        title.hjust    = 0.5,              # 标题居中
        barwidth       = unit(8, "cm"),    # 色条宽度
        barheight      = unit(0.8, "cm")  # 色条高度
      )
    ) +
    labs(
      title    = title,
      subtitle = subtitle,
      caption  = ""
    ) +
    theme_void(base_size = 12) +
    theme(
      legend.position    = "bottom",   # Legend 在图下方
      legend.title.align = 0.5,        # 标题文字居中
      legend.title = element_text(size = 26, face = "bold"),
      legend.text  = element_text(size = 26),
      plot.title = element_text(face = "bold", size = 26, hjust = 0.5)
    )
}


# 画“每 Bezirk 的相关系数”条形图
plot_corr_by_bezirk <- function(corr_df) {
  ggplot(corr_df,
         aes(x = corr,
             y = fct_reorder(bezirk, corr))) +
    geom_col(fill = "#1f77b4") +
    geom_vline(xintercept = 0, color = "grey40") +
    labs(
      title    = "Korrelation pro Stadtteile (2007–2024)",
      subtitle = "Frauenbeschäftigung und Kinderbetreuung (0–2)",
      x        = "Korrelationskoeffizient",
      y        = "Stadtteile"
    ) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank())
}

# 画“每年的空间相关系数”折线图
plot_corr_by_year <- function(corr_df) {
  ggplot(corr_df, aes(x = Jahr, y = corr)) +
    geom_line(color = "#08306b", linewidth = 1.3) +
    geom_point(color = "#08306b", size = 2) +
    geom_hline(yintercept = 0, color = "grey50", linetype = "dashed") +
    scale_x_continuous(breaks = seq(2007,2024, by = 2), limits = c(2007, 2024)) +
    labs(
      title    = "Jährliche Korrelation über alle Stadtteile",
      subtitle = "Frauenbeschäftigungsquote und Kinderbetreuungsquote (0–2 Jahre)",
      x        = "Jahr",
      y        = "Korrelationskoeffizient"
    ) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank())
}



#──────────────────────── 1. 读数据 & 基础整理 ────────────────────────

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

#——————————————————————————————————————————————————————————————————————————




# (A) Non-enrolled 0–2 share, weighted by number of children
# -----------------------------
# 1) 数据
# -----------------------------
enrolled_year <- df_betreut %>%
  filter(Jahr >= 2007, Jahr <= 2024) %>%
  group_by(Jahr) %>%
  summarise(
    share_enrolled = sum(kinder_betreut, na.rm = TRUE) / 
      sum(kinder_total,    na.rm = TRUE) * 100,
    .groups = "drop"
  )

emp_year <- sozial_anteil_weiblich %>%
  filter(Jahr >= 2007, Jahr <= 2024, Raumbezug == "Stadt München") %>%
  transmute(Jahr, emp_women = sozial_weiblich * 100)

ts_dual <- enrolled_year %>%
  inner_join(emp_year, by = "Jahr")

# -----------------------------
# 2) scaling（这次是把 emp_women 缩放到 Kinderbetreuung 那边）
# -----------------------------
range_left  <- range(ts_dual$emp_women,      na.rm = TRUE)  # 左轴：Frauenbeschäftigung
range_right <- range(ts_dual$share_enrolled, na.rm = TRUE)  # 右轴：Kinderbetreuung

scale_factor <- (range_left[2] - range_left[1]) / 
  (range_right[2] - range_right[1])

ts_dual <- ts_dual %>%
  mutate(
    share_scaled = (share_enrolled - range_right[1]) * scale_factor + range_left[1]
  )

# -----------------------------
# 3) Plot （左右 y 轴已对调）
# -----------------------------
ki_dual_trend <- ggplot(ts_dual, aes(x = Jahr)) +
  
  # 左轴：Frauenbeschäftigung (blue)
  geom_line(aes(y = emp_women, color = "Frauenbeschäftigung"), size = 1.2) +
  geom_point(aes(y = emp_women, color = "Frauenbeschäftigung"), size = 2) +
  
  # 右轴：Kinderbetreuung (red) —— 使用 share_scaled
  geom_line(aes(y = share_scaled, color = "Kinderbetreuung"), size = 1.2) +
  geom_point(aes(y = share_scaled, color = "Kinderbetreuung"), size = 2) +
  
  # 左轴 = Frauenbeschäftigung；右轴 = Kinderbetreuung
  scale_y_continuous(
    name = "Frauenbeschäftigung (%)",
    sec.axis = sec_axis(
      ~ (. - range_left[1]) / scale_factor + range_right[1],
      name = "Kinderbetreuung (%)"
    )
  ) +
  
  scale_x_continuous(breaks = c(2007, 2011, 2015, 2019, 2022, 2024)) +
  
  scale_color_manual(
    name = NULL,
    values = c(
      "Kinderbetreuung"     = "#d62728",
      "Frauenbeschäftigung" = "#1f77b4"
    )
  ) +
  
  labs(x = "Jahr", title = "", subtitle = "") +
  theme_bw(base_size = 13) +
  theme(
    legend.position   = "top",
    panel.grid.minor  = element_blank(),
    axis.title.y.right = element_text(margin = margin(l = 0)),
    axis.text.y.right  = element_text(margin = margin(l = 0)),
    axis.ticks.length.y.right = unit(0, "pt")
    
  )

ki_dual_trend

saveRDS(ki_dual_trend, "results/figures/Kinderbetreuung/ki_dual_trend.rds")




#──────────────────────── 2. 图 1：双轴时间趋势 ────────────────────────

library(scales)

p_trend_kita <- ggplot(enrolled_year, aes(x = Jahr, y = share_enrolled)) +
  geom_line(color = "#7f2704", linewidth = 1.3) +
  geom_point(color = "#7f2704", size = 2) +
  scale_y_continuous(
    labels = percent_format(scale = 1),   # 直接用百分比标注，比如 20%
    name   = "Betreuungsanteil (0–2 Jahre) [%]"
  ) +
  labs(
    title    = "Trend der Kinderbetreuung (0–2 Jahre) in München (2007–2024)",
    subtitle = "Stadtweit gewichteter Durchschnittsanteil der betreuten 0–2-Jährigen",
    x        = "Jahr"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank()
  )

p_trend_kita

#saveRDS(p_trend_kita, "results/figures/Kinderbetreuung/trend_ki.rds")

#__________________________________________________________________________________

p_trend_emp <- ggplot(emp_year, aes(x = Jahr, y = emp_women)) +
  
  # 把颜色映射写进 aes()
  geom_line(aes(color = "Frauenbeschäftigung"), linewidth = 1.3) +
  geom_point(aes(color = "Frauenbeschäftigung"), size = 2) +
  
  scale_y_continuous(
    name   = "Frauenbeschäftigung (%)"
  ) +
  
  scale_x_continuous(breaks = c(2007, 2011, 2015, 2019, 2022, 2024)) +
  
  # ⭐ 手动指定图例颜色
  scale_color_manual(
    name = NULL,
    values = c("Frauenbeschäftigung" = "#1f77b4")
  ) +
  
  labs(x = "Jahr", title = "", subtitle = "") +
  
  theme_bw(base_size = 13) +
  theme(
    legend.position = "top",       # ⭐ 把 legend 放到顶部
    panel.grid.minor = element_blank(),
    axis.title.y.right = element_text(margin = margin(l = 0)),
    axis.text.y.right  = element_text(margin = margin(l = 0)),
    axis.ticks.length.y.right = unit(0, "pt")
  )

p_trend_emp

saveRDS(p_trend_emp, "results/figures/Kinderbetreuung/trend_sv.rds")


#──────────────────────── 3. 图 2 & 3：2015 年两张地图 ─────────────────────

year_map <- 2015

# 2015 Betreuungsquote 地图
map_betreuung_2015 <- munich_map2 %>%
  left_join(
    df_betreut_bezirk %>%
      filter(Jahr == year_map) %>%
      transmute(sb, betreuungsquote = anteil_betreut),
    by = c("sb_nummer" = "sb")
  )

p_map_betreuung <- plot_bezirk_map(
  map_betreuung_2015,
  value_col    = "betreuungsquote",
  title        = paste0("Kinderbetreuung - ", year_map),
  subtitle     = "",
  legend_title = "Kinderbetreuung (%)",
  limits       = c(10, 55),
  low_col      = "#fff5eb",  # 很浅的橙/米色
  high_col     = "#7f2704"   # 深橙/棕色
)
p_map_betreuung

saveRDS(p_map_betreuung, "results/figures/Kinderbetreuung/map_ki_2015.rds")

# ggsave("fig/map_betreuung_2015.png", p_map_betreuung, width = 7.5, height = 6.8, dpi = 300)

# 2015 weibliche SV-Beschäftigung 地图
soz_bezirk <- sozial_anteil_weiblich %>%
  add_sb() %>%
  filter(!is.na(sb)) %>%
  mutate(sozial_weiblich_pct = 100 * sozial_weiblich)

map_sozial_2015 <- munich_map2 %>%
  left_join(
    soz_bezirk %>%
      filter(Jahr == year_map) %>%
      select(sb, sozial_weiblich_pct),
    by = c("sb_nummer" = "sb")
  )

p_map_sozial <- plot_bezirk_map(
  map_sozial_2015,
  value_col    = "sozial_weiblich_pct",
  title        = paste0("Frauenbeschäftigung - ", year_map),
  subtitle     = "",
  legend_title = "Frauenbeschäftigung (%)",
  limits       = c(50, 62),
  low_col      = "#f7fbff",  # 浅蓝
  high_col     = "#08306b"   # 深蓝
)

p_map_sozial

saveRDS(p_map_sozial, "results/figures/Kinderbetreuung/map_sv_2015.rds")

# ggsave("fig/map_sozial_weiblich_2015.png", p_map_sozial, width = 7.5, height = 6.8, dpi = 300)

#──────────────────────── 4. 图 4：按 Bezirk 的时间相关性 ──────────────────

corr_bezirk <- df_merge %>%
  group_by(sb) %>%
  summarise(
    corr = if (sum(complete.cases(anteil_betreut, sozial_weiblich)) > 1)
      cor(anteil_betreut, sozial_weiblich, use = "complete.obs")
    else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    bezirk_label = paste0(row_number())   # ★ 匿名标签：B1, B2, B3…
  )


p_corr_stadtteile <- ggplot(corr_bezirk,
                        aes(x = corr,
                            y = fct_reorder(bezirk_label, corr))) +
  geom_col(fill = "#1f77b4") +
  geom_vline(xintercept = 0, color = "grey40") +
  labs(
    title    = "Korrelation pro Stadteile",
    subtitle = "Kinderbetreuung (0–2 Jahre) vs. Frauenbeschäftigung",
    x        = "Korrelationskoeffizient",
    y        = "Stadtteile"
  ) +
  theme_minimal(base_size = 13) +
  scale_y_discrete(labels = NULL) +
  theme(
    panel.grid.minor = element_blank()
  )

p_corr_stadtteile

#saveRDS(p_corr_stadtteile, "results/figures/Kinderbetreuung/korr_zeitlich.rds")
# ggsave("fig/corr_by_bezirk.png", p_corr_bezirk, width = 9, height = 6, dpi = 300)

#──────────────────────── 5. 图 5：按 Jahr 的空间相关性 ─────────────────────

corr_year <- df_merge %>%
  group_by(Jahr) %>%
  summarise(
    corr = if (sum(complete.cases(anteil_betreut, sozial_weiblich)) > 1)
      cor(anteil_betreut, sozial_weiblich, use = "complete.obs")
    else NA_real_,
    .groups = "drop"
  )

p_corr_year <- plot_corr_by_year(corr_year)
p_corr_year

#saveRDS(p_corr_year, "results/figures/Kinderbetreuung/korr_raumlich.rds")


# ggsave("fig/corr_by_year.png", p_corr_year, width = 9, height = 4.5, dpi = 300)



#──────────────────────── 6. Korrelation: Kinderbetreuung (0–2) vs. Frauenbeschäftigung ────────────────────────
# 目标：构造一个 korrelations_daten_clean，让 x=hmk 实际上是 Betreuungsquote (0–2 Jahre)

# 1) 用 Kinderbetreuung 替代 hmk
#    这里故意把列名叫做 hmk，这样下面的 ggplot 代码完全不用改
all_districts_and_city <- df_betreut %>%
  select(
    Jahr,
    Raumbezug,
    hmk = anteil_betreut   # 注意：这里的 hmk 现在代表 Betreuungsquote 0–2 Jahre (%)
  )

# 2) Frauenbeschäftigung-Anteil (保持列名 anteil 不变)
Sozialversicherungspflichtig_Beschäftigte_anteil_frau <- sozial_anteil_weiblich %>%
  mutate(
    anteil = 100 * sozial_weiblich   # sozial_weiblich 是 0–1，这里转成百分比
  ) %>%
  select(Jahr, Raumbezug, anteil)

# 3) 像原来一样 inner_join + 去掉 Gesamtstadt
korrelations_daten <- inner_join(
  all_districts_and_city, 
  Sozialversicherungspflichtig_Beschäftigte_anteil_frau, 
  by = c("Raumbezug", "Jahr")
)

korrelations_daten_clean <- korrelations_daten %>%
  filter(Raumbezug != "Stadt München",
         Jahr >= 2007,
         Jahr <= 2024)



#_________________________________________________________________________________-


ki_korr_gesamt_sw <- ggplot(korrelations_daten_clean, aes(x = hmk, y = anteil)) +
  geom_point(size = 1.3, color = "grey90", alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1) +
  labs(
    title = "",
    x = "Kinderbetreuung (%)",
    y = "Frauenbeschäftigung (%)"
  ) +
  theme_bw()

ki_korr_gesamt_sw

saveRDS(ki_korr_gesamt_sw, "results/figures/Kinderbetreuung/ki_point_korr_gesamt_sw.rds")


# ----------------------------------------------------------------------------------
ki_year_color_point <- ggplot(korrelations_daten_clean, aes(x = hmk, y = anteil)) +
  geom_point(aes(color = factor(Jahr)), 
             size = 1.3,   
             alpha = 0.7) +   
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1) +
  stat_cor(
    label.x.npc = "left",   
    label.y.npc = "top",    
    color = "black",
    size = 5
  ) + 
  coord_cartesian(xlim = c(8, 28), ylim = c(48, 68)) +
  labs(
    title = "Korrelationskoeffizient nach Jahren zwischen Kinderbetreuung und Frauenbeschäftigung",
    x = "Kinderbetreuung (%)",
    y = "Frauenbeschäftigung (%)",
    color = "Jahr"
  ) +
  guides(color = guide_legend(ncol = 1)) + 
  theme_minimal()

ki_year_color_point

#saveRDS(ki_year_color_point, "results/figures/Kinderbetreuung/ki_point_year_color.rds")

# ... 后面那几段按 Jahr 算 r、画多条 lm 线、highlight 负相关年份的代码，也都可以不改



#——————————————————————————————————————————————————————————————————————————————————————————————


# 按 Jahr 计算相关系数 r，并拼到每一行上
plot_data_final_year <- korrelations_daten_clean %>%
  group_by(Jahr) %>% 
  mutate(
    n_complete = sum(complete.cases(hmk, anteil)),
    r = ifelse(
      n_complete > 1,
      cor(hmk, anteil, use = "complete.obs"),
      NA_real_
    ),
    r_label   = sprintf("(R=%.2f)", r),
    Jahr_Label = paste(Jahr, r_label, sep = " ")
  ) %>%
  ungroup()

# 给不同 Jahr_Label 分配颜色
all_labels_year  <- sort(unique(plot_data_final_year$Jahr_Label))
all_colors_year  <- scales::hue_pal()(length(all_labels_year))
jahr_palette     <- all_colors_year
names(jahr_palette) <- all_labels_year
#————————————————————————————
library(dplyr)
library(RColorBrewer)
library(grid)   

# 1. 保证 Jahr 是数字，方便排序
plot_data_final_year <- plot_data_final_year %>%
  mutate(Jahr = as.integer(Jahr))

# 2. 按 Jahr 排序 Jahr_Label，让 2007 在最上，2024 在最后
plot_data_final_year <- plot_data_final_year %>%
  arrange(Jahr) %>%
  mutate(
    Jahr_Label = factor(Jahr_Label, levels = unique(Jahr_Label))
  )

# 3. 生成红色从浅到深的调色板
n_years <- length(levels(plot_data_final_year$Jahr_Label))  # 应该是 18 年
jahr_palette <- colorRampPalette(brewer.pal(9, "Reds"))(n_years)


#————————————————————————————
# 画图：每年一条彩色回归线 + 黑色总体回归线
# 年份映射成连续变量，用于渐变色
plot_data_final_year <- plot_data_final_year %>%
  mutate(Jahr_num = as.numeric(as.character(Jahr)))

ki_point_line_color <- ggplot(
  plot_data_final_year,
  aes(x = hmk, y = anteil)
) +
  
  # ⭐ 把散点放到最底层（第一层）
  geom_point(
    color = "grey90",
    size  = 1.1,
    alpha = 0.5
  ) +
  
  # 年份回归线（在点上面）
  geom_smooth(
    aes(color = Jahr_num, group = Jahr_Label),
    method    = "lm",
    se        = FALSE,
    linewidth = 1.1,
    alpha     = 0.8
  ) +
  
  # 总体回归线（最上层）
  geom_smooth(
    aes(group = 1),
    method    = "lm",
    color     = "black",
    linewidth = 1.1,
    se        = FALSE
  ) +
  
  # 连续色标
  scale_color_gradient(
    name   = "Jahr",
    limits = c(2007, 2024),
    breaks = c(2007, 2011, 2015, 2020, 2024),
    labels = c("2007", "2011", "2015", "2020", "2024"),
    low    = "#fee5e5",
    high   = "#990000",
    guide  = guide_colorbar(
      title.position = "top",
      barheight      = unit(6, "cm"),
      barwidth       = unit(0.4, "cm")
    )
  ) +
  
  labs(
    title = "",
    x     = "Kinderbetreuung (%)",
    y     = "Frauenbeschäftigung (%)"
  ) +
  
  theme_bw()


ki_point_line_color

saveRDS(ki_point_line_color, "results/figures/Kinderbetreuung/ki_point_line_color.rds")

#——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————


# 按 Jahr 计算 r，并标记正负
r_werte_check_year <- korrelations_daten_clean %>%
  group_by(Jahr) %>% 
  summarise(
    n_complete = sum(complete.cases(hmk, anteil)),
    r_wert = if (n_complete > 1)
      cor(hmk, anteil, use = "complete.obs")
    else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    is_negative = r_wert < 0,
    Jahr_Label  = paste0(Jahr, " (R=", round(r_wert, 2), ")")
  )

# 把 r 信息并回原数据
plot_data_highlight_year <- korrelations_daten_clean %>%
  left_join(r_werte_check_year, by = "Jahr") 

# 图：灰色 = R ≥ 0，彩色 = R < 0
p_non_simpson <- ggplot(plot_data_highlight_year, aes(x = hmk, y = anteil)) +
  geom_smooth(
    data = subset(plot_data_highlight_year, is_negative == FALSE | is.na(is_negative)),
    aes(group = Jahr), 
    method = "lm", 
    se = FALSE, 
    color = "grey85", 
    linewidth = 0.8,
    alpha = 0.5
  ) +
  geom_smooth(
    data = subset(plot_data_highlight_year, is_negative == TRUE),
    aes(color = Jahr_Label, group = Jahr), 
    method = "lm", 
    se = FALSE, 
    linewidth = 1.1, 
    alpha = 1
  ) +
  geom_smooth(
    aes(group = 1), 
    method = "lm", 
    color = "black", 
    linewidth = 1.1, 
    se = FALSE
  ) +
  scale_color_manual(values = jahr_palette) +
  labs(
    title = "non Simpson's Paradox (nach Jahr)",
    subtitle = "Grau = positiver Zusammenhang (R ≥ 0), Bunt = negativer Zusammenhang (R < 0)",
    x = "Kinderbetreuung (0–2) (%)",
    y = "Anteil Frauenbeschäftigung (%)",
    color = "Jahr mit R < 0" 
  ) +
  coord_cartesian(xlim = c(min(korrelations_daten_clean$hmk, na.rm = TRUE),
                           max(korrelations_daten_clean$hmk, na.rm = TRUE)),
                  ylim = c(min(korrelations_daten_clean$anteil, na.rm = TRUE),
                           max(korrelations_daten_clean$anteil, na.rm = TRUE))) +
  theme_minimal() +
  theme(legend.position = "none")


p_non_simpson

#saveRDS(p_non_simpson, "results/figures/Kinderbetreuung/ki_p_non_simpson.rds")



#————————————————————————————————————————————————————————————————————————————————————————————————


plot_data_colored_year <- korrelations_daten_clean %>%
  group_by(Jahr) %>%  
  mutate(
    n_complete = sum(complete.cases(hmk, anteil)),
    r_wert = ifelse(
      n_complete > 1,
      cor(hmk, anteil, use = "complete.obs"),
      NA_real_
    ),
    trend_richtung = ifelse(r_wert >= 0 | is.na(r_wert), "positiv", "negativ") 
  ) %>%
  ungroup() 

ki_non_simpson_blau <- ggplot(plot_data_colored_year, aes(x = hmk, y = anteil)) +
  geom_smooth(
    aes(
      color = trend_richtung, 
      group = Jahr            
    ), 
    method = "lm", 
    se = FALSE, 
    linewidth = 1.1,    
    alpha = 0.6          
  ) +
  geom_smooth(aes(group = 1),
              method = "lm",
              color = "black", linewidth = 1.1, se = FALSE) +
  scale_color_manual(values = c(
    "positiv" = "red",  
    "negativ" = "blue" 
  )) +
  labs(
    title = "non Simpson's Paradox (nach Jahr)",
    subtitle = "Rot = positiver Zusammenhang (R ≥ 0), Blau = negativer Zusammenhang (R < 0)",
    x = "Kinderbetreuung (0–2) (%)",
    y = "Anteil Frauenbeschäftigung (%)"
  ) +
  coord_cartesian(xlim = c(min(korrelations_daten_clean$hmk, na.rm = TRUE),
                           max(korrelations_daten_clean$hmk, na.rm = TRUE))) +
  theme_minimal() +
  theme(legend.position = "none")


ki_non_simpson_blau

#saveRDS(ki_non_simpson_blau, "results/figures/Kinderbetreuung/ki_non_simpson_blau.rds")

#——————————————————————————————————————————————————————————————————————————————————————————————



#── 1) Kinderbetreuung (0–2 Jahre) 作为 x 变量 ─────────────────────
# 前提：你已经在前面脚本里算过 df_betreut 和 sozial_anteil_weiblich
# df_betreut: Jahr, Raumbezug, anteil_betreut (%)
# sozial_anteil_weiblich: Jahr, Raumbezug, sozial_weiblich (0–1)

all_districts_and_city <- df_betreut %>%
  select(
    Jahr,
    Raumbezug,
    hmk = anteil_betreut   # 这里的 hmk = Kinderbetreuungsquote 0–2 Jahre (%)
  )

Sozialversicherungspflichtig_Beschäftigte_anteil_frau <- sozial_anteil_weiblich %>%
  mutate(
    anteil = 100 * sozial_weiblich      # 转成百分比
  ) %>%
  select(Jahr, Raumbezug, anteil)

korrelations_daten <- inner_join(
  all_districts_and_city,
  Sozialversicherungspflichtig_Beschäftigte_anteil_frau,
  by = c("Raumbezug", "Jahr")
)

korrelations_daten_clean <- korrelations_daten %>%
  filter(
    Raumbezug != "Stadt München",
    Jahr >= 2007, Jahr <= 2024           # 只用 2007–2024
  )

# 为后面的 coord_cartesian 计算一下范围（可选）
x_range <- range(korrelations_daten_clean$hmk, na.rm = TRUE)
y_range <- range(korrelations_daten_clean$anteil, na.rm = TRUE)



#————————————————————————————————————————————————————————————————————————————————————————



ki_korr_nach_stadtteile_sw <- ggplot(korrelations_daten_clean, aes(x = hmk, y = anteil)) +
  geom_point(size = 1.3, color = "grey90", alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1) +
  labs(
    title = "",
    x = "Kinderbetreuung (%)",
    y = "Frauenbeschäftigung (%)"
  ) +
  theme_bw()


ki_korr_nach_stadtteile_sw

saveRDS(ki_korr_nach_stadtteile_sw, "results/figures/Kinderbetreuung/ki_korr_point_nach_stadtteile_sw.rds")


#————————————————————————————————————————————————————————————————————————————————————————————


ki_korr_point_nach_stadtteile_color <- ggplot(korrelations_daten_clean, aes(x = hmk, y = anteil)) +
  geom_point(aes(color = Raumbezug),
             size  = 1.3,
             alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1) +
  ggpubr::stat_cor(
    label.x.npc = "left",
    label.y.npc = "top",
    color       = "black",
    size        = 5
  ) +
  coord_cartesian(xlim = x_range, ylim = y_range) +
  labs(
    title = "Korrelationskoeffizient nach Stadtteilen zwischen Kinderbetreuung (0–2 Jahre) und Frauenbeschäftigung",
    x     = "Kinderbetreuung 0–2 Jahre (%)",
    y     = "Anteil Frauenbeschäftigung (%)",
    color = "Stadtteil"
  ) +
  guides(color = guide_legend(ncol = 1)) +
  theme_minimal() +
   theme(legend.position = "none")

ki_korr_point_nach_stadtteile_color
#saveRDS(ki_korr_point_nach_stadtteile_color, "results/figures/Kinderbetreuung/ki_korr_point_nach_stadtteile_color.rds")

#————————————————————————————————————————————————————————————————————————————————————————————————



plot_data_final <- korrelations_daten_clean %>%
  group_by(Raumbezug) %>%
  mutate(
    n_complete = sum(complete.cases(hmk, anteil)),
    r = ifelse(
      n_complete > 1,
      cor(hmk, anteil, use = "complete.obs"),
      NA_real_
    ),
    r_label         = sprintf("(R=%.2f)", r),
    Raumbezug_Label = paste(Raumbezug, r_label, sep = " ")
  ) %>%
  ungroup()

all_labels <- sort(unique(plot_data_final$Raumbezug_Label))
all_colors <- scales::hue_pal()(length(all_labels))
stadtteil_palette <- all_colors
names(stadtteil_palette) <- all_labels

ki_point_line_korr_nach_stadtteile_color <- ggplot(plot_data_final, aes(x = hmk, y = anteil)) +
  geom_smooth(aes(color = Raumbezug_Label, group = Raumbezug_Label),
              method = "lm", se = FALSE, linewidth = 1.1, alpha = 0.8) +
  geom_point(aes(color = Raumbezug_Label), size = 1.1, alpha = 0.5) +
  geom_smooth(aes(group = 1), method = "lm",
              color = "black", linewidth = 1.1, se = FALSE) +
  scale_color_manual(values = stadtteil_palette) +
  labs(
    title = "",
    x     = "Kinderbetreuung (0–2) (%)",
    y     = "Anteil Frauenbeschäftigung (%)",
    color = "Stadtteil"
  ) +
  coord_cartesian(xlim = x_range, ylim = y_range) +
  theme_minimal() +
  theme(
    legend.text       = element_text(size = 7),
    legend.key.height = unit(0.4, "cm")
  ) +
  guides(color = guide_legend(ncol = 1)) +
 theme(legend.position = "none")

ki_point_line_korr_nach_stadtteile_color  

saveRDS(ki_point_line_korr_nach_stadtteile_color, "results/figures/Kinderbetreuung/ki_point_line_korr_nach_stadtteile_color.rds")

#——————————————————————————————————————————————————————————————————————————————————————————————————



r_werte_check <- korrelations_daten_clean %>%
  group_by(Raumbezug) %>%
  summarise(
    n_complete = sum(complete.cases(hmk, anteil)),
    r_wert = if (n_complete > 1)
      cor(hmk, anteil, use = "complete.obs")
    else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    is_negative      = r_wert < 0,
    Raumbezug_Label  = paste0(Raumbezug, " (R=", round(r_wert, 2), ")")
  )

plot_data_highlight <- korrelations_daten_clean %>%
  left_join(r_werte_check, by = "Raumbezug")

ki_simpson_sw <- ggplot(plot_data_highlight, aes(x = hmk, y = anteil)) +
  # 1) 先画“普通” Stadtteile 的点（灰色）
  geom_point(
    data = subset(plot_data_highlight, is_negative == FALSE | is.na(is_negative)),
    color = "grey90",
    alpha = 0.6,
    size  = 1.8
  ) +
  # 2) 再画 R<0 的 Stadtteile 的点（用线条的颜色）
  geom_point(
    data = subset(plot_data_highlight, is_negative == TRUE),
    aes(color = Raumbezug_Label),
    alpha = 0.9,
    size  = 2.3
  ) +
  # 3) 正相关/其他 Stadtteile 的回归线（灰色）
  geom_smooth(
    data = subset(plot_data_highlight, is_negative == FALSE | is.na(is_negative)),
    aes(group = Raumbezug),
    method   = "lm",
    se       = FALSE,
    color    = "grey70",
    linewidth = 0.8,
    alpha     = 0.5
  ) +
  # 4) R<0 的 Stadtteile 回归线（带颜色）
  geom_smooth(
    data = subset(plot_data_highlight, is_negative == TRUE),
    aes(color = Raumbezug_Label, group = Raumbezug),
    method   = "lm",
    se       = FALSE,
    linewidth = 1.1,
    alpha     = 1
  ) +
  # 5) Gesamt 回归线（黑色）
  geom_smooth(
    aes(group = 1),
    method   = "lm",
    color    = "black",
    linewidth = 1.1,
    se        = FALSE
  ) +
  scale_color_manual(values = stadtteil_palette) +
  labs(
    title    = "",
    subtitle = "",
    x        = "Haushalte mit Kindern (%)",
    y        = "Frauenbeschäftigung (%)",
    color    = "Stadtteile mit R < 0"
  ) +
  coord_cartesian(xlim = x_range, ylim = y_range) +
  theme_minimal() +
  theme(
    legend.position = "right"
  )

ki_simpson_sw

saveRDS(ki_simpson_sw, "results/figures/Kinderbetreuung/ki_simpson_sw.rds")


#——————————————————————————————————————————————————————————————————————————————————————————————


plot_data_colored <- korrelations_daten_clean %>%
  group_by(Raumbezug) %>%
  mutate(
    n_complete = sum(complete.cases(hmk, anteil)),
    r_wert = ifelse(
      n_complete > 1,
      cor(hmk, anteil, use = "complete.obs"),
      NA_real_
    ),
    trend_richtung = ifelse(r_wert >= 0 | is.na(r_wert), "positiv", "negativ")
  ) %>%
  ungroup()

ki_simpson_blau_rot <- ggplot(plot_data_colored, aes(x = hmk, y = anteil)) +
  geom_smooth(
    aes(
      color = trend_richtung,
      group = Raumbezug
    ),
    method = "lm",
    se     = FALSE,
    linewidth = 1.1,
    alpha    = 0.6
  ) +
  geom_smooth(aes(group = 1),
              method = "lm",
              color  = "black",
              linewidth = 1.1,
              se      = FALSE) +
  scale_color_manual(values = c(
    "positiv" = "red",
    "negativ" = "blue"
  )) +
  labs(
    title    = "Simpson's Paradox (nach Stadtteil)",
    subtitle = "Rot = positiver Zusammenhang (R ≥ 0), Blau = negativer Zusammenhang (R < 0)",
    x        = "Betreuungsquote Kinder (0–2) (%)",
    y        = "Anteil sozialversicherungspflichtig beschäftigter Frauen (%)"
  ) +
  coord_cartesian(xlim = x_range, ylim = y_range) +
  theme_minimal() +
  theme(legend.position = "none")

ki_simpson_blau_rot
#saveRDS(ki_simpson_blau_rot, "results/figures/Kinderbetreuung/ki_simpson_blau_rot.rds")


#————————————————————————————————————————————————————————————————————————————————————————————————







