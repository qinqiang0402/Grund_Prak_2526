library(tidyverse)
library(readxl)
library(sf)
library(ggplot2)
library(dplyr)


ar <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")

sozial_anteil_weiblich <- ar %>% 
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(sozial_weiblich = `Basiswert 1` / `Basiswert 2`) %>% 
  dplyr::select(Jahr, Raumbezug, sozial_weiblich)
  




be <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")

be_0bis2 <- be %>% 
  filter(
    Indikator == "Altersgruppen",
    Ausprägung == "bis 2 Jahre",
  ) %>%
  dplyr::select(Indikator, Ausprägung, Jahr, Raumbezug, `Basiswert 1` )


ki <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

ki_0bis2 <- ki %>% 
  filter(
    Indikator == "Altersgruppen",
    Ausprägung == "bis 2 Jahre"
  ) %>% 
  dplyr::select(Indikator, Ausprägung, Jahr, Raumbezug, `Basiswert 1`)

be_0bis2 <- be_0bis2 %>%
  rename(kinder_total = `Basiswert 1`)

ki_0bis2 <- ki_0bis2 %>%
  rename(kinder_betreut = `Basiswert 1`)


# Merge based on Jahr (year) and Raumbezug (district)
df_betreut <- left_join(
  be_0bis2,
  ki_0bis2,
  by = c("Jahr", "Raumbezug", "Indikator", "Ausprägung")
)

# Calculate non-enrolled children and percentages
df_betreut <- df_betreut %>%
  mutate(
    kinder_unbetreut = kinder_total - kinder_betreut,
    anteil_unbetreut = kinder_unbetreut / kinder_total * 100,
    anteil_betreut = kinder_betreut / kinder_total * 100
  )

df_betreut %>%
  dplyr::select(Raumbezug, kinder_total, kinder_betreut, kinder_unbetreut,
         anteil_betreut, anteil_unbetreut) %>%
  arrange(desc(anteil_unbetreut)) %>%
  head()



#——————————————————————————————————————————————————————————————————————————————————————————

geojson_url <- "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gsm_wfs:vablock_stadtbezirke_opendata&outputFormat=application/json"
munich_map <- st_read(geojson_url)

library(dplyr)
library(stringr)
library(ggplot2)
library(sf)

# 1) Keep 2007–2024, create a 2-digit district code
df_win <- df_betreut %>%
  filter(Jahr >= 2007, Jahr <= 2024) %>%
  mutate(sb_nummer = str_pad(str_extract(Raumbezug, "^[0-9]+"), 2, pad = "0"))

# 2) Compute averages per district
#    (a) simple mean of yearly percentages
avg_simple <- df_win %>%
  group_by(sb_nummer) %>%
  summarise(avg_enrolled_simple = mean(anteil_betreut, na.rm = TRUE), .groups = "drop")

#    (b) weighted mean by number of children (recommended)
avg_weighted <- df_win %>%
  group_by(sb_nummer) %>%
  summarise(
    avg_enrolled_weighted = sum(kinder_betreut, na.rm = TRUE) / 
      sum(kinder_total, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# 3) Prepare map (no graticules, no background)
munich_map2 <- munich_map %>%
  mutate(sb_nummer = str_pad(as.character(sb_nummer), 2, pad = "0"))

map_avg <- munich_map2 %>%
  left_join(avg_weighted, by = "sb_nummer")   # use avg_simple if you prefer

# 4) Plot: clean, no grid, no axes
ggplot(map_avg) +
  geom_sf(aes(fill = avg_enrolled_weighted), color = "white", size = 0.25) +
  coord_sf(datum = NA) +                     # removes lat/lon graticules
  scale_fill_gradient(
    low = "#deebf7", high = "#08306b",
    name = "Enrolled share (%)",
    na.value = "grey90"
  ) +
  labs(
    title = "Average Share of Enrolled Children (0–2 years), Munich (2007–2024)",
    subtitle = "Weighted by the number of 0–2-year-olds per year\nLighter = lower share · Darker = higher share",
    caption = "Data: Landeshauptstadt München, Statistikamt"
  ) +
  theme_void(base_size = 12) +               # removes background, axes, ticks
  theme(legend.position = "right")


#————————————————————————————————————————————————————————————————————————————————————————————————




# -----------------------------
# 1) Build yearly citywide series (2007–2024)
# -----------------------------

# (A) Non-enrolled 0–2 share, weighted by number of children
enrolled_year <- df_betreut %>%
  filter(Jahr >= 2007, Jahr <= 2024) %>%
  group_by(Jahr) %>%
  summarise(
    share_enrolled = sum(kinder_betreut, na.rm = TRUE) / 
      sum(kinder_total,    na.rm = TRUE) * 100,
    .groups = "drop"
  )

# (B) Female unemployment – prefer the city total row if available
#     If you DON'T have "Stadt München", uncomment the second block.

# Option 1: direct city total row
emp_year <- sozial_anteil_weiblich %>%
  filter(Jahr >= 2007, Jahr <= 2024, Raumbezug == "Stadt München") %>%
  transmute(Jahr, emp_women = sozial_weiblich)

# Option 2 (fallback): average across districts (simple mean)
# unemp_year <- arbeitslos_weiblich %>%
#   filter(Jahr >= 2007, Jahr <= 2024, str_detect(Raumbezug, "^[0-9]+")) %>%
#   group_by(Jahr) %>%
#   summarise(unemp_women = mean(arbeitslos_weiblich, na.rm = TRUE), .groups = "drop")

# Merge both series
ts_dual <- enrolled_year %>%
  inner_join(emp_year, by = "Jahr")

# -----------------------------
# 2) Build a scaling to map right-axis data to left scale
#    We scale unemployment onto the left axis; the secondary axis reverses it.
# -----------------------------
range_left  <- range(ts_dual$share_enrolled, na.rm = TRUE)
range_right <- range(ts_dual$emp_women,      na.rm = TRUE)

scale_factor <- (range_left[2] - range_left[1]) / (range_right[2] - range_right[1])

# -----------------------------
# 3) Plot dual-axis trend
# -----------------------------
ggplot(ts_dual, aes(x = Jahr)) +
  # Left axis series (enrolled share)
  geom_line(aes(y = share_enrolled, color = "Enrolled (0–2)"), size = 1.2) +
  geom_point(aes(y = share_enrolled, color = "Enrolled (0–2)"), size = 2) +
  
  # Right axis series (female employment), rescaled onto left axis
  geom_line(aes(y = (emp_women - range_right[1]) * scale_factor + range_left[1],
                color = "Female employment"), size = 1.2) +
  geom_point(aes(y = (emp_women - range_right[1]) * scale_factor + range_left[1],
                 color = "Female employment"), size = 2) +
  
  scale_y_continuous(
    name = "Share of enrolled children (0–2) [%]",
    sec.axis = sec_axis(~ (. - range_left[1]) / scale_factor + range_right[1],
                        name = "Female employment rate [%]")
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Enrolled (0–2)" = "#1f77b4", "Female employment" = "#d62728")
  ) +
  labs(
    title    = "Dual-axis Trend: Enrolled 0–2-year-olds vs Female Employment (2007–2024)",
    subtitle = "Left: share of enrolled 0–2-year-olds (weighted) · Right: female employment rate",
    x        = "Year"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )




library(dplyr)
library(ggplot2)

# ────────────────────────────────────────────────
# 1️⃣ Compute yearly citywide (weighted) average
# ────────────────────────────────────────────────

enrolled_trend <- df_betreut %>%
  filter(Jahr >= 2007, Jahr <= 2024) %>%
  group_by(Jahr) %>%
  summarise(
    # Weighted average: (total enrolled / total children) * 100
    share_enrolled = sum(kinder_betreut, na.rm = TRUE) / 
      sum(kinder_total, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# ────────────────────────────────────────────────
# 2️⃣ Plot time trend for Munich
# ────────────────────────────────────────────────

ggplot(enrolled_trend, aes(x = Jahr, y = share_enrolled)) +
  geom_line(color = "#08306b", size = 1.3) +     # dark blue line
  geom_point(color = "#08306b", size = 2.2) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Trend of Enrolled Children (0–2 years) in Munich (2007–2024)",
    subtitle = "Citywide weighted average share of 0–2-year-olds enrolled in childcare",
    x = "Year",
    y = "Share of enrolled children (0–2 years) [%]",
    caption = "Data: Landeshauptstadt München, Statistikamt"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )








#————————————————————————————————————————————————————————————————————
#
#————————————————————————————————————————————————————————————————————


library(tidyverse)
library(stringr)
library(ggplot2)

#——— 0) 只保留“城区”行（排除 Stadt München 等汇总），提取两位区号 ———
bet <- df_betreut %>%
  filter(str_detect(Raumbezug, "^[0-9]+")) %>%                     # 只要“01 … 25 …”这种开头
  mutate(sb_nummer = str_pad(str_extract(Raumbezug, "^[0-9]+"), 2, pad = "0")) %>%
  transmute(Jahr, Raumbezug, sb_nummer,
            share_enrolled = kinder_betreut / kinder_total * 100)  # 当年该区托管率（%）

emp <- sozial_anteil_weiblich %>%
  filter(str_detect(Raumbezug, "^[0-9]+")) %>%
  mutate(sb_nummer = str_pad(str_extract(Raumbezug, "^[0-9]+"), 2, pad = "0")) %>%
  dplyr::select(Jahr, Raumbezug, sb_nummer, emp_women = sozial_weiblich) %>%
  mutate(emp_women = emp_women * 100)                              # 转为百分比（%）

#——— 1) 合并同一“区×年”的两条序列 ———
by_dist_year <- full_join(bet, emp, by = c("Jahr", "Raumbezug", "sb_nummer")) %>%
  filter(Jahr >= 2007, Jahr <= 2024)

#——— 2) 输出目录 ———
dir.create("fig/dualaxis", recursive = TRUE, showWarnings = FALSE)

#——— 3) 辅助：给右轴（女性就业）做线性缩放到左轴范围 ———
scale_to_left <- function(y, left_range, right_range) {
  # 防止右轴范围为 0 导致除零
  span_r <- diff(range(right_range, na.rm = TRUE))
  span_r <- ifelse(span_r == 0, 1e-6, span_r)
  span_l <- diff(range(left_range,  na.rm = TRUE))
  ( (y - right_range[1]) / span_r ) * span_l + left_range[1]
}

inv_from_left <- function(yl, left_range, right_range) {
  span_r <- diff(range(right_range, na.rm = TRUE))
  span_r <- ifelse(span_r == 0, 1e-6, span_r)
  span_l <- diff(range(left_range,  na.rm = TRUE))
  ( (yl - left_range[1]) / span_l ) * span_r + right_range[1]
}

#——— 4) 按区循环绘图并保存 —— 每个区 1 张图 ———
unique_dists <- by_dist_year %>%
  distinct(sb_nummer, Raumbezug) %>%
  arrange(sb_nummer)

for (i in seq_len(nrow(unique_dists))) {
  sb   <- unique_dists$sb_nummer[i]
  name <- unique_dists$Raumbezug[i]
  
  dat <- by_dist_year %>% filter(sb_nummer == sb) %>% arrange(Jahr)
  
  # 左右轴取值范围
  left_rng  <- range(dat$share_enrolled, na.rm = TRUE)
  right_rng <- range(dat$emp_women,      na.rm = TRUE)
  
  p <- ggplot(dat, aes(x = Jahr)) +
    # 左轴：Betreuungsquote（0–2岁）%
    geom_line(aes(y = share_enrolled, color = "Betreuungsquote (0–2 Jahre)"), linewidth = 1.2) +
    geom_point(aes(y = share_enrolled, color = "Betreuungsquote (0–2 Jahre)"), size = 2) +
    
    # 右轴：Anteil sozialversicherungspflichtig Beschäftigter (weiblich) %
    geom_line(aes(y = scale_to_left(emp_women, left_rng, right_rng),
                  color = "SV-pflichtig Beschäftigte (weiblich)"), linewidth = 1.2) +
    geom_point(aes(y = scale_to_left(emp_women, left_rng, right_rng),
                   color = "SV-pflichtig Beschäftigte (weiblich)"), size = 2) +
    
    scale_y_continuous(
      name = "Betreuungsquote (0–2 Jahre) [%]",
      sec.axis = sec_axis(
        ~ inv_from_left(., left_rng, right_rng),
        name = "Anteil sozialversicherungspflichtig Beschäftigter (weiblich) [%]"
      )
    ) +
    scale_color_manual(
      name = NULL,
      values = c("Betreuungsquote (0–2 Jahre)" = "#1f77b4",
                 "SV-pflichtig Beschäftigte (weiblich)" = "#d62728")
    ) +
    labs(
      title    = paste0("Stadtbezirk ", sb, " – ", name),
      subtitle = "2007–2024: Betreuungsquote (0–2 Jahre) vs. weibliche SV-Beschäftigung",
      x        = "Jahr"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top",
          panel.grid.minor = element_blank())
  
#  ggsave(filename = file.path("fig/dualaxis",
#                              paste0("bezirk_", sb, "_dualaxis_2007_2024.png")),
#         plot = p, width = 9, height = 5.2, dpi = 300)
}

#——————————————————————————————————————————————————————————
#-——————————————————————————————————————————————————————————



library(sf)
library(ggplot2)
library(dplyr)
library(stringr)

#——— 0) 计算每年每区的 Betreuungsquote（%）并接地图 —— 
bezirks_yearly <- df_betreut %>%
  filter(Jahr >= 2007, Jahr <= 2024, str_detect(Raumbezug, "^[0-9]+")) %>%
  mutate(sb_nummer = str_pad(str_extract(Raumbezug, "^[0-9]+"), 2, pad = "0"),
         betreuungsquote = kinder_betreut / kinder_total * 100) %>%
  dplyr::select(Jahr, sb_nummer, betreuungsquote)

munich_map2 <- munich_map %>%
  mutate(sb_nummer = str_pad(as.character(sb_nummer), 2, pad = "0"))

dir.create("fig/maps_yearly", recursive = TRUE, showWarnings = FALSE)

years <- sort(unique(bezirks_yearly$Jahr))

for (yr in years) {
  map_year <- munich_map2 %>%
    left_join(bezirks_yearly %>% filter(Jahr == yr),
              by = "sb_nummer")
  
  pmap <- ggplot(map_year) +
    geom_sf(aes(fill = betreuungsquote), color = "white", linewidth = 0.25) +
    coord_sf(datum = NA) +
    scale_fill_gradient(
      low = "#deebf7", high = "#08306b",
      name = "Betreuungsquote (%)",
      na.value = "grey90",
      limits = c(0, 100)
    ) +
    labs(
      title = paste0("Betreuungsquote (0–2 Jahre) – München, ", yr),
      subtitle = "Anteil betreuter Kinder (nach Stadtbezirk)",
      caption = "Daten: Landeshauptstadt München – Statistisches Amt"
    ) +
    theme_void(base_size = 12) +
    theme(legend.position = "right",
          plot.title = element_text(face = "bold"))
  
#  ggsave(filename = file.path("fig/maps_yearly",
#                              paste0("map_betreuung_", yr, ".png")),
#         plot = pmap, width = 7.5, height = 6.8, dpi = 300)
}




#——————————————————————————————————————————————————————————————————————————————
#——————————————————————————————————————————————————————————————————————————————


library(dplyr)
library(stringr)

#-------------------------------------------------------
# 数据准备
#-------------------------------------------------------
df_all <- df_betreut %>%
  mutate(sb = str_pad(str_extract(Raumbezug, "^[0-9]+"), 2, pad = "0")) %>%
  filter(str_detect(Raumbezug, "^[0-9]+")) %>%
  dplyr::select(Jahr, sb, anteil_betreut)

emp_all <- sozial_anteil_weiblich %>%
  mutate(sb = str_pad(str_extract(Raumbezug, "^[0-9]+"), 2, pad = "0")) %>%
  filter(str_detect(Raumbezug, "^[0-9]+")) %>%
  dplyr::select(Jahr, sb, sozial_weiblich)

df_merge <- left_join(df_all, emp_all, by = c("Jahr", "sb"))

#-------------------------------------------------------
# ① 时间趋势相关性（每个区）
#-------------------------------------------------------
corr_time <- df_merge %>%
  group_by(sb) %>%
  summarise(
    corr_within_time = if (sum(complete.cases(anteil_betreut, sozial_weiblich)) > 1)
      cor(anteil_betreut, sozial_weiblich, use = "complete.obs")
    else NA_real_
  ) %>%
  ungroup()

#-------------------------------------------------------
# ② 空间横截面相关性（每年）
#-------------------------------------------------------
corr_space <- df_merge %>%
  group_by(Jahr) %>%
  summarise(
    corr_within_year = if (sum(complete.cases(anteil_betreut, sozial_weiblich)) > 1)
      cor(anteil_betreut, sozial_weiblich, use = "complete.obs")
    else NA_real_
  ) %>%
  ungroup()

#-------------------------------------------------------
# ③ 全样本总体相关性
#-------------------------------------------------------
corr_overall <- cor(df_merge$anteil_betreut, df_merge$sozial_weiblich, use = "complete.obs")

#-------------------------------------------------------
# 打印结果
#-------------------------------------------------------
cat("① 每个区的时间趋势相关性（前几行）:\n")
print(corr_time, n = 25)

cat("\n② 每一年的空间相关性:\n")
print(corr_space, n = 25)

cat("\n③ 全体总体相关性:", round(corr_overall, 3), "\n")






#____________________________________________________________________
#____________________________________________________________________


###. Zeitliche Korrelation innerhalb der Stadtbezirke

ggplot(corr_time, aes(x = reorder(sb, corr_within_time), y = corr_within_time)) +
  geom_col(fill = "#1f77b4") +
  geom_hline(yintercept = 0, color = "grey40") +
  labs(title = "Zeitliche Korrelation innerhalb der Stadtbezirke",
       x = "Stadtbezirk", y = "Korrelationskoeffizient") +
  theme_minimal(base_size = 13)



###_______________________________________________________________
#_________________________________________________________________


ggplot(corr_space, aes(x = Jahr, y = corr_within_year)) +
  geom_line(color = "#08306b", linewidth = 1.3) +
  geom_point(color = "#08306b", size = 2) +
  geom_hline(yintercept = 0, color = "grey50") +
  labs(title = "Räumliche Korrelation zwischen Bezirken (pro Jahr)",
       y = "Korrelationskoeffizient", x = "Jahr") +
  theme_minimal(base_size = 13)









#————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————



# 安装一次即可：
# install.packages(c("tidyverse","stringr","slider"))

library(tidyverse)
library(stringr)
library(slider)
library(ggplot2)

#——— 0) 准备：合并到“区×年”的一个表 ———
df_all <- df_betreut %>%
  # 只保留带编号的城区（去掉“Stadt München”等汇总）
  filter(str_detect(Raumbezug, "^[0-9]+")) %>%
  mutate(
    sb  = str_pad(str_extract(Raumbezug, "^[0-9]+"), 2, pad = "0"),
    Jahr = as.integer(Jahr)
  ) %>%
  dplyr::select(Jahr, sb, Raumbezug, anteil_betreut)

emp_all <- sozial_anteil_weiblich %>%
  filter(str_detect(Raumbezug, "^[0-9]+")) %>%
  mutate(
    sb  = str_pad(str_extract(Raumbezug, "^[0-9]+"), 2, pad = "0"),
    Jahr = as.integer(Jahr)
  ) %>%
  dplyr::select(Jahr, sb, sozial_weiblich)

df_merge <- left_join(df_all, emp_all, by = c("Jahr","sb")) %>%
  arrange(sb, Jahr)

#——— 1) 参数：窗口长度（年） ———
win <- 5   # 你可以改成 3/7/10 等

#——— 2) 定义安全的相关函数（只要有效点>1才算） ———
safe_cor <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) > 1) cor(x[ok], y[ok]) else NA_real_
}

#——— 3A) 计算“滚动相关（gleitende Korrelation）” ———
# 在每个区内，沿年份顺序，用长度=win 的滑动窗口计算皮尔逊相关
roll_corr <- df_merge %>%
  group_by(sb, Raumbezug) %>%
  arrange(Jahr, .by_group = TRUE) %>%
  mutate(
    corr_roll = slide2_dbl(
      .x = anteil_betreut, .y = sozial_weiblich,
      .f = ~ safe_cor(.x, .y),
      .before = win - 1,   # 当前年及其之前共 win 年
      .complete = TRUE     # 窗口不满时返回 NA
    )
  ) %>%
  ungroup()

#——— 3B) 计算“累积相关（kumulative Korrelation）”（可选）
# 从起始年累积到当前年（扩展窗口），如果你也想画这种，把下面一段的注释打开：
 expand_corr <- df_merge %>%
   group_by(sb, Raumbezug) %>%
   arrange(Jahr, .by_group = TRUE) %>%
   mutate(
     corr_expand = slide2_dbl(
       .x = anteil_betreut, .y = sozial_weiblich,
       .f = ~ safe_cor(.x, .y),
       .before = Inf,      # 从序列开头到当前
       .complete = TRUE
     )
   ) %>%
   ungroup()

#——— 4) 输出目录 ———
outdir <- "fig/corr_by_district"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

#——— 5) 循环绘图：每个区一张（滚动相关） ———
dists <- roll_corr %>% distinct(sb, Raumbezug) %>% arrange(sb)

for (i in seq_len(nrow(dists))) {
  sb  <- dists$sb[i]
  nm  <- dists$Raumbezug[i]
  dd  <- roll_corr %>% filter(sb == !!sb)
  
  p <- ggplot(dd, aes(x = Jahr, y = corr_roll)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_line(color = "#1f77b4", linewidth = 1.2, na.rm = TRUE) +
    geom_point(color = "#1f77b4", size = 2, na.rm = TRUE) +
    coord_cartesian(ylim = c(-1, 1)) +
    scale_x_continuous(breaks = seq(2007, 2024, by = 1)) +
    labs(
      title    = paste0("Stadtbezirk ", sb, " – ", nm),
      subtitle = paste0("Gleitende Korrelation (Fenster=", win, " Jahre): ",
                        "Betreuungsquote (0–2) vs. weibliche SV-Beschäftigung"),
      x = "Jahr", y = "Korrelationskoeffizient (−1 … +1)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
  
  #ggsave(file.path(outdir, sprintf("bezirk_%s_corr_rolling_win%d.png", sb, win)),
   #      p, width = 9, height = 5.2, dpi = 300)
}

#——— 6)（可选）若也想输出“累积相关”的 25 张图，追加这一段 —— 
 dists2 <- expand_corr %>% distinct(sb, Raumbezug) %>% arrange(sb)
 for (i in seq_len(nrow(dists2))) {
   sb <- dists2$sb[i]; nm <- dists2$Raumbezug[i]
   dd <- expand_corr %>% filter(sb == !!sb)
   p <- ggplot(dd, aes(Jahr, corr_expand)) +
     geom_hline(yintercept = 0, color = "grey60") +
     geom_line(color = "#d62728", linewidth = 1.2, na.rm = TRUE) +
     geom_point(color = "#d62728", size = 2, na.rm = TRUE) +
     coord_cartesian(ylim = c(-1, 1)) +
     scale_x_continuous(breaks = seq(2007, 2024, by = 1)) +
     labs(title = paste0("Stadtbezirk ", sb, " – ", nm),
          subtitle = "Kumulative Korrelation: von Startjahr bis zum laufenden Jahr",
          x = "Jahr", y = "Korrelationskoeffizient (−1 … +1)") +
     theme_minimal(base_size = 13) +
     theme(axis.text.x = element_text(angle = 45, hjust = 1),
           panel.grid.minor = element_blank())
   #ggsave(file.path(outdir, sprintf("bezirk_%s_corr_expanding.png", sb)),
  #        p, width = 9, height = 5.2, dpi = 300)
 }


 
 
 # 0) 只保留“有编号的城区”，提取两位数区号；把比例转成百分比
 soz_bezirk <- sozial_anteil_weiblich %>%
   filter(str_detect(Raumbezug, "^[0-9]+")) %>%                           # 排除 "Stadt München"
   mutate(
     sb_nummer = str_pad(str_extract(Raumbezug, "^[0-9]+"), 2, pad = "0"),
     Jahr = as.integer(Jahr),
     sozial_weiblich_pct = sozial_weiblich * 100                          # 0–1 → %
   ) %>%
   dplyr::select(Jahr, sb_nummer, sozial_weiblich_pct)
 
 # 1) 地图对象也统一两位区号
 munich_map2 <- munich_map %>%
   mutate(sb_nummer = str_pad(as.character(sb_nummer), 2, pad = "0"))
 
 # 2) 计算全时期的全局范围，用于统一图例刻度（跨年可比）
 rng <- range(soz_bezirk$sozial_weiblich_pct, na.rm = TRUE)
 # 也可以手动设定，比如 limits = c(50, 70)
 
 # 3) 输出目录
 outdir <- "fig/maps_yearly_sozial_weiblich"
 dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
 
 # 4) 年份序列
 years <- 2007:2024
 
 # 5) 逐年绘图并保存
 for (yr in years) {
   dat_year <- munich_map2 %>%
     left_join(soz_bezirk %>% filter(Jahr == yr), by = "sb_nummer")
   
   p <- ggplot(dat_year) +
     geom_sf(aes(fill = sozial_weiblich_pct), color = "white", linewidth = 0.25, na.rm = TRUE) +
     coord_sf(datum = NA) +
     scale_fill_gradient(
       low = "#deebf7", high = "#08306b",
       name = "Anteil weiblich\nSV-Beschäftigte (%)",
       limits = rng,                   # 用统一范围保证跨年可比
       na.value = "grey90"
     ) +
     labs(
       title = paste0("Sozialversicherungspflichtig Beschäftigte (weiblich) – ", yr),
       subtitle = "Anteil in % nach Stadtbezirk (München)",
       caption = "Daten: Landeshauptstadt München – Statistisches Amt"
     ) +
     theme_void(base_size = 12) +
     theme(
       legend.position = "right",
       plot.title = element_text(face = "bold")
     )
   
#   ggsave(
#     filename = file.path(outdir, sprintf("map_sozial_weiblich_%d.png", yr)),
#    plot = p, width = 7.5, height = 6.8, dpi = 300
#   )
 }
 
 
 
 
 
 

