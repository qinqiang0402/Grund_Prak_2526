library(tidyverse)
library(readxl)
library(sf)
library(stringr)
library(forcats)

#──────────────────────── 0. 帮助函数 ────────────────────────

# 给 Raumbezug / sb_nummer 统一生成两位数 Bezirk-Code
add_sb <- function(x, var = "Raumbezug", new = "sb") {
  x %>%
    mutate(
      !!new := str_pad(str_extract(.data[[var]], "^[0-9]+"), 2, pad = "0")
    )
}

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
      low = low_col,         # 用参数
      high = high_col,       # 用参数
      name = legend_title,
      limits = limits,
      na.value = "grey90"
    ) +
    labs(title = title,
         subtitle = subtitle,
         caption = "Daten: Landeshauptstadt München – Statistisches Amt") +
    theme_void(base_size = 12) +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold")
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
      title    = "Korrelation pro Stadtbezirk (2007–2024)",
      subtitle = "Frauenbeschäftigung und Kinderbetreuung (0–2 Jahre)",
      x        = "Korrelationskoeffizient",
      y        = "Stadtbezirk"
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
    scale_x_continuous(breaks = 2007:2024, limits = c(2007, 2024)) +
    labs(
      title    = "Jährliche Korrelation über alle Stadtbezirke",
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
geojson_url <- "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gsm_wfs:vablock_stadtbezirke_opendata&outputFormat=application/json"
munich_map2 <- st_read(geojson_url, quiet = TRUE) %>%
  add_sb("sb_nummer", "sb_nummer")

# 合并“儿童托管 + 女性 Beschäftigung”到 Bezirk×Jahr
df_merge <- df_betreut_bezirk %>%
  select(Jahr, sb, Raumbezug, anteil_betreut) %>%
  left_join(
    sozial_anteil_weiblich %>% add_sb() %>%
      select(Jahr, sb, sozial_weiblich),
    by = c("Jahr", "sb")
  ) %>%
  arrange(sb, Jahr)

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
    title    = "Trend der betreuten Kinder (0–2 Jahre) in München (2007–2024)",
    subtitle = "Stadtweit gewichteter Durchschnittsanteil der betreuten 0–2-Jährigen",
    x        = "Jahr"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank()
  )

p_trend_kita


#__________________________________________________________________________________

p_trend_emp <- ggplot(emp_year, aes(x = Jahr, y = emp_women)) +
  geom_line(color = "#08306b", linewidth = 1.3) +
  geom_point(color = "#08306b", size = 2) +
  scale_y_continuous(
    labels = percent_format(scale = 1),
    name   = "Anteil Frauenbeschaftigung [%]"
  ) +
  labs(
    title    = "Trend der Frauenbeschaftigung in München (2007–2024)",
    subtitle = "Anteil Frauenbeschaftigung (Stadt insgesamt)",
    x        = "Jahr"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank()
  )

p_trend_emp


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
  title        = paste0("Betreuungsquote (0–2 Jahre) – München, ", year_map),
  subtitle     = "Anteil betreuter Kinder nach Stadtbezirk",
  legend_title = "Betreuungsquote (%)",
  limits       = c(10, 55),
  low_col      = "#fff5eb",  # 很浅的橙/米色
  high_col     = "#7f2704"   # 深橙/棕色
)
p_map_betreuung
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
  title        = paste0("Frauenbeschaftigung – ", year_map),
  subtitle     = "Anteil in % nach Stadtbezirk (München)",
  legend_title = "Anteil Frauenbeschaftigung (%)",
  limits       = c(50, 62),
  low_col      = "#f7fbff",  # 浅蓝
  high_col     = "#08306b"   # 深蓝
)

p_map_sozial
# ggsave("fig/map_sozial_weiblich_2015.png", p_map_sozial, width = 7.5, height = 6.8, dpi = 300)

#──────────────────────── 4. 图 4：按 Bezirk 的时间相关性 ──────────────────

corr_bezirk <- df_merge %>%
  group_by(sb) %>%
  summarise(
    bezirk = first(Raumbezug),
    corr   = if (sum(complete.cases(anteil_betreut, sozial_weiblich)) > 1)
      cor(anteil_betreut, sozial_weiblich, use = "complete.obs")
    else NA_real_,
    .groups = "drop"
  )

p_corr_bezirk <- plot_corr_by_bezirk(corr_bezirk)
p_corr_bezirk
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
# ggsave("fig/corr_by_year.png", p_corr_year, width = 9, height = 4.5, dpi = 300)
