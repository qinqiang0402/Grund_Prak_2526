library(tidyverse)
library(readxl)
library(ggplot2)

ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")

#get
erwerb_weiblich <- ar_sheet %>%
  filter(
    Indikator == "Erwerbsfähige",
    Ausprägung == "weiblich"
  ) %>%
  mutate(Indikatorwert = `Basiswert 1` / `Basiswert 2`) %>%
  select(Jahr, Raumbezug, Indikatorwert) %>%
  rename(erwerb_weiblich = Indikatorwert)

#get
arbeitslos_anteil_weiblich <- ar_sheet %>%
  filter(
    Indikator == "Arbeitslose - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(Indikatorwert = `Basiswert 1` / `Basiswert 2`) %>%
  select(Jahr, Raumbezug, Indikatorwert) %>%
  rename(arbeitslos_anteil_weiblich = Indikatorwert)


#combine them

combined <- left_join(
  arbeitslos_anteil_weiblich,
  erwerb_weiblich,
  by = c("Jahr", "Raumbezug")
)


#plot
#allgemein Zusammenhang
ggplot(combined, aes(x = erwerb_weiblich, y = arbeitslos_anteil_weiblich)) +
  geom_point(alpha = 0.7, color = "#1F77B4", size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Zusammenhang zwischen weiblichen Erwerbsfähigen und weiblicher Arbeitslosenquote (2024)",
    x = "Erwerbsfähige Bevölkerung (weiblich)",
    y = "Arbeitslosenquote (weiblich)"
  ) +
  theme_minimal()


#nach Jahr
library(dplyr)
library(ggplot2)

# mean von Stadtteile
munich_time <- combined %>%
  group_by(Jahr) %>%
  summarise(
    mean_arbeitslos = mean(arbeitslos_anteil_weiblich, na.rm = TRUE),
    mean_erwerb = mean(erwerb_weiblich, na.rm = TRUE)
  )

# linediagramm
ggplot(munich_time, aes(x = Jahr)) +
  geom_line(aes(y = mean_arbeitslos * 100, color = "Arbeitslosigkeit (%)"), size = 1.2) +
  geom_line(aes(y = mean_erwerb * 100, color = "Erwerbsfähige Frauen (%)"), size = 1.2) +
  labs(
    title = "Entwicklung von weiblicher Erwerbsfähigkeit und Arbeitslosenquote in München (2000–2024)",
    x = "Jahr", y = "Prozent"
  ) +
  scale_color_manual(values = c("#E74C3C", "#2E86AB")) +
  theme_minimal()


#log

library(dplyr)
library(tidyr)

munich_time_long <- munich_time %>%
  mutate(
    mean_arbeitslos = mean_arbeitslos * 100,
    mean_erwerb = mean_erwerb * 100
  ) %>%
  pivot_longer(
    cols = c(mean_arbeitslos, mean_erwerb),
    names_to = "Variable",
    values_to = "Prozent"
  ) %>%
  mutate(
    Variable = recode(
      Variable,
      "mean_arbeitslos" = "Arbeitslosigkeit (%)",
      "mean_erwerb" = "Erwerbsfähige Frauen (%)"
    )
  )


ggplot(munich_time_long, aes(x = Jahr, y = Prozent, color = Variable)) +
  geom_line(size = 1.2) +
  scale_y_log10() +
  labs(
    title = "Logarithmische Darstellung: Erwerbsfähige Frauen vs. Arbeitslosigkeit",
    y = "log10(Prozentwert)"
  ) +
  scale_color_manual(values = c("#2E86AB", "#E74C3C")) +
  theme_minimal()





library(ggplot2)

ggplot(munich_time, aes(x = Jahr)) +
  geom_line(aes(y = mean_erwerb * 100, color = "Erwerbsfähige Frauen (%)"), size = 1.2) +
  geom_line(aes(y = mean_arbeitslos * 100 * 15, color = "Arbeitslosigkeit (%)"), size = 1.2) +  # 乘以15只是为了视觉匹配
  scale_y_continuous(
    name = "Erwerbsfähige Frauen (%)",
    sec.axis = sec_axis(~./15, name = "Arbeitslosigkeit (%)")  # 第二y轴缩放回来
  ) +
  scale_color_manual(values = c("#2E86AB", "#E74C3C")) +
  labs(
    title = "Zeitliche Entwicklung: Erwerbsfähige Frauen vs. Arbeitslosigkeit (2000–2024)",
    x = "Jahr", color = ""
  ) +
  theme_minimal(base_size = 12)





#nach Stadtteile


library(sf)

geojson_url <- "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gsm_wfs:vablock_stadtbezirke_opendata&outputFormat=application/json"
munich_map <- st_read(geojson_url)

combined <- combined %>%
  mutate(
    sb_nummer = str_pad(str_extract(Raumbezug, "^[0-9]+"), 2, pad = "0")  # 确保前导 0 保留
  )

munich_map <- munich_map %>%
  mutate(
    sb_nummer = str_pad(as.character(sb_nummer), 2, pad = "0")
  )


data_2024 <- combined %>%
  filter(Jahr == 2024)

map_data <- left_join(munich_map, data_2024, by = "sb_nummer")


#arbeitslose_anteil_weiblich
ggplot(map_data) +
  geom_sf(aes(fill = arbeitslos_anteil_weiblich), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(
    title = "Weibliche Arbeitslosenquote in München (2024)",
    fill = "Arbeitslosen-\nquote"
  ) +
  theme_void(base_size = 12)

#


map_long <- map_data %>%
  select(sb_nummer, arbeitslos_anteil_weiblich, erwerb_weiblich, geometry) %>%
  pivot_longer(
    cols = c(arbeitslos_anteil_weiblich, erwerb_weiblich),
    names_to = "Variable",
    values_to = "Wert"
  ) %>%
  mutate(
    Variable = recode(
      Variable,
      arbeitslos_anteil_weiblich = "Arbeitslosenquote (weiblich)",
      erwerb_weiblich            = "Erwerbsfähige Frauen (Anteil)"
    )
  )

ggplot(map_long) +
  geom_sf(aes(fill = Wert), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  facet_wrap(~ Variable, ncol = 2) +
  labs(
    title = "München 2024: Weibliche Arbeitslosenquote vs. Erwerbsfähige Frauen",
    fill = "Anteil"
  ) +
  theme_void(base_size = 12)




#


ggplot(combined, aes(x = Jahr)) +
  geom_line(aes(y = erwerb_weiblich * 100, color = "Erwerbsfähige Frauen (%)"), linewidth = 1) +
  geom_line(aes(y = arbeitslos_anteil_weiblich * 100 * 15, color = "Arbeitslosigkeit (%)"), linewidth = 1) +
  facet_wrap(~ Raumbezug, ncol = 5) +
  scale_y_continuous(
    name = "Erwerbsfähige Frauen (%)",
    sec.axis = sec_axis(~./15, name = "Arbeitslosenquote (%)")
  ) +
  scale_color_manual(values = c("#2E86AB", "#E74C3C")) +
  labs(
    title = "Zeitliche Entwicklung beider Variablen (2000–2024)",
    x = "Jahr", color = ""
  ) +
  theme_minimal(base_size = 11)


#___


library(dplyr)

cor_spatio <- combined %>%
  group_by(Raumbezug) %>%
  summarise(
    cor_erwerb_arbeitslos = cor(erwerb_weiblich, arbeitslos_anteil_weiblich, use = "complete.obs")
  ) %>%
  arrange(cor_erwerb_arbeitslos)


ggplot(cor_spatio, aes(x = reorder(Raumbezug, cor_erwerb_arbeitslos), y = cor_erwerb_arbeitslos)) +
  geom_col(fill = "#3498DB") +
  coord_flip() +
  labs(
    title = "Korrelation zwischen Erwerbsfähigkeit und Arbeitslosigkeit (2000–2024)",
    x = "Stadtbezirk", y = "Korrelationskoeffizient (r)"
  ) +
  theme_minimal(base_size = 12)




library(sf)

# 统一区号
cor_spatio <- combined %>%
  group_by(sb_nummer) %>%
  summarise(
    cor_erwerb_arbeitslos = cor(erwerb_weiblich, arbeitslos_anteil_weiblich, use = "complete.obs")
  )

# 与地图合并
map_cor <- left_join(munich_map, cor_spatio, by = "sb_nummer")

ggplot(map_cor) +
  geom_sf(aes(fill = cor_erwerb_arbeitslos), color = "white", linewidth = 0.3) +
  scale_fill_gradient2(
    low = "#E74C3C", mid = "white", high = "#2ECC71",
    midpoint = 0, limits = c(-1, 1),
    name = "r(Korrelation)"
  ) +
  labs(
    title = "Räumlich aggregierte Korrelation (2000–2024)",
    subtitle = "Zusammenhang zwischen weiblicher Erwerbsfähigkeit und Arbeitslosigkeit",
    caption = "Negativ = je mehr erwerbsfähig, desto weniger arbeitslos"
  ) +
  theme_void(base_size = 12)


install.packages("gganimate")
library(gganimate)

map_anim <- left_join(munich_map, combined, by = "sb_nummer")

ggplot(map_anim) +
  geom_sf(aes(fill = arbeitslos_anteil_weiblich), color = NA) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(
    title = "Weibliche Arbeitslosenquote in München: {frame_time}",
    fill = "Arbeitslosenquote"
  ) +
  transition_time(Jahr) +
  ease_aes('linear') +
  theme_void(base_size = 12)


#______


trend_df <- combined %>%
  group_by(sb_nummer) %>%
  summarise(
    trend_erwerb = coef(lm(erwerb_weiblich ~ Jahr))[2],
    trend_arbeitslos = coef(lm(arbeitslos_anteil_weiblich ~ Jahr))[2]
  )

# 合并到地图
map_trend <- left_join(munich_map, trend_df, by = "sb_nummer")

library(ggplot2)
library(scales)

ggplot(map_trend) +
  geom_sf(aes(fill = trend_arbeitslos), color = "white", linewidth = 0.2) +
  scale_fill_gradient2(
    low = "green", mid = "white", high = "red",
    midpoint = 0,
    labels = scales::percent_format(accuracy = 0.01),  # ✅ 显示为百分比
    name = "Steigung (Δ pro Jahr)"
  ) +
  labs(
    title = "Trend der weiblichen Arbeitslosigkeit (2000–2024)",
    subtitle = "Positive Werte = Verbesserung (Rückgang der Arbeitslosigkeit)",
    caption = "Datenquelle: Stadt München, Arbeitsmarktstatistik"
  ) +
  theme_void(base_size = 12)



#korrelation nach Jahr _ Pearson

library(dplyr)
library(ggplot2)

cor_by_year <- combined %>%
  group_by(Jahr) %>%
  summarise(
    r_pearson = cor(erwerb_weiblich, arbeitslos_anteil_weiblich, use = "complete.obs")
  )

ggplot(cor_by_year, aes(x = Jahr, y = r_pearson)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.2)) +
  labs(
    title = "Jährliche Korrelation: Erwerbsfähige (w) vs. Arbeitslosenquote (w)",
    x = "Jahr", y = "Korrelationskoeffizient (r, Pearson)"
  ) +
  theme_minimal(base_size = 12)


# Spearman vs Pearson

cor_by_year_both <- combined %>%
  group_by(Jahr) %>%
  summarise(
    r_pearson  = cor(erwerb_weiblich, arbeitslos_anteil_weiblich, use = "complete.obs", method = "pearson"),
    r_spearman = cor(erwerb_weiblich, arbeitslos_anteil_weiblich, use = "complete.obs", method = "spearman")
  ) %>%
  tidyr::pivot_longer(cols = starts_with("r_"), names_to = "Methode", values_to = "r")

ggplot(cor_by_year_both, aes(Jahr, r, color = Methode)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.2)) +
  scale_color_manual(values = c("r_pearson" = "#1f77b4", "r_spearman" = "#d62728"),
                     labels = c("Pearson", "Spearman")) +
  labs(title = "Jährliche Korrelation (Pearson vs. Spearman)",
       x = "Jahr", y = "r", color = "Methode") +
  theme_minimal(base_size = 12)






#——————————————



library(sf)
library(ggplot2)
library(dplyr)
library(scales)

# 计算每个区的几何中心（标签位置）
label_points <- st_point_on_surface(munich_map)

# 给每个区加编号列（例如 01–25）
label_points <- label_points %>%
  arrange(sb_nummer) %>%
  mutate(id_label = sprintf("%02d", as.integer(sb_nummer)))  # 补零编号

# 绘制地图（编号标签）
ggplot(map_trend) +
  geom_sf(aes(fill = trend_arbeitslos), color = "white", linewidth = 0.2) +
  geom_sf_text(
    data = label_points,
    aes(label = id_label),
    size = 3.5,
    color = "black",
    fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "green", mid = "white", high = "red", midpoint = 0,
    labels = percent_format(accuracy = 0.01),
    name = "Steigung (Δ pro Jahr)"
  ) +
  labs(
    title = "Trend der weiblichen Arbeitslosigkeit (2000–2024)",
    subtitle = "Positive Werte = Verbesserung (Rückgang der Arbeitslosigkeit)",
    caption = "Datenquelle: Stadt München, Arbeitsmarktstatistik"
  ) +
  theme_void(base_size = 12)



#————————————————


library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(scales)
library(gridExtra)    # tableGrob
library(ggplotify)    # as.ggplot
library(patchwork)    # 拼图

# 1) 标签点（只显示编号）
label_points <- st_point_on_surface(munich_map) %>%
  mutate(id_label = str_pad(as.character(sb_nummer), 2, pad = "0"))

# 2) 地图主体：把图例放到底部
min_val <- min(map_trend$trend_arbeitslos, na.rm = TRUE)
max_val <- max(map_trend$trend_arbeitslos, na.rm = TRUE)

p_map <- ggplot(map_trend) +
  geom_sf(aes(fill = trend_arbeitslos), color = "white", linewidth = 0.25) +
  geom_sf_text(data = label_points, aes(label = id_label),
               size = 3.5, fontface = "bold", color = "black") +
  scale_fill_gradient2(
    low = "darkblue", mid = "white", high = "lightblue", midpoint = 0,
    name   = "Steigung (Δ pro Jahr)",
    breaks = c(min_val, 0, max_val),               # 仅显示 最小/0/最大
    # 如果只想最小和最大：breaks = c(min_val, max_val),
    labels = percent_format(accuracy = 0.01),
    guide  = guide_colorbar(
      direction      = "vertical",                 # ✅ 竖直
      title.position = "top",                      # 标题在上
      label.position = "right",                    # 刻度在右
      barheight = unit(38, "mm"),                  # 竖直长度
      barwidth  = unit(5,  "mm"),                  # 色条宽度
      ticks.colour = NA
    )
  ) +
  labs(
    title = "Trend der weiblichen Arbeitslosigkeit (2000–2024)",
    subtitle = "Positive Werte = Verbesserung (Rückgang der Arbeitslosigkeit)"
  ) +
  coord_sf(expand = FALSE) +
  theme_void(base_size = 12) +
  theme(
    legend.position      = c(0.08, 0.12),          # ✅ 左下角（可微调）
    legend.justification = c(0, 0),
    legend.background    = element_rect(fill = alpha("white", 0.85), color = NA),
    legend.title = element_text(size = 10, margin = margin(b = 4)),
    legend.text  = element_text(size = 9)
  )
p_map


# 3) 右侧对照表（编号 → 区名）
bezirk_table <- label_points %>%
  st_drop_geometry() %>%
  transmute(`Nr.` = id_label, `Stadtbezirk` = name) %>%
  arrange(`Nr.`)

tbl_grob <- tableGrob(
  bezirk_table, rows = NULL,
  theme = ttheme_minimal(
    base_size = 10,
    core = list(padding = unit(c(3, 4), "mm")),
    colhead = list(fg_params = list(fontface = "bold"))
  )
)

p_table <- as.ggplot(tbl_grob)

# 4) 左图右表拼接（右侧窄一些）
final_plot <- p_map + p_table + plot_layout(widths = c(3, 1))
final_plot



#————————————————


library(scales)

rng   <- range(map_trend$trend_arbeitslos, na.rm = TRUE)

p_map <- ggplot(map_trend) +
  geom_sf(aes(fill = trend_arbeitslos), color = "white", linewidth = 0.25) +
  # … 你的 geom_sf_text 等保持不变 …
  scale_fill_gradient2(
    low = "green", mid = "white", high = "red", midpoint = 0,
    # ① 用百分比显示，② 控制刻度数（更少更清晰）
    labels = percent_format(accuracy = 0.01),
    breaks = breaks_pretty(n = 5),        # 或者：seq(round(rng[1], 4), 0, length.out = 5)
    name   = "Steigung (Δ pro Jahr)",
    guide  = guide_colorbar(
      direction      = "horizontal",
      title.position = "top",             # 标题在上
      label.position = "bottom",          # 刻度在下
      # 间距与尺寸
      barwidth  = unit(70, "mm"),
      barheight = unit(5,  "mm"),
      ticks.colour = NA
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 11, margin = margin(b = 4)),  # 标题与色条间距
    legend.text  = element_text(size = 9,  margin = margin(t = 2)),  # 刻度与色条间距
    legend.box.margin = margin(t = 4)
  )


#————————————————————————

#2024 arbeitslos_anteil_weiblich


library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(scales)

# ① 从 Raumbezug 提取区号，与地图对齐
combined <- combined %>%
  mutate(sb_nummer = str_pad(str_extract(Raumbezug, "^[0-9]+"), 2, pad = "0"))

munich_map <- munich_map %>%
  mutate(sb_nummer = str_pad(as.character(sb_nummer), 2, pad = "0"))

# ② 选年份（可改）
target_year <- 2024
data_year <- combined %>% filter(Jahr == target_year)

# ③ 合并到地图，并在图上放编号
map_year <- left_join(munich_map, data_year, by = "sb_nummer")
label_points <- st_point_on_surface(munich_map) %>%
  mutate(id_label = sb_nummer)

# ④ 画图（女性失业率 = arbeitslos_anteil_weiblich）
ggplot(map_year) +
  geom_sf(aes(fill = arbeitslos_anteil_weiblich), color = "white", linewidth = 0.25) +
  geom_sf_text(data = label_points, aes(label = id_label),
               size = 3.5, fontface = "bold") +
  scale_fill_viridis_c(
    option = "plasma", direction = -1,
    labels = percent_format(accuracy = 0.01),
    name = "Arbeitslosenquote (w)"
  ) +
  labs(
    title = paste0("Weibliche Arbeitslosenquote – München ", target_year),
    subtitle = "Anteil der arbeitslosen Frauen an allen erwerbsfähigen Frauen",
    caption = "Datenquelle: Stadt München"
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position      = c(0.08, 0.12),     # 左下角；按需微调
    legend.justification = c(0, 0),
    legend.background    = element_rect(fill = scales::alpha("white", 0.85), color = NA)
  )



library(classInt)

cuts <- classIntervals(map_year$arbeitslos_anteil_weiblich, n = 5, style = "quantile")$brks

ggplot(map_year) +
  geom_sf(aes(fill = cut(arbeitslos_anteil_weiblich, breaks = cuts, include.lowest = TRUE)),
          color = "white", linewidth = 0.25) +
  scale_fill_brewer(palette = "YlOrRd", direction = 1,
                    name = "Arbeitslosenquote (w)",
                    labels = function(x) gsub(",", "–", x)) +
  labs(title = paste0("Weibliche Arbeitslosenquote – München ", target_year)) +
  theme_void(base_size = 12) +
  theme(legend.position = "bottom")


#——————————————


library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(scales)

# 1️⃣ 提取区号，确保和地图对应
combined <- combined %>%
  mutate(sb_nummer = str_pad(str_extract(Raumbezug, "^[0-9]+"), 2, pad = "0"))

munich_map <- munich_map %>%
  mutate(sb_nummer = str_pad(as.character(sb_nummer), 2, pad = "0"))

# 2️⃣ 选择年份（你可以换成 2010、2015 等）
target_year <- 2024
data_year <- combined %>% filter(Jahr == target_year)

# 3️⃣ 合并地图与失业率数据
map_year <- left_join(munich_map, data_year, by = "sb_nummer")

# 4️⃣ 在每个区中心添加编号
label_points <- st_point_on_surface(munich_map) %>%
  mutate(id_label = sb_nummer)

# 5️⃣ 绘图：渐变蓝色调，深蓝 = 高失业率
p_unemp <- ggplot(map_year) +
  geom_sf(aes(fill = arbeitslos_anteil_weiblich), color = "white", linewidth = 0.25) +
  geom_sf_text(data = label_points, aes(label = id_label),
               size = 3.5, fontface = "bold", color = "black") +
  scale_fill_gradient(
    low  = "#B3D9FF",   # 浅蓝
    high = "#003366",   # 深蓝
    name = "Arbeitslosenquote (w)",
    labels = percent_format(accuracy = 0.1)
  ) +
  labs(
    title = paste0("Weibliche Arbeitslosenquote in München (", target_year, ")"),
    subtitle = "Tiefblau = höhere Arbeitslosigkeit  |  Hellblau = niedrigere Arbeitslosigkeit",
    caption = "Datenquelle: Stadt München, Arbeitsmarktstatistik"
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position      = c(0.1, 0.15),          # 左下角
    legend.justification = c(0, 0),
    legend.background    = element_rect(fill = alpha("white", 0.85), color = NA),
    legend.title = element_text(size = 10, margin = margin(b = 4)),
    legend.text  = element_text(size = 9),
    plot.title   = element_text(face = "bold", size = 14),
    plot.subtitle= element_text(size = 10)
  )

p_unemp


##——————————————


munich_map <- munich_map %>%
  group_by(sb_nummer, name) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()


library(sf)
library(dplyr)

geojson_url <- "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gsm_wfs:vablock_stadtbezirke_opendata&outputFormat=application/json"
munich_map <- st_read(geojson_url)

# ✅ 按区号合并重复几何区域
munich_map <- munich_map %>%
  mutate(sb_nummer = str_pad(as.character(sb_nummer), 2, pad = "0")) %>%
  group_by(sb_nummer, name) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()


library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(scales)
library(gridExtra)    # tableGrob
library(ggplotify)    # as.ggplot
library(patchwork)    # 拼图

## 1) 准备数据：区号与年份
target_year <- 2024

combined <- combined %>%
  mutate(sb_nummer = str_pad(str_extract(Raumbezug, "^[0-9]+"), 2, pad = "0"))

munich_map <- munich_map %>%
  mutate(sb_nummer = str_pad(as.character(sb_nummer), 2, pad = "0"))

data_year <- combined %>% filter(Jahr == target_year)
map_year  <- left_join(munich_map, data_year, by = "sb_nummer")

label_points <- st_point_on_surface(munich_map) %>%
  mutate(id_label = sb_nummer)

## 2) 左侧：蓝色渐变失业率地图（深蓝=高，浅蓝=低）
p_map <- ggplot(map_year) +
  geom_sf(aes(fill = arbeitslos_anteil_weiblich), color = "white", linewidth = 0.25) +
  geom_sf_text(data = label_points, aes(label = id_label),
               size = 3.5, fontface = "bold", color = "black") +
  scale_fill_gradient(
    low  = "#B3D9FF",   # 浅蓝（低失业率）
    high = "#003366",   # 深蓝（高失业率）
    name = "Arbeitslosenquote (w)",
    labels = percent_format(accuracy = 0.1)
  ) +
  labs(
    title    = paste0("Weibliche Arbeitslosenquote in München (", target_year, ")"),
    subtitle = "Tiefblau = höhere Arbeitslosigkeit  |  Hellblau = niedrigere Arbeitslosigkeit",
    caption  = "Datenquelle: Stadt München"
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position      = c(0.1, 0.15),   # 图内左下角，可微调
    legend.justification = c(0, 0),
    legend.background    = element_rect(fill = alpha("white", 0.85), color = NA),
    legend.title = element_text(size = 10, margin = margin(b = 4)),
    legend.text  = element_text(size = 9),
    plot.title   = element_text(face = "bold")
  )

## 3) 右侧：编号 → 区名对照表
bezirk_table <- label_points %>%
  st_drop_geometry() %>%
  transmute(`Nr.` = id_label,
            Stadtbezirk = name) %>%
  arrange(`Nr.`)

# 若想让超长区名自动换行，可加：Stadtbezirk = stringr::str_wrap(name, width = 24)

tbl_grob <- tableGrob(
  bezirk_table, rows = NULL,
  theme = ttheme_minimal(
    base_size = 10,
    core    = list(padding = unit(c(3, 4), "mm")),
    colhead = list(fg_params = list(fontface = "bold"))
  )
)
p_table <- as.ggplot(tbl_grob)

## 4) 拼图：左图右表
final_plot <- p_map + p_table + plot_layout(widths = c(3, 1))
final_plot

## 5) 导出（可选）
# ggsave("map_unemployment_blue_with_table_2024.png", final_plot, width = 14, height = 8, dpi = 300)




#——————————————————————
#女性劳动人口2000-2024


library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(scales)
library(gridExtra)
library(ggplotify)
library(patchwork)

# --- 1) 地图数据：确保每个区号唯一（合并多多边形）
# 若你已运行过本段，可跳过
geojson_url <- "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gsm_wfs:vablock_stadtbezirke_opendata&outputFormat=application/json"
munich_map <- st_read(geojson_url, quiet = TRUE) %>%
  mutate(sb_nummer = str_pad(as.character(sb_nummer), 2, pad = "0")) %>%
  group_by(sb_nummer, name) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# --- 2) 计算 2000–2024 平均“女性劳动人口比例”（erwerb_weiblich）
# combined 中应含：Jahr, Raumbezug, erwerb_weiblich（0-1比例）
combined_avg <- combined %>%
  mutate(sb_nummer = str_pad(str_extract(Raumbezug, "^[0-9]+"), 2, pad = "0")) %>%
  filter(Jahr >= 2000, Jahr <= 2024) %>%
  group_by(sb_nummer) %>%
  summarise(
    erwerb_weiblich_mean = mean(erwerb_weiblich, na.rm = TRUE),
    .groups = "drop"
  )

# --- 3) 合并到地图，生成标签点与编号
map_avg <- left_join(munich_map, combined_avg, by = "sb_nummer")
label_points <- st_point_on_surface(munich_map) %>%
  mutate(id_label = sb_nummer)

# --- 4) 左侧：蓝色渐变地图（深蓝=更高的女性劳动人口比例）
p_map <- ggplot(map_avg) +
  geom_sf(aes(fill = erwerb_weiblich_mean), color = "white", linewidth = 0.25) +
  geom_sf_text(data = label_points, aes(label = id_label),
               size = 3.5, fontface = "bold", color = "black") + #B3D9FF
  scale_fill_gradient(
    low  = "#003366",   # 浅蓝（低）
    high = "#B3D9FF",   # 深蓝（高）
    name = "Ø Erwerbsfähige (w)\n2000–2024",
    labels = percent_format(accuracy = 0.1)
  ) +
  labs(
    title    = "Durchschnittlicher Anteil der weiblichen Erwerbsfähigen (2000–2024)",
    subtitle = "Tiefblau = höherer Anteil  |  Hellblau = niedrigerer Anteil",
    caption  = "Datenquelle: Stadt München"
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position      = c(0.10, 0.15),   # 左下角，可微调
    legend.justification = c(0, 0),
    legend.background    = element_rect(fill = alpha("white", 0.85), color = NA),
    legend.title = element_text(size = 10, margin = margin(b = 4)),
    legend.text  = element_text(size = 9),
    plot.title   = element_text(face = "bold")
  )

# 若要竖直色条放左下角，可加：
# guides(fill = guide_colorbar(direction = "vertical",
#                              barheight = unit(38, "mm"),
#                              barwidth  = unit(5, "mm")))

# --- 5) 右侧：编号 → 区名对照表
bezirk_table <- label_points %>%
  st_drop_geometry() %>%
  transmute(`Nr.` = id_label, Stadtbezirk = name) %>%
  arrange(`Nr.`)

tbl_grob <- tableGrob(
  bezirk_table, rows = NULL,
  theme = ttheme_minimal(
    base_size = 10,
    core    = list(padding = unit(c(3, 4), "mm")),
    colhead = list(fg_params = list(fontface = "bold"))
  )
)
p_table <- as.ggplot(tbl_grob)

# --- 6) 拼图：左图右表
final_plot <- p_map + p_table + plot_layout(widths = c(3, 1))
final_plot

# 可选导出：
# ggsave("avg_erwerbsfaehige_weiblich_2000_2024_map_with_table.png",
#        final_plot, width = 14, height = 8, dpi = 300)





#————————————————————————
#mean_arbeitslos_anteil_weiblich_2000-2024:


library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(scales)

# --- 1) 让区号与名称唯一（把同一区的多个多边形合并）
munich_map_u <- munich_map %>%
  mutate(sb_nummer = str_pad(as.character(sb_nummer), 2, pad = "0")) %>%
  group_by(sb_nummer, name) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# --- 2) 计算 2000–2024 平均女性失业率（按区）
avg_unemp <- combined %>%
  mutate(sb_nummer = str_pad(str_extract(Raumbezug, "^[0-9]+"), 2, pad = "0")) %>%
  filter(Jahr >= 2000, Jahr <= 2024) %>%
  group_by(sb_nummer) %>%
  summarise(
    unemp_w_mean = mean(arbeitslos_anteil_weiblich, na.rm = TRUE),
    .groups = "drop"
  )

# --- 3) 合并到地图 + 标签点（编号）
map_avg <- left_join(munich_map_u, avg_unemp, by = "sb_nummer")
label_points <- st_point_on_surface(munich_map_u) %>%
  mutate(id_label = sb_nummer)

# --- 4) 绘图：蓝色渐变（深蓝=更高平均失业率）
p_unemp_avg <- ggplot(map_avg) +
  geom_sf(aes(fill = unemp_w_mean), color = "white", linewidth = 0.25) +
  geom_sf_text(data = label_points, aes(label = id_label),
               size = 3.5, fontface = "bold", color = "black") +
  scale_fill_gradient(
    low  = "#B3D9FF",  # 浅蓝（低）
    high = "#003366",  # 深蓝（高）
    name = "Ø Arbeitslosenquote (w)\n2000–2024",
    labels = percent_format(accuracy = 0.1)
  ) +
  labs(
    title    = "Durchschnittliche weibliche Arbeitslosenquote nach Stadtbezirk (2000–2024)",
    subtitle = "Tiefblau = höhere durchschnittliche Arbeitslosigkeit  |  Hellblau = niedrigere",
    caption  = "Datenquelle: Stadt München, Arbeitsmarktstatistik"
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position      = c(0.10, 0.15),   # 左下角，可微调
    legend.justification = c(0, 0),
    legend.background    = element_rect(fill = alpha("white", 0.85), color = NA),
    legend.title = element_text(size = 10, margin = margin(b = 4)),
    legend.text  = element_text(size = 9),
    plot.title   = element_text(face = "bold", size = 14)
  )

p_unemp_avg


# ── 右侧对照表（编号 → 区名） ───────────────────────────────────────────
library(gridExtra)
library(ggplotify)
library(patchwork)

# 用与地图一致的数据源，保证顺序与编号正确
bezirk_table <- munich_map_u %>%
  st_drop_geometry() %>%
  transmute(`Nr.` = sb_nummer,
            Stadtbezirk = name) %>%
  arrange(`Nr.`)
# 长名字可自动换行（可选）
# bezirk_table$Stadtbezirk <- stringr::str_wrap(bezirk_table$Stadtbezirk, width = 28)

tbl_grob <- tableGrob(
  bezirk_table, rows = NULL,
  theme = ttheme_minimal(
    base_size = 10,
    core    = list(padding = unit(c(3, 4), "mm"),
                   fg_params = list(hjust = 0.5)),   # 居中些更紧凑
    colhead = list(fg_params = list(fontface = "bold"))
  )
)
p_table <- as.ggplot(tbl_grob)

# ── 左图右表拼板 ────────────────────────────────────────────────────────
final_plot <- p_unemp_avg + p_table + plot_layout(widths = c(3, 1))
final_plot

# 保存（可选）
# ggsave("avg_unemp_w_2000_2024_map_with_table.png",
#        final_plot, width = 14, height = 8, dpi = 300)


