library(tidyverse)

setwd("/Users/qq/QQ/LMU/P_11_Grund_Prak/Grund_Prak_2526/scripts/clean_data/Haushalt_Alleinerziehend_weiblich")
getwd()


ha_weiblich <- readRDS("/Users/qq/QQ/LMU/P11_Grund_Prak/Grund_Prak_2526/scripts/clean_data/Haushalt_Alleinerziehend_weiblich/haushalte_alleinerziehend_weiblich.rds")
arbeitslos_weiblich <- readRDS("/Users/qq/QQ/LMU/P11_Grund_Prak/Grund_Prak_2526/scripts/clean_data/Haushalt_Alleinerziehend_weiblich/arbeitslos_weiblich.rds")

df_merge <- inner_join(ha_weiblich, arbeitslos_weiblich,
                       by = c("Jahr","Raumbezug"),
                       suffix = c("_ha","_arb"))

df_time <- df_merge %>%
  group_by(Jahr) %>%
  summarise(
    mean_ha = mean(Indikatorwert_ha, na.rm = TRUE),
    mean_arb = mean(Indikatorwert_arb, na.rm = TRUE)
  )


# timline
ggplot(df_time, aes(x = Jahr)) +
  geom_line(aes(y = mean_ha, color = "Alleinerziehende Frauen")) +
  geom_line(aes(y = mean_arb, color = "Arbeitslose Frauen")) +
  labs(
    title = "Zeitliche Entwicklung: Haushalte Alleinerziehender Frauen vs. Arbeitslosenquote",
    y = "Indikatorwert (Proportion)",
    color = "Indikator"
  ) +
  theme_minimal()



# Ploten nach Stadtteil

# 1️⃣.    nach Stadtteil
df_time_bezirk <- df_merge %>%
  group_by(Jahr, Raumbezug) %>%
  summarise(
    mean_ha = mean(Indikatorwert_ha, na.rm = TRUE),
    mean_arb = mean(Indikatorwert_arb, na.rm = TRUE)
  )

# 
df_long <- df_time_bezirk %>%
  pivot_longer(
    cols = c(mean_ha, mean_arb),
    names_to = "Indikator",
    values_to = "Wert"
  )

### facet nach Stadtteil
ggplot(df_long, aes(x = Jahr, y = Wert, color = Indikator)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ Raumbezug, scales = "free_y") +   
  labs(
    title = "Entwicklung von Alleinerziehendenhaushalten (w) und Arbeitslosenquote (w)",
    subtitle = "nach Stadtbezirken in München",
    x = "Jahr",
    y = "Indikatorwert",
    color = "Indikator"
  ) +
  scale_color_manual(
    values = c("mean_ha" = "#E69F00", "mean_arb" = "#56B4E9"),
    labels = c("Alleinerziehende Frauen", "Arbeitslose Frauen")
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8, face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )


# 1) ausgewählt Spalten
plot_dat <- df_merge %>%
  select(Jahr, Raumbezug, Indikatorwert_ha, Indikatorwert_arb) %>%
  mutate(
    Bezirk_Nr = str_extract(Raumbezug, "^[0-9]+")   # 提取区号（01, 02, ...）
  ) %>%
  drop_na()

# 2) Ausgewählt Jahre
years_to_plot <- plot_dat %>% distinct(Jahr) %>% arrange(desc(Jahr)) %>% 
  slice_head(n = 13) %>% pull(Jahr)          # letzte zwei Jahre
plot_dat <- filter(plot_dat, Jahr %in% years_to_plot)

# 3) facetieren
g <- ggplot(plot_dat, aes(
  x = Indikatorwert_ha,
  y = Indikatorwert_arb,
  color = Raumbezug
)) +
  geom_point(size = 2, alpha = 0.8) +
  
  
  geom_text(aes(label = Bezirk_Nr),
            size = 2.5,
            vjust = -0.7,
            hjust = 0.5,
            color = "black") +
  
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.6) +
  facet_wrap(~ Jahr, scales = "free", ncol = 4) +
  labs(
    title = "Zusammenhang: Alleinerziehende (w) vs. Arbeitslosenquote (w)",
    subtitle = "Jeder Punkt = Stadtbezirk, Zahl = Bezirksnummer, Farben = Stadtbezirke",
    x = "Anteil Haushalte alleinerziehender Frauen",
    y = "Arbeitslosenquote Frauen",
    color = "Stadtbezirk"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 7),
    strip.text = element_text(size = 9, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )

print(g)




#-----------------------------------------------------------


#  2012 -2024
plot_dat_filtered <- plot_dat %>%
  filter(Jahr >= 2012 & Jahr <= 2024)

# nach Jahre verteilt
plot_list <- split(plot_dat_filtered, plot_dat_filtered$Jahr)

# loop
for (jahr in names(plot_list)) {
  df_year <- plot_list[[jahr]]
  
  p <- ggplot(df_year, aes(
    x = Indikatorwert_ha,
    y = Indikatorwert_arb,
    color = Raumbezug
  )) +
    geom_point(size = 2, alpha = 0.8) +
    geom_text(aes(label = Bezirk_Nr),
              size = 2.5, vjust = -0.7, hjust = 0.5, color = "black") +
    geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.6) +
    labs(
      title = paste("Zusammenhang: Alleinerziehende (w) vs. Arbeitslosenquote (w) — Jahr", jahr),
      subtitle = "Jeder Punkt = Stadtbezirk, Zahl = Bezirksnummer, Farben = Stadtbezirke",
      x = "Anteil Haushalte alleinerziehender Frauen",
      y = "Arbeitslosenquote Frauen",
      color = "Stadtbezirk"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      legend.key.size = unit(0.5, "cm"),
      legend.text = element_text(size = 7),
      plot.title = element_text(size = 14, face = "bold")
    )
  
  # ✅ 只显示，不保存
  print(p)
  cat("--------- Jahr:", jahr, "---------\n")
}

