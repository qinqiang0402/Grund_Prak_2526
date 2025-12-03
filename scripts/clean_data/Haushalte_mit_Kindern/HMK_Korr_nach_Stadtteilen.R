library(readxl)
library(tidyverse)
library(ggpubr)
library(ggplot2)


export_be <- read_excel("data/raw/export_be.xlsx")
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")

export_ar <- read_excel("data/raw/export_ar.xlsx")
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")


all_districts_and_city <- be_sheet %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    hmk = 100 * `Basiswert 1` / `Basiswert 2`
  )

Sozialversicherungspflichtig_Beschäftigte_anteil_frau <- ar_sheet %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(
    anteil = 100 * `Basiswert 1` / `Basiswert 2`
  )


korrelations_daten <- inner_join(
  all_districts_and_city, 
  Sozialversicherungspflichtig_Beschäftigte_anteil_frau, 
  by = c("Raumbezug", "Jahr")
)

korrelations_daten_clean <- korrelations_daten %>%
  filter(Raumbezug != "Stadt München")

test_res <- cor.test(korrelations_daten_clean$hmk, korrelations_daten_clean$anteil)
p_txt <- ifelse(test_res$p.value < 0.001, "< 0.001", round(test_res$p.value, 3))
fussnote_text <- paste0("R = ", round(test_res$estimate, 2), ", p = ", p_txt)

hmk_korr_gesamt_point_sw <- ggplot(korrelations_daten_clean, aes(x = hmk, y = anteil)) +
  geom_point(size = 1.3, color = "grey", alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1) +
  labs(
    x = "Haushalte mit Kindern (%)",
    y = "Frauenbeschäftigung (%)",
    caption = fussnote_text
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, size = 10, margin = margin(t = 10))
  )

hmk_korr_gesamt_point_sw
saveRDS(hmk_korr_gesamt_point_sw, "results/figures/Haushalt_mit_Kindern/hmk_korr_gesamt_point_sw.rds")

#-----------------------------------------------------------------------
plot_data_final <- korrelations_daten_clean %>%
  group_by(Raumbezug) %>%
  mutate(
    r = cor(hmk, anteil, use = "complete.obs"),
    r_label = sprintf("(R=%.2f)", r),
    Raumbezug_Label = paste(Raumbezug, r_label, sep = " ")
  ) %>%
  ungroup()


hmk_korr_point_line_nach_stadtteile_color<- ggplot(plot_data_final, aes(x = hmk, y = anteil)) +
  geom_smooth(aes(group = Raumbezug_Label), 
              method = "lm", se = FALSE, 
              color = "grey", linewidth = 1.1, alpha = 0.8) +
  geom_point(color = "grey", size = 1.1, alpha = 0.5) +
  geom_smooth(aes(group = 1), method = "lm", color = "black", linewidth = 1.1, se = FALSE) +
  labs(
    x = "Haushalte mit Kindern (%)",
    y = "Frauenbeschäftigung (%)"
  ) +
  coord_cartesian(xlim = c(8, 28), ylim = c(48, 68)) +
  theme_minimal() +
  theme(legend.position = "none")


hmk_korr_point_line_nach_stadtteile_color

saveRDS(hmk_korr_point_line_nach_stadtteile_color, "results/figures/Haushalt_mit_Kindern/hmk_korr_point_line_nach_stadtteile_color.rds")


#----------------------------------------------------------------------------
r_werte_check <- korrelations_daten_clean %>%
  group_by(Raumbezug) %>%
  summarise(
    r_wert = cor(hmk, anteil)
  ) %>%
  mutate(
    is_negative = r_wert < 0,
    Raumbezug_Label = paste0(Raumbezug, " (R=", round(r_wert, 2), ")")
  )

plot_data_highlight <- korrelations_daten_clean %>%
  left_join(r_werte_check, by = "Raumbezug")

hmk_simpson_stadtteile_kleiner_0 <- ggplot(plot_data_highlight, aes(x = hmk, y = anteil)) +
  geom_smooth(
    data = subset(plot_data_highlight, is_negative == FALSE),
    aes(group = Raumbezug), 
    method = "lm", 
    se = FALSE, 
    color = "grey85", 
    linewidth = 0.8,
    alpha = 0.5
  ) +
  geom_smooth(
    data = subset(plot_data_highlight, is_negative == TRUE),
    aes(color = Raumbezug_Label, group = Raumbezug), 
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
  labs(
    x = "Haushalte mit Kindern (%)",
    y = "Frauenbeschäftigung (%)"
  ) +
  coord_cartesian(xlim = c(8, 28), ylim = c(48, 68)) +
  theme_minimal() +
  theme(legend.position = "none")

hmk_simpson_stadtteile_kleiner_0
saveRDS(hmk_simpson_stadtteile_kleiner_0, "results/figures/Haushalt_mit_Kindern/hmk_simpson_stadtteile_kleiner_0.rds")
