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

hmk_korr_gesamt_sw <- ggplot(korrelations_daten_clean, aes(x = hmk, y = anteil)) +
  geom_point(size = 1.3, color = "grey", alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1) +labs(
    x = "Anteil Haushalte mit Kindern (%)",
    y = "Anteil Frauenbeschäftigung (%)",
    caption = fussnote_text
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, size = 10, margin = margin(t = 10))
  )

hmk_korr_gesamt_sw
saveRDS(hmk_korr_gesamt_sw, "results/figures/Haushalt_mit_Kindern/hmk_korr_gesamt_sw.rds")


# -----------------------------------------------------------------------
hmk_point_line_nach_jahr_color <- ggplot(korrelations_daten_clean, aes(x = hmk, y = anteil)) +
  geom_smooth(aes(color = Jahr, group = Jahr),
              method = "lm", se = FALSE, linewidth = 1.1, alpha = 0.8) +
  geom_point(aes(color = Jahr), size = 1.1, alpha = 0.5) +
  geom_smooth(aes(group = 1), method = "lm", color = "black", linewidth = 1.1, se = FALSE) +
  scale_color_gradient(
    low = "#e5f5e0", 
    high = "#238b45",
    breaks = pretty_breaks(n = 5) 
  ) +
  coord_cartesian(xlim = c(8, 28), ylim = c(48, 68)) +
  labs(
    x = "Anteil Haushalte mit Kindern (%)",
    y = "Anteil Frauenbeschäftigung (%)",
    color = "Jahr"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(barwidth = 1, barheight = 10))

hmk_point_line_nach_jahr_color
saveRDS(hmk_point_line_nach_jahr_color, "results/figures/Haushalt_mit_Kindern/hmk_korr_point_line_nach_jahr_color.rds")

# ----------------------------------------------------------------------------
r_werte_check_year <- korrelations_daten_clean %>%
  group_by(Jahr) %>% 
  summarise(
    r_wert = cor(hmk, anteil)
  ) %>%
  mutate(
    is_negative = r_wert < 0,
    Jahr_Label = paste0(Jahr, " (R=", round(r_wert, 2), ")")
  )

plot_data_highlight_year <- korrelations_daten_clean %>%
  left_join(r_werte_check_year, by = "Jahr") 

hmk_non_simpson_nach_jahr <- ggplot(plot_data_highlight_year, aes(x = hmk, y = anteil)) +
  geom_smooth(
    data = subset(plot_data_highlight_year, is_negative == FALSE),
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
    subtitle = "Grau = Positiver Zusammenhang (R ≥ 0), Bunt = Negativer Zusammenhang (R < 0)",
    x = "Anteil Haushalte mit Kindern (%)",
    y = "Anteil Frauenbeschäftigung (%)",
    color = "Jahr mit R < 0" 
  ) +
  coord_cartesian(xlim = c(8, 28), ylim = c(48, 68)) +
  theme_minimal() +
  theme(legend.position = "right")

hmk_non_simpson_nach_jahr
saveRDS(hmk_non_simpson_nach_jahr, "results/figures/Haushalt_mit_Kindern/hmk_non_simpson_nach_jahr.rds")
