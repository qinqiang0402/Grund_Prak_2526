library(readxl)
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(scales)

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
    x = "Haushalte mit Kindern (%)",
    y = "Frauenbeschäftigung (%)",
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
    x = "Haushalte mit Kindern (%)",
    y = "Frauenbeschäftigung (%)",
    color = "Jahr"
  ) +
  theme_minimal() +
  guides(color = guide_colorbar(barwidth = 1, barheight = 10))

hmk_point_line_nach_jahr_color
saveRDS(hmk_point_line_nach_jahr_color, "results/figures/Haushalt_mit_Kindern/hmk_korr_point_line_nach_jahr_color.rds")

