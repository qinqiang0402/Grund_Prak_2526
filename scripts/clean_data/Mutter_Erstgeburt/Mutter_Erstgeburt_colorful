install.packages("readxl")
library(readxl)
export_be <- read_excel("data/raw/export_be.xlsx")
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
View(be_sheet)

export_ar <- read_excel("data/raw/export_ar.xlsx")
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")
View(ar_sheet)

install.packages("tidyverse")
library(tidyverse)

all_districts_mean_age <- be_sheet %>%
  filter(
    Indikator == "Durchschnittsalter Mütter erstgebärend",
    Ausprägung == "insgesamt",
    Raumbezug != "Stadt München"
  ) %>%
  mutate(
    mean_age = `Basiswert 1` / `Basiswert 2`
  )



district_averages <- all_districts_mean_age %>%
  group_by(Raumbezug) %>% 
  summarise(avg_age_overall = mean(mean_age, na.rm = TRUE))

max_district <- district_averages %>%
  slice_max(order_by = avg_age_overall, n = 1) %>%
  pull(Raumbezug)

min_district <- district_averages %>%
  slice_min(order_by = avg_age_overall, n = 1) %>%
  pull(Raumbezug)

data_for_plot <- all_districts_mean_age %>%
  mutate(
    highlight_status = case_when(
      Raumbezug == max_district ~ "Max (Durchschnitt)",
      Raumbezug == min_district ~ "Min (Durchschnitt)",
      TRUE ~ "Normal"
    )
  )

ggplot(data = data_for_plot, mapping = aes(x = Jahr, y = mean_age, color = Raumbezug)) +
  geom_line(data = . %>% filter(highlight_status == "Normal")) +
  geom_line(data = . %>% filter(highlight_status != "Normal"), linewidth = 1.2) +
  scale_x_continuous(breaks = seq(min(data_for_plot$Jahr), max(data_for_plot$Jahr), by = 1)) +
  guides(color = guide_legend(ncol = 1)) +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "cm")
  ) +
  
  labs(
    title = "Durchschnittsalter von Müttern bei Erstgeburt in München",
    subtitle = "Alle Stadtbezirke im Vergleich (höchster/niedrigster Gesamtdurchschnitt hervorgehoben)",
    x = "Jahr",
    y = "Durchschnittliches Alter (Jahre)"
  )




