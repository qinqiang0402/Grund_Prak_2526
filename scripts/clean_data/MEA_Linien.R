library(readxl)
library(tidyverse)
library(ggplot2)

# Einlesen der Rohdaten aus den Excel-Dateien
export_be <- read_excel("data/raw/export_be.xlsx")
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")

# Datensatz: Durchschnittsalter Mütter erstgebärend
all_districts_and_city <- be_sheet %>%
  filter(
    Indikator == "Durchschnittsalter Mütter erstgebärend",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    mean_age = `Basiswert 1` / `Basiswert 2`
  )

# Durchschnitt über alle Jahre pro Stadtteil
district_averages <- all_districts_and_city %>%
  filter(Raumbezug != "Stadt München") %>%  # Gesamtschnitt rausfiltern
  group_by(Raumbezug) %>%
  summarise(
    avg_age_overall = mean(mean_age, na.rm = TRUE) # Mittelwert über alle Jahre
  )

# max und min Stadtteil zeigen
max_district <- district_averages %>%
  slice_max(order_by = avg_age_overall, n = 1) %>%
  pull(Raumbezug)

min_district <- district_averages %>%
  slice_min(order_by = avg_age_overall, n = 1) %>%
  pull(Raumbezug)


#Daten für Plot vorbereiten
data_for_plot <- all_districts_and_city %>%
  mutate(
    highlight_status = case_when(
      Raumbezug == "Stadt München" ~ "München avg", 
      Raumbezug == max_district ~ "Max (Durchschnitt)",
      Raumbezug == min_district ~ "Min (Durchschnitt)",
      TRUE ~ "Normal" 
    )
  )

#Ploten
ggplot(data = data_for_plot, mapping = aes(x = Jahr, y = mean_age)) +
  geom_line(data = . %>% filter(highlight_status == "Normal"), 
            aes(group = Raumbezug), 
            color = "grey80", 
            linewidth = 0.5) +
  geom_line(data = . %>% filter(highlight_status != "Normal"), 
            aes(color = highlight_status, group = Raumbezug), 
            linewidth = 1.2) +
  scale_color_manual(
    name = "Legende:", 
    values = c(
      "München avg" = "black",              
      "Max (Durchschnitt)" = "#e41a1c",       
      "Min (Durchschnitt)" = "#377eb8"       
    ),
    labels = c(
      "München avg" = "Gesamtdurchschnitt Stadt München",
      "Max (Durchschnitt)" = paste("Höchster Durchschnitt (Stadtteil):", max_district),
      "Min (Durchschnitt)" = paste("Niedrigster Durchschnitt (Stadtteil):", min_district)
    )
  ) +
  scale_x_continuous(breaks = seq(min(data_for_plot$Jahr), max(data_for_plot$Jahr), by = 1)) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.justification = "left") +
  labs(
    title = "Durchschnittsalter von Müttern bei Erstgeburt in München",
    subtitle = "Vergleich der Stadtteile mit dem gesamtstädtischen Durchschnitt",
    x = "Jahr",
    y = "Durchschnittliches Alter (Jahre)"
  )

