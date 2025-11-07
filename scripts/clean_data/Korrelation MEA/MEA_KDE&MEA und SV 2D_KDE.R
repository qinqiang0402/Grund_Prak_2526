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

install.packages("ggpubr")
library(ggpubr)

all_districts_and_city <- be_sheet %>%
  filter(
    Indikator == "Durchschnittsalter Mütter erstgebärend",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    mean_age = `Basiswert 1` / `Basiswert 2`
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




library(ggplot2)
library(ggpubr) 

ggplot(korrelations_daten_clean, aes(x = mean_age)) +
  geom_density(color = "blue", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(mean_age)), color = "red", linetype = "dashed") +
  
  labs(
    title = "KDE:Durchschnittliches Alter der Mutter (Erstgeburt)",
    x = "Durchschnittliches Alter der Mutter (Erstgeburt)",
    y = "Dichte"
  ) +
  theme_minimal()

#--------------------------------------------------------------------------------------
ggplot(korrelations_daten_clean, aes(x = mean_age, y = anteil)) +
  stat_density_2d(aes(fill = ..density..), geom = "tile", contour = FALSE) +
  scale_fill_viridis_c(option = "C", name = "Dichte") +

  labs(
    title = "KDE (Heatmap): Gemeinsame Dichte der beiden Merkmale",
    x = "Durchschnittliches Alter der Mutter (Erstgeburt)",
    y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)"
  ) +
  theme_minimal()



library(ggplot2)
ggplot(korrelations_daten_clean, aes(x = mean_age, y = anteil)) +
  stat_density_2d(aes(fill = ..density..), geom = "tile", contour = FALSE) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Dichte") +
labs(
  title = "KDE (Heatmap): Gemeinsame Dichte der beiden Merkmale",
  x = "Durchschnittliches Alter der Mutter (Erstgeburt)",
  y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)"
) +
  theme_minimal()



