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

arbeitslos_anteil_frau <- ar_sheet %>%
  filter(
    Indikator == "Arbeitslose - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(
    anteil = 100 * `Basiswert 1` / `Basiswert 2`
  )


korrelations_daten <- inner_join(
  all_districts_and_city, 
  arbeitslos_anteil_frau, 
  by = c("Raumbezug", "Jahr")
)



korrelations_daten_clean <- korrelations_daten %>%
  filter(Raumbezug != "Stadt München")

ggplot(korrelations_daten_clean, aes(x = mean_age, y = anteil)) +
  geom_point(size = 0.5, color = "grey") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  stat_cor(method = "pearson") +
labs(
  title = "Korrelationskoeffizient zwischen Mütteralter (Erstgeburt) und Frauenarbeitslosigkeit",
  x = "Durchschnittliches Alter der Mutter (Erstgeburt)",
  y = "Anteil arbeitslose Frauen (%)"
)

#--------------------------------------------------------------------------------------------------------
library(ggplot2)
library(ggpubr)

ggplot(korrelations_daten, aes(x = mean_age, y = anteil)) +
  geom_point(aes(color = Raumbezug), 
             size = 1.0,  
             alpha = 0.7) + 
  geom_smooth(method = "lm", color = "blue", se = FALSE, size = 1) +
  stat_cor(method = "pearson", 
           label.x = 29.5,
           label.y = 4.8,
           color = "black") +
  labs(
    title = "Korrelationskoeffizient zwischen Mütteralter (Erstgeburt) und Frauenarbeitslosigkeit",
    x = "Durchschnittliches Alter der Mutter (Erstgeburt)",
    y = "Anteil arbeitslose Frauen (%)",
    color = "Bezirk (Raumbezug)" 
  ) +
  guides(color = guide_legend(ncol = 1)) +
  
  theme_minimal()
#-------------------------------------------------------------------------------------------------------
