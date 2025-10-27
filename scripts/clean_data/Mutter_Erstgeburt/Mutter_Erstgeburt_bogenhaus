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

bogenhausen_mutter_alter <- be_sheet %>%
  filter(
    Indikator == "Durchschnittsalter Mütter erstgebärend",
    Ausprägung == "insgesamt",
    Raumbezug == "13 Bogenhausen"
  ) %>%
  mutate(
    mean_age = `Basiswert 1` / `Basiswert 2`
  )


ggplot(data = bogenhausen_mutter_alter, mapping = aes(x = Jahr, y = mean_age)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Durchschnittsalter von Müttern bei Erstgeburt in Bogenhausen",
    subtitle = "Zeitliche Entwicklung des Durchschnittsalters",
    x = "Jahr",
    y = "Durchschnittliches Alter (Jahre)"
  )+ 
  scale_x_continuous(breaks = bogenhausen_mutter_alter$Jahr)




ggplot(data = bogenhausen_mutter_alter, mapping = aes(x = Jahr, y = mean_age)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(
    aes(label = round(mean_age, 2)),  
    vjust = -1.5,                     
    size = 3.5,                       
    color = "black"                  
  ) +
  scale_x_continuous(breaks = bogenhausen_mutter_alter$Jahr) +
  scale_y_continuous(breaks = seq(31.6, 32.7, by = 0.1), limits = c(31.6, 32.7)) +
  labs(
    title = "Durchschnittsalter von Müttern bei Erstgeburt in Bogenhausen",
    subtitle = "Zeitliche Entwicklung des Durchschnittsalters",
    x = "Jahr",
    y = "Durchschnittliches Alter (Jahre)",
    caption = "Datenquelle: Statistisches Amt München"
  ) +
  theme_minimal()


#install.packages("janitor")
#library(janitor)

#be_sheet_clean <- be_sheet %>% clean_names()
#colnames(be_sheet_clean)
#be_analysis <- be_sheet_clean %>%
#mutate(mean_age = basiswert_2 / basiswert_1) 

