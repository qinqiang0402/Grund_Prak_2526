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

all_districts_and_city <- be_sheet %>%
  filter(
    Indikator == "Durchschnittsalter Mütter erstgebärend",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    mean_age = `Basiswert 1` / `Basiswert 2`
  )


library(ggplot2)
ggplot(data = all_districts_and_city, 
       mapping = aes(x = Jahr, y = Raumbezug, fill = mean_age)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "magma", direction = -1) + 
  labs(
    title = "Heatmap: Durchschnittsalter von Müttern bei Erstgeburt",
    subtitle = "Vergleich der Münchner Stadtbezirke über die Jahre",
    x = "Jahr",
    y = "Stadtbezirk",
    fill = "Durchschn.\nAlter"
  ) +
  
  theme_minimal()
