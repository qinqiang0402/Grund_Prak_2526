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

library(dplyr)
library(broom) 

korrelations_stats <- korrelations_daten %>%
  
  group_by(Raumbezug) %>%
  summarize(
    test_stats = list(broom::tidy(cor.test(mean_age, anteil))),
    .groups = "drop" # .groups="drop" ist gute Praxis
  ) %>%
  unnest(test_stats) %>%
  mutate(bezirk_nr = substr(Raumbezug, 1, 2))


library(ggplot2)


ggplot(korrelations_stats, 
       aes(x = reorder(Raumbezug, estimate), y = estimate)) +
  geom_segment(aes(xend = reorder(Raumbezug, estimate), yend = 0), color = "grey") +
  geom_point(aes(color = estimate), size = 3) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  coord_flip() +
  labs(
    title = "Korrelationskoeffizient zwischen Mütteralter (Erstgeburt) und Anteil Sozialversicherungspflichtigbeschäftigte Frauen",
    x = "Stadtteile (sortiert nach R-Wert)",
    y = "Pearson Korrelationskoeffizient",
    color = "R-Wert" # (DE) Legenden-Titel
  ) +
  theme_minimal()


#----------------------------------------------------------------------------------------------------------


library(ggplot2)
ggplot(korrelations_stats, 
       aes(x = reorder(Raumbezug, estimate), y = estimate)) +
  geom_segment(aes(x = reorder(Raumbezug, estimate), xend = reorder(Raumbezug, estimate), yend = 0), 
               color = "grey") +
  
  geom_point(aes(color = estimate), size = 3) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
  
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Korrelationskoeffizient zwischen Mütteralter (Erstgeburt) und Anteil Sozialversicherungspflichtigbeschäftigte Frauen",
    x = "Stadtteile (sortiert nach R-Wert)",
    y = "Pearson Korrelationskoeffizient",
    color = "R-Wert"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) 
