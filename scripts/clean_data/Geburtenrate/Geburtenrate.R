install.packages("readxl")   
library(readxl)
install.packages("tidyr")
library(tidyr)
data_bis2009 <- read_excel("~/Documents/LMU/Projekt/export_bis2009.xlsx")
data_ab2010 <- read_excel("~/Documents/LMU/Projekt/export_ab2010.xlsx")
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
data <- bind_rows(data_bis2009, data_ab2010)
head(data)

population_2009 <- read_excel("~/Documents/LMU/Projekt/export_bis2009.xlsx", 
                                 sheet = "BEVÖLKERUNG") %>% 
  mutate(
    Indikatorwert = gsub(",", ".", Indikatorwert),
    Indikatorwert = gsub("%", "", Indikatorwert),
    Indikatorwert = trimws(Indikatorwert),
    Indikatorwert = as.numeric(Indikatorwert)
  )

population_2010 <- read_excel("~/Documents/LMU/Projekt/export_ab2010.xlsx", 
                                sheet = "BEVÖLKERUNG") %>%
  mutate(
    Indikatorwert = gsub(",", ".", Indikatorwert),
    Indikatorwert = gsub("%", "", Indikatorwert),
    Indikatorwert = trimws(Indikatorwert),
    Indikatorwert = as.numeric(Indikatorwert)
  )

population <- bind_rows(population_2009, population_2010)

labormarket_2009 <- read_excel("~/Documents/LMU/Projekt/export_bis2009.xlsx", 
                                  sheet = "ARBEITSMARKT") %>%
  mutate(
    Indikatorwert = gsub(",", ".", Indikatorwert),
    Indikatorwert = gsub("%", "", Indikatorwert),
    Indikatorwert = trimws(Indikatorwert),
    Indikatorwert = as.numeric(Indikatorwert)
  )

labormarket_2010 <- read_excel("~/Documents/LMU/Projekt/export_ab2010.xlsx", 
                                 sheet = "ARBEITSMARKT") %>%
  mutate(
    Indikatorwert = gsub(",", ".", Indikatorwert),
    Indikatorwert = gsub("%", "", Indikatorwert),
    Indikatorwert = trimws(Indikatorwert),
    Indikatorwert = as.numeric(Indikatorwert)
  )
labormarket <- bind_rows(labormarket_2009, labormarket_2010)

# plot nach Raumbezug：
install.packages("ggplot2")
library(ggplot2)

employment_female <- labormarket%>%
  filter(Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
         Ausprägung == "weiblich") %>%
  select(Jahr, Raumbezug, employment_female = Indikatorwert) %>%
  distinct(Raumbezug, Jahr, .keep_all = TRUE)
employment_female

birthrate <- population %>%
  filter(Indikator == "Allgemeine Geburtenrate") %>%
  mutate(Indikatorwert = Indikatorwert/10) %>%
  select(Jahr, Raumbezug, birthrate = Indikatorwert) %>%
  distinct(Raumbezug, Jahr, .keep_all = TRUE)
birthrate

data_joined <- left_join(birthrate, employment_female, by = c("Jahr", "Raumbezug")) %>%
  pivot_longer(cols = c("birthrate", "employment_female"),
               names_to = "Indikator",
               values_to = "Wert")

data_group1 <- data_joined %>%
  filter(
    Raumbezug %in% c("Stadt München", "01 Altstadt - Lehel", "02 Ludwigsvorstadt - Isarvorstadt", 
                     "03 Maxvorstadt", "04 Schwabing - West", "05 Au - Haidhausen")
  )
data_group2 <- data_joined %>%
  filter(
    Raumbezug %in% c("06 Sendling", "07 Sendling - Westpark", "08 Schwanthalerhöhe",
                     "09 Neuhausen - Nymphenburg", "10 Moosach", "11 Milbertshofen - Am Hart")
  )
data_group3 <- data_joined %>%
  filter(
    Raumbezug %in% c("12 Schwabing - Freimann", "13 Bogenhausen", "14 Berg am Laim", 
                     "15 Trudering - Riem", "16 Ramersdorf - Perlach", 
                     "17 Obergiesing - Fasangarten")
  )
data_group4 <- data_joined %>%
  filter(
    Raumbezug %in% c("18 Untergiesing - Harlaching", "19 Thalkirchen - Obersendling - Forstenried - Fürstenried - Solln",
                     "20 Hadern", "21 Pasing - Obermenzing", "22 Aubing - Lochhausen - Langwied",
                     "23 Allach - Untermenzing","24 Feldmoching - Hasenbergl","25 Laim")
  )
unique(data_group1$Indikator)

data_group1 %>%
  count(Raumbezug, Jahr, Indikator) %>%
  filter(n > 1)
anti_join(birthrate, employment_female, by = c("Raumbezug", "Jahr"))


#plot Munich areas
p1 <- ggplot(data_group1, aes(x = Jahr, y = Wert, color = Indikator,
                                  group = Indikator)) +
  geom_line(aes(color = Indikator),size = 1) +
  geom_point(aes(color = Indikator),size = 1.5) +
  facet_wrap(~ Raumbezug, ncol = 2) +
  scale_color_manual(
    values = c("birthrate" = "#E74C3C", "employment_female" = "#3498DB"),
    name = "Indikator",
    labels = c("Allgemeine Geburtenrate", "Beschäftigungsrate")) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 4)) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Allgemeine Geburtenrate und weiblicher Beschäftigungsrate Stadtteile Münchens",
    subtitle = "Teil 1",
    x = "Jahr",
    y = "Prozent (%)",
    color = "Indikator"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(size = 7),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )
p1
p2 <- ggplot(data_group2, aes(x = Jahr, y = Wert, color = Indikator,
                              group = Indikator)) +
  geom_line(aes(color = Indikator),size = 1) +
  geom_point(aes(color = Indikator),size = 1.5) +
  facet_wrap(~ Raumbezug, ncol = 2) +
  scale_color_manual(
    values = c("birthrate" = "#E74C3C", "employment_female" = "#3498DB"),
    name = "Indikator",
    labels = c("Allgemeine Geburtenrate", "Beschäftigungsrate")) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 4)) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Allgemeine Geburtenrate und weiblicher Beschäftigungsrate Stadtteile Münchens",
    subtitle = "Teil 2",
    x = "Jahr",
    y = "Prozent (%)",
    color = "Indikator"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(size = 7),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )
p2
p3 <- ggplot(data_group3, aes(x = Jahr, y = Wert, color = Indikator,
                              group = Indikator)) +
  geom_line(aes(color = Indikator),size = 1) +
  geom_point(aes(color = Indikator),size = 1.5) +
  facet_wrap(~ Raumbezug, ncol = 2) +
  scale_color_manual(
    values = c("birthrate" = "#E74C3C", "employment_female" = "#3498DB"),
    name = "Indikator",
    labels = c("Allgemeine Geburtenrate", "Beschäftigungsrate")) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 4)) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Allgemeine Geburtenrate und weiblicher Beschäftigungsrate Stadtteile Münchens",
    subtitle = "Teil 3",
    x = "Jahr",
    y = "Prozent (%)",
    color = "Indikator"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(size = 7),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )
p3
p4 <- ggplot(data_group4, aes(x = Jahr, y = Wert, color = Indikator,
                              group = Indikator)) +
  geom_line(aes(color = Indikator),size = 1) +
  geom_point(aes(color = Indikator),size = 1.5) +
  facet_wrap(~ Raumbezug, ncol = 2) +
  scale_color_manual(
    values = c("birthrate" = "#E74C3C", "employment_female" = "#3498DB"),
    name = "Indikator",
    labels = c("Allgemeine Geburtenrate", "Beschäftigungsrate")) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 4)) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Allgemeine Geburtenrate und weiblicher Beschäftigungsrate Stadtteile Münchens",
    subtitle = "Teil 4",
    x = "Jahr",
    y = "Prozent (%)",
    color = "Indikator"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(size = 7),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )
p4

#plot year
names(data_joined)
head(data_joined)
data_wide <- data_joined %>%
  pivot_wider(
    names_from = Indikator,
    values_from = Wert
  )
head(data_wide)

year_group1 <- data_wide %>%
  filter(
    Jahr %in% c(2000, 2001, 2002, 2003, 2004))
                
p_y1 <- ggplot(year_group1, aes(x = employment_female, y = birthrate, colour = Raumbezug)) +
  geom_point(size = 2, alpha = 0.8) +
  facet_wrap(~ Jahr, ncol = 5) +
  labs(
    title = "Zusammenhang: Allgemeine Geburtenrate und weiblicher Beschäftigungsrate",
    subtitle = "nach ausgewählten Jahren",
    x = "Beschäftigungsrate (%)",
    y = "Allgemeine Geburtenrate (%)",
    color = "Raumbezug"
  ) +
  scale_y_continuous(limits = c(0,6), breaks = seq(0, 6, 1)) +
  scale_x_continuous(limits = c(40,60), breaks = seq(40, 60, 5)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = 7),
    axis.title.y = element_text(size = 7),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11)
  )
p_y1

year_group2 <- data_wide %>%
  filter(
    Jahr %in% c(2005, 2006, 2007, 2008, 2009))
p_y2 <- ggplot(year_group2, aes(x = employment_female, y = birthrate, colour = Raumbezug)) +
  geom_point(size = 2, alpha = 0.8) +
  facet_wrap(~ Jahr, ncol = 5) +
  labs(
    title = "Zusammenhang: Allgemeine Geburtenrate und weiblicher Beschäftigungsrate",
    subtitle = "nach ausgewählten Jahren",
    x = "Beschäftigungsrate (%)",
    y = "Allgemeine Geburtenrate (%)",
    color = "Raumbezug"
  ) +
  scale_y_continuous(limits = c(0,6), breaks = seq(0, 6, 1)) +
  scale_x_continuous(limits = c(40,60), breaks = seq(40, 60, 5)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = 7),
    axis.title.y = element_text(size = 7),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11)
  )
p_y2

year_group3 <- data_wide %>%
  filter(
    Jahr %in% c(2010, 2011, 2012, 2013, 2014))
p_y3 <- ggplot(year_group3, aes(x = employment_female, y = birthrate, colour = Raumbezug)) +
  geom_point(size = 2, alpha = 0.8) +
  facet_wrap(~ Jahr, ncol = 5) +
  labs(
    title = "Zusammenhang: Allgemeine Geburtenrate und weiblicher Beschäftigungsrate",
    subtitle = "nach ausgewählten Jahren",
    x = "Beschäftigungsrate (%)",
    y = "Allgemeine Geburtenrate (%)",
    color = "Raumbezug"
  ) +
  scale_x_continuous(limits = c(40,70), breaks = seq(40, 70, 10)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = 7),
    axis.title.y = element_text(size = 7),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11)
  )
p_y3

year_group4 <- data_wide %>%
  filter(
    Jahr %in% c(2015, 2016, 2017, 2018, 2019))
p_y4 <- ggplot(year_group4, aes(x = employment_female, y = birthrate, colour = Raumbezug)) +
  geom_point(size = 2, alpha = 0.8) +
  facet_wrap(~ Jahr, ncol = 5) +
  labs(
    title = "Zusammenhang: Allgemeine Geburtenrate und weiblicher Beschäftigungsrate",
    subtitle = "nach ausgewählten Jahren",
    x = "Beschäftigungsrate (%)",
    y = "Allgemeine Geburtenrate (%)",
    color = "Raumbezug"
  ) +
  scale_x_continuous(limits = c(40,70), breaks = seq(40, 70, 5)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = 7),
    axis.title.y = element_text(size = 7),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11)
  )
p_y4

year_group5 <- data_wide %>%
  filter(
    Jahr %in% c(2020, 2021, 2022, 2023, 2024))
p_y5 <- ggplot(year_group5, aes(x = employment_female, y = birthrate, colour = Raumbezug)) +
  geom_point(size = 2, alpha = 0.8) +
  facet_wrap(~ Jahr, ncol = 5) +
  labs(
    title = "Zusammenhang: Allgemeine Geburtenrate und weiblicher Beschäftigungsrate",
    subtitle = "nach ausgewählten Jahren",
    x = "Beschäftigungsrate (%)",
    y = "Allgemeine Geburtenrate (%)",
    color = "Raumbezug"
  ) +
  scale_x_continuous(limits = c(40,70), breaks = seq(40, 70, 10)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = 7),
    axis.title.y = element_text(size = 7),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11)
  )
p_y5

install.packages("leaflet")
library(leaflet)
