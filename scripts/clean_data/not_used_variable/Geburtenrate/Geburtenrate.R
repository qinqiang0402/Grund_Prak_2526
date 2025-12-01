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

# plot by district
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
data_joined
data_wide

# function giving plot of birthrate & female employment
# input: district name
plot_birthrate_district <- function(input.district) {
  # variation of input
  districts <- unique(data_wide$Raumbezug)
  # input is number:
  if (is.numeric(input.district)) {
    number <- sprintf("%02d", input.district)   # example: 1 -> "01"
    district.name <- districts[str_detect(districts, paste0("^", number, " "))]
    
  } else {
    input.district <- as.character(input.district)
  # input is excactly same as data:
  if (input.district %in% districts) {
    district.name <- district.input
      
  } else {
    # input is a part of district name 
    # example: "Lehel" instead of "Altstadt - Lehel"
    district.name <- districts[str_detect(districts, regex(input.district, ignore_case = TRUE))]
    }
  }
  # error, if no district found with input
  if (length(district.name) == 0) {
    stop("No district found. Please check the input.")
  }
  
  # If more than one district found with input → plot the first & warning
  if (length(district.name) > 1) {
    message("ℹ More than one district been found. Now plotting: ", district.name[1])
    district.name <- district.name[1]
  }
  
  # data from long into wide
  df <- data_wide %>%
    filter(Raumbezug == district.name)
  
  df <- df %>% 
    tidyr::drop_na(birthrate, employment_female)
  
  # extra axis for variable "birthrate" for better vsualization
  range_emp  <- range(df$employment_female, na.rm = TRUE)
  range_birth <- range(df$birthrate, na.rm = TRUE)
  
  scale_factor <- diff(range_emp) / diff(range_birth)
  
  df <- df %>%
    mutate(birthrate_scaled = birthrate * scale_factor)
  
  # plot
  ggplot(df, aes(x = Jahr)) +
    # female employment (axis left)
    geom_line(aes(y = employment_female,
                  color = "Beschäftigungsrate"), linewidth = 1.1) +
    geom_point(aes(y = employment_female,
                   color = "Beschäftigungsrate"), size = 1.8) +
    
    # birthrate (axis right)
    geom_line(aes(y = birthrate_scaled,
                  color = "Allgemeine Geburtenrate"), linewidth = 1.1) +
    geom_point(aes(y = birthrate_scaled,
                   color = "Allgemeine Geburtenrate"), size = 1.8) +

    scale_y_continuous(
      name = "Beschäftigungsrate (%)",
      sec.axis = sec_axis(
        ~ . / scale_factor,
        name = "Allgemeine Geburtenrate"
      )
    ) +
    scale_x_continuous(breaks = seq(2000, 2024, by = 2)) +
    
    scale_color_manual(
      name   = "Indikator",
      values = c("Allgemeine Geburtenrate" = "#E74C3C",   # rot
                 "Beschäftigungsrate"      = "#3498DB")   # blau
    ) +
    
    labs(
      title    = "Allgemeine Geburtenrate und weibliche SV-pflichtige Beschäftigungsrate",
      subtitle = district.name,
      x = "Jahr"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title      = element_text(face = "bold", hjust = 0.5, size = 16),
      plot.subtitle   = element_text(hjust = 0.5),
      axis.title.y.right = element_text(margin = margin(l = 8)),
      legend.position = "bottom",
      legend.title    = element_text(face = "bold")
    )
}
plot_birthrate_district("Stadt")
