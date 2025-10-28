install.packages("sf")

library(tidyverse)
library(sf)

geojson_url <- "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gsm_wfs:vablock_stadtbezirke_opendata&outputFormat=application/json"
munich_map <- st_read(geojson_url)

install.packages("readxl")
library(readxl)
export_be <- read_excel("data/raw/export_be.xlsx")
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")

export_ar <- read_excel("data/raw/export_ar.xlsx")
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")

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

average_data <- all_districts_and_city %>%
  group_by(Raumbezug) %>%
  summarise(mean_age_overall = mean(mean_age, na.rm = TRUE))


average_data_cleaned <- average_data %>%
  mutate(
    bezirksnummer = as.numeric(str_extract(Raumbezug, "^\\d+"))
  ) %>%
  filter(!is.na(bezirksnummer))

average_data_formatted <- average_data_cleaned %>%
  mutate(
    bezirksnummer = sprintf("%02d", bezirksnummer)
  )

merged_data_avg <- merge(
  munich_map,
  average_data_formatted,
  by.x = "sb_nummer", 
  by.y = "bezirksnummer",     
  all.x = TRUE                
)

ggplot(data = merged_data_avg, 
       mapping = aes(geometry = geometry, fill = mean_age_overall)) +
  geom_sf(color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Durchschnittliches\nAlter 2015-2024") +
  labs(
    title = "Durchschnittsalter von Müttern bei Erstgeburt in München",
    subtitle = "Durchschnitt der Jahre 2015-2024"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5)            
  )





#merged_data_avg_with_textcolor <- merged_data_avg %>%
  #mutate(
    #text_color = ifelse(mean_age_overall < 31.8, "black", "white")
#ggplot(data = merged_data_avg_with_textcolor,
       #mapping = aes(geometry = geometry, fill = mean_age_overall)) +
  #geom_sf(color = "white", linewidth = 0.2) +
  #geom_sf_text(aes(label = name, color = text_color), 
               #size = 2.5,
               #check_overlap = TRUE) +
  #scale_color_manual(values = c("black" = "black", "white" = "white"), guide = "none") + 
 # scale_fill_viridis_c(option = "magma", direction = -1, name = "Durchschnittliches\nAlter 2015-2024") +
  #labs(
    #title = "Durchschnittsalter von Müttern bei Erstgeburt in München",
    #subtitle = "Durchschnitt der Jahre 2015-2024"
 # ) +
  #theme_void() +
  #theme(
    #legend.position = "right",
   # plot.title = element_text(hjust = 0.5, face = "bold"),
   # plot.subtitle = element_text(hjust = 0.5)
 # )











