install.packages("sf")
library(tidyverse)
library(sf)
library(ggplot2)
library(dplyr)
library(stringr)
geojson_url <- "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gsm_wfs:vablock_stadtbezirke_opendata&outputFormat=application/json"
munich_map <- st_read(geojson_url)

data_employment <- employment_female %>%
  group_by(Raumbezug) %>%
  summarise(mean_employment = mean(employment_female, na.rm = TRUE)) %>%
  mutate(district_number = as.numeric(str_extract(Raumbezug, "^\\d+"))) %>%
  filter(!is.na(district_number))

data_employment_map <- munich_map %>%
  mutate(district_number = as.numeric(sb_nummer)) %>%
  left_join(data_employment, by = "district_number")

label_positions <- data_employment_map %>%
  group_by(district_number) %>%
  summarise(geometry = st_union(geometry)) %>%    # Polygone des gleichen Bezirks zusammenführen
  mutate(centroid = st_centroid(geometry)) %>%    # Zentrum berechnen
  st_as_sf()    

# map plot
map_employment_plot <- ggplot(data_employment_map) +
  geom_sf(aes(fill = mean_employment), color = "white", size = 0.2) +
  geom_sf_text(data = label_positions, aes(label = sprintf("%02d", district_number)), color = "black", 
               size = 3.2, fontface = "bold") +
  scale_fill_gradient(
    low = "#deebf7",  # hellblau (niedrig)
    high = "#08306b", # dunkelblau (hoch)
    name = "Beschäftigungsrate (%)"
  ) +
  labs(
    title = "Anteil weiblich sozialversicherungspflichtig Beschäftigte Münchens",
    subtitle = "Durchschnitt der Jahren 2000–2024",
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "left",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
map_employment_plot

# districts legend
district_labels <- data.frame(
  district_number = 1:25,
  district = c(
    "Altstadt-Lehel", "Ludwigsvorstadt-Isarvorstadt", "Maxvorstadt", "Schwabing-West",
    "Au-Haidhausen", "Sendling", "Sendling-Westpark", "Schwanthalerhöhe",
    "Neuhausen-Nymphenburg", "Moosach", "Milbertshofen-Am Hart", "Schwabing-Freimann",
    "Bogenhausen", "Berg am Laim", "Trudering-Riem", "Ramersdorf-Perlach",
    "Obergiesing-Fasangarten", "Untergiesing-Harlaching", "Thalkirchen-Obersendling-Forstenried-Fürstenried-Solln",
    "Hadern", "Pasing-Obermenzing", "Aubing-Lochhausen-Langwied", "Allach-Untermenzing",
    "Feldmoching-Hasenbergl", "Laim"
  )
)
district_labels <- district_labels %>%
  arrange(district_number) %>%
  mutate(row = district_number)


legend_plot <- ggplot(district_labels, aes(x = 1, y = -row)) +
  geom_text(
    aes(label = paste0(sprintf("%02d", district_number), " – ", district)),
    hjust = 0, size = 2.5, lineheight = 0.6
  ) +
  theme_void() +
  xlim(0.8, 3.2) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0, size = 8),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  annotate("text", x = 1, y = 1.5, label = "Bezirke Münchens", 
           hjust = 0, fontface = "bold", size = 4) +
  scale_x_continuous(limits = c(0.8, 3.2))
legend_plot

# combine map plot + legend plot
combined_employment_plot <- map_employment_plot + legend_plot +
  plot_layout(widths = c(2.8, 1.6)) & 
  theme(plot.margin = margin(10, 10, 10, 10))
combined_employment_plot
