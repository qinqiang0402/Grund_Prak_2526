library(tidyverse)
library(sf)
library(ggplot2)
library(dplyr)
library(stringr)
geojson_url <- "https://geoportal.muenchen.de/geoserver/gsm_wfs/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gsm_wfs:vablock_stadtbezirke_opendata&outputFormat=application/json"
munich_map <- st_read(geojson_url)

data_birthrate <- birthrate %>%
  group_by(Raumbezug) %>%
  summarise(mean_birthrate = mean(birthrate, na.rm = TRUE)) %>%
  mutate(district_number = as.numeric(str_extract(Raumbezug, "^\\d+"))) %>%
  filter(!is.na(district_number))

data_birthrate_map <- munich_map %>%
  mutate(district_number = as.numeric(sb_nummer)) %>%
  left_join(data_birthrate, by = "district_number")

label_pos <- data_birthrate_map %>%
  group_by(district_number) %>%
  summarise(geometry = st_union(geometry)) %>%    # Polygone des gleichen Bezirks zusammenführen
  mutate(centroid = st_centroid(geometry)) %>%    # Zentrum berechnen
  st_as_sf()  
# map plot
map_birthrate_plot <- ggplot(data_birthrate_map) +
  geom_sf(aes(fill = mean_birthrate), color = "white", size = 0.2) +
  geom_sf_text(data = label_pos, aes(label = sprintf("%02d", district_number)), color = "black", 
               size = 3.2, fontface = "bold") +
  scale_fill_gradient(
    low = "#deebf7",  # hellblau (niedrig)
    high = "#08306b", # dunkelblau (hoch)
    name = "Geburtenrate (%)"
  ) +
  labs(
    title = "Geburtenrate der Stadtbezirke Münchens",
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
map_birthrate_plot
#combine map plot + legend plot
combined_birthrate_plot <- map_birthrate_plot + legend_plot +
  plot_layout(widths = c(2.8, 1.6)) & 
  theme(plot.margin = margin(10, 10, 10, 10))
combined_birthrate_plot
