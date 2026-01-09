birthrate_average <- birthrate %>%
  group_by(Raumbezug) %>%
  mutate(mean_birthrate = mean(birthrate, na.rm = TRUE)) %>%
  ungroup()

# district with lowest average
lowest_birth_district <- birthrate_average %>%
  filter(Raumbezug != "Stadt München") %>%
  group_by(Raumbezug) %>%
  summarise(mean_birthrate = mean(birthrate, na.rm = TRUE)) %>%
  slice_min(mean_birthrate, n = 1) %>%
  pull(Raumbezug)

# district with highest average
highest_birth_district <- birthrate_average %>%
  filter(Raumbezug != "Stadt München") %>%
  group_by(Raumbezug) %>%
  summarise(mean_birthrate = mean(birthrate, na.rm = TRUE)) %>%
  slice_max(mean_birthrate, n = 1) %>%
  pull(Raumbezug)

#group Munich, district highest, district lowest
line_birthrate_data <- birthrate %>%
  mutate(
    line_birthrate_group = case_when(
      Raumbezug == "Stadt München" ~ "Stadt München", 
      Raumbezug == highest_birth_district ~ "highest",
      Raumbezug == lowest_birth_district ~ "lowest",
      TRUE ~ "rest"
    )
  )

#line diagram birthrate
ggplot() +
  geom_line(data = line_birthrate_data %>% 
              filter(line_birthrate_group =="rest"),
            aes(x = Jahr, y = birthrate, group = Raumbezug), 
            color = "grey80", 
            linewidth = 0.5) +
  geom_line(data = line_birthrate_data %>% 
              filter(line_birthrate_group != "rest"), 
            aes(x = Jahr, y = birthrate,color = line_birthrate_group, group = Raumbezug), 
            linewidth = 1.2) +
  scale_color_manual(
    name = "Legende",
    values = c(
      "Stadt München" = "black",
      "highest" = "red",
      "lowest" = "blue"),
    labels = c(
      "Stadt München" = "Gesamtdurchschnitt Münchens",
      "highest" = paste(highest_birth_district, "(Bezirk mit höchstem Durchschnitt)"),
      "lowest" = paste(lowest_birth_district, "(Bezirk mit niedrigstem Durchschnitt)")
    )
  ) +
  labs(
    title = "Entwicklung der Geburtenrate in Stadtbezirken Münchens (2000-2024)",
    x = "Jahr",
    y = "Anteil (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
