employment_female
# employment rate average
rate_average <- employment_female %>%
  group_by(Raumbezug) %>%
  mutate(mean_rate = mean(employment_female, na.rm = TRUE)) %>%
  ungroup()
# district with lowest, highest average
lowest_district  <- rate_average %>%
  filter(Raumbezug != "Stadt München") %>%
  group_by(Raumbezug) %>%
  summarise(mean_rate = mean(employment_female, na.rm = TRUE)) %>%
  slice_min(mean_rate, n = 1) %>%
  pull(Raumbezug)

highest_district <- rate_average %>%
  filter(Raumbezug != "Stadt München") %>%
  group_by(Raumbezug) %>%
  summarise(mean_rate = mean(employment_female, na.rm = TRUE)) %>%
  slice_max(mean_rate, n = 1) %>%
  pull(Raumbezug)

line_data <- employment_female %>%
  mutate(
    line_group = case_when(
      Raumbezug == "Stadt München" ~ "Stadt München", 
      Raumbezug == highest_district ~ "highest",
      Raumbezug == lowest_district ~ "lowest",
      TRUE ~ "rest"
      )
  )
line_data
#line diagram
ggplot() +
  geom_line(data = line_data %>% 
              filter(line_group =="rest"),
    aes(x = Jahr, y = employment_female, group = Raumbezug), 
    color = "grey80", 
    linewidth = 0.5) +
  geom_line(data = line_data %>% 
              filter(line_group != "rest"), 
    aes(x = Jahr, y = employment_female,color = line_group, group = Raumbezug), 
    linewidth = 1.2) +
  scale_color_manual(
   name = "Legende",
   values = c(
     "Stadt München" = "black",
     "highest" = "red",
     "lowest" = "blue"),
   labels = c(
     "Stadt München" = "Gesamtdurchschnitt Münchens",
     "highest" = paste(highest_district, "(Bezirk mit höchstem Durchschnitt)"),
     "lowest" = paste(lowest_district, "(Bezirk mit niedrigstem Durchschnitt)")
   )
    ) +
  labs(
       title = "Entwicklung der weiblichen sozialversicherungspflichtig Beschäftigte in München (2000–2024)",
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
