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
lowest_birth_district
# district with highest average
highest_birth_district <- birthrate_average %>%
  filter(Raumbezug != "Stadt München") %>%
  group_by(Raumbezug) %>%
  summarise(mean_birthrate = mean(birthrate, na.rm = TRUE)) %>%
  slice_max(mean_birthrate, n = 1) %>%
  pull(Raumbezug)
highest_birth_district
#group Munich, highest, lowest
line_birthrate_data <- birthrate %>%
  mutate(
    line_birthrate_group = case_when(
      Raumbezug == "Stadt München" ~ "Stadt München", 
      Raumbezug == highest_birth_district ~ "highest",
      Raumbezug == lowest_birth_district ~ "lowest",
      TRUE ~ "rest"
    )
  )

# three district with highest average
three_highest_birth_district <- birthrate_average %>%
  filter(Raumbezug != "Stadt München") %>%
  group_by(Raumbezug) %>%
  summarise(mean_birthrate = mean(birthrate, na.rm = TRUE)) %>%
  slice_max(mean_birthrate, n = 3) %>%
  pull(Raumbezug)
three_highest_birth_district

# district with lowest average
three_lowest_birth_district <- birthrate_average %>%
  filter(Raumbezug != "Stadt München") %>%
  group_by(Raumbezug) %>%
  summarise(mean_birthrate = mean(birthrate, na.rm = TRUE)) %>%
  slice_min(mean_birthrate, n = 3) %>%
  pull(Raumbezug)
three_lowest_birth_district

#group (top three) Munich, highest, lowest
line_birthrate_data_three <- birthrate %>%
  mutate(
    line_birthrate_group_three = case_when(
      Raumbezug == "Stadt München" ~ "Stadt München", 
      Raumbezug == three_highest_birth_district ~ "highest",
      Raumbezug == three_lowest_birth_district ~ "lowest",
      TRUE ~ "rest"
    )
  )

line_birthrate_data
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

# line plot birthrate (top3 highest/lowest district marked)
## 1. Top-3 und Bottom-3 Bezirke nach durchschnittlicher Geburtenrate

birthrate_means <- data_wide %>%
  filter(Raumbezug != "Stadt München") %>%      # Stadt gesamt raus
  group_by(Raumbezug) %>%
  summarise(mean_birthrate = mean(birthrate, na.rm = TRUE)) %>%
  arrange(desc(mean_birthrate))

top3    <- birthrate_means %>% slice_max(mean_birthrate, n = 3)
bottom3 <- birthrate_means %>% slice_min(mean_birthrate, n = 3)

top3
bottom3

## 2. Daten für den Linienplot vorbereiten

line_birthrate_data <- data_wide %>%
  filter(Jahr >= 2000, Jahr <= 2024) %>%
  mutate(
    group = case_when(
      Raumbezug == "Stadt München"        ~ "Munich",
      Raumbezug == top3$Raumbezug[1]      ~ "Top 1",
      Raumbezug == top3$Raumbezug[2]      ~ "Top 2",
      Raumbezug == top3$Raumbezug[3]      ~ "Top 3",
      Raumbezug == bottom3$Raumbezug[1]   ~ "Bottom 1",
      Raumbezug == bottom3$Raumbezug[2]   ~ "Bottom 2",
      Raumbezug == bottom3$Raumbezug[3]   ~ "Bottom 3",
      TRUE                                ~ "Rest"
    )
  )
## 3. Farben und Legende definieren

group_colors <- c(
  "Top 1"    = "#b2182b",   # dunkles Rot
  "Top 2"    = "#d6604d",
  "Top 3"    = "#f4a582",   # helles Rot
  "Bottom 1" = "#2166ac",   # dunkles Blau
  "Bottom 2" = "#4393c3",
  "Bottom 3" = "#92c5de",   # helles Blau
  "Munich"   = "black"
)

group_labels <- c(
  "Top 1"    = paste0("Top 1 (Avg): ",    top3$Raumbezug[1]),
  "Top 2"    = paste0("Top 2 (Avg): ",    top3$Raumbezug[2]),
  "Top 3"    = paste0("Top 3 (Avg): ",    top3$Raumbezug[3]),
  "Bottom 1" = paste0("Bottom 1 (Avg): ", bottom3$Raumbezug[1]),
  "Bottom 2" = paste0("Bottom 2 (Avg): ", bottom3$Raumbezug[2]),
  "Bottom 3" = paste0("Bottom 3 (Avg): ", bottom3$Raumbezug[3]),
  "Munich"   = "Gesamtdurchschnitt Stadt München"
)
## 4. Plot: alle Bezirke grau, Top/Bottom 3 farbig, Stadt München schwarz

ggplot() +
  # alle anderen Bezirke grau
  geom_line(
    data = line_birthrate_data %>% filter(group == "Rest"),
    aes(x = Jahr, y = birthrate, group = Raumbezug),
    color = "grey85",
    linewidth = 0.4
  ) +
  # Top/Bottom 3 + München farbig
  geom_line(
    data = line_birthrate_data %>% filter(group != "Rest"),
    aes(x = Jahr, y = birthrate, color = group, group = Raumbezug),
    linewidth = 1.1
  ) +
  scale_color_manual(
    name   = "Legende",
    values = group_colors,
    labels = group_labels
  ) +
  labs(
    title = "Durchschnittliche Geburtenrate in den Stadtbezirken Münchens (2000–2024)",
    subtitle = "Vergleich der Top/Bottom 3 Stadtbezirke mit dem gesamstädtischen Durchschnitt",
    x = "Jahr",
    y = "Geburtenrate (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 15, hjust = 0.5),
    legend.position = "bottom",
    legend.title    = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# line plot birthrate, mark top&bottom 3 employmentrate districts

# 1. Bezirksgruppen definieren ----
birthrate$Raumbezug
rot_bezirke <- c("17 Obergiesing - Fasangarten",
                 "25 Laim",
                 "06 Sendling")

blau_bezirke <- c("01 Altstadt - Lehel",
                  "21 Pasing - Obermenzing",
                  "22 Aubing - Lochhausen - Langwied")

# 2. Gruppe pro Bezirk zuweisen ----

line_birthrate_data_fixed <- data_wide %>%
  mutate(
    group = case_when(
      Raumbezug %in% rot_bezirke  ~ "Rot",
      Raumbezug %in% blau_bezirke ~ "Blau",
      Raumbezug == "Stadt München" ~ "Munich",
      TRUE ~ "Rest"
    )
  )

group_colors <- c(
  # Rot-Palette von hell → dunkel
  "Rot_06 Sendling"                    = "#f4a582",
  "Rot_25 Laim"                        = "#d6604d",
  "Rot_17 Obergiesing - Fasangarten"   = "#b2182b",
  
  # Blau-Palette von dunkel → hell
  "Blau_01 Altstadt - Lehel"           = "#2166ac",
  "Blau_21 Pasing - Obermenzing"       = "#4393c3",
  "Blau_22 Aubing - Lochhausen - Langwied" = "#92c5de",
  
  # Stadt München
  "Munich" = "black"
)

line_birthrate_data_fixed <- line_birthrate_data_fixed %>%
  mutate(
    color_key = case_when(
      Raumbezug %in% rot_bezirke  ~ paste0("Rot_", Raumbezug),
      Raumbezug %in% blau_bezirke ~ paste0("Blau_", Raumbezug),
      Raumbezug == "Stadt München" ~ "Munich",
      TRUE ~ "Rest"
    )
  )

ggplot() +
  # alle anderen Bezirke grau
  geom_line(
    data = line_birthrate_data_fixed %>% filter(color_key == "Rest"),
    aes(x = Jahr, y = birthrate, group = Raumbezug),
    color = "grey85",
    linewidth = 0.4
  ) +
  
  # färbige Linien
  geom_line(
    data = line_birthrate_data_fixed %>% filter(color_key != "Rest"),
    aes(x = Jahr, y = birthrate, color = color_key, group = Raumbezug),
    linewidth = 1.2
  ) +
  
  scale_color_manual(
    name = "Legende",
    values = group_colors,
    labels = c(
      "Munich" = "Gesamtdurchschnitt Stadt München",
      "Rot_06 Sendling" = "06 Sendling",
      "Rot_25 Laim" = "25 Laim",
      "Rot_17 Obergiesing - Fasangarten" = "17 Obergiesing – Fasangarten",
      "Blau_01 Altstadt - Lehel" = "01 Altstadt – Lehel",
      "Blau_21 Pasing - Obermenzing" = "21 Pasing – Obermenzing",
      "Blau_22 Aubing - Lochhausen - Langewied" = "22 Aubing – Lochhausen – Langwied"
    )
  ) +
  
  labs(
    title = "Entwicklung der Geburtenrate in Stadtbezirken Münchens (2000–2024)",
    subtitle = "Farblich markiert: 3 ausgewählte Bezirke mit hoher und niedriger Beschäftigungsrate",
    x = "Jahr",
    y = "Geburtenrate (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


