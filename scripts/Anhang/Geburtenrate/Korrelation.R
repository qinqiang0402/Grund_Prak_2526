# Spearman Correlation between birthrate and female employment

# spatial dimension
# average of years 2000-2024
# correlation by district including whole Munich
cor_birthrate_employment_district <- data_wide %>% 
  group_by(Raumbezug) %>%
  summarise(
    spearman_rho = cor(birthrate, employment_female, method = "spearman", use = "complete.obs"),
    n = n()  # Jahre
  ) %>%
  arrange(desc(spearman_rho))  # descent order
print (cor_birthrate_employment_district, n=26)
# plot
ggplot(cor_birthrate_employment_district, 
       aes(x = reorder(Raumbezug, spearman_rho), y = spearman_rho,
           fill = ifelse(spearman_rho >= 0, "Positiv", "Negativ"))) +
  geom_col() +
  scale_fill_manual(
    name = "Korrelation",
    values = c("Positiv" = "steelblue", "Negativ" = "firebrick3")
  ) +
  coord_flip() +  # Bezirke von oben nach unten anzeigen
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  labs(
    title = "Korrelation zwischen Geburtenrate und weiblicher Beschäftigungsrate",
    subtitle = "Bezirke Münchens, Durchschnitt der Jahre 2000–2024",
    x = "Stadtbezirk",
    y = "Spearman ρ"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10)
  )


# time dimension of whole Munich
# correlation by year
cor_birthrate_employment_year <- data_wide %>%
  filter(Raumbezug != "Stadt München") %>%
  group_by(Jahr) %>%
  summarise(
    spearman_rho = cor(birthrate, employment_female,
                       method = "spearman", use = "complete.obs")
  )

#plot
ggplot() +
  geom_line(data = cor_birthrate_employment_year,
            aes(x = Jahr, y = spearman_rho), 
            color = "darkblue", 
            linewidth = 1) +
  labs(
    title = "Entwicklung der Korrelation zwischen Geburtenrate und weibliche Beschäftigung",
    subtitle = "Stadt München, 2000-2024",
    x = "Jahr",
    y = "Spearman p"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


# function: scatterplot birthrate vs. female employment of one year
# input: year
plot_birthrate_employment_scatter <- function(year) {
  
  # Data of the year of input
  df_year <- data_wide %>%
    filter(
      Jahr == year
    ) %>%
    mutate(
      district_number = as.numeric(str_extract(Raumbezug, "^\\d+"))
    )
  
  # prove input
  if (nrow(df_year) == 0) {
    stop("Keine Daten für dieses Jahr vorhanden: ", year)
  }
  
  if (all(is.na(df_year$district_number))) {
    stop("Konnte keine Bezirksnummern aus 'Raumbezug' extrahieren.")
  }
  
  ggplot(df_year,
         aes(x = birthrate,
             y = employment_female)) +
    
    geom_point(aes(color = Raumbezug), size = 2) +
    
    # district numbers above points
    geom_text(
      aes(label = sprintf("%02d", district_number)),
      vjust = -0.7,      
      size  = 3,
      color = "black",
      show.legend = FALSE 
    ) +
    labs(
      title    = "Geburtenrate vs. weibliche SV-pflichtige Beschäftigung",
      subtitle = paste0("Stadtbezirke Münchens, Jahr ", year),
      x        = "Geburtenrate (%)",
      y        = "weibliche Beschäftigungsrate (%)",
      color    = "Stadtbezirk"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title      = element_text(face = "bold", size = 16),
      legend.position = "right"
    )
}
plot_birthrate_employment_scatter(2024)
