library(tidyverse)

data_ar <- read_excel("data/raw/export_ar.xlsx", sheet = 2)
data_be <- read_excel("data/raw/export_be.xlsx", sheet = 2)

# Calculate gender difference in unemployment rate
data_diff <- data_ar %>%
  select(Indikator, Ausprägung, Jahr, Raumbezug, Indikatorwert) %>%
  filter(Raumbezug != "Stadt München") %>%
  mutate(Indikatorwert = as.numeric(str_replace(Indikatorwert, ",", "."))) %>%
  filter(
    Indikator == "Arbeitslose - Anteil",
    Ausprägung %in% c("weiblich", "männlich")
  ) %>%
  pivot_wider(names_from = Ausprägung, values_from = Indikatorwert) %>%
  mutate(diff = männlich - weiblich)

# Plot: yearly distribution of male–female unemployment rate differences
ggplot(data_diff, aes(x = factor(Jahr), y = diff)) +
  geom_boxplot(fill = "#56B1F7", alpha = 0.7, outlier.color = "#E69F00") +
  geom_jitter(width = 0.15, alpha = 0.6, color = "grey40") +
  labs(
    title = "Distribution of Gender Differences (Male − Female)\nin Unemployment Rates Over Time",
    subtitle = "Each box represents the distribution across Munich city districts per year",
    x = "Year",
    y = "Unemployment rate difference (percentage points)",
    caption = "Data source: City of Munich · Own calculation"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5, lineheight = 1.1),
    plot.subtitle = element_text(size = 10, color = "grey30", hjust = 0.5),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "grey25"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 8, color = "grey40", hjust = 1)
  )
