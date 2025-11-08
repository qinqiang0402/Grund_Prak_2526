library(readxl)
library(tidyverse)
library(ggrepel)
library(ggpmisc)

# 1. Load and clean unemployment data
data_ar <- read_excel("data/raw/export_ar.xlsx", sheet = 2)

data_ar_clean <- data_ar %>%
  select(Indikator, Ausprägung, Jahr, Raumbezug, Indikatorwert) %>%
  # Keep only female unemployment rate for all years
  filter(
    Indikator == "Arbeitslose - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  # Convert decimal commas to numeric format
  mutate(Indikatorwert = as.numeric(str_replace(Indikatorwert, ",", "."))) %>%
  # Rename for clarity
  rename(arbeitslos_weiblich = Indikatorwert) %>%
  # Exclude city-level total observation
  filter(Raumbezug != "Stadt München")

# 2. Load and clean household-with-children data
data_be <- read_excel("data/raw/export_be.xlsx", sheet = 2)

data_be_clean <- data_be %>%
  select(Indikator, Ausprägung, Jahr, Raumbezug, Indikatorwert) %>%
  # Keep total households with children for all years
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  # Convert decimal commas to numeric format
  mutate(Indikatorwert = as.numeric(str_replace(Indikatorwert, ",", "."))) %>%
  # Rename for clarity
  rename(haushalt_kinder = Indikatorwert) %>%
  # Exclude city-level total observation
  filter(Raumbezug != "Stadt München")

# 3. Merge datasets by year and district
df <- inner_join(data_ar_clean, data_be_clean, by = c("Jahr", "Raumbezug"))

# Group by district and calculate correlation between household-with-children rate and female unemployment rate
district_cor <- df %>%
  group_by(Raumbezug) %>%
  summarize(
    r = cor(haushalt_kinder, arbeitslos_weiblich, use = "complete.obs")
  )

# View correlation coefficients by district
district_cor
district_cor$r

ggplot(district_cor, aes(x = Raumbezug, y = r, fill = r > 0)) +
  # Horizontal bar chart (x-axis = district, y-axis = correlation coefficient)
  geom_col(width = 0.7, alpha = 0.8) +
  # Add labels (display correlation coefficient for each district)
  geom_text(
    aes(label = sprintf("%.2f", r)),
    hjust = ifelse(district_cor$r > 0, -0.1, 1.1),
    size = 3,
    color = "grey30"
  ) +
  # Reference line (r = 0, no correlation)
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.8) +
  # Set y-axis range (-1 to 1)
  ylim(-1, 1) +
  # Flip coordinates (for better readability of district names)
  coord_flip() +
  # Color by correlation sign (red = positive, blue = negative)
  scale_fill_manual(
    values = c("FALSE" = "#3B82F6", "TRUE" = "#EF4444"),
    guide = "none"
  ) +
  # Titles and labels
  labs(
    title = "Correlation by District: Households with Children vs Female Unemployment Rate",
    subtitle = "Sorted by absolute correlation coefficient",
    x = "District",
    y = "Correlation coefficient (r)",
    caption = "Note: Red = positive correlation, Blue = negative correlation"
  ) +
  # Theme settings
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    plot.caption = element_text(size = 9, color = "grey50"),
    axis.text.y = element_text(size = 9),
    panel.grid.minor = element_blank()
  )
