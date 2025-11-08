library(readxl)
library(tidyverse)
library(ggrepel)


data_ar <- read_excel("data/raw/export_ar.xlsx", sheet = 2)

data_ar_clean <- data_ar %>%
  # Keep relevant columns only
  select(Indikator, Ausprägung, Jahr, Raumbezug, Indikatorwert) %>%
  # Filter for unemployment rate (male/female) in 2024
  filter(
    Indikator == "Arbeitslose - Anteil",
    Ausprägung %in% c("weiblich", "männlich"),
    Jahr == 2024
  ) %>%
  # Convert decimal commas to numeric values
  mutate(Indikatorwert = as.numeric(str_replace(Indikatorwert, ",", "."))) %>%
  # Pivot: male/female into separate columns
  pivot_wider(
    names_from = Ausprägung,
    values_from = Indikatorwert,
    names_prefix = "arbeitslos_"
  ) %>%
  # Compute gender difference (male - female)
  mutate(geschlechter_diff = arbeitslos_männlich - arbeitslos_weiblich) %>%
  # Exclude total city of Munich
  filter(Raumbezug != "Stadt München")


data_be <- read_excel("data/raw/export_be.xlsx", sheet = 2)

data_be_clean <- data_be %>%
  select(Indikator, Ausprägung, Jahr, Raumbezug, Indikatorwert) %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt",
    Jahr == 2024
  ) %>%
  mutate(Indikatorwert = as.numeric(str_replace(Indikatorwert, ",", "."))) %>%
  filter(Raumbezug != "Stadt München")



#  Merge datasets and visualize

df <- inner_join(data_ar_clean, data_be_clean, by = c("Jahr", "Raumbezug"))


#  Visualization: Regression between child-household share and gender gap in unemployment
ggplot(df, aes(x = Indikatorwert, y = geschlechter_diff, label = Raumbezug)) +
  
  # Points: semi-transparent blue
  geom_point(size = 3, color = "#2C7BB6", alpha = 0.8) +
  
  # Regression line: dashed orange, with soft confidence band
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#D55E00",
    fill = "grey80",
    linetype = "dashed",
    linewidth = 0.8,
    alpha = 0.3
  ) +
  
  # District labels with light gray leader lines
  geom_text_repel(
    size = 2.8,
    color = "grey10",
    segment.color = "grey70",
    segment.size = 0.3,
    segment.alpha = 0.6,
    box.padding = 0.35,
    point.padding = 0.2,
    max.overlaps = 50,
    force = 4,
    min.segment.length = 0
  ) +
  
  # Axis scales
  scale_x_continuous(limits = c(5, 30), breaks = seq(5, 30, 5)) +
  scale_y_continuous(limits = c(-0.1, 1.3), breaks = seq(0, 1.2, 0.2)) +
  
  # Titles and labels
  labs(
    title = "Relationship Between Households with Children and Gender Gap in Unemployment (2024)",
    subtitle = "Level of analysis: Munich city districts (excluding total city)",
    x = "Share of households with children (%)",
    y = "Unemployment rate difference (male − female, percentage points)",
    caption = "Data source: City of Munich · Own calculation"
  ) +
  
  # Theme customization: smaller base font and balanced layout
  theme_minimal(base_size = 11, base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, lineheight = 1.1),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey30"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "grey25"),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 8, color = "grey40", hjust = 1),
    plot.margin = margin(10, 20, 10, 10)
  ) +
  coord_cartesian(expand = TRUE)
