# =========================================================
# Visualization: Evolution of the Relationship between
# Households with Children and Female Unemployment Rate (2012–2024)
# Data source: City of Munich (district level)
# =========================================================

library(readxl)
library(tidyverse)
library(ggrepel)
library(ggpmisc)

# --- 1. Load and clean unemployment data ---
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
  # Exclude total city-level observation
  filter(Raumbezug != "Stadt München")


# --- 2. Load and clean household-with-children data ---
data_be <- read_excel("data/raw/export_be.xlsx", sheet = 2)

data_be_clean <- data_be %>%
  select(Indikator, Ausprägung, Jahr, Raumbezug, Indikatorwert) %>%
  # Keep overall households with children for all years
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  # Convert decimal commas to numeric format
  mutate(Indikatorwert = as.numeric(str_replace(Indikatorwert, ",", "."))) %>%
  # Rename for clarity
  rename(haushalt_kinder = Indikatorwert) %>%
  # Exclude total city-level observation
  filter(Raumbezug != "Stadt München")


# --- 3. Merge datasets by year and district ---
df <- inner_join(data_ar_clean, data_be_clean, by = c("Jahr", "Raumbezug"))


# --- 4. Compute all three correlation coefficients per year ---
cor_data <- df %>%
  group_by(Jahr) %>%
  summarize(
    pearson  = cor(haushalt_kinder, arbeitslos_weiblich, method = "pearson",  use = "complete.obs"),
    spearman = cor(haushalt_kinder, arbeitslos_weiblich, method = "spearman", use = "complete.obs"),
    kendall  = cor(haushalt_kinder, arbeitslos_weiblich, method = "kendall",  use = "complete.obs")
  ) %>%
  pivot_longer(cols = c(pearson, spearman, kendall),
               names_to = "method",
               values_to = "r")

# --- 5. Visualization ---
plot2 <-ggplot(cor_data, aes(x = Jahr, y = r, color = method, shape = method)) +
  
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(size = 2.5, alpha = 0.9) +
  
  # Text labels (small, readable)
  geom_text(
    aes(label = sprintf("%.2f", r)),
    vjust = -0.8,
    size = 3,
    color = "black",
    check_overlap = TRUE
  ) +
  
  scale_color_manual(
    values = c("pearson" = "#440154FF", "spearman" = "#21908CFF", "kendall" = "#FDE725FF"),
    name = "Correlation Type",
    labels = c("pearson" = "Pearson", "spearman" = "Spearman", "kendall" = "Kendall")  # 命名向量，明确对应关系
  ) +
  scale_shape_manual(
    values = c("pearson" = 16, "spearman" = 17, "kendall" = 15),
    name = "Correlation Type",
    labels = c("pearson" = "Pearson", "spearman" = "Spearman", "kendall" = "Kendall")  # 同上
  ) +
  
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.2)) +
  scale_x_continuous(breaks = seq(2012, 2024, 1)) +
  
  labs(
    title = "Yearly Correlations (2012–2024)",
    subtitle = "Comparison of Pearson, Spearman, and Kendall coefficients",
    x = "Year",
    y = "Correlation (r)",
    caption = "Data source: City of Munich · Own calculation"
  ) +
  
  theme_minimal(base_size = 11, base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey30"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "grey25"),
    legend.position = "bottom",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8)
  )
plot2
