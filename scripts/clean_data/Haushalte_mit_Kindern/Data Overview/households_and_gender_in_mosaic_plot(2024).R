library(tidyverse)
library(readxl)
library(ggrepel)

data_ar <- read_excel("data/raw/export_ar.xlsx", sheet = 2)
data_be <- read_excel("data/raw/export_be.xlsx", sheet = 2)

# Clean unemployment data (male/female)
data_ar_clean <- data_ar %>%
  select(Indikator, Ausprägung, Jahr, Raumbezug, Indikatorwert) %>%
  filter(
    Indikator == "Arbeitslose - Anteil",
    Ausprägung %in% c("weiblich", "männlich"),
    Jahr == 2024
  ) %>%
  mutate(Indikatorwert = as.numeric(str_replace(Indikatorwert, ",", "."))) %>%
  pivot_wider(
    names_from = Ausprägung,
    values_from = Indikatorwert,
    names_prefix = "arbeitslos_"
  ) %>%
  mutate(geschlechter_diff = arbeitslos_männlich - arbeitslos_weiblich) %>%
  filter(Raumbezug != "Stadt München")

# Clean data for households with children
data_be_clean <- data_be %>%
  select(Indikator, Ausprägung, Jahr, Raumbezug, Indikatorwert) %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt",
    Jahr == 2024
  ) %>%
  mutate(Indikatorwert = as.numeric(str_replace(Indikatorwert, ",", "."))) %>%
  filter(Raumbezug != "Stadt München")

# Merge both datasets
df <- inner_join(data_ar_clean, data_be_clean, by = c("Jahr", "Raumbezug"))

# Split continuous variables into two groups (above/below median)
median_diff <- median(df$geschlechter_diff, na.rm = TRUE)
median_kinder <- median(df$Indikatorwert, na.rm = TRUE)

df_mosaic2 <- df %>%
  mutate(
    diff_grp = ifelse(geschlechter_diff > median_diff, "High", "Low"),
    kinder_grp = ifelse(Indikatorwert > median_kinder, "Many children", "Few children")
  )

# Create contingency table
tab2 <- table(df_mosaic2$kinder_grp, df_mosaic2$diff_grp)
tab2

# Chi-square test for independence
chisq.test(tab2)
# p-value < 0.05 → statistically significant association
# p-value > 0.05 → no significant association (only a weak visual trend)

# Basic mosaic plot
mosaicplot(
  tab2,
  color = c("#56B1F7", "#E69F00"),
  main = "Relationship Between Households with Children and Gender Gap in Unemployment (2024)",
  xlab = "Share of households with children",
  ylab = "Gender gap (male − female)",
  las = 1,
  border = "grey40",
  cex.main = 0.9,   # smaller title
  cex.axis = 0.9,   # smaller axis text
  cex.lab = 0.9     # smaller axis labels
)

