library(readxl)
library(dplyr)
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")

df_emp <- ar_sheet %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(
    Jahr = as.numeric(Jahr),
    emp_female_pct = 100 * `Basiswert 1` / `Basiswert 2`
  ) %>%
  select(Jahr, Raumbezug, emp_female_pct)


be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")

df_households <- be_sheet %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    Jahr = as.numeric(Jahr),
    households_pct = 100 * `Basiswert 1` / `Basiswert 2`,
  ) %>%
  select(Jahr, Raumbezug, households_pct)

df_merged <- df_emp %>%
  inner_join(df_households,
             by = c("Jahr", "Raumbezug"))


df_corr_by_district <- df_merged %>%
  group_by(Raumbezug) %>%
  summarise(
    cor_district = cor(emp_female_pct, households_pct, use = "complete.obs")
  )

ggplot(df_corr_by_district,
       aes(x = reorder(Raumbezug, cor_district), y = cor_district)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Korrelation pro Stadtbezirk (2005–2024)",
    subtitle = "Frauenbeschäftigung vs. Haushalte mit Kindern",
    x = "Stadtbezirk",
    y = "Korrelationskoeffizient"
  )
