library(readxl)
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

#————————————————————————————————————————————————
# Option1：Pearson

df_corr_by_district <- df_merged %>%
  group_by(Raumbezug) %>%
  summarise(
    cor_district = cor(emp_female_pct, households_pct, use = "complete.obs")
  )

df_corr_yearly <- df_merged %>%
  group_by(Jahr) %>%
  summarise(cor_year = cor(emp_female_pct, households_pct, use = "complete.obs"))

#————————————————————————————————————————————————————————————————
# Option2: Spearman
df_corr_by_district <- df_merged %>%
  group_by(Raumbezug) %>%
  summarise(
    cor_district = cor(emp_female_pct, households_pct,
                       use = "complete.obs",
                       method = "spearman")
  )

df_corr_yearly <- df_merged %>%
  group_by(Jahr) %>%
  summarise(
    cor_year = cor(emp_female_pct, households_pct,
                   use = "complete.obs",
                   method = "spearman")
  )


#————————————————————————————————————————————————————————————————————
# plots

library(ggplot2)

ggplot(df_merged, aes(x = households_pct, y = emp_female_pct)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal()


ggplot(df_corr_yearly, aes(x = Jahr, y = cor_year)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_line(color = "steelblue", linewidth = 1.1) +
  geom_point(color = "steelblue", size = 2) +
  theme_minimal() +
  labs(
    title = "Jährliche Korrelation über alle Stadtbezirke",
    subtitle = "Frauenbeschäftigungsquote vs. Haushalte mit Kindern",
    x = "Jahr",
    y = "Korrelationskoeffizient (−1 … +1)"
  )

