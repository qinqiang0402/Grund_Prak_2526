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

#————————————————————————————————————————————————

df_corr_yearly <- df_merged %>%
  group_by(Jahr) %>%
  summarise(cor_year = cor(emp_female_pct, households_pct, use = "complete.obs"))

#————————————————————————————————————————————————————————————————————
# plots

library(ggplot2)


HMK_korr_nach_Jahr <- ggplot(df_corr_yearly, aes(x = Jahr, y = cor_year)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_line(color = "steelblue", linewidth = 1.1) +
  geom_point(color = "steelblue", size = 2) +
  theme_minimal() +
  labs(
    title = "Jährliche Korrelation über alle Stadtbezirke",
    subtitle = "Frauenbeschäftigung und Haushalte mit Kindern",
    x = "Jahr",
    y = "Korrelationskoeffizient"
  )

HMK_korr_nach_Jahr
saveRDS(HMK_korr_nach_Jahr, "results/figures/Haushalt_mit_Kindern/HMK_korr_nach_Jahr.rds")
