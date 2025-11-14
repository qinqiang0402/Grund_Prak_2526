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
df_corr_by_district <- df_merged %>%
  group_by(Raumbezug) %>%
  summarise(
    cor_district = cor(emp_female_pct, households_pct, use = "complete.obs")
  )

df_corr_yearly <- df_merged %>%
  group_by(Jahr) %>%
  summarise(cor_year = cor(emp_female_pct, households_pct, use = "complete.obs"))

df_roll <- df_merged %>%
  arrange(Raumbezug, Jahr) %>%
  group_by(Raumbezug) %>%
  mutate(
    rolling_cor = zoo::rollapplyr(
      data = cbind(emp_female_pct, households_pct),
      width = 5,
      FUN = function(m) {
        if (any(is.na(m[,1])) || any(is.na(m[,2]))) {
          return(NA_real_)
        } else {
          return(cor(m[,1], m[,2]))
        }
      },
      by.column = FALSE,
      fill = NA
    )
  )

bezirk_name <- "01 Altstadt - Lehel"

df_one <- df_roll %>% filter(Raumbezug == bezirk_name)

ggplot(df_one, aes(x = Jahr, y = rolling_cor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  theme_minimal() +
  labs(
    title = paste("Stadtbezirk:", bezirk_name),
    subtitle = "Gleitende Korrelation (Fenster = 5 Jahre)",
    x = "Jahr",
    y = "Korrelationskoeffizient (−1 … +1)"
  )

ggplot(df_roll, aes(x = Jahr, y = rolling_cor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey80") +
  geom_line(color = "steelblue") +
  facet_wrap(~ Raumbezug) +
  theme_minimal() +
  labs(
    title = "Gleitende 5-Jahres-Korrelation pro Stadtbezirk",
    subtitle = "Frauenbeschäftigung vs. Haushalte mit Kindern",
    x = "Jahr",
    y = "Korrelationskoeffizient"
  )
