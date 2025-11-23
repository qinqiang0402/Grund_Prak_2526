library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)

be <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")

be_0bis2 <- be %>% 
  filter(
    Indikator == "Altersgruppen",
    Ausprägung == "bis 2 Jahre",
  ) %>%
  dplyr::select(Indikator, Ausprägung, Jahr, Raumbezug, `Basiswert 1` )


ki <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

ki_0bis2 <- ki %>% 
  filter(
    Indikator == "Altersgruppen",
    Ausprägung == "bis 2 Jahre"
  ) %>% 
  dplyr::select(Indikator, Ausprägung, Jahr, Raumbezug, `Basiswert 1`)

be_0bis2 <- be_0bis2 %>%
  rename(kinder_total = `Basiswert 1`)

ki_0bis2 <- ki_0bis2 %>%
  rename(kinder_betreut = `Basiswert 1`)


# Merge based on Jahr (year) and Raumbezug (district)
df_betreut <- left_join(
  be_0bis2,
  ki_0bis2,
  by = c("Jahr", "Raumbezug", "Indikator", "Ausprägung")
)

# Calculate non-enrolled children and percentages
df_betreut <- df_betreut %>%
  mutate(
    kinder_unbetreut = kinder_total - kinder_betreut,
    anteil_unbetreut = kinder_unbetreut / kinder_total * 100,
    anteil_betreut = kinder_betreut / kinder_total * 100
  )

df_betreut %>%
  dplyr::select(Raumbezug, kinder_total, kinder_betreut, kinder_unbetreut,
                anteil_betreut, anteil_unbetreut) %>%
  arrange(desc(anteil_unbetreut)) %>%
  head()

#——————————————————————————————————————————————————————
df_2024 <- df_betreut %>%
  filter(Jahr == 2024)

# Haushalte mit Kindern und Frauen SV

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

# Join together
df_2024 <- df_merged %>%
  inner_join(df_betreut %>% filter(Jahr == 2024) %>% 
               select(Raumbezug, anteil_betreut),
             by = "Raumbezug") %>%
  filter(Jahr == 2024)

# Quantile group
quantiles_2024 <- quantile(df_2024$anteil_betreut, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

df_2024 <- df_2024 %>%
  mutate(
    betreuung_group = cut(
      anteil_betreut,
      breaks = quantiles_2024,
      labels = c("Low", "Mid", "High"),
      include.lowest = TRUE
    )
  )

# 1）Onhe Group
cor_total <- cor(df_2024$emp_female_pct,
                 df_2024$households_pct,
                 use = "complete.obs",
                 method = "pearson")

# 2）Low Betreuung（bottom 1/3）
cor_low <- df_2024 %>%
  filter(betreuung_group == "Low") %>%
  summarise(cor = cor(emp_female_pct, households_pct,
                      use="complete.obs", method="pearson")) %>%
  pull(cor)

# 3）High Betreuung（top 1/3）
cor_high <- df_2024 %>%
  filter(betreuung_group == "High") %>%
  summarise(cor = cor(emp_female_pct, households_pct,
                      use="complete.obs", method="pearson")) %>%
  pull(cor)


df_bar <- tibble(
  group = c("Gesamt", "Low Betreuung", "High Betreuung"),
  corr = c(cor_total, cor_low, cor_high)
)

ggplot(df_bar, aes(x = group, y = corr, fill = group)) +
  geom_col(width = 0.6, alpha = 0.8) +
  geom_text(aes(label = round(corr, 2)),
            vjust = -0.5, size = 5) +
  geom_hline(yintercept = 0, linetype="dashed", color="grey60") +
  scale_fill_manual(values = c("grey50", "#f8766d", "#00bfc4")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Korrelation 2024: Frauenbeschäftigung vs. Haushalte mit Kindern",
    subtitle = "Vergleich: Gesamt vs. Low/High Kinderbetreuung (0–2 Jahre)",
    x = "",
    y = "Korrelationskoeffizient"
  ) +
  theme(legend.position = "none")
