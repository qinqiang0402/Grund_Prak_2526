library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)

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

#============ BEVÖLKERUNG Haushalte mit Kindern =============
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")

df_households <- be_sheet %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    Jahr = as.numeric(Jahr),
    households_pct = 100 * `Basiswert 1` / `Basiswert 2`
  ) %>%
  select(Jahr, Raumbezug, households_pct)

# ——————————————————Kinderbetreuung————————————————————————————————
ki <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

ki_0bis2 <- ki %>% 
  filter(
    Indikator == "Altersgruppen",
    Ausprägung == "bis 2 Jahre"
  ) %>% 
  select(Jahr, Raumbezug, `Basiswert 1`) %>%
  rename(kinder_betreut = `Basiswert 1`)

be_0bis2 <- be_sheet %>% 
  filter(
    Indikator == "Altersgruppen",
    Ausprägung == "bis 2 Jahre"
  ) %>% 
  select(Jahr, Raumbezug, `Basiswert 1`) %>%
  rename(kinder_total = `Basiswert 1`)

df_betreut <- left_join(
  be_0bis2, ki_0bis2,
  by = c("Jahr", "Raumbezug")
) %>%
  mutate(
    anteil_betreut = 100 * kinder_betreut / kinder_total
  ) %>%
  select(Jahr, Raumbezug, anteil_betreut)

# Join together
df_all <- df_emp %>%
  inner_join(df_households, by = c("Jahr", "Raumbezug")) %>%
  inner_join(df_betreut,    by = c("Jahr", "Raumbezug"))

# Yearly correlation
years <- sort(unique(df_all$Jahr))

corr_df <- map_df(years, function(y) {
  
  df_y <- df_all %>% filter(Jahr == y)
  
  qu <- quantile(df_y$anteil_betreut, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
  
  df_y <- df_y %>%
    mutate(
      betreuung_group = cut(
        anteil_betreut,
        breaks = qu,
        labels = c("Niedrig", "Mittel", "Hoch"),
        include.lowest = TRUE
      )
    )
  
  tibble(
    Jahr = y,
    
    # ⭐ 用 Mittel 替代 Gesamt
    Mittel = cor(
      df_y %>% filter(betreuung_group == "Mittel") %>% pull(emp_female_pct),
      df_y %>% filter(betreuung_group == "Mittel") %>% pull(households_pct),
      use = "complete.obs"
    ),
    
    Niedrig = cor(
      df_y %>% filter(betreuung_group == "Niedrig") %>% pull(emp_female_pct),
      df_y %>% filter(betreuung_group == "Niedrig") %>% pull(households_pct),
      use = "complete.obs"
    ),
    
    Hoch = cor(
      df_y %>% filter(betreuung_group == "Hoch") %>% pull(emp_female_pct),
      df_y %>% filter(betreuung_group == "Hoch") %>% pull(households_pct),
      use = "complete.obs"
    )
  )
})

# ------- 变成长格式 --------
corr_long <- corr_df %>%
  pivot_longer(cols = c("Mittel", "Niedrig", "Hoch"),
               names_to = "Group",
               values_to = "Correlation")

# ------- 绘图（全部德语）--------
 kor_group <- ggplot(corr_long, aes(x = Jahr, y = Correlation, color = Group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "Mittel"  = "grey40",
    "Niedrig" = "#00bfc4",
    "Hoch"    = "#f8766d"
  )) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Jährliche Korrelationen: Frauenbeschäftigung und Haushalte mit Kindern",
    subtitle = "Vergleich: Mittel-, Niedrig- und Hochbetreuung (0–2 Jahre)",
    x = "Jahr",
    y = "Korrelationskoeffizient",
    color = "Gruppe"
  )

saveRDS(kor_group, "results/figures/m_effekt/m_effekt_kor_group.rds")
