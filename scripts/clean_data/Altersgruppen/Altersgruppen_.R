library(tidyverse)
library(tidyr)
library(corrr)
library(ggplot2)

Altersgruppen <- readRDS("scripts/clean_data/Altersgruppen/Altersgruppen.rds")
arbeitslos_weiblich <- readRDS("scripts/clean_data/Altersgruppen/arbeitslose_Anteil_weiblich.rds")
be_raw <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")



Altersgruppen_a <- Altersgruppen %>%
  mutate(
    Indikatorwert = as.numeric(str_replace(Indikatorwert, ",", "."))
  )


ziel_auspraegung <- c(
  "bis 17 Jahre", "bis 2 Jahre", "insgesamt", 
  "15 bis 17 Jahre", "15 bis 64 Jahre", "18 Jahre und älter",
  "3 bis 5 Jahre", "6 bis 14 Jahre", 
  "65 Jahre und älter", "75 Jahre und älter"
)


Altersgruppen <- Altersgruppen_a %>%
  filter(Ausprägung %in% ziel_auspraegung) %>%
  mutate(Indikatorwert = as.numeric(str_replace(Indikatorwert, ",", "."))) %>%
  select(Jahr, Raumbezug, Ausprägung, Indikatorwert)


Altersgruppen_wide <- Altersgruppen %>%
  pivot_wider(
    names_from = Ausprägung,
    values_from = Indikatorwert
  )



#————————————————————————————————————————————————————————————————————————————————


df_merge <- arbeitslos_weiblich %>%
  select(Jahr, Raumbezug, arbeitslos_weiblich) %>%
  rename(arbeitslosenquote_w = arbeitslos_weiblich) %>%
  inner_join(Altersgruppen_wide, by = c("Jahr", "Raumbezug"))



cor_result <- df_merge %>%
  select(arbeitslosenquote_w, `bis 2 Jahre`,`bis 17 Jahre`, `15 bis 17 Jahre`,
         `15 bis 64 Jahre`, `18 Jahre und älter`,
         `65 Jahre und älter`, `75 Jahre und älter`) %>%
  correlate() %>%
  focus(arbeitslosenquote_w) %>%
  arrange(desc(abs(arbeitslosenquote_w)))

cor_result



cor_result %>%
  mutate(var = reorder(term, arbeitslosenquote_w)) %>%
  ggplot(aes(x = var, y = arbeitslosenquote_w, fill = arbeitslosenquote_w)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(
    title = "Korrelation zwischen Altersstruktur und weiblicher Arbeitslosenquote",
    x = "Altersgruppe",
    y = "Korrelationskoeffizient (r)"
  ) +
  theme_minimal(base_size = 13)




# Nach Stadtteile
cor_by_district <- df_merge %>%
  group_by(Raumbezug) %>%
  summarise(
    cor_15_64 = cor(`15 bis 64 Jahre`, arbeitslosenquote_w, use = "complete.obs"),
    cor_65plus = cor(`65 Jahre und älter`, arbeitslosenquote_w, use = "complete.obs")
  ) %>%
  pivot_longer(cols = c(cor_15_64, cor_65plus), names_to = "Altersgruppe", values_to = "Korrelation")

# 
ggplot(cor_by_district, aes(x = reorder(Raumbezug, Korrelation), y = Korrelation, fill = Altersgruppe)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("cor_15_64" = "steelblue", "cor_65plus" = "tomato")) +
  labs(
    title = "Korrelation zwischen Altersstruktur und weiblicher Arbeitslosenquote nach Stadtbezirk",
    x = "Stadtbezirk",
    y = "Korrelationskoeffizient (r)",
    fill = "Altersgruppe"
  ) +
  theme_minimal(base_size = 12)







# nach Jahr

cor_by_year <- df_merge %>%
  group_by(Jahr) %>%
  summarise(
    cor_15_64 = cor(`15 bis 64 Jahre`, arbeitslosenquote_w, use = "complete.obs"),
    cor_65plus = cor(`65 Jahre und älter`, arbeitslosenquote_w, use = "complete.obs")
  ) %>%
  pivot_longer(cols = starts_with("cor_"), names_to = "Altersgruppe", values_to = "Korrelation")

# Zeitlich
ggplot(cor_by_year, aes(x = Jahr, y = Korrelation, color = Altersgruppe)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("cor_15_64" = "steelblue", "cor_65plus" = "tomato")) +
  labs(
    title = "Zeitliche Entwicklung der Korrelation zwischen Altersstruktur und weiblicher Arbeitslosenquote",
    y = "Korrelationskoeffizient (r)",
    color = "Altersgruppe"
  ) +
  theme_minimal(base_size = 13)







