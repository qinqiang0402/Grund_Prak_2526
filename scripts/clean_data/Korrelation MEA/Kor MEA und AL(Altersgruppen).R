install.packages("readxl")
library(readxl)
export_be <- read_excel("data/raw/export_be.xlsx")
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
View(be_sheet)

export_ar <- read_excel("data/raw/export_ar.xlsx")
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")
View(ar_sheet)

install.packages("tidyverse")
library(tidyverse)

install.packages("ggpubr")
library(ggpubr)

library(ggplot2)

all_districts_and_city <- be_sheet %>%
  filter(
    Indikator == "Durchschnittsalter Mütter erstgebärend",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    mean_age = `Basiswert 1` / `Basiswert 2`
  )

arbeitslos_anteil_frau <- ar_sheet %>%
  filter(
    Indikator == "Arbeitslose - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(
    anteil = 100 * `Basiswert 1` / `Basiswert 2`
  )


korrelations_daten <- inner_join(
  all_districts_and_city, 
  arbeitslos_anteil_frau, 
  by = c("Raumbezug", "Jahr")
)

korrelations_daten_clean <- korrelations_daten %>%
  filter(Raumbezug != "Stadt München")

library(broom)

age_quantiles <- quantile(
  korrelations_daten_clean$mean_age, 
  probs = c(0, 0.25, 0.5, 0.75, 1), 
  na.rm = TRUE 
)

print(age_quantiles)

daten_mit_quartilen <- korrelations_daten_clean %>%
  mutate(
    alters_quartil = cut(
      mean_age,
      breaks = age_quantiles,     
      include.lowest = TRUE,    
      labels = c("Q1 (25% Niedrigste)",  
                 "Q2 (25%-50%)", 
                 "Q3 (50%-75%)", 
                 "Q4 (25% Höchste)")
    )
  )

stats_nach_quartil <- daten_mit_quartilen %>%
  filter(!is.na(alters_quartil)) %>% 
  
  group_by(alters_quartil) %>%
  summarize(
    test_stats = list(broom::tidy(cor.test(mean_age, anteil))), 
    .groups = "drop"
  ) %>%
  unnest(test_stats)

print(stats_nach_quartil)

facet_labels_quartil <- stats_nach_quartil %>%
  mutate(
    label_text = paste0(
      "R = ", round(estimate, 2),
      "\np = ", format.pval(p.value, digits = 2, eps = 0.001)
    )
  )

ggplot(daten_mit_quartilen, aes(x = mean_age, y = anteil)) +
  
  geom_point(aes(color = alters_quartil), size = 1, alpha = 0.7) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") + 
  facet_wrap(~ alters_quartil, scales = "free_x") + 
  geom_text(
    data = facet_labels_quartil,
    aes(x = -Inf, y = Inf, label = label_text), # Oben-Links
    hjust = -0.1, vjust = 1.1, size = 3, color = "black"
  ) +
  
  labs(
    title = "Korrelation (nach Alters-Quartil) zwischen Mütteralter und Frauenarbeitslosigkeit",
    x = "Erstgeburtsalter von Mutter",
    y = "Anteil arbeitslose Frauen (%)"
  ) +
  theme_bw() +
  theme(legend.position = "none")



#----------------------------------------------------------------------------------------------
label_data_plot <- stats_nach_quartil %>%
  mutate(
    label_text = paste0(
      "R = ", round(estimate, 2),
      "\np = ", format.pval(p.value, digits = 2, eps = 0.001)
    ),
    
    x_pos = case_when(
      alters_quartil == "Q1 (25% Niedrigste)" ~ 29.5,
      alters_quartil == "Q2 (25%-50%)"       ~ 31.1,
      alters_quartil == "Q3 (50%-75%)"       ~ 31.9,
      alters_quartil == "Q4 (25% Höchste)"   ~ 33.0
    ),
    
    y_pos = case_when(
      alters_quartil == "Q1 (25% Niedrigste)" ~ 5.0, 
      alters_quartil == "Q2 (25%-50%)"       ~ 5.0, 
      alters_quartil == "Q3 (50%-75%)"       ~ 4.8, 
      alters_quartil == "Q4 (25% Höchste)"   ~ 4.8  
    )
  )

library(ggplot2)
ggplot(daten_mit_quartilen, aes(x = mean_age, y = anteil)) +
  
  geom_point(aes(color = alters_quartil), size = 1, alpha = 0.7) +
  
  geom_smooth(aes(color = alters_quartil), method = "lm", se = FALSE, size = 1.2) +
  
  geom_text(
    data = label_data_plot,
    aes(x = x_pos, y = y_pos, label = label_text, color = alters_quartil),
    size = 3.5,
    vjust = 1, 
    show.legend = FALSE 
  ) +
  
  labs(
    title = "Korrelation zwischen Müttererstgeburtsalter und Frauenarbeitslosigkeit",
    x = "Müttererstgeburtsalter",
    y = "Anteil arbeitslose Frauen (%)",
    color = "Alters-Quartil" 
  ) +
  scale_color_manual(values = c(
    "Q1 (25% Niedrigste)" = "#490092", 
    "Q2 (25%-50%)"       = "#006DDB", 
    "Q3 (50%-75%)"       = "#C77CFF", 
    "Q4 (25% Höchste)"   = "#FF6DB6"   
  )) +
  
  theme_minimal()
