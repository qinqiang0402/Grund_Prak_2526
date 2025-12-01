install.packages("readxl")
library(readxl)
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
install.packages("ggrepel")
library(ggrepel)

install.packages("RColorBrewer") 
library(RColorBrewer) 



export_ar <- read_excel("data/raw/export_ar.xlsx")
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")

arbeitslos_anteil_frau <- ar_sheet %>%
  filter(
    Indikator == "Arbeitslose - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(
    anteil = 100 * `Basiswert 1` / `Basiswert 2`
  )


district_averages <- arbeitslos_anteil_frau %>%
  group_by(Raumbezug) %>% 
  summarise(avg_anteil_overall = mean(anteil, na.rm = TRUE))

top3_districts <- district_averages %>%
  filter(Raumbezug != "Stadt München") %>%
  arrange(desc(avg_anteil_overall)) %>%
  slice(1:3) %>%
  pull(Raumbezug)

bottom3_districts <- district_averages %>%
  filter(Raumbezug != "Stadt München") %>%
  arrange(avg_anteil_overall) %>%
  slice(1:3) %>%
  pull(Raumbezug)

highlight_districts <- c("Stadt München", top3_districts, bottom3_districts)

data_for_plot <- arbeitslos_anteil_frau %>%
  mutate(
    highlight_status = case_when(
      Raumbezug %in% highlight_districts ~ Raumbezug, 
      TRUE ~ "Normal"
    )
  )


orange_colors <- brewer.pal(n = 5, name = "Oranges")[c(5, 4, 3)] 
blue_colors <- brewer.pal(n = 5, name = "Blues")[c(5, 4, 3)]


plot_colors <- c(
  setNames(orange_colors, top3_districts),    # Top 1-3 (Zeile 1)
  setNames(blue_colors, bottom3_districts), # Bottom 1-3 (Zeile 2)
  "Stadt München" = "black"                   # Avg (Zeile 3)
)


plot_labels <- c(
  setNames(paste("Top 1 (Avg):", top3_districts[1]), top3_districts[1]),
  setNames(paste("Top 2 (Avg):", top3_districts[2]), top3_districts[2]),
  setNames(paste("Top 3 (Avg):", top3_districts[3]), top3_districts[3]),
  setNames(paste("Bottom 1 (Avg):", bottom3_districts[1]), bottom3_districts[1]),
  setNames(paste("Bottom 2 (Avg):", bottom3_districts[2]), bottom3_districts[2]),
  setNames(paste("Bottom 3 (Avg):", bottom3_districts[3]), bottom3_districts[3]),
  "Stadt München" = "Gesamtdurchschnitt Stadt München"
)


label_data <- data_for_plot %>%
  filter(highlight_status != "Normal") %>% 
  filter(Jahr == max(Jahr, na.rm = TRUE)) %>% 
  distinct(Raumbezug, .keep_all = TRUE)



ggplot(data = data_for_plot, mapping = aes(x = Jahr, y = anteil)) +
  geom_line(data = . %>% filter(highlight_status == "Normal"), 
            aes(group = Raumbezug), 
            color = "grey80", 
            linewidth = 0.5) +
  geom_line(data = . %>% filter(highlight_status != "Normal"), 
            aes(color = highlight_status, group = Raumbezug),
            linewidth = 1.2) +
  geom_text_repel(
    data = label_data,
    aes(label = Raumbezug, color = highlight_status), 
    nudge_x = 0.5,             
    segment.color = "grey50",  
    segment.size = 0.3,
    size = 2.5,                
    show.legend = FALSE,       
    hjust = 0,                 
    direction = "y"            
  ) +
  scale_color_manual(
    name = "Legende:", 
    values = plot_colors, 
    labels = plot_labels, 
    breaks = names(plot_labels) 
  ) +
  scale_x_continuous(
    breaks = seq(min(data_for_plot$Jahr, na.rm = TRUE), max(data_for_plot$Jahr, na.rm = TRUE), by = 1),
    limits = c(min(data_for_plot$Jahr, na.rm = TRUE), max(data_for_plot$Jahr, na.rm = TRUE) + 1.5) 
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    legend.position = "bottom", 
    legend.justification = "left",
    plot.margin = unit(c(0.5, 4, 0.5, 0.5), "cm") 
  ) +
  guides(color = guide_legend(
    ncol = 3,       
    byrow = TRUE    
  )) +
  labs(
    title = "Durchschnittlicher Arbeitslose-Anteil von Frauen in München",
    subtitle = "Vergleich der Top/Bottom 3 Stadtbezirke mit dem gesamtstädtischen Durchschnitt (Blau-Rot Palette)",
    x = "Jahr",
    y = "Durchschnittlicher Anteil (%)"
  )

