library(readxl)
library(tidyverse)
library(ggpubr)

export_be <- read_excel("data/raw/export_be.xlsx")
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")

export_ar <- read_excel("data/raw/export_ar.xlsx")
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")


all_districts_and_city <- be_sheet %>%
  filter(
    Indikator == "Durchschnittsalter Mütter erstgebärend",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    mean_age = `Basiswert 1` / `Basiswert 2`
  )

Sozialversicherungspflichtig_Beschäftigte_anteil_frau <- ar_sheet %>%
  filter(
    Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
    Ausprägung == "weiblich"
  ) %>%
  mutate(
    anteil = 100 * `Basiswert 1` / `Basiswert 2`
  )


korrelations_daten <- inner_join(
  all_districts_and_city, 
  Sozialversicherungspflichtig_Beschäftigte_anteil_frau, 
  by = c("Raumbezug", "Jahr")
)

korrelations_daten_clean <- korrelations_daten %>%
  filter(Raumbezug != "Stadt München")

ggplot(korrelations_daten_clean, aes(x = mean_age, y = anteil)) +
  geom_point(size = 1.3, color = "grey", alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1) +
  stat_cor(method = "spearman", label.x.npc = "left", label.y.npc = "top") + 
  labs(
    title = "Korrelationskoeffizient zwischen Erstgeburtsalter von Mutter und Anteil Sozialversicherungspflichtigbeschäftigte Frauen(Spearman)",
    x = "Erstgeburtsalter von Mutter",
    y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)"
  ) +
  theme_minimal()

#----------------------------------------------------------------------------------
library(ggplot2)
library(ggpubr)

ggplot(korrelations_daten_clean, aes(x = mean_age, y = anteil)) +
  geom_point(aes(color = Raumbezug), 
             size = 1.3,    
             alpha = 0.7) + 
  geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = 1) +
  
  stat_cor(method = "spearman", 
           label.x = 29.5,  
           label.y = 66,    
           color = "black",
           size = 5) +      
  
  labs(
    title = "Korrelationskoeffizient zwischen Erstgeburtsalter von Mutter und Anteil Sozialversicherungspflichtigbeschäftigte Frauen(Spearman)",
    x = "Erstgeburtsalter von Mutter",
    y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)",
    color = "Stadtteile" 
  ) +
  scale_x_continuous(limits = c(29, 35)) + 
  scale_y_continuous(limits = c(50, 70)) +
  guides(color = guide_legend(ncol = 1)) +
  theme_minimal()


#-----------------------------------------------------------------------
library(tidyverse)
library(ggpubr)
library(scales)
library(ggrepel)


district_stats <- korrelations_daten_clean %>%
  group_by(Raumbezug) %>%
  summarize(
    r_val = round(cor(mean_age, anteil, method = "spearman"), 2),
    max_x = max(mean_age),
    pos_y_line = predict(lm(anteil ~ mean_age), newdata = data.frame(mean_age = max(mean_age))),
    .groups = "drop"
  ) %>%
  mutate(
    legend_label = paste0(Raumbezug, " (R=", r_val, ")"),
    plot_label = paste0(substr(Raumbezug, 1, 2), ": R=", r_val)
  )

plot_data <- korrelations_daten_clean %>%
  inner_join(district_stats, by = "Raumbezug")


total_r <- round(cor(korrelations_daten_clean$mean_age, 
                     korrelations_daten_clean$anteil, 
                     method = "spearman"), 2)
total_label_clean <- paste0("Gesamtstadt (R=", total_r, ")")

sorted_districts <- sort(unique(district_stats$legend_label))

my_levels <- c(sorted_districts, total_label_clean)

plot_data$legend_label <- factor(plot_data$legend_label, levels = my_levels)

my_colors <- hue_pal()(length(sorted_districts))
names(my_colors) <- sorted_districts

all_colors <- c(my_colors, setNames("black", total_label_clean))

ggplot(plot_data, aes(x = mean_age, y = anteil)) +
  geom_point(aes(color = legend_label), size = 1.3, alpha = 0.7) +
  geom_smooth(aes(color = legend_label, group = legend_label), 
              method = "lm", se = FALSE, size = 1) +
  geom_smooth(aes(color = total_label_clean), 
              method = "lm", se = FALSE, size = 1) +
  geom_text_repel(
    data = district_stats,
    aes(x = max_x, y = pos_y_line, label = plot_label, color = legend_label),
    size = 3,
    fontface = "bold",
    hjust = 0,
    nudge_x = 0.3,     
    direction = "y",   
    segment.size = 0.2,
    max.overlaps = 50
  ) +
  scale_color_manual(values = all_colors, limits = my_levels) +
  labs(
    title = "Korrelationskoeffizient zwischen Erstgeburtsalter von Mutter und Anteil Sozialversicherungspflichtigbeschäftigte Frauen(Spearman) nach Stadtteile",
    x = "Erstgeburtsalter von Mutter",
    y = "Anteil Sozialversicherungspflichtigbeschäftigte Frauen (%)",
    color = "Stadtteile" 
  ) +
  guides(color = guide_legend(ncol = 1)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8),
    legend.key.height = unit(0.4, "cm")
  )

