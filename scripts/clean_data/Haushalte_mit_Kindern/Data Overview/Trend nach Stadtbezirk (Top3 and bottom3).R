library(ggplot2)
library(dplyr)
library(readxl)

data <- read_excel("data/raw/export_be.xlsx", sheet = 2)

df_trend <- data %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    Indikatorwert = as.numeric(gsub(",", ".", as.character(Indikatorwert))),
    color_group = case_when(
      Raumbezug == "06 Sendling" ~ "top1",
      Raumbezug == "25 Laim" ~ "top2",
      Raumbezug == "17 Obergiesing - Fasangarten" ~ "top3",
      Raumbezug == "01 Altstadt - Lehel" ~ "bottom1",
      Raumbezug == "21 Pasing - Obermenzing" ~ "bottom2",
      Raumbezug == "22 Aubing - Lochhausen - Langwied" ~ "bottom3",
      Raumbezug == "Stadt München" ~ "munich",
      TRUE ~ "other"
    )
  )

# ---- Color palette identical to your uploaded image ----
cols <- c(
  "top1"    = "#C45100",  
  "top2"    = "#E56900",  
  "top3"    = "#FF8C00",  
  
  "bottom1" = "#0072B2",
  "bottom2" = "#56B4E9",
  "bottom3" = "#92C9F7",
  
  "munich"  = "black",
  
  "other"   = "grey80"
)


sizes <- c(
  "top1" = 1.3, "top2" = 1.3, "top3" = 1.3,
  "bottom1" = 1.3, "bottom2" = 1.3, "bottom3" = 1.3,
  "munich" = 1.5,
  "other" = 0.5
)

# ---- Create label data (latest year only) ----
latest_year <- max(df_trend$Jahr, na.rm = TRUE)

df_labels <- df_trend %>%
  filter(
    Jahr == latest_year,
    color_group != "other"   # 只给重点区加标签
  ) %>%
  select(Raumbezug, Jahr, Indikatorwert, color_group)

# ---- Plot with labels ----
ggplot(df_trend, aes(
  x = Jahr,
  y = Indikatorwert,
  group = Raumbezug,
  color = color_group,
  size = color_group
)) +
  geom_line(alpha = 0.95) +
  geom_text(
    data = df_labels,
    aes(
      label = Raumbezug,
      x = Jahr + 0.3        # 右移标签
    ),
    hjust = 0,
    size = 3.2,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_color_manual(values = cols) +
  scale_size_manual(values = sizes) +
  expand_limits(x = latest_year + 1.2) +   # 为右侧标签留空间
  labs(
    title = "Haushalte mit Kindern – Trend nach Stadtbezirk",
    subtitle = "Top 3 (Orange), Bottom 3 (Blau), Stadt München (Schwarz) — mit Bezirkslabels",
    x = "Jahr",
    y = "Anteil (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    legend.position = "none"
  )
