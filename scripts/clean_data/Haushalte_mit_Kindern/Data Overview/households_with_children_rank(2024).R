library(readxl)
library(ggplot2)
library(dplyr)

data <- read_excel("data/raw/export_be.xlsx", sheet = 2)

# Filter and clean numeric: convert "14,1" -> 14.1
df <- data %>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt",
    Jahr == 2024,
    Raumbezug != "Stadt München"
  ) %>%
  mutate(
    # force numeric even if it came in as character with commas
    Indikatorwert = suppressWarnings(
      as.numeric(gsub(",", ".", as.character(Indikatorwert)))
    )
  ) %>%
  arrange(desc(Indikatorwert)) %>%
  mutate(Rank = row_number())

# Precompute max safely
y_max <- max(df$Indikatorwert, na.rm = TRUE)

ggplot(df, aes(x = factor(Rank), y = Indikatorwert)) +
  geom_col(fill = "#4F81BD") +  # calm single color
  geom_hline(yintercept = 14.1, color = "red", linetype = "dashed", size = 1) +
  # city names to the right of bars
  geom_text(aes(label = Raumbezug), hjust = -0.05, size = 3.3) +
  labs(
    title = "Share of German Households with Children by District (2024)",
    x = "Rank (sorted by share)",
    y = "Share (%)"
  ) +
  coord_flip() +
  # leave headroom for labels without hard-coding limits
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank()
  )
