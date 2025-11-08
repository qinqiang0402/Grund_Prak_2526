library(dplyr)
library(ggplot2)
library(ggrepel)
library(readxl)
library(RColorBrewer)


be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")

# filter indicator "Haushalte mit Kindern"
households_with_children <- be_sheet %>%
  select(Indikator, Ausprägung, Jahr, Raumbezug, Indikatorwert, "Basiswert 1", "Basiswert 2")%>%
  filter(
    Indikator == "Haushalte mit Kindern",
    Ausprägung == "insgesamt"
  ) %>%
  mutate(
    anteil = 100 * `Basiswert 1` / `Basiswert 2`
  )

# calculate district averages
district_averages <- households_with_children %>%
  group_by(Raumbezug) %>%
  summarize(avg_pct = mean(anteil, na.rm = TRUE), .groups = "drop")

# get top 3 and bottom 3 districts (excluding city total)
top3 <- district_averages %>%
  filter(Raumbezug != "Stadt München") %>%
  arrange(desc(avg_pct)) %>%
  slice(1:3) %>%
  pull(Raumbezug)

bottom3 <- district_averages %>%
  filter(Raumbezug != "Stadt München") %>%
  arrange(avg_pct) %>%
  slice(1:3) %>%
  pull(Raumbezug)

highlight_districts <- c("Stadt München", top3, bottom3)

# prepare data for plotting
data_for_plot <- households_with_children %>%
  mutate(
    highlight_status = case_when(
      Raumbezug == "Stadt München" ~ "Munich Average",
      Raumbezug %in% top3 ~ "Top 3",
      Raumbezug %in% bottom3 ~ "Bottom 3",
      TRUE ~ "Other"
    )
  )

# color palette
colors <- c(
  "Top 3" = brewer.pal(5, "Oranges")[5],
  "Bottom 3" = brewer.pal(5, "Blues")[5],
  "Munich Average" = "black",
  "Other" = "grey80"
)

# label for the latest year
label_data <- data_for_plot %>%
  filter(Jahr == max(Jahr, na.rm = TRUE)) %>%
  filter(Raumbezug %in% highlight_districts) %>%
  distinct(Raumbezug, .keep_all = TRUE)

# plot
haushalt_kindern_jahr <- ggplot(data_for_plot, aes(x = Jahr, y = anteil, group = Raumbezug)) +
  # grey background lines for all other districts
  geom_line(data = subset(data_for_plot, highlight_status == "Other"),
            color = "grey80", linewidth = 0.6, alpha = 0.6) +
  
  # colored lines for highlighted groups
  geom_line(data = subset(data_for_plot, highlight_status != "Other"),
            aes(color = highlight_status), linewidth = 1.2, alpha = 0.9) +
  
  # text labels for rightmost points (latest year)
  geom_text_repel(
    data = label_data,
    aes(label = Raumbezug, color = highlight_status),
    nudge_x = 0.3,
    size = 3,
    show.legend = FALSE,
    direction = "y",
    segment.size = 0.3,
    segment.color = "grey50"
  ) +
  
  # color scale
  scale_color_manual(values = colors) +
  
  # x-axis: auto breaks, no hard limits
  scale_x_continuous(
    breaks = seq(min(data_for_plot$Jahr, na.rm = TRUE),
                 max(data_for_plot$Jahr, na.rm = TRUE), by = 2)
  ) +
  
  # y-axis nice formatting
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  
  # make sure labels can go outside
  coord_cartesian(clip = "off") +
  
  # clean theme
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.margin = unit(c(0.5, 3, 0.5, 0.5), "cm"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  ) +
  
  # legend arrangement
  guides(color = guide_legend(ncol = 3)) +
  
  # labels
  labs(
    title = "Haushalte mit Kindern in München (2012–2024)",
    subtitle = "Top 3 und Bottom 3 Stadtbezirke im Vergleich zum gesamtstädtischen Durchschnitt",
    x = "Jahr",
    y = "Anteil der Haushalte mit Kindern (%)"
  )

haushalt_kindern_jahr
