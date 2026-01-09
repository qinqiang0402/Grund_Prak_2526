library(readxl)
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)

# import data
export_be <- read_excel("data/raw/export_be.xlsx")
be_sheet <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
export_ar <- read_excel("data/raw/export_ar.xlsx")
ar_sheet <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")

labormarket <- ar_sheet %>%
  mutate(
    Indikatorwert = gsub(",", ".", Indikatorwert),
    Indikatorwert = gsub("%", "", Indikatorwert),
    Indikatorwert = trimws(Indikatorwert),
    Indikatorwert = as.numeric(Indikatorwert)
  )
population <- be_sheet %>%
  mutate(
    Indikatorwert = gsub(",", ".", Indikatorwert),
    Indikatorwert = gsub("%", "", Indikatorwert),
    Indikatorwert = trimws(Indikatorwert),
    Indikatorwert = as.numeric(Indikatorwert)
  )

# plot of birthrate & female employment by district
employment_female <- labormarket %>%
  filter(Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
         Ausprägung == "weiblich") %>%
  select(Jahr, Raumbezug, employment_female = Indikatorwert) %>%
  distinct(Raumbezug, Jahr, .keep_all = TRUE)

birthrate <- population %>%
  filter(Indikator == "Allgemeine Geburtenrate") %>%
  mutate(Indikatorwert = Indikatorwert/10) %>%
  select(Jahr, Raumbezug, birthrate = Indikatorwert) %>%
  distinct(Raumbezug, Jahr, .keep_all = TRUE)

data_joined <- left_join(birthrate, employment_female, by = c("Jahr", "Raumbezug")) %>%
  pivot_longer(cols = c("birthrate", "employment_female"),
               names_to = "Indikator",
               values_to = "Wert")

data_wide <- birthrate %>%
  left_join(employment_female, by = c("Jahr", "Raumbezug"))

# function giving plot
# input: district name (can be e.g. "01", "1", "'München'", "'Lehel'")
# output: plot by district
plot_birthrate_district <- function(input.district) {
  # variation of input
  districts <- unique(data_wide$Raumbezug)
  # input is number
  if (is.numeric(input.district)) {
    number <- sprintf("%02d", input.district)   # example: 1 -> "01"
    district.name <- districts[str_detect(districts, paste0("^", number, " "))]
    
  } else {
    input.district <- as.character(input.district)
  # input is excactly same as data:
  if (input.district %in% districts) {
    district.name <- district.input
      
  } else {
    # input is a part of district name 
    # example: "Lehel" instead of "Altstadt - Lehel"
    district.name <- districts[str_detect(districts, regex(input.district, ignore_case = TRUE))]
    }
  }
  # error, if no district found with input
  if (length(district.name) == 0) {
    stop("No district found. Please check the input.")
  }
  
  # If more than one district found with input → plot the first & warning
  if (length(district.name) > 1) {
    message("ℹ More than one district been found. Now plotting: ", district.name[1])
    district.name <- district.name[1]
  }
  
  # data from long into wide
  df <- data_wide %>%
    filter(Raumbezug == district.name)
  
  df <- df %>% 
    tidyr::drop_na(birthrate, employment_female)
  
  # extra axis for variable "birthrate" for better vsualization
  range_emp  <- range(df$employment_female, na.rm = TRUE)
  range_birth <- range(df$birthrate, na.rm = TRUE)
  
  scale_factor <- diff(range_emp) / diff(range_birth)
  
  df <- df %>%
    mutate(birthrate_scaled = birthrate * scale_factor)
  
  # plot
  ggplot(df, aes(x = Jahr)) +
    # female employment (axis left)
    geom_line(aes(y = employment_female,
                  color = "Beschäftigungsrate"), linewidth = 1.1) +
    geom_point(aes(y = employment_female,
                   color = "Beschäftigungsrate"), size = 1.8) +
    
    # birthrate (axis right)
    geom_line(aes(y = birthrate_scaled,
                  color = "Allgemeine Geburtenrate"), linewidth = 1.1) +
    geom_point(aes(y = birthrate_scaled,
                   color = "Allgemeine Geburtenrate"), size = 1.8) +

    scale_y_continuous(
      name = "Beschäftigungsrate (%)",
      sec.axis = sec_axis(
        ~ . / scale_factor,
        name = "Allgemeine Geburtenrate"
      )
    ) +
    scale_x_continuous(breaks = seq(2000, 2024, by = 2)) +
    
    scale_color_manual(
      name   = "Indikator",
      values = c("Allgemeine Geburtenrate" = "#E74C3C",   # rot
                 "Beschäftigungsrate"      = "#3498DB")   # blau
    ) +
    
    labs(
      title    = "Allgemeine Geburtenrate und weibliche SV-pflichtige Beschäftigungsrate",
      subtitle = district.name,
      x = "Jahr"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title      = element_text(face = "bold", hjust = 0.5, size = 16),
      plot.subtitle   = element_text(hjust = 0.5),
      axis.title.y.right = element_text(margin = margin(l = 8)),
      legend.position = "bottom",
      legend.title    = element_text(face = "bold")
    )
}
p_01 <- plot_birthrate_district("München")

saveRDS(p_01, "results/figures/Geburtenrate/geburtenrate_dual_trend.rds")

#——————————————————————————————————————————————————————————————————————————————————————————————————————
#
# line birthrate
#


birthrate_average <- birthrate %>%
  group_by(Raumbezug) %>%
  mutate(mean_birthrate = mean(birthrate, na.rm = TRUE)) %>%
  ungroup()

# district with lowest average
lowest_birth_district <- birthrate_average %>%
  filter(Raumbezug != "Stadt München") %>%
  group_by(Raumbezug) %>%
  summarise(mean_birthrate = mean(birthrate, na.rm = TRUE)) %>%
  slice_min(mean_birthrate, n = 1) %>%
  pull(Raumbezug)

# district with highest average
highest_birth_district <- birthrate_average %>%
  filter(Raumbezug != "Stadt München") %>%
  group_by(Raumbezug) %>%
  summarise(mean_birthrate = mean(birthrate, na.rm = TRUE)) %>%
  slice_max(mean_birthrate, n = 1) %>%
  pull(Raumbezug)

#group Munich, district highest, district lowest
line_birthrate_data <- birthrate %>%
  mutate(
    line_birthrate_group = case_when(
      Raumbezug == "Stadt München" ~ "Stadt München", 
      Raumbezug == highest_birth_district ~ "highest",
      Raumbezug == lowest_birth_district ~ "lowest",
      TRUE ~ "rest"
    )
  )

#line diagram birthrate
geburtenrate_trend_nach_stadtteile <- ggplot() +
  geom_line(data = line_birthrate_data %>% 
              filter(line_birthrate_group =="rest"),
            aes(x = Jahr, y = birthrate, group = Raumbezug), 
            color = "grey80", 
            linewidth = 0.5) +
  geom_line(data = line_birthrate_data %>% 
              filter(line_birthrate_group != "rest"), 
            aes(x = Jahr, y = birthrate,color = line_birthrate_group, group = Raumbezug), 
            linewidth = 1.2) +
  scale_color_manual(
    name = "",
    values = c(
      "Stadt München" = "black",
      "highest" = "red",
      "lowest" = "blue"),
    labels = c(
      "Stadt München" = "Gesamtdurchschnitt Münchens",
      "highest" = paste(highest_birth_district, "(Bezirk mit höchstem Durchschnitt)"),
      "lowest" = paste(lowest_birth_district, "(Bezirk mit niedrigstem Durchschnitt)")
    )
  ) +
  labs(
    title = "Entwicklung der Geburtenrate in Stadtbezirken Münchens (2000-2024)",
    x = "Jahr",
    y = "Anteil (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  guides(
    color = guide_legend(
      nrow = 3,      
      byrow = TRUE    
    ))

geburtenrate_trend_nach_stadtteile

saveRDS(geburtenrate_trend_nach_stadtteile, "results/figures/Geburtenrate/geburtenrate_trend_nach_stadtteile.rds")

#_________________________________________________________________________________________
#
# Korrelation
#



# Spearman Correlation between birthrate and female employment

# spatial dimension
# average of years 2000-2024
# correlation by district including whole Munich
cor_birthrate_employment_district <- data_wide %>% 
  group_by(Raumbezug) %>%
  summarise(
    spearman_rho = cor(birthrate, employment_female, method = "spearman", use = "complete.obs"),
    n = n()  # Jahre
  ) %>%
  arrange(desc(spearman_rho))  # descent order
print (cor_birthrate_employment_district, n=26)
# plot
geburtenrate_korr_nach_stadtteile <- ggplot(cor_birthrate_employment_district, 
       aes(x = reorder(Raumbezug, spearman_rho), y = spearman_rho,
           fill = ifelse(spearman_rho >= 0, "Positiv", "Negativ"))) +
  geom_col() +
  scale_fill_manual(
    name = "Korrelation",
    values = c("Positiv" = "steelblue", "Negativ" = "firebrick3")
  ) +
  coord_flip() +  # Bezirke von oben nach unten anzeigen
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  labs(
    title = "Korrelation zwischen Geburtenrate und weiblicher Beschäftigungsrate",
    subtitle = "Bezirke Münchens, Durchschnitt der Jahre 2000–2024",
    x = "Stadtbezirk",
    y = "Spearman ρ"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10)
  )

geburtenrate_korr_nach_stadtteile

saveRDS(geburtenrate_korr_nach_stadtteile, "results/figures/Geburtenrate/geburtenrate_korr_nach_stadtteile.rds")




# time dimension of whole Munich
# correlation by year
cor_birthrate_employment_year <- data_wide %>%
  filter(Raumbezug != "Stadt München") %>%
  group_by(Jahr) %>%
  summarise(
    spearman_rho = cor(birthrate, employment_female,
                       method = "spearman", use = "complete.obs")
  )

#plot
geburtenrate_korr_trend_nach_jahr <- ggplot() +
  geom_line(data = cor_birthrate_employment_year,
            aes(x = Jahr, y = spearman_rho), 
            color = "darkblue", 
            linewidth = 1) +
  labs(
    title = "Entwicklung der Korrelation zwischen Geburtenrate und weibliche Beschäftigung",
    subtitle = "Stadt München, 2000-2024",
    x = "Jahr",
    y = "Spearman p"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
geburtenrate_korr_trend_nach_jahr

saveRDS(geburtenrate_korr_trend_nach_jahr, "results/figures/Geburtenrate/geburtenrate_korr_trend_nach_jahr.rds")


