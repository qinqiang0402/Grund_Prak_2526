library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(kableExtra)

#────────────────────────────────────────────────────────────
# 0) 统一列名（目的：让不同来源的数据能 bind_rows）
#────────────────────────────────────────────────────────────
col_names <- c("Indikator", "Ausprägung", "Jahr", "Raumbezug", "Indikatorwert")

#────────────────────────────────────────────────────────────
# 1) 读取原始数据（目的：一次性载入三张表）
#────────────────────────────────────────────────────────────
ar_raw <- read_excel("data/raw/export_ar.xlsx", sheet = "ARBEITSMARKT")
be_raw <- read_excel("data/raw/export_be.xlsx", sheet = "BEVÖLKERUNG")
ki_raw <- read_excel("data/raw/export_ki.xlsx", sheet = "KINDERBETREUUNG")

#────────────────────────────────────────────────────────────
# 2) 提取用于表格展示的原始字段（目的：只保留前 5 列并统一成字符）
#    注意：这里先不做筛选，后面用 selection 精准挑行
#────────────────────────────────────────────────────────────
ar_tab <- ar_raw %>%
  select(1:5) %>%
  setNames(col_names) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(Quelle = "Arbeitsmarkt")

be_tab <- be_raw %>%
  select(1:5) %>%
  setNames(col_names) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(Quelle = "Bevölkerung")

# 只保留“Haushalte mit Kindern”（目的：减少后续 join 的规模）
be_tab_household <- be_tab %>%
  filter(str_detect(Indikator, regex("Haushalte mit Kindern", ignore_case = TRUE)))

#────────────────────────────────────────────────────────────
# 3) Kinderbetreuung：计算 0–2 岁被托管比例（目的：从 BE + KI 合成一个指标）
#    关键修复：Jahr 在原始数据里通常是 numeric，所以筛选时用 2007 / 2024（不要写成 "2007"）
#────────────────────────────────────────────────────────────
ki_tab <- be_raw %>%
  filter(
    Indikator == "Altersgruppen",
    Ausprägung == "bis 2 Jahre"
  ) %>%
  transmute(
    Jahr,
    Raumbezug,
    kinder_total = as.numeric(`Basiswert 1`)
  ) %>%
  left_join(
    ki_raw %>%
      filter(
        Indikator == "Altersgruppen",
        Ausprägung == "bis 2 Jahre"
      ) %>%
      transmute(
        Jahr,
        Raumbezug,
        kinder_betreut = as.numeric(`Basiswert 1`)
      ),
    by = c("Jahr", "Raumbezug")
  ) %>%
  mutate(
    anteil_betreut = 100 * kinder_betreut / kinder_total
  ) %>%
  # 这里筛选你想展示的两行（目的：和你原来的例子一致）
  filter(
    (Jahr == 2007 & Raumbezug == "25 Laim") |
      (Jahr == 2024 & Raumbezug == "10 Moosach")
  )

df_childcare <- ki_tab %>%
  transmute(
    Indikator = "Kinderbetreuung",
    Ausprägung = "bis 2 Jahre",
    Jahr = as.character(Jahr),
    Raumbezug = as.character(Raumbezug),
    # 这里用 1 位小数（目的：PPT表格更干净）
    Indikatorwert = format(round(anteil_betreut, 1), nsmall = 1)
  ) %>%
  arrange(Jahr)

#────────────────────────────────────────────────────────────
# 4) selection：你要展示的“精确行”（目的：避免用模糊过滤拿错行）
#────────────────────────────────────────────────────────────
selection <- tribble(
  ~Indikator,                                           ~Ausprägung, ~Jahr,  ~Raumbezug,       ~Quelle,
  "Sozialversicherungspflichtig Beschäftigte - Anteil", "weiblich",   "2000","13 Bogenhausen","Arbeitsmarkt",
  "Sozialversicherungspflichtig Beschäftigte - Anteil", "weiblich",   "2024","Stadt München", "Arbeitsmarkt",
  "Haushalte mit Kindern",                              "insgesamt",  "2012","03 Maxvorstadt","Bevölkerung",
  "Haushalte mit Kindern",                              "insgesamt",  "2024","06 Sendling",   "Bevölkerung"
)

#────────────────────────────────────────────────────────────
# 5) 从 Arbeitsmarkt + Bevölkerung 中挑出那 4 行（目的：做你“Daten”页表格）
#────────────────────────────────────────────────────────────
df_data_4 <- bind_rows(ar_tab, be_tab_household) %>%
  semi_join(
    selection,
    by = c("Indikator", "Ausprägung", "Jahr", "Raumbezug", "Quelle")
  ) %>%
  select(-Quelle) %>%
  mutate(
    Indikator = if_else(
      Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
      "Frauenbeschäftigung",
      Indikator
    ),
    Indikator = factor(
      Indikator,
      levels = c("Frauenbeschäftigung", "Haushalte mit Kindern")
    )
  ) %>%
  arrange(Indikator, as.numeric(Jahr))

# 分成两个小表（目的：你现在有多页 Daten，按模块展示更清楚）
df_employment <- df_data_4 %>%
  filter(Indikator == "Frauenbeschäftigung") %>%
  arrange(as.numeric(Jahr))

df_household <- df_data_4 %>%
  filter(Indikator == "Haushalte mit Kindern") %>%
  arrange(as.numeric(Jahr))

# 最终 6 行总表（目的：如果你想一页展示 6 行就用它）
df_data_final <- bind_rows(df_employment, df_household, df_childcare) %>%
  mutate(across(everything(), ~ ifelse(is.na(.) | . == "NA", "", .)))

#────────────────────────────────────────────────────────────
# 6) 关键：固定列宽输出函数（目的：Quarto 中不同页表格列间距一致）
#────────────────────────────────────────────────────────────
make_daten_table <- function(df) {
  df %>%
    knitr::kable(
      format = "html",
      escape = TRUE,
      align = c("l", "l", "c", "l", "c"),
      col.names = col_names
    ) %>%
    kable_styling(full_width = TRUE) %>%
    column_spec(1, width = "32%") %>%  # Indikator
    column_spec(2, width = "16%") %>%  # Ausprägung
    column_spec(3, width = "10%") %>%  # Jahr
    column_spec(4, width = "28%") %>%  # Raumbezug
    column_spec(5, width = "14%")      # Indikatorwert
}

# 你可以在 Quarto 里分别用这三个对象
table_data_01 <- df_employment
table_data_02 <- df_household
table_data_03 <- df_childcare


view(table_data_01)
view(table_data_02)
view(table_data_03)

saveRDS(table_data_03, "results/figures/table/table_data_03.rds")
saveRDS(table_data_01, "results/figures/table/table_data_01.rds")
saveRDS(table_data_02, "results/figures/table/table_data_02.rds")
