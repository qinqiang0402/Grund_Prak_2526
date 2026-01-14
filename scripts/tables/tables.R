# ============================================================
# Script: generate_tables.R
# Purpose: Generate cumulative data tables for presentation slides.
#          Matching the specific visual requirements (Accumulating rows).
# Returns: List containing t1, t2, t3 and the styling function.
# ============================================================

library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(kableExtra)
library(knitr)

# ------------------------------------------------------------
# 1. Styling Function (用于 QMD中美化表格)
# ------------------------------------------------------------
make_daten_table <- function(df) {
  col_names <- c("Indikator", "Ausprägung", "Jahr", "Raumbezug", "Indikatorwert")
  
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

# ------------------------------------------------------------
# 2. Data Processing Function
# ------------------------------------------------------------
generate_tables <- function(ar_raw, be_raw, ki_raw) {
  
  col_names <- c("Indikator", "Ausprägung", "Jahr", "Raumbezug", "Indikatorwert")
  
  # --- A. 基础数据准备 ---
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
  
  # 只保留家庭相关数据
  be_tab_household <- be_tab %>%
    filter(str_detect(Indikator, regex("Haushalte mit Kindern", ignore_case = TRUE)))
  
  # --- B. 计算 Kinderbetreuung (0-2 岁) ---
  ki_tab <- be_raw %>%
    filter(Indikator == "Altersgruppen", Ausprägung == "bis 2 Jahre") %>%
    transmute(
      Jahr = as.numeric(Jahr),
      Raumbezug,
      kinder_total = as.numeric(`Basiswert 1`)
    ) %>%
    left_join(
      ki_raw %>%
        filter(Indikator == "Altersgruppen", Ausprägung == "bis 2 Jahre") %>%
        transmute(
          Jahr = as.numeric(Jahr),
          Raumbezug,
          kinder_betreut = as.numeric(`Basiswert 1`)
        ),
      by = c("Jahr", "Raumbezug")
    ) %>%
    mutate(
      anteil_betreut = 100 * kinder_betreut / kinder_total
    ) %>%
    # 筛选特定的两行 (Laim 2007, Moosach 2024)
    filter(
      (Jahr == 2007 & Raumbezug == "25 Laim") |
        (Jahr == 2024 & Raumbezug == "10 Moosach")
    )
  
  # 格式化 Kinderbetreuung 数据框
  df_childcare <- ki_tab %>%
    transmute(
      Indikator = "Kinderbetreuung",
      Ausprägung = "bis 2 Jahre",
      Jahr = as.character(Jahr),
      Raumbezug = as.character(Raumbezug),
      Indikatorwert = format(round(anteil_betreut, 1), nsmall = 1)
    ) %>%
    arrange(Jahr)
  
  # --- C. 筛选 Employment 和 Household 的特定行 ---
  selection <- tribble(
    ~Indikator,                                           ~Ausprägung, ~Jahr,  ~Raumbezug,       ~Quelle,
    "Sozialversicherungspflichtig Beschäftigte - Anteil", "weiblich",   "2000","13 Bogenhausen","Arbeitsmarkt",
    "Sozialversicherungspflichtig Beschäftigte - Anteil", "weiblich",   "2024","Stadt München", "Arbeitsmarkt",
    "Haushalte mit Kindern",                              "insgesamt",  "2012","03 Maxvorstadt","Bevölkerung",
    "Haushalte mit Kindern",                              "insgesamt",  "2024","06 Sendling",   "Bevölkerung"
  )
  
  df_combined <- bind_rows(ar_tab, be_tab_household) %>%
    semi_join(selection, by = c("Indikator", "Ausprägung", "Jahr", "Raumbezug", "Quelle")) %>%
    select(-Quelle) %>%
    mutate(
      # 缩短名称以匹配截图
      Indikator = if_else(
        Indikator == "Sozialversicherungspflichtig Beschäftigte - Anteil",
        "Frauenbeschäftigung",
        Indikator
      )
    )
  
  # 分离出两部分以便排序和重组
  df_employment <- df_combined %>%
    filter(Indikator == "Frauenbeschäftigung") %>%
    arrange(as.numeric(Jahr))
  
  df_household <- df_combined %>%
    filter(Indikator == "Haushalte mit Kindern") %>%
    arrange(as.numeric(Jahr))
  
  # --- D. 构建最终的三个表格 (关键步骤：累加逻辑) ---
  
  # 表格 1: 只有就业
  t1 <- df_employment
  
  # 表格 2: 就业 + 家庭 (bind_rows)
  t2 <- bind_rows(df_employment, df_household)
  
  # 表格 3: 就业 + 家庭 + 育儿 (bind_rows)
  t3 <- bind_rows(df_employment, df_household, df_childcare)
  
  # 处理 NA 为空字符串 (为了美观)
  t1 <- t1 %>% mutate(across(everything(), ~ ifelse(is.na(.) | . == "NA", "", .)))
  t2 <- t2 %>% mutate(across(everything(), ~ ifelse(is.na(.) | . == "NA", "", .)))
  t3 <- t3 %>% mutate(across(everything(), ~ ifelse(is.na(.) | . == "NA", "", .)))
  
  return(list(t1 = t1, t2 = t2, t3 = t3))
}