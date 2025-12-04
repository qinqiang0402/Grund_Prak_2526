### env_setup.R – 一键搭建项目环境（自动安装缺失包） ###

## 0. 如果在 RStudio 里，就把工作目录设成这个文件所在目录
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

## 1. 设置 CRAN 镜像（解决 trying to use CRAN without setting a mirror）
repos <- getOption("repos")
if (is.null(repos) || is.na(repos["CRAN"]) || repos["CRAN"] == "@CRAN@" || repos["CRAN"] == "") {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

## 2. BiocManager + Icens（Icens 是 Bioconductor 的包）
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
if (!requireNamespace("Icens", quietly = TRUE)) {
  BiocManager::install("Icens", ask = FALSE, update = FALSE)
}

## 3. 需要用到的包列表（CRAN 上的）
pkgs <- c(
  "tidyr",
  "dplyr",
  "stringr",
  "checkmate",
  "lubridate",
  "ggplot2",
  "purrr",
  "knitr",
  "pROC",
  "rpart",
  "survival",
  "ggsurvfit",
  "survminer",
  "moments",
  "zoo",
  "kableExtra",
  "readr",
  "patchwork",
  "epitools",
  "e1071",
  "RColorBrewer",
  "icenReg",
  "interval",
  "scales",
  "ggthemes",
  "shiny",
  "tidyverse",
  "readxl",
  "sf",
  "forcats",
  "ggpubr",
  "leaflet",
  "htmltools"
)

## 4. 自动安装 + 加载
for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(">> Installing package: ", pkg)
    install.packages(pkg)  # 会用上面设置好的 CRAN 镜像
  }
  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  )
}

## 5. 如果变量名里带 "/"（Mac 某些情况下会出现），重命名一下
for (var in ls()) {
  if (stringr::str_detect(var, "/")) {
    new_var <- gsub("/", "", var)
    assign(new_var, get(var))
    rm(list = var)
  }
}

## 6. 清理临时变量
rm(pkgs, repos)

