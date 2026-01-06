### env_setup.R – One-click project environment setup (auto-install missing packages) ###

## 0. If running in RStudio, set the working directory to the directory of this file
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

## 1. Set a CRAN mirror (fixes: "trying to use CRAN without setting a mirror")
repos <- getOption("repos")
if (is.null(repos) || is.na(repos["CRAN"]) || repos["CRAN"] == "@CRAN@" || repos["CRAN"] == "") {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

## 2. BiocManager + Icens (Icens is a Bioconductor package)
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
if (!requireNamespace("Icens", quietly = TRUE)) {
  BiocManager::install("Icens", ask = FALSE, update = FALSE)
}

## 3. Required packages (from CRAN)
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

## 4. Auto-install + load packages
for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(">> Installing package: ", pkg)
    install.packages(pkg)  # Uses the CRAN mirror set above
  }
  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  )
}

## 5. If any object names contain "/" (can happen on some systems), rename them
for (var in ls()) {
  if (stringr::str_detect(var, "/")) {
    new_var <- gsub("/", "", var)
    assign(new_var, get(var))
    rm(list = var)
  }
}

## 6. Clean up temporary variables
rm(pkgs, repos)