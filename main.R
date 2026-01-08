# main.R

# 1. Define all required packages
required_packages <- c(
  "quarto", "shiny", "tidyverse", "readxl", 
  "sf", "stringr", "forcats", "ggpubr", 
  "leaflet", "htmltools", "htmlwidgets","kableExtra"
)

# 2. Check and install missing packages
message("--- Checking dependencies ---")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages, repos = "https://cloud.r-project.org")
}

# 3. Fix Quarto Path (Supports both Mac and Windows)
if (Sys.which("quarto") == "") {
  if (.Platform$OS.type == "unix") {
    # macOS / Linux paths
    quarto_paths <- c("/usr/local/bin/quarto", "/opt/quarto/bin/quarto", "/Applications/quarto/bin/quarto")
  } else {
    # Windows common paths
    quarto_paths <- c(
      paste0(Sys.getenv("ProgramFiles"), "\\Quarto\\bin\\quarto.exe"),
      paste0(Sys.getenv("LocalAppData"), "\\Programs\\Quarto\\bin\\quarto.exe")
    )
  }
  
  found_path <- Filter(file.exists, quarto_paths)
  
  if (length(found_path) > 0) {
    Sys.setenv(QUARTO_PATH = found_path[1])
    message(paste("Quarto found at:", found_path[1]))
  } else {
    message("ERROR: Quarto CLI not found!")
    stop("Please install Quarto from: https://quarto.org/docs/get-started/")
  }
}

# 4. Launch the Quarto Shiny Server
target_file <- "presentation.qmd"
if (file.exists(target_file)) {
  message("--- Starting Interactive Shiny Report ---")
  quarto::quarto_serve(target_file)
} else {
  stop("Error: presentation.qmd not found.")
}