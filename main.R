# main.R

# 1. Define all required packages
required_packages <- c(
  "quarto", "shiny", "tidyverse", "readxl", 
  "sf", "stringr", "forcats", "ggpubr", 
  "leaflet", "htmltools", "htmlwidgets"
)

# 2. Check and install missing packages
message("--- Checking dependencies ---")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages, repos = "https://cloud.r-project.org")
}

# 3. Fix Quarto Path (Crucial for Mac)
if (Sys.which("quarto") == "") {
  # Common installation paths for Quarto on macOS
  quarto_paths <- c(
    "/usr/local/bin/quarto",
    "/opt/quarto/bin/quarto",
    "/Applications/quarto/bin/quarto"
  )
  
  found_path <- Filter(file.exists, quarto_paths)
  
  if (length(found_path) > 0) {
    Sys.setenv(QUARTO_PATH = found_path[1])
    message(paste("Quarto found at:", found_path[1]))
  } else {
    message("----------------------------------------------------------")
    message("ERROR: Quarto CLI not found on your system!")
    message("Please install Quarto from: https://quarto.org/docs/get-started/")
    message("----------------------------------------------------------")
    stop("Quarto not installed.")
  }
}

# 4. Launch the Quarto Shiny Server
target_file <- "presentation.qmd"

if (file.exists(target_file)) {
  message("--- Starting Interactive Shiny Report ---")
  quarto::quarto_serve(target_file)
} else {
  stop(paste("Error: The file", target_file, "was not found."))
}