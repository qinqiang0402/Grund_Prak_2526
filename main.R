# ============================================================
# Script: main.R
# Purpose: Entry point for reproducibility.
#          1. Forcefully activates 'renv' (bypassing .Rprofile).
#          2. Restores the project library from 'renv.lock'.
#          3. Checks for Quarto CLI and launches the Shiny report.
# ============================================================

# ------------------------------------------------------------
# 1. Force Environment Activation
# ------------------------------------------------------------
# This ensures local library is used even if .Rprofile is missing
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}

message("\n=== Step 1: Setting up Reproducible Environment ===")

# Check if renv is installed (bootstrapping)
if (!require("renv", quietly = TRUE)) {
  message("Installing 'renv' package...")
  install.packages("renv", repos = "https://cloud.r-project.org")
}

# --- 🚑 CRITICAL FIX FOR MATRIX PACKAGE 🚑 ---
if ("Matrix" %in% installed.packages()[,"Package"]) {
  tryCatch({
    library(Matrix)
  }, error = function(e) {
    message("⚠️ Detected broken Matrix installation. Re-installing Matrix...")
    install.packages("Matrix", repos = "https://cloud.r-project.org")
  })
}
# -----------------------------------------------

# Restore the project library
if (file.exists("renv.lock")) {
  message("Restoring packages from lockfile (this may take a few minutes)...")
  # Use library location specific to this project
  renv::restore(prompt = FALSE)
} else {
  warning("renv.lock not found! Please make sure you have initialized renv.")
}

# ------------------------------------------------------------
# 2. Quarto Path Detection
# ------------------------------------------------------------
message("\n=== Step 2: Checking Quarto CLI ===")

if (Sys.which("quarto") == "") {
  if (.Platform$OS.type == "unix") {
    quarto_paths <- c("/usr/local/bin/quarto", "/opt/quarto/bin/quarto", "/Applications/quarto/bin/quarto")
  } else {
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
} else {
  message("Quarto is available in PATH.")
}

# ------------------------------------------------------------
# 3. Launch Application
# ------------------------------------------------------------
message("\n=== Step 3: Launching Interactive Report ===")
target_file <- "presentation.qmd"

if (file.exists(target_file)) {
  message("--- Starting Interactive Shiny Report ---")
  quarto::quarto_serve(target_file, browse = TRUE)
} else {
  stop("Error: 'presentation.qmd' not found in the working directory.")
}