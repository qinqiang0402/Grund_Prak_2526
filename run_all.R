# run_all.R
# Purpose: One-click reproducibility — the instructor only needs to click "Source" once.

# ===================== 0) User settings =====================
REBUILD_RDS <- FALSE              # TRUE = rebuild .rds via scripts; FALSE = use cached .rds from repo
AUTO_RESTORE_RENV <- FALSE        # TRUE = run renv::restore() (recommended for reproducibility)
CHECK_RDS <- TRUE                 # TRUE = auto-detect readRDS(...) in QMD and verify files exist

RUN_MODE <- "preview"             # "preview" for Shiny docs; "render" for static output
QMD_FILE <- "presentation.qmd"    # Quarto entry file (must exist)
OUTPUT_DIR <- NULL                # Only relevant for RUN_MODE = "render" (NULL = Quarto default)

# ===================== 1) Locate project root =====================
# Purpose: Avoid absolute paths. The script should work regardless of where it is sourced from.

get_script_dir <- function() {
  # If sourced via source("run_all.R"), sys.frame(1)$ofile is usually available
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(ofile) && nzchar(ofile)) {
    return(dirname(normalizePath(ofile)))
  }
  
  # If running inside RStudio, use the active document path when possible
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
    if (!is.null(ctx$path) && nzchar(ctx$path)) {
      return(dirname(normalizePath(ctx$path)))
    }
  }
  
  # Fallback: current working directory
  normalizePath(getwd())
}

root <- get_script_dir()
setwd(root)
message("Project root: ", root)

# ===================== 2) Hard check: R version compatibility with renv.lock =====================
# Purpose: Fail early with a clear message instead of a long Bioconductor download error.
# Rationale: Bioconductor version in renv.lock may require a minimum R version.

lockfile <- file.path(root, "renv.lock")
if (file.exists(lockfile)) {
  txt <- readLines(lockfile, warn = FALSE)
  
  # Heuristic check: Bioconductor 3.22 requires R >= 4.5
  uses_bioc_322 <- any(grepl('"Bioconductor"\\s*:\\s*\\{', txt)) &&
    any(grepl('"Version"\\s*:\\s*"3\\.22', txt))
  
  if (uses_bioc_322 && getRversion() < "4.5.0") {
    stop(
      "R version mismatch.\n",
      "- renv.lock uses Bioconductor 3.22, which requires R >= 4.5.\n",
      "- You are running R ", getRversion(), ".\n\n",
      "Fix:\n",
      "1) Install R 4.5.x\n",
      "2) Restart R/RStudio\n",
      "3) Re-run source('run_all.R')\n"
    )
  }
}

# ===================== 3) (Optional) Restore dependencies via renv =====================
# Purpose: Ensure the instructor gets the same package versions (high reproducibility).

if (AUTO_RESTORE_RENV && file.exists("renv.lock")) {
  if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv", repos = "https://cloud.r-project.org")
  }
  
  message("Restoring packages from renv.lock ...")
  
  tryCatch(
    renv::restore(prompt = FALSE),
    error = function(e) {
      stop(
        "renv::restore() failed.\n",
        "Message: ", conditionMessage(e), "\n\n",
        "Tip: If you only want to test rendering with the already-installed packages,\n",
        "set AUTO_RESTORE_RENV <- FALSE and re-run.\n"
      )
    }
  )
} else if (file.exists("renv.lock")) {
  message("renv.lock found. (Optional) Set AUTO_RESTORE_RENV <- TRUE to run renv::restore().")
}

# ===================== 4) (Optional) Rebuild .rds results =====================
# Purpose: Full rebuild from raw data (traceability / reproducibility from source).

if (isTRUE(REBUILD_RDS)) {
  pipeline <- "scripts/00_run_pipeline.R"
  if (!file.exists(pipeline)) {
    stop("REBUILD_RDS=TRUE but missing pipeline script: ", pipeline)
  }
  message("Rebuilding .rds via: ", pipeline)
  source(pipeline, local = new.env())
}

# ===================== 5) Sanity check: validate .rds dependencies from the entry QMD =====================
# Purpose: Automatically detect readRDS("...") paths used in QMD and fail early if files are missing.

extract_rds_paths_from_qmd <- function(qmd_file) {
  x <- readLines(qmd_file, warn = FALSE)
  
  # Remove comment-only lines to avoid matching commented readRDS(...)
  x <- x[!grepl("^\\s*#", x)]
  
  # Match readRDS("...rds") and readRDS('...rds')
  m <- gregexpr("readRDS\\(\\s*['\"]([^'\"]+\\.rds)['\"]\\s*\\)", x, perl = TRUE)
  hits <- regmatches(x, m)
  
  paths <- unlist(hits, use.names = FALSE)
  if (length(paths) == 0) return(character())
  
  paths <- sub(".*readRDS\\(\\s*['\"]", "", paths)
  paths <- sub("['\"]\\s*\\).*", "", paths)
  
  unique(paths)
}

if (!file.exists(QMD_FILE)) {
  stop("Missing Quarto entry file: ", QMD_FILE)
}

if (isTRUE(CHECK_RDS)) {
  rds_paths <- extract_rds_paths_from_qmd(QMD_FILE)
  
  if (length(rds_paths) > 0) {
    missing <- rds_paths[!file.exists(rds_paths)]
    if (length(missing) > 0) {
      stop(
        "Missing .rds files referenced in ", QMD_FILE, ":\n- ",
        paste(missing, collapse = "\n- "),
        "\n\nTip: set REBUILD_RDS <- TRUE to regenerate them (pipeline).\n"
      )
    }
    message("RDS check passed (auto). Found ", length(rds_paths), " .rds files.")
  } else {
    message("No readRDS() calls found in ", QMD_FILE, " (skipping RDS check).")
  }
} else {
  message("CHECK_RDS=FALSE (skipping RDS check).")
}



# ===================== 6) Minimal packages for Quarto rendering =====================
# Purpose: Even if we skip renv::restore(), Quarto still needs knitr/rmarkdown in the active library.

ensure_pkgs <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing)
  }
}

# Quarto must have these to render .qmd
ensure_pkgs(c("knitr", "rmarkdown"))

# runtime: shiny needs shiny
# (You already use Shiny in the presentation)
ensure_pkgs(c("shiny"))







# ===================== 6) Run Quarto via CLI (most robust) =====================
# Purpose: Shiny documents must be started via 'quarto preview' to run an R/Shiny backend.
# Note: Do NOT rely on the R package {quarto}; the CLI is more universally available.

check_quarto_cli <- function() {
  status <- suppressWarnings(system("quarto --version", ignore.stdout = TRUE, ignore.stderr = TRUE))
  identical(status, 0L)
}

if (!check_quarto_cli()) {
  stop(
    "Quarto CLI not found.\n",
    "Fix: Install Quarto and ensure the 'quarto' command is available in PATH.\n",
    "Then re-run source('run_all.R').\n"
  )
}

if (RUN_MODE == "preview") {
  message("Starting Quarto preview (Shiny): ", QMD_FILE)
  system(sprintf('quarto preview "%s"', QMD_FILE))
} else {
  message("Rendering Quarto document (static): ", QMD_FILE)
  cmd <- if (is.null(OUTPUT_DIR)) {
    sprintf('quarto render "%s"', QMD_FILE)
  } else {
    dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
    sprintf('quarto render "%s" --output-dir "%s"', QMD_FILE, OUTPUT_DIR)
  }
  status <- system(cmd)
  if (!identical(status, 0L)) stop("Quarto render failed.")
}