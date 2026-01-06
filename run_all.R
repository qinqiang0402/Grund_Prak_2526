# run_all.R
# Purpose: One-click reproducibility — the instructor only needs to click "Source" once.

# ===================== 0) User settings =====================
# Purpose: Centralize the behavior here so the instructor doesn't need to touch the rest.

REBUILD_RDS <- FALSE            # TRUE = rebuild .rds via scripts; FALSE = use cached .rds from repo
RUN_MODE    <- "preview"        # "preview" for Shiny docs; "render" for static output
QMD_FILE    <- "presentation.qmd"  # Quarto entry file (the one you want to run)
OUTPUT_DIR  <- NULL             # Only relevant for RUN_MODE = "render" (NULL = default)

# ===================== 1) Locate project root =====================
# Purpose: Avoid absolute paths. The script should work no matter where the instructor runs it.

# Prefer {here} if available (works well with .Rproj / git repo)
if (requireNamespace("here", quietly = TRUE)) {
  root <- here::here()
} else {
  # Fallback: assume current working directory is the project root
  root <- normalizePath(getwd())
}

setwd(root)
message("Project root: ", root)

# ===================== 2) (Optional) Restore dependencies via renv =====================
# Purpose: Ensure the instructor gets the same package versions (higher reproducibility).
# Keep this OFF by default if you want to avoid long first-time installs.

AUTO_RESTORE_RENV <- FALSE

if (AUTO_RESTORE_RENV && file.exists("renv.lock")) {
  if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv")
  }
  message("Restoring packages from renv.lock ...")
  renv::restore(prompt = FALSE)
} else if (file.exists("renv.lock")) {
  message("renv.lock found. (Optional) Set AUTO_RESTORE_RENV <- TRUE to run renv::restore().")
}

# ===================== 3) (Optional) Rebuild .rds results =====================
# Purpose: Enable full rebuild from raw data (traceability / reproducibility from source).

if (isTRUE(REBUILD_RDS)) {
  pipeline <- "scripts/00_run_pipeline.R"
  if (!file.exists(pipeline)) {
    stop("REBUILD_RDS=TRUE but missing pipeline script: ", pipeline)
  }
  message("Rebuilding .rds via: ", pipeline)
  source(pipeline, local = new.env())
}

# ===================== 4) Sanity check: validate .rds dependencies from the entry QMD =====================
# Purpose: Don't maintain a hard-coded list. Automatically detect all readRDS("...") paths
# used in the entry file and fail early with a clear error message.

extract_rds_paths_from_qmd <- function(qmd_file) {
  x <- readLines(qmd_file, warn = FALSE)
  
  # Ignore pure comment lines to avoid counting commented-out readRDS(...)
  x <- x[!grepl("^\\s*#", x)]
  
  # Match readRDS("...rds") and readRDS('...rds')
  m <- gregexpr("readRDS\\(\\s*['\"]([^'\"]+\\.rds)['\"]\\s*\\)", x, perl = TRUE)
  hits <- regmatches(x, m)
  if (length(hits) == 0) return(character())
  
  # Convert matched calls into plain file paths
  paths <- unlist(hits, use.names = FALSE)
  paths <- sub(".*readRDS\\(\\s*['\"]", "", paths)
  paths <- sub("['\"]\\s*\\).*", "", paths)
  
  unique(paths)
}

if (!file.exists(QMD_FILE)) {
  stop("Missing Quarto entry file: ", QMD_FILE)
}

rds_paths <- extract_rds_paths_from_qmd(QMD_FILE)

# If no readRDS() found, that's fine (some projects may compute on the fly)
if (length(rds_paths) > 0) {
  missing <- rds_paths[!file.exists(rds_paths)]
  if (length(missing) > 0) {
    stop(
      "Missing .rds files referenced in ", QMD_FILE, ":\n- ",
      paste(missing, collapse = "\n- "),
      "\n\nTip: set REBUILD_RDS <- TRUE to regenerate them (Pipeline)."
    )
  }
  message("RDS check passed (auto). Found ", length(rds_paths), " .rds files.")
} else {
  message("No readRDS() calls found in ", QMD_FILE, " (skipping RDS check).")
}

# ===================== 5) Run Quarto =====================
# Purpose: Shiny documents must be started via 'preview' to run an R/Shiny backend.

if (RUN_MODE == "preview") {
  if (requireNamespace("quarto", quietly = TRUE)) {
    message("Starting Quarto preview (Shiny): ", QMD_FILE)
    quarto::quarto_preview(QMD_FILE)
  } else {
    message("Package 'quarto' not found; trying CLI: quarto preview ...")
    system(sprintf('quarto preview "%s"', QMD_FILE))
  }
} else {
  if (requireNamespace("quarto", quietly = TRUE)) {
    message("Rendering Quarto document (static): ", QMD_FILE)
    if (is.null(OUTPUT_DIR)) {
      quarto::quarto_render(QMD_FILE)
    } else {
      dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
      quarto::quarto_render(QMD_FILE, output_dir = OUTPUT_DIR)
    }
  } else {
    message("Package 'quarto' not found; trying CLI: quarto render ...")
    cmd <- if (is.null(OUTPUT_DIR)) {
      sprintf('quarto render "%s"', QMD_FILE)
    } else {
      dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
      sprintf('quarto render "%s" --output-dir "%s"', QMD_FILE, OUTPUT_DIR)
    }
    status <- system(cmd)
    if (!identical(status, 0L)) stop("Quarto rendering failed.")
  }
}