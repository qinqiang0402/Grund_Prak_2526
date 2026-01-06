# run_all.R
# 目的：一键复现（Reproduzierbarkeit）——教授只需点一次 Run

# ========== 0) 参数区 ==========
REBUILD_RDS <- FALSE   # TRUE = 从脚本重建 rds；FALSE = 直接用仓库已有 rds
QMD_FILE    <- "test_pre.qmd"  # 你的 Quarto 入口文件
# OUTPUT_DIR <- "docs"         # 可选：把输出放到 docs/（做 GitHub Pages 用）
OUTPUT_DIR  <- NULL            # NULL = 输出在 qmd 同目录（最稳，不容易炸路径）
RUN_MODE <- "preview"  # "preview"（用于 Shiny） 或 "render"（只生成静态文件）

# 你截图里能确认存在的一些关键 rds（Sanity Check / 自检）
REQUIRED_RDS <- c(
  "results/figures/Kinderbetreuung/ki_dual_trend.rds",
  "results/figures/Kinderbetreuung/ki_korr_stadtteile_point_line_sw.rds",
  "results/figures/Kinderbetreuung/ki_point_korr_gesamt_sw.rds",
  "results/figures/Kinderbetreuung/ki_point_line_rot.rds",
  "results/figures/Kinderbetreuung/map_ki_2015.rds",
  "results/figures/Kinderbetreuung/map_sv_2015.rds"
)

# ========== 1) 确保在项目根目录运行（Arbeitsverzeichnis） ==========
# 目的：不依赖你电脑的绝对路径；教授在哪个目录点 Run 都能对
if (requireNamespace("here", quietly = TRUE)) {
  root <- here::here()
} else {
  # fallback：假设 run_all.R 在项目根目录
  root <- normalizePath(getwd())
}
setwd(root)
message("Project root: ", root)

# ========== 2) （可选）恢复依赖（Abhängigkeiten / renv） ==========
# 目的：避免“你能跑、教授不能跑”的包版本问题
if (file.exists("renv.lock") && requireNamespace("renv", quietly = TRUE)) {
  message("renv.lock found. (Optional) You can run renv::restore() if needed.")
  # 交付时你可以选择启用这一行：
  # renv::restore(prompt = FALSE)
}

# ========== 3) （可选）重建 rds（Pipeline） ==========
# 目的：让结果可追溯（Nachvollziehbarkeit），也支持从 raw data 重建
if (isTRUE(REBUILD_RDS)) {
  pipeline <- "scripts/00_run_pipeline.R"
  if (!file.exists(pipeline)) {
    stop("REBUILD_RDS=TRUE but missing: ", pipeline)
  }
  message("Rebuilding RDS via: ", pipeline)
  source(pipeline, local = new.env())
}

# ========== 4) 自检：关键 rds 是否存在（Sanity Check） ==========
# 目的：让错误在渲染前就暴露，报错信息清晰
missing_rds <- REQUIRED_RDS[!file.exists(REQUIRED_RDS)]
if (length(missing_rds) > 0) {
  stop(
    "Missing required .rds files:\n- ",
    paste(missing_rds, collapse = "\n- "),
    "\n\nTip: set REBUILD_RDS <- TRUE to regenerate them."
  )
}
message("RDS check passed.")

# ========== 5) 渲染/预览 Quarto ==========
if (!file.exists(QMD_FILE)) stop("Missing Quarto file: ", QMD_FILE)

if (RUN_MODE == "preview") {
  # 目的：启动本地服务，让 runtime: shiny 真正跑起来
  if (requireNamespace("quarto", quietly = TRUE)) {
    quarto::quarto_preview(QMD_FILE)
  } else {
    system(sprintf('quarto preview "%s"', QMD_FILE))
  }
} else {
  # 目的：只生成输出文件（适合纯静态文档）
  if (requireNamespace("quarto", quietly = TRUE)) {
    if (is.null(OUTPUT_DIR)) quarto::quarto_render(QMD_FILE)
    else quarto::quarto_render(QMD_FILE, output_dir = OUTPUT_DIR)
  } else {
    cmd <- if (is.null(OUTPUT_DIR)) sprintf('quarto render "%s"', QMD_FILE)
    else sprintf('quarto render "%s" --output-dir "%s"', QMD_FILE, OUTPUT_DIR)
    system(cmd)
  }
}