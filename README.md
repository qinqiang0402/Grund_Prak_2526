# 📊 Grundlegendes Praxisprojekt (WiSe 2025/26)
## Frauen, Kinder und Arbeit in München

![Project Status](https://img.shields.io/badge/Status-Active-brightgreen)
![R Version](https://img.shields.io/badge/R-%3E%3D4.2-blue)
![Tool](https://img.shields.io/badge/Render-Quarto-blueviolet)

> **Authors:** Shihan Hu, Yuechen Wang, Qiang Qin, Yuqin Huang

---

## 📝 Project Description

This project analyzes the relationship between **demography and the labor market** in Munich. We focus on district-level patterns and time trends using publicly available data from the *City of Munich*.

**Key Objectives:**
- Analyze spatial distribution of employment.
- Investigate correlations between family structure and labor participation.
- Visualize trends over time.

---

## 🚀 Quick Start (Reproducibility)

This repository is designed for **one-click reproducibility**.

### Option A: Run via R (Recommended) ⭐

1.  Open `Grund_Prak_2526.Rproj` in RStudio.
2.  Open the file `run_all.R`.
3.  Click the **Source** button (or run `source("run_all.R")` in the console).

> **⚠️ Important Note:**
> A browser window will open displaying the interactive **Shiny** document.
> Please **keep the R session running** in the background while interacting with the web page.

### Option B: Run via Quarto CLI
From the project root directory:
```bash
quarto preview presentation.qmd

```


## 📂 Repository Structure
```

Grund_Prak_2526/
├─ README.md
├─ run_all.R              # one-click entry point (R)
├─ presentation.qmd       # Quarto entry file
├─ env_setup.R            # loads packages / sets paths (optional)
├─ customstyle.css
├─ images/
├─ data/
│  └─ raw/                # raw data
├─ scripts/               # R scripts to generate .rds 
└─ results/
   ├─ figures/            # cached objects/plots as .rds used by Quarto
   └─ geo/                # spatial data / shapefiles etc.

```

---

## ✍️Literate Programming System
We use **Quarto** (`.qmd`) as our literate programming system.  
Quarto allows us to combine code, text, and visualizations in one document and to render it into HTML or PDF reports.

---

##  📏Naming Conventions
| Category | Rule | Example |
|-----------|------|----------|
| Folders | lowercase + underscores | `data_raw`, `data_processed` |
| Files | descriptive English names | `clean_data.R`, `analyze_employment.R` |
| Reports | clear English names | `final_report.qmd` |

---


##  🛠️Tools
- R, RStudio  
- Quarto  
- Git & GitHub

---
