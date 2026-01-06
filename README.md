# Grundlegendes Praxisprojekt (WiSe 2025/26) – Frauen-Kinder und Arbeit

**Authors:** Shihan Hu, Yuechen Wang, Qiang Qin, Yuqin Huang

## Project Description
This project analyzes the relationship between demography and the labor market in Munich.
We focus on district-level patterns and time trends using publicly available data from the City of Munich.

---

## Quick Start (Reproduzierbarkeit)
This repository is designed so that the instructor can run the project with **one click**.

### Option A (recommended): Run via R
1. Open `Grund_Prak_2526.Rproj` in RStudio.
2. Open `run_all.R`.
3. Click **Source** (or run `source("run_all.R")` in the console).

A browser window will open and start the interactive Quarto document

**Note:** Because this is an interactive **Shiny** document, it runs via a local R session (**R-Session**).  
Please keep the R session running while using the web page.

### Option B: Run via Quarto CLI
From the project root directory:
```bash
quarto preview presentation.qmd

```


## ️ Repository Structure
```

Grund_Prak_2526/
├─ README.md
├─ run_all.R              # one-click entry point (R)
├─ presentation.qmd           # Quarto entry file (runtime: shiny)
├─ env_setup.R            # loads packages / sets paths (optional)
├─ customstyle.css
├─ images/
├─ data/
│  └─ raw/                # raw data (never modified)
├─ scripts/               # R scripts to generate .rds (pipeline)
└─ results/
   ├─ figures/            # cached objects/plots as .rds used by Quarto
   └─ geo/                # spatial data / shapefiles etc.

```

---

## Literate Programming System
We use **Quarto** (`.qmd`) as our literate programming system.  
Quarto allows us to combine code, text, and visualizations in one document and to render it into HTML or PDF reports.

---

##  Naming Conventions
| Category | Rule | Example |
|-----------|------|----------|
| Folders | lowercase + underscores | `data_raw`, `data_processed` |
| Files | descriptive English names | `clean_data.R`, `analyze_employment.R` |
| Reports | clear English names | `final_report.qmd` |

---


##  Tools
- R, RStudio  
- Quarto  
- Git & GitHub

---

##  References

