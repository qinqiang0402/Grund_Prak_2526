## Demographie & Arbeitsmarkt

## Author: ** Qiang Qin, Yuqin Huang, Yuechen Wang, Shihan Hu

##  Project Description
This project analyzes the relationship between population and the labor market in Munich.  
The goal is to explore trends in employment, unemployment, and demographic changes using publicly available statistical data.

---

## ️ Repository Structure
```
population_labor_market/
├── README.md
│
├── data/
│   ├── raw/                 # Raw data (never modified)
│   └── processed/           # Cleaned and processed data
├── scripts/                 # R scripts and functions
│   ├── utils.R
│   └── clean_data.R
├── analysis/
│   ├── report.qmd           # Quarto report
│   ├── presentation.qmd     # Quarto presentation
│   └── outliers.R           # some analysis that did not end up in the final report
├── results/
│   ├── figures/
│   └── tables/
├── report.pdf
└── presentation.html

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

