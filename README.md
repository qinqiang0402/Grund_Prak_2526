# 📊 Grundlegendes Praxisprojekt (WiSe 2025/26)
## **Frauen, Kinder und Arbeit**  -      **Frauenbeschäftigung in München**

## **Authors:** Qiang Qin, Yuqin Huang, Yuechen Wang, Shihan Hu

---

## 📝 Project Description

This project analyzes the relationship between **demography and the labor market** in Munich. We focus on district-level patterns and time trends using publicly available data from the *City of Munich*.

[![Summary PDF](https://img.shields.io/badge/Dokumentation-Executive--Summary-blue?style=flat-square&logo=adobeacrobatreader&logoColor=white)](./Executive_Summary.pdf)


[![KI-Dokumentation](https://img.shields.io/badge/Dokumentation-KI--Dokumentation-green?style=flat-square&logo=openai&logoColor=white)](./KI_Dokumentation.pdf)


---

## 🚀 Quick Start (One-Click Execution)

This repository is designed for **one-click reproducibility** across different operating systems.

### 1. Prerequisites
Before running, please ensure you have the following installed:
* **R**: [Download from CRAN](https://cran.r-project.org/)
* **Quarto CLI**: [Download from Quarto.org](https://quarto.org/docs/get-started/) *(Required for rendering the report)*

### 2. How to Run
Depending on your operating system, follow the steps below:

#### **🍎 For Mac Users**
1.  Locate `run_me.command` in the project root.
2.  **Right-click** and select **Open** (or double-click).
3.  The terminal will automatically install dependencies and launch the browser.

#### **🪟 For Windows Users**
1.  Locate `run_me_windows.bat` in the project root.
2.  **Double-click** the file to execute.

#### **Manual Run via RStudio (Fallback)**

If the scripts above fail due to environment path issues, follow these steps:

1. Open `Grund_Prak_2526.Rproj` in RStudio.
2. Open `main.R` and run all lines (this installs missing packages).
3. This will trigger `renv::restore()` and launch the Quarto server.


> **⚠️ Important Note:**
> A browser window will open displaying the interactive **Shiny** document.
> Please **KEEP the Terminal window running** in the background. Closing the terminal will terminate the Shiny server.

---

## 📂 Repository Structure
```text
Grund_Prak_2526/
├─ README.md                           
├─ Executive_Summary.pdf               # Scientific summary (1 DIN A4) 
├─ KI_Dokumentation.pdf                # AI usage disclosure
├─ run_me.command                      # One-click entry point (Mac)
├─ run_me_windows.bat                  # One-click entry point (Windows)
├─ main.R                              # Main logic: checks packages & launches Quarto
├─ presentation.qmd                    # Final Report (v2.0) 
├─ presentation_original_version.qmd   # Original presentation version (v1.0) 
├─ customstyle.css                     # Custom styling for the report
├─ Grund_Prak_2526.Rproj               # RStudio project file
├─ images/                             # Static images used in the report
├─ data/                               # Raw datasets
├─ scripts/                            # R scripts used for data generation 
└─ results/                            # Generated outputs
   └─ geo/                             # Geographic data for Munich map visualizations
```
---

## 💡 Data & Plot Generation Strategy

To ensure **full reproducibility** and code transparency, this project adopts a **dynamic generation workflow**:

* **Modular Logic**: Data processing and plotting logic are encapsulated as functions within the `scripts/` folder.
* **Dynamic Rendering**: The main report (`presentation.qmd`) generates all tables, maps, and plots **on-the-fly** from raw datasets during runtime.
* **Zero Caching**: To guarantee consistency and avoid environment conflicts, no intermediate `.rds` files are used or stored. Every visual is computed fresh from the source.

---

## ✍️Literate Programming System
We use **Quarto** (`.qmd`) as our literate programming system.  
Quarto allows us to combine code, text, and visualizations in one document and to render it into HTML or PDF reports.

---


##  🛠️Tools
- R, RStudio  
- Quarto  
- Git & GitHub

---
