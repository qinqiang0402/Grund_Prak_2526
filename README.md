# 📊 Grundlegendes Praxisprojekt (WiSe 2025/26)
## **Frauen, Kinder und Arbeit**  -      **Frauenbeschäftigung in München**

## **Authors:** Qiang Qin, Yuqin Huang, Yuechen Wang, Shihan Hu

---

## 📝 Project Description

This project analyzes the relationship between **demography and the labor market** in Munich. We focus on district-level patterns and time trends using publicly available data from the *City of Munich*.

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
3. In the R Console, type the following command and press Enter:

```
quarto::quarto_serve("presentation.qmd")
```    
    
4.  An interactive browser window will open automatically.

> **⚠️ Important Note:**
> A browser window will open displaying the interactive **Shiny** document.
> Please **KEEP the Terminal window running** in the background. Closing the terminal will terminate the Shiny server.

---

## 📂 Repository Structure
```text
Grund_Prak_2526/
├─ README.md
├─ run_me.command         # One-click entry point (Mac)
├─ run_me_windows.bat     # One-click entry point (Windows)
├─ main.R                 # Main logic: checks packages & launches Quarto
├─ presentation.qmd       # Quarto entry file (Interactive Shiny)
├─ customstyle.css        # Custom styling for the report
├─ images/                # Static images used in the report
├─ data/                  # Raw data
├─ scripts/               # R scripts used for data generation
└─ results/
   ├─ figures/            # Cached .rds objects used by Quarto
   └─ geo/                # Munich city map for Shiny
```
---

## 💡 Data & Plot Generation Note
To ensure high performance and clean code, this project follows a modular structure:

Logic: The original R code used to process data and create visualizations is located in the scripts/ folder.

Storage: These scripts save the finalized plot objects as .rds files into results/figures/.

Rendering: The main report (presentation.qmd) does not regenerate plots from scratch; instead, it imports the pre-rendered .rds objects for a faster and more stable user experience.



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
