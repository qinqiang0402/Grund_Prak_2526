@echo off
:: Change directory to the script's location (Crucial for relative paths!)
cd /d "%~dp0"

echo ==========================================================
echo Initializing R Project Environment for Windows...
echo The report will open in your browser automatically.
echo PLEASE DO NOT CLOSE THIS WINDOW WHILE USING THE REPORT.
echo ==========================================================

:: 1. Check if Rscript is available in the system PATH
where Rscript >nul 2>nul
if %errorlevel% neq 0 (
    echo.
    echo ----------------------------------------------------------
    echo [ERROR] R is not found on this computer!
    echo.
    echo Possible reasons:
    echo 1. R is not installed.
    echo 2. R is installed but not added to the System PATH.
    echo.
    echo Please install R from: https://cloud.r-project.org/
    echo ----------------------------------------------------------
    echo.
    pause
    exit /b
)

:: 2. Execute the R script
Rscript main.R

:: 3. Keep window open when finished or if error occurs
echo.
echo ----------------------------------------------------------
echo Server process has ended.
pause