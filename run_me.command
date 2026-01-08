#!/bin/bash

# Move to the directory where this script is located
cd "$(dirname "$0")"

echo "=========================================================="
echo "Initializing R Project Environment..."
echo "The report will open in your browser automatically."
echo "PLEASE DO NOT CLOSE THIS WINDOW WHILE USING THE REPORT."
echo "=========================================================="

# Execute the R script
Rscript main.R

# Keep the terminal open if the process stops unexpectedly
echo ""
echo "----------------------------------------------------------"
echo "Server process has ended."
read -n 1 -p "Press any key to exit and close this window..."