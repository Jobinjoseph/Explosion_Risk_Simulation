# 💥 Explosion Risk Model

This R project calculates explosion risks in bunker and tunnel structures using formulas from the **TLM ** guidelines. It automates several engineering steps, including overpressure estimation, duration of impact, and wall friction adjustments.

---

## 📁 Structure

- `main_script.R` – Main script to run the calculation
- `WA_Lager_functions.R` – Contains all modular functions used in the calculation
- `data/example_input.xlsx` – Example Excel input file (can be replaced with your own)
- `packages.R` – Script to install and load required R packages
- `requirements.txt` – Informational list of required R packages

---

## 🚀 How to Run

1. Clone the repository:
   
   git clone https://github.com/Jobinjoseph/explosion-risk-model.git
   cd explosion-risk-model

2. Open R or RStudio and run:
source("packages.R")           # Install and load required packages
source("WA_Lager_functions.R") # Load all functions
source("main_script.R")        # Run the main workflow
3. Results will be printed in the console or written to file, depending on the script logic.
4. Input Format
The input is expected as an Excel file (.xlsx) with fields such as:

Fk, Fo, Ls, ds, Klotz, Q, Vk, Lk, dB, S, W, etc.

Use the example in data/example_input.xlsx as a guide.
5. Dependencies
Install all dependencies by running:

source("packages.R")
Manual list available in requirements.txt.
6. Disclaimer
This software implements engineering models based on official guidelines (TLM) and is intended for technical users. Always validate output against engineering judgment.
7. License
MIT License – free to use, modify, and distribute with attribution.
Author 
Jobin Joseph
