
# NMC-AEN-HW-NYC LL84 Building Energy Explorer
[![License: CC BY-NC 4.0](https://img.shields.io/badge/License-CC%20BY--NC%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc/4.0/)  
*A Shiny app for exploring New York City Local Law 84 energy benchmarking data*

---

## Purpose
The **NYC LL84 Building Energy Explorer** is an interactive Shiny application designed for urban energy analysts, building facility managers, and students to explore NYC building energy and water performance.  

It helps users:

- Visualize distributions of energy and water metrics  
- Compare buildings and identify trends  
- Prioritize buildings for energy efficiency improvements  

The app enables interpretation of publicly disclosed LL84 data **without coding**.

---

## Group Members

| Name        | Class | GitHub ID        |
|-------------|-------|-----------------|
| Anna N.     | 613   | `anna-nagurney` |
| Honglin W    | 613   | `229153013`   |
| Nabihi    | 613   | `github-id-3`   |

> Replace placeholders with actual names/IDs.

---

## App Elements

1. **Introduction Tab** – Overview of LL84 requirements, data sources, and purpose  
2. **Univariate Analysis Tab** – Histograms, boxplots, quartile summaries, and t-tests  
3. **Bivariate Analysis Tab** – Scatter plots, linear & non-linear smoothers, correlation and regression analysis  
4. **Data Table Tab** – Interactive table for filtering, sorting, and exporting building data  
5. **Filtering Controls** – Report year, property type, borough, building age, floor area, and optional data quality switches  

---

## Data Source

- **Dataset:** [NYC Building Energy and Water Data Disclosure (Local Law 84)](https://data.cityofnewyork.us/Environment/NYC-Building-Energy-and-Water-Data-Disclosure-for-/5zyy-y8am)  
- The app expects a CSV named `NYC_Building_Energy_and_Water_Data_Disclosure_for_Local_Law_84_(2022-Present)_20251027.csv` in the project root or `inst/extdata/`  
- Variables not limited to: Site EUI, Electricity Grid Usage, Gross Floor Area, Property Type, Borough, Year Built, Reporting Year  

---

## API Keys

No API keys are required; all data are publicly available.

---

## License

This project is licensed under **Creative Commons Attribution–NonCommercial 4.0 International (CC BY-NC 4.0)**:  

- Share, copy, redistribute, remix, and build upon the material  
- **Attribution** is required  
- **NonCommercial**: cannot be used for commercial purposes  

Full license: [https://creativecommons.org/licenses/by-nc/4.0/](https://creativecommons.org/licenses/by-nc/4.0/)

---

## How to Run the App

1. Install required packages:

```r
install.packages(c(
  "shiny", "tidyverse", "broom", "DT",
  "bslib", "thematic", "showtext"
))
```
2. Place NYC_Building_Energy_and_Water_Data_Disclosure_for_Local_Law_84_(2022-Present)_20251027.csv in the project root or inst/extdata/.

3. Run the app:
```r
shiny::runApp("app.R")
```

Citation

NYC LL84 Building Energy Explorer (2025). Developed for UConn BADM 413/613.
=======
# NMC-AEN-HW-LL84-EDA
Interactive EDA app for NYC LL84 Building Energy &amp; Water (2022–Present).
>>>>>>> 03b9ff3 (Add project files and ignore large LL84 data)
