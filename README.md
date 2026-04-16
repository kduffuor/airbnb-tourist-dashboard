[![R](https://img.shields.io/badge/R-4.0+-blue)](https://www.r-project.org/)

# Airbnb Tourist Analytics Dashboard
An interactive R Shiny dashboard analyzing Airbnb listings across six global cities, integrated with cost of living data to help travelers make informed accommodation and budget decisions.

🔗 **Live Demo:** [kduffuor.shinyapps.io/airbnb-tourist-analytics-dashboard](https://kduffuor.shinyapps.io/airbnb-tourist-analytics-dashboard/)
[![ShinyApps.io](https://img.shields.io/badge/ShinyApps.io-Live-green)](https://kduffuor.shinyapps.io/airbnb-tourist-analytics-dashboard)

## Cities Covered
| City | Country |
|------|---------|
| Austin | United States |
| Bangkok | Thailand |
| Buenos Aires | Argentina |
| Cape Town | South Africa |
| Istanbul | Turkey |
| Melbourne | Australia |

## Features
- **Interactive Map** — Leaflet-powered map with clustered listing markers and detailed popups
- **Pricing Analysis** — Average nightly rates by city and room type, price distribution, and price vs. review scatter plot
- **Availability Patterns** — Yearly availability heatmap across cities and room types
- **Value Assessment** — Price-per-review ratio to identify best value destinations
- **Budget Planning** — Total trip cost calculator for 3, 5, and 7-day stays including accommodation and daily living expenses
- **Dynamic Filtering** — All charts update in real time based on city, room type, price range, and review count filters

## Tech Stack
- [R Shiny](https://shiny.posit.co/) — Dashboard framework
- [Leaflet](https://rstudio.github.io/leaflet/) — Interactive mapping
- [Plotly](https://plotly.com/r/) — Dynamic visualizations
- [DT](https://rstudio.github.io/DT/) — Interactive data tables
- [Tidyverse](https://www.tidyverse.org/) — Data processing

## Run Locally
**1. Clone the repository**
```bash
git clone https://github.com/kduffuor/airbnb-tourist-dashboard.git
cd airbnb-tourist-dashboard
```

**2. Install required R packages**
```r
install.packages(c("shiny", "shinydashboard", "tidyverse", "plotly", 
                   "DT", "viridis", "leaflet", "leaflet.extras"))
```

**3. Launch the app**
```r
shiny::runApp()
```

## Data Sources
- **Airbnb Listings** — Public dataset with property details, pricing, reviews, and availability
- **Cost of Living** — Daily budget estimates sourced from Numbeo

## Team
- [Kwabena Duffuor Asante](https://kduffuor.github.io) — Team Lead
- Audrey Appraku
- Usha Rani Nallaparaju
- Gifty Afful