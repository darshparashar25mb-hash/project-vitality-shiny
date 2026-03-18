# Project Vitality – PulsePoint AI Monte Carlo DCF (R Shiny App)

This repository contains an R Shiny dashboard built for the **Project Vitality** case on PulsePoint AI.  
It implements a 5‑year DCF and a Monte Carlo simulation of the implied share price using the case assumptions.

## Repository

- GitHub username: **Darsh123**
- Author: **Darsh Parashar**

## Features

- Interactive input panel for:
  - Revenue growth (Years 1–5)
  - EBIT margin, tax rate, D&A, CapEx, and ΔNWC (as % of revenue)
  - Capital structure (debt weight, beta, risk‑free rate, market risk premium, cost of debt)
  - Perpetuity growth rate, net debt, and shares outstanding
- Base‑case DCF tab:
  - 5‑year FCFF table
  - FCFF line chart
  - WACC breakdown
  - Enterprise value, equity value, and implied share price
- Monte Carlo tab:
  - Simulation of revenue growth, EBIT margin, beta, and perpetuity growth
  - Distribution of implied share prices with summary statistics (mean, median, percentiles)

## How to run locally

1. Install R and RStudio.
2. Install required packages:

   ```r
   install.packages(c("shiny", "dplyr", "ggplot2", "scales", "DT"))
