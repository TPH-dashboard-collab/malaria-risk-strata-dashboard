# malaria-risk-strata-dashboard

## Project Overview
Development of an interactive R-Shiny dashboard that visualizes the movement of country regions/districts between malaria risk strata, comparing Business as Usual (BAU) versus National Strategic Plan (NSP) scenarios. 

This repository currently reflects **Phase1: Data Architecture & UI Framework**.

## Core Features
- **Automated Stratification:** Recalculates risk levels from raw `prevalenceRate` outputs based on current available thresholds.
- **Seed Aggregation:** Collapses multiple simulation seeds into a  mean value per district.
- **Cascading Filters:** Users can filter by Intervention Plan and Year. The District (Admin 2) menu dynamically populates based on the selected Region (Admin 1).
- **Spatial Integration:** Simulation data is joined with DHIS2 shapefiles, transformed  to the WGS84 coordinate system for leaflet compatabilty.

## Key Data Info
Strata are calculated using the **PfPR₂₋₁₀ (Prevalence Rate)** indicator:
- **Very Low:** ≤ 1.9%
- **Low:** 1.9% – 3.3%
- **Moderate:** 3.3% – 11%
- **High:** > 11%


## Core Directory Scripts
- `ui.R`: Dashboard layout and inputs.
- `server.R`: Data reactivity, filtering logic, and preliminary visualizations.
- `functions.R`: Currently holds functions for aggregation and risk calculation.


## Next steps
-  Core Visualization Development (Sankey & Persistence table)
-  Improved Geospatial Interactivity
