# malaria-risk-strata-dashboard
Development of an interactive R-Shiny dashboard that visualizes the movement of country regions between malaria risk strata, comparing Business as Usual (BAU) versus National Strategic Plan (NSP) scenarios. 

# Tanzania Malaria Risk Strata Dashboard - Phase 1

## Project Goal
To develop a functional, interactive R-Shiny dashboard visualizing the movement of Tanzanian districts between malaria risk strata. This tool compares Business as Usual (BAU) vs. National Strategic Plan (NSP) scenarios.

## Phase 1: Data Architecture & UI Framework
This initial release focuses on the foundational data processing and user interface structure:
- **Recalculated Strata:** The `risk_stratum` column is dynamically generated from raw `prevalenceRate` using PfPR₂₋₁₀ thresholds.
- **Seed Aggregation:** Logic implemented to average simulation outputs across multiple seeds for stability.
- **Hierarchical Filtering:** Responsive menus for Intervention Plan, Year Range, Region (Admin 1), and District (Admin 2).
- **Spatial Alignment:** Successful integration of DHIS2-compliant shapefiles with simulation data.

## Data Logic
- **Indicator:** prevalenceRate (PfPR₂₋₁₀).
- **Thresholds:**
    - Very Low: ≤ 1.9%
    - Low: 1.9% - 3.3%
    - Moderate: 3.3% - 11%
    - High: > 11%

## Repository Structure
- `ui.R`: Dashboard layout and input controls.
- `server.R`: Reactive logic and spatial join operations.
- `functions.R`: Core analytical functions (Colors, Stratification, Aggregation).