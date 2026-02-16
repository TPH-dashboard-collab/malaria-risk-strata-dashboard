# malaria-risk-strata-dashboard

## Project Overview

Development of an interactive R-Shiny dashboard that visualizes the movement of country regions/districts between malaria risk strata, comparing Business as Usual (BAU) versus National Strategic Plan (NSP) scenarios.


## Core Features

-   **Comparative Evaluation:** Side-by-side layout for BAU vs. NSP across all modules to highlight the "net impact" of strategic interventions.
-   **Transition Analytics (Sankey):** Interactive flow diagrams visualizing the movement of districts across risk levels.
-   **Synchronized Geospatial Mapping:** Dual Leaflet maps with basic hover features.
-   **Persistence Tables:** Automated identification of districts that remain in High or Moderate strata despite interventions in rudimentary table.
-   **Modern UI/UX:** Built with `bslib` using a card-based layout, sub-tab navigation (`navset_card_pill`), and full-screen toggles for all visualizations.
-   **Reactive & Optimized Filtering:**
    -   **Temporal:** Range-slider for custom year-on-year transitions.
    -   **Geographical:** Cascading filters (Region $\rightarrow$ District) for targeted sub-national deep dives.

## Technical Architecture

-   **Seed Aggregation:** Collapses multiple simulation seeds into their arithmetic mean values.
-   **Spatial Logic:** Administrative boundaries (DHIS2-compliant) transformed to **WGS84 (EPSG:4326)** for `Leaflet` compatability.

## Risk Stratification Logic

Strata are dynamically recalculated from raw `prevalenceRate` outputs based on **PfPR₂₋₁₀** thresholds: - **Very Low:** ≤ 1.9% - **Low:** 1.9% – 3.3% - **Moderate:** 3.3% – 11% - **High:** \> 11%

## Core Directory Scripts

-   `ui.R`: Dashboard layout and inputs.
-   `server.R`: Data reactivity, filtering logic, and visualizations.
-   `functions.R`: Currently holds functions for aggregation, risk calculation and sankey generation.

## Next steps

-   Refine reactivity and filtering logic.
-   Improve Geospatial Interactivity With detailed hover features and auto-zoom.
-   Improve table features, with alert box.
