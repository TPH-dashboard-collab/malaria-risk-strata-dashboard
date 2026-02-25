# malaria-risk-strata-dashboard

## Project Overview

Development of an interactive R-Shiny dashboard that visualizes the movement of country regions/districts between malaria risk strata, comparing Business as Usual (BAU) versus National Strategic Plan (NSP) scenarios.

## Core Features

-   **Comparative Evaluation:** Dual-column layout comparing BAU vs. NSP across all modules to highlight the "net impact" of strategic interventions.
-   **Transition Analytics (Sankey):** Interactive flow diagrams visualizing the movement of districts across risk strata.
    -   **Toggle Feature:** Users can switch between **District Counts** (Administrative burden) and **Population Weights** (Public health burden).
-   **Synchronized Geospatial Mapping:** Dual Leaflet maps with synchronized auto-zoom. Features include:
    -   **Metadata Hovers:** Real-time calculation of baseline vs. projected PfPR and descriptive strata movement (eg."High to Moderate").
    -   **Synchronized Camera:** Navigation on one map (Zoom/Pan) is mirrored on the counterfactual view.
-   **Persistence Tables:** Bootstrap-styled alerts and tables identifying districts that remain in High or Moderate strata, including metrics for absolute PfPR change.
-   **Modern UI/UX:** Built with `bslib` using a card-based layout, sub-tab navigation (`navset_card_pill`), and full-screen toggles for all visualizations.
-   **Reactive & Optimized Filtering:**
    -   **Temporal:** Range-slider for custom year-on-year transitions.
    -   **Geographical:** Cascading filters (Region $\rightarrow$ District) for targeted sub-national deep dives.
    -   **Age Group:** Fully reactive selection (defaulting to 0-5).

## Technical Architecture

-   **Performance Optimization:** Non-reactive data loading for large CSV simulation outputs and Shapefiles (loaded statically to minimize Disk I/O).
-   **Data Engine:**
    -   **Seed Aggregation:** Collapses multiple simulation seeds into their arithmetic mean values per district/year.
    -   **Piecewise Stratification:** Dynamic recalculation of risk strata based on user-selected PfPR indicators and age groups.
-   **Spatial Logic:** Administrative boundaries (DHIS2-compliant) transformed to **WGS84 (EPSG:4326)** for `Leaflet` compatability.

## Risk Stratification Logic

Strata are dynamically recalculated from raw `prevalenceRate` outputs based on **PfPR₂₋₁₀** thresholds: - **Very Low:** ≤ 1.9% - **Low:** 1.9% – 3.3% - **Moderate:** 3.3% – 11% - **High:** \> 11%

## Directory Structure

-   `ui.R`: Side-by-side comparison framework using `bslib` cards and hierarchical inputs.
-   `server.R`: Orchestrates dual-plan data streams, synchronized map proxies, and reactive aggregation layers.
-   `functions.R`: Central analytical engine housing functions for Sankey flow generation, HTML hover formatting, and seed aggregation.

## Implementation Roadmap

-   [x] **Phase 1:** Data Architecture & UI Framework
-   [x] **Phase 2:** Core Visualization Development (Comparative Sankeys & Persistence)
-   [x] **Phase 3:** Geospatial Interactivity & Synchronized Comparison
-   [ ] **Phase 4:** Technical Optimization & Final Delivery (March 3rd Presentation Ready)
