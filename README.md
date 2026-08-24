# Melocactus-pachyacanthus
R code workflow (ESMs, CMIP6 climate projections, and disturbance overlays) for the conservation assessment of Melocactus pachyacanthus.

This repository contains all R scripts, data processing pipelines, and analytical workflows developed for modeling the bioclimatic suitability and anthropogenic disturbance overlays for the narrow-endemic cactus *Melocactus pachyacanthus* (Cactaceae).

### Repository Overview
- **Modeling Framework:** Ensembles of Small Models (ESMs) via `flexsdm` in R.
- **Algorithms:** GLM, GAM, SVM, and MaxEnt with repeated 3-fold cross-validation.
- **Scenarios:** Current baseline (1970–2000) and 2050 horizon CMIP6 projections (SSP2-4.5 and SSP5-8.5).
- **Disturbance Analysis:** Spatial overlays with MapBiomas Collection 8 Land Use & Land Cover (LULC) and Cumulative Fire (CF) time series.
- **Uncertainty Mapping:** Multivariate Environmental Similarity Surfaces (MESS) and pixel-b
