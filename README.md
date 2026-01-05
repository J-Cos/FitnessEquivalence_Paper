# Fitness Equivalence Paper Codebase

This repository contains the code and data for the Williams et al. analysis of fitness equivalence in tropical forests.

## Directory Structure

- **Code/**: R and Python scripts for analysis.
- **Data/**: Input datasets (CSV, Shapefiles).
- **Outputs/**: Generated results and figures.
- **Figures/**: Final figures for the manuscript.

## Setup

### Prerequisites

- **R**: Version 4.0 or higher recommended.
- **Python**: Version 3.8 or higher recommended.

### Dependencies

Common R and Python packages as detailed in scripts.

### Data Access

All data files are publicly available at the links provided in the main text. Specicifically, download the following files from these links and place this in the `Data/` directory:

- **comm.csv**: Community matrix (Site x Species abundance).
    - `Region`: Sampling region.
    - `Region_Plot`: Unique plot identifier.
    - `Binomial_correct`: Corrected species binomial name.
    - `Genus`, `family`: Taxonomic classification.
    - `DBH`: Diameter at Breast Height.
- **traits.csv**: Functional traits data.
    - `Binomial_correct`: Species name (key to `comm.csv`).
    - `WD`: Wood Density.
    - `LMA`: Leaf Mass per Area.
    - `SM`: Seed Mass.
- **env.csv**: Environmental variables for each plot.
- **Jurua_Flora_FullDB.csv**: Raw flora database for Jurua region.
- **Ecoregions2017/**: Shapefiles for ecoregions.

## Running the Analysis

The scripts are numbered to indicate the order of execution:

1.  `01_CombinePinho&JuaraData.R`: Merges datasets.
2.  `02_GetEcosystemMacrostates.R`: Calculates ecosystem macrostates.
3.  `02b_MakeMacrostateScatterplots.r`: Visualizes macrostates.
4.  `03_FitEEOS.py`: Fits the Ecological Equation of State (EEOS) model.
    *   Calls: `Functions/eeos_functions.py`
5.  `04_FitSimpleEeosDevRegression.r`: Regresses EEOS deviations and makes figure 1
6.  `05_FitCatsRegressions.r`: Fits categorical regressions.
7.  `06_SEMs_CatsRegressions.r`: Structural Equation Models for categorical regressions and figure 2
8.  `07_getPlotSubsets.r`: Subsets plots.
9.  `08_FitOverallSloanAndExtractNonneutralAbundances.r`: Fits Sloan neutral model.
    *   Calls: `Functions/runPermutationTest().r`
    *   Calls: `Functions/sncm.fit().R`
10. `08b_TestNonNeutralTraitDifferences.r`: Tests trait differences.
11. `09_SEMs_NonNeutralAbundances.r`: SEMs for non-neutral abundances and figure 3.
12. `10_FitForestCoverSubgroupSloans.r`: Fits Sloan model for forest cover subgroups.
    *   Calls: `Functions/sncm.fit().R`
13. `11_SEMs_ForestCoverSubGrouSLoans.r`: SEMs for forest cover subgroups and figure 4.
