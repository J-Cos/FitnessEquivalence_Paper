# Fitness equivalence and deviations from the Ecological Equation of State

Code for *Disturbance-induced reductions in tree fitness equivalence explain systematic deviations from
macroecological theory*, submitted to *Journal of Ecology*.

The pipeline computes plot macrostates (`S`, `N`, `E`, `B`) for 224 Amazonian forest plots using stem basal
area for metabolic rate and the `BIOMASS` package (Réjou-Méchain et al. 2017; Chave et al. 2014) for
aboveground biomass, fits the Ecological Equation of State (EEoS; exact METE sum-and-integral), and decomposes
signed EEoS deviation (`dev = ln(B / B_pred)`) exactly into functional wood-density (`w_rho`), site bioclimatic
height-allometry (`w_H`), and pure MaxEnt size-structure (`r_size`) channels. It then tests:
- **H1:** whether landscape forest loss drives systematic EEoS deviation (signed deviation primary; absolute
  deviation retained as secondary);
- **H2a–c:** whether three distinct measures of tree fitness (non-)equivalence mediate that effect via the
  5-step Model Selection Approach to Structural Equation Modelling (MSA-SEM; Garrido et al. 2022):
  - **H2a:** Negative Binomial CATS local trait-selection deviance $R^2$ (Warton, Shipley & Hastie 2015; with
    classical Poisson CATS and 3-trait sensitivity checks);
  - **H2b:** per-plot relative stem abundance of regional non-neutral taxa;
  - **H2c:** neutral random-sampling model fit ($R^2$) across non-overlapping 5-plot subgroups;
- **Channel decomposition:** how H1 and H2a–c (including a wood-density-free CATS non-circularity check)
  partition across the exact additive channels of signed EEoS deviation (`dev = w_rho + w_H + r_size`).

## Layout

This repository is the `Code/` folder of the project. Scripts are run from the **project root**, the folder
that contains `Code/`, `Data/`, `Outputs/` and `Figures/`:

```
project_root/
├── Code/       this repository: numbered scripts (01–12), Functions/, tests/, run_all.R
├── Data/       input data (not version controlled; see "Data")
├── Outputs/    generated CSV/RDS tables, plus logs/ (created by the pipeline)
└── Figures/    generated publication and diagnostic figures (created by the pipeline)
```

Only scripts, this README and the environment lockfiles are tracked (see `.gitignore`).

## Requirements

- **R** 4.3+. Restore recorded package versions with `renv::restore(lockfile = "Code/renv.lock")`.
  Main packages: `tidyverse`, `BIOMASS`, `proj4`, `lavaan`, `patchwork`, `cowplot`, `ggforce`, `ggsci`,
  `boot`, `lmtest`, `sandwich`, `MASS`, `robustbase`, `quantreg`, `minpack.lm`, `Hmisc`, `bbmle`, `terra`,
  `sf`, `rnaturalearth`, `tidyterra`, `testthat`.
  - `BIOMASS::computeAGB()` caches bioclimatic rasters locally on first run (controlled via
    `options(BIOMASS.cache = ...)` in `Functions/config.R`).
- **Python** 3.12 with `numpy` 1.26.4, `scipy` 1.14.1, `pandas` 2.2.2. Create it with
  `conda env create -f Code/environment.yml`.
  - `run_all.R` automatically locates the `EEOS-env` conda environment (or uses the interpreter specified
    in the `EEOS_PYTHON` environment variable, falling back to `python3` on `PATH`).
  - It sets `PYTHONNOUSERSITE=1` so user-site packages cannot shadow pinned environment versions.

## Data

Place the following in `Data/`. All are publicly available from the sources cited in the manuscript.

| File | Content |
| :--- | :--- |
| `comm.csv` | Pinho et al. stems (Balbina, PGM, STM): `Region`, `Region_Plot`, `Binomial_correct`, `Genus`, `family`, `DBH` (cm) |
| `traits.csv` | Pinho et al. species traits: `Binomial_correct`, `WD`, `lnSM`, `LMA`, `Hmax`, `DS` |
| `Jurua_Flora_FullDB.csv` | Scabin et al. Juruá stems and traits (`parcel`, `species`, `genus`, `family`, `DBH`, `WD`, `SM`, `LMA`) |
| `env.csv` | Plot environment incl. coordinates (`X`, `Y`) and forest cover in a 2 km radius (`fc2000`). Juruá plots are absent (intact forest; forest cover set to 1, coordinates set to the regional centroid in `Functions/config.R`). |
| `Ecoregions2017/` | Ecoregions 2017 shapefile (map in Figure 1A) |
| `Harte2022_data_statevariables.csv` | Harte et al. (2022) empirical state-variable benchmark dataset (used by `pytest`) |

## Running

```bash
Rscript Code/run_all.R            # full 01–12 pipeline; logs in Outputs/logs/
Rscript Code/run_all.R 04 10      # only selected steps, by script number
```

Each step runs in a fresh process and the pipeline stops at the first failure. R session info and Python
package versions are written to `Outputs/logs/`.

| Step | Script | Main outputs (`Outputs/` & `Figures/`) | Manuscript item |
| :--- | :--- | :--- | :--- |
| 01 | `01_CombinePinhoJuruaData.R` | `comm.csv`, `traits.csv` | Data harmonisation |
| 02 | `02_GetEcosystemMacrostates.R` | `Macrostates.csv` (`S`, `N`, `E`, `B`, 3-run `BIOMASS` totals `B_rho`, `B_size`, and Monte Carlo `B_cv`) | Macrostate methods |
| 02b | `02b_MakeMacrostateScatterplots.R` | `FigureS1.png`, `Figure_BiomassCV.png` | Figs. S1–S2 |
| 03 | `03_FitEEOS.py` | `EEOSPreds.csv` (`B_pred`, `dev`, `absdev`, `w_rho`, `w_H`, `r_size`, METE constraint checks) | Exact METE EEoS solver |
| 04 | `04_FitSimpleEeosDevRegression.R` | `H1_Signed_*` (primary), `H1_Absolute_*` (retained), `H1_AllometricBenchmark.csv`; `Figure_H1.png`, `Figure_H1_absolutesupplement.png`, `Figures/diagnostics/H1_*_QQ.png` | H1; Fig. 1, Fig. S3 |
| 05 | `05_FitCatsRegressions.R` | `CatsOutput.csv`, `CatsSpeciesPredictions.RDS` | H2a fitness-equivalence metric |
| 06 | `06_FitRegionalSloanNonNeutral.R` | `PlotSubsets.RDS`, `SloanOutput.RDS`, `SloanExampleFits.RDS`, `SloanR2_ByRegion.csv`, `SloanRegionalAverageFit.csv`, `NonNeutralTaxa*.csv`, `NonNeutralProportions.csv`, `NonNeutralTraitTests.csv`, `NonNeutralTraitZScores.csv` | H2b metric & trait departures; Table S1 |
| 07 | `07_FitSubgroupSloans.R` | `NonOverlappingOrderedPlotSubsets.RDS`, `ForestCoverSubgroupSloanObjects.RDS`, `ForestCoverSubgroupSloanFits.csv` | H2c subgroup neutral-fit metric |
| 08 | `08_PlotFitnessEquivalenceMeasures.R` | `Figure_FitnessEquivalence.png`, `Figure_RegionalSloan_supplement.png`, `Figure_traitsOfNonneutrals.png`, `Figure_sloansubsets_supplement.png` | Fig. 2, Figs. S4–S6 |
| 09 | `09_FitMediationSEMs.R` | `H2{a,b,c}_{Signed,Absolute}_*.csv`, `H2a_{3trait_*,Poisson_*}_*.csv`, `H2_Mediation_Summary.csv` | H2a–c Garrido 8-model MSA-SEMs |
| 10 | `10_PlotMediationSEMs.R` | `Figure_H2_SEMs.png`, `Figure_H2_SEMs_absolutesupplement.png` | Fig. 3, Fig. S7 |
| 11 | `11_FitBiomassDecomposition.R` | `BiomassDecomposition_{NoWD_Cats,OLS,SEM_Summary,SEM_MMI}.csv` | 3-run channel decomposition & WD-free CATS check |
| 12 | `12_PlotBiomassDecomposition.R` | `Figure_BiomassDecomposition.png` | Fig. 4 |

### Macrostate formulation and exact 3-run `BIOMASS` decomposition

All macrostates are defined once in `Functions/macrostates.R` relative to a reference tree at the census
threshold (`DBH_min = 10 cm`) with fixed study-wide stem-mean wood density (`ρ_ref`) and pooled study-mean
bioclimatic stress (`E_clim,ref`):
- **Metabolic rate:** `ε_i = (DBH_i / DBH_min)²`, summed per plot to `E = ∑ ε_i`.
- **Biomass:** `b_i = AGB_i / AGB_ref`, summed per plot to `B = ∑ b_i`, where `AGB_i` is computed by
  `BIOMASS::computeAGB()` from stem `DBH` (cm), species wood density `WD` (`Outputs/traits.csv`), and plot
  coordinates `(X, Y)`.

To decompose deviations without analytic approximations, `computeAGB()` is evaluated three times, switching
off one source of variation at a time:

| Run | Plot total | Wood density | Height / bioclimate |
| :--- | :--- | :--- | :--- |
| `agb` | `B` | species `WD` (`Outputs/traits.csv`) | plot coordinates `(X, Y)` |
| `agb_rho` | `B_rho` | fixed reference `ρ_ref` | plot coordinates `(X, Y)` |
| `agb_size` | `B_size` | fixed reference `ρ_ref` | pooled study-wide `E_clim,ref` |

By the exact telescoping product `B / B_pred = (B / B_rho) × (B_rho / B_size) × (B_size / B_pred)`, taking
natural logarithms splits signed EEoS deviation `dev = ln(B / B_pred)` into three exact additive channels:
- `w_rho = ln(B / B_rho)` — functional wood-density replacement channel;
- `w_H = ln(B_rho / B_size)` — site bioclimatic height-allometry channel;
- `r_size = ln(B_size / B_pred)` — pure MaxEnt diameter/size-structure channel.

## Code organisation

| File | Contents |
| :--- | :--- |
| `Functions/config.R` | Constants (`REGIONS`, `DBH_MIN_CM`, `BIOMASS` cache path, subsample/subgroup sizes), project-root check, paths, Python lookup |
| `Functions/data_prep.R` | Raw stem/trait harmonisation (`01`) and validated loading/joining of plot and subgroup analysis tables |
| `Functions/macrostates.R` | Organism-level `ε_i`, 3-run `BIOMASS` `b_i`, plot macrostates (`S`, `N`, `E`, `B`, `B_rho`, `B_size`), and Monte Carlo `B_cv` |
| `Functions/eeos_functions.py` | METE Lagrange multipliers (`β`, `λ₂`), exact constraint checks, and exact EEoS biomass prediction (`B_pred`) |
| `Functions/regression_helpers.R` | Allometric uncertainty benchmark, H1 regression + ANCOVA/HC3/WLS/robust/bootstrap suite, and exact additive channel OLS decomposition |
| `Functions/cats_helpers.R` | Boundary-safe Negative Binomial (Warton et al. 2015) and Poisson (Shipley et al. 2006) CATS regressions with metacommunity offset |
| `Functions/sloan_helpers.R` | Sloan neutral and binomial/Poisson random-sampling models (after Burns et al. 2016), non-neutral classification, and regional pool trait z-tests |
| `Functions/sem_helpers.R` | Balanced $2^3 = 8$-model Garrido et al. (2022) MSA-SEM suite (`0*` focal constraints on shared `6×6` `S`), AICc multimodel inference, and bootstrap indirect effects |
| `Functions/plot_helpers.R` | Shared `theme_boxed()`, marginal common-slope predictions, scatter/boxplot/mediation panels, Garrido MMI SEM pathway diagrams, and mediated-share stacked bars |

## Tests

```bash
Rscript -e 'testthat::test_dir("Code/tests/testthat")'
PYTHONNOUSERSITE=1 conda run -n EEOS-env python -m pytest Code/tests -q
```

- **Unit tests** (`test-cats-helpers.R`, `test-data-prep.R`, `test-macrostates.R`, `test-plot-helpers.R`,
  `test-regression-helpers.R`, `test-sem-helpers.R`, `test-sloan.R`, and `test_eeos_functions.py`) run on
  compact synthetic/benchmark fixtures without requiring pipeline outputs.
- **`test-integration.R`** validates all end-to-end `Outputs/` tables, exact mathematical additivity identities,
  and all 11 publication figures in `Figures/` (skipping cleanly if `Outputs/` has not yet been generated).
