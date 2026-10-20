# Fitness equivalence and deviations from the Ecological Equation of State

Code for Williams et al., *Stressor-induced reductions in tree fitness equivalence explain systematic
deviations from macroecological theory* (Journal of Ecology, JEcol-2026-0304).

The pipeline computes plot macrostates (S, N, E, B) for 224 Amazonian forest plots and fits the Ecological
Equation of State (EEoS; exact METE sum-and-integral). It then tests:
- **H1:** whether forest loss increases EEoS deviation;
- **H2a–c:** whether three measures of fitness (non-)equivalence mediate that effect:
  - **H2a:** CATS trait-selection R²;
  - **H2b:** relative abundance of non-neutral taxa;
  - **H2c:** neutral random-sampling model fit.

## Layout

This repository is the `Code/` folder of the project. Scripts are run from the **project root**, the folder
that contains `Code/`, `Data/`, `Outputs/` and `Figures/`:

```
project_root/
├── Code/       this repository: numbered scripts, Functions/, tests/, run_all.R
├── Data/       input data (not version controlled; see "Data")
├── Outputs/    generated tables, plus logs/ (created by the pipeline)
└── Figures/    generated figures (created by the pipeline)
```

Only scripts, this README and the environment lockfiles are tracked (see `.gitignore`).

## Requirements

- **R** 4.3.3. Restore the recorded package versions with `renv::restore(lockfile = "Code/renv.lock")`.
  Main packages: tidyverse, lavaan, cowplot, ggsci, boot, lmtest, sandwich, MASS, robustbase, quantreg, minpack.lm, Hmisc,
  bbmle, terra, sf, rnaturalearth, tidyterra, testthat.
- **Python** 3.12 with numpy 1.26.4, scipy 1.14.1, pandas 2.2.2. Create it with
  `conda env create -f Code/environment.yml`.
  - `run_all.R` uses `/home/j/miniconda3/envs/EEOS-env/bin/python` if it exists. Otherwise it uses the
    interpreter named in the environment variable `EEOS_PYTHON`, then `python3`.
  - It sets `PYTHONNOUSERSITE=1`, so packages in `~/.local` cannot shadow the pinned versions.

## Data

Place the following in `Data/`. All are publicly available from the sources cited in the manuscript.

| File | Content |
| :--- | :--- |
| `comm.csv` | Pinho et al. stems (Balbina, PGM, STM): `Region`, `Region_Plot`, `Binomial_correct`, `Genus`, `family`, `DBH` (cm) |
| `traits.csv` | Pinho et al. species traits: `Binomial_correct`, `WD`, `lnSM`, `LMA`, `Hmax`, `DS` |
| `Jurua_Flora_FullDB.csv` | Scabin et al. Juruá stems and traits (`parcel`, `species`, `genus`, `family`, `DBH`, `WD`, `SM`, `LMA`) |
| `env.csv` | Plot environment incl. coordinates (`X`, `Y`) and forest cover in a 2 km radius (`fc2000`). Juruá plots are absent (intact forest; forest cover set to 1). |
| `Ecoregions2017/` | Ecoregions 2017 shapefile (map in Figure 1A) |

## Running

```bash
Rscript Code/run_all.R            # full pipeline (~10 min); logs in Outputs/logs/
Rscript Code/run_all.R 04 06      # only selected steps, by script number
```

Each step runs in a fresh process and the pipeline stops at the first failure. The R session info and
Python versions are written to `Outputs/logs/`.

| Step | Script | Main outputs | Manuscript item |
| :--- | :--- | :--- | :--- |
| 01 | `01_CombinePinhoJuruaData.R` | `comm.csv`, `traits.csv` | Data |
| 02 | `02_GetEcosystemMacrostates.R` | `Macrostates_<scaling>.csv` (6 scalings) | Methods |
| 02b | `02b_MakeMacrostateScatterplots.R` | `FigureS1.png` | Fig. S1 |
| 03 | `03_FitEEOS.py` | `EEOSPreds_<scaling>.csv` (B_pred, dev, METE constraint errors) | Methods |
| 04 | `04_FitSimpleEeosDevRegression.R` | `H1_Model_*`, `H1_ANCOVA_*`, `H1_Robustness_*`, `H1_BootstrapCI_*`, `H1_InfluentialPlots_*`; `Figure_H1.png`, `Figure_H1_signedsupplement.png` | H1; Fig. 1, Fig. S2 |
| 05 | `05_FitCatsRegressions.R` | `CatsOutput.csv` | H2a metric |
| 06 | `06_SEMs_CatsRegressions.R` | `H2a_*` (absolute) and `H2a_Signed_*` (signed), incl. `_IndirectEffect`; `H2a_3trait*` sensitivity; `Figure_H2a.png` | H2a; Fig. 2 |
| 07 | `07_GetPlotSubsets.R` | plot subsets (RDS) | Methods |
| 08 | `08_FitOverallSloanAndExtractNonneutralAbundances.R` | `SloanOutput.RDS`, `SloanR2_ByRegion.csv`, `NonNeutralTaxa*.csv`, `NonNeutralProportions.csv`; `Figure_ExampleSLoanFits.png`, `Figure_SloanAvgFit.png` | H2b metric; Table S1; Figs. S3–S4 |
| 08b | `08b_TestNonNeutralTraitDifferences.R` | `NonNeutralTraitTests.csv`; `Figure_traitsOfNonneutrals.png` | Fig. S5 |
| 09 | `09_SEMs_NonNeutralAbundances.R` | `H2b_*`, `H2b_Signed_*` (incl. `_IndirectEffect`); `Figure_H2b.png` | H2b; Fig. 3 |
| 10 | `10_FitForestCoverSubgroupSloans.R` | `ForestCoverSubgroupSloanFits.csv`; `Figure_sloansubsets_supplement.png` | H2c metric; Fig. S6 |
| 11 | `11_SEMs_ForestCoverSubgroupSloans.R` | `H2c_*` (RMSE), `H2c_Signed_*` (median signed dev), incl. `_IndirectEffect`; `Figure_H2c.png` | H2c; Fig. 4 |
| 12 | `12_SensitivityAnalysis.R` | `SensitivityAnalysisResults_{Absolute,Signed,long}.csv` | Table S2 |
| 13 | `13_WoodDensityDecomposition.R` | `WoodDensityDecomposition.csv` | SI Note S3 |
| 14 | `14_NS_TruncationCounterfactual.py` | `TableS3_NS_Counterfactual*.csv` | Table S3 |

`<scaling>` is one of the six macrostate formulations. All are defined once, in `Functions/macrostates.R`:

| Suffix | Formulation |
| :--- | :--- |
| `harte_nowd` | ε = DBH/DBH_min, m = ε^(4/3) |
| `harte_wd` | **main analysis**: as `harte_nowd`, with m multiplied by centred wood density |
| `quad_nowd` | ε = (DBH/DBH_min)², m = ε^(4/3) |
| `quad_wd` | as `quad_nowd`, with m multiplied by centred wood density |
| `mte_wd` | ε ∝ ((DBH/DBH_min)^(8/3) × centred wood density)^(3/4), rescaled so the smallest ε = 1; m = ε^(4/3) |
| `chave` | Chave et al. (2014) allometry with DBH in cm, rescaled so the smallest stem has m = ε = 1 |

## Code organisation

| File | Contents |
| :--- | :--- |
| `Functions/config.R` | Constants (regions, scalings, sample sizes), project-root check, paths, Python lookup |
| `Functions/macrostates.R` | Organism-level ε and m for each scaling; plot macrostates |
| `Functions/eeos_functions.py` | METE Lagrange multipliers, constraint check, exact EEoS biomass |
| `Functions/data_prep.R` | Validated loading and joining of the analysis tables (fails loudly on missing data) |
| `Functions/sem_helpers.R` | Mediation SEM suite: full/partial mediation and common cause, LRT, AICc, indirect effect |
| `Functions/plot_helpers.R` | Marginal common-slope lines, mediation panels, figure saving, regional map |
| `Functions/sloan_helpers.R` | Sloan neutral model and random-sampling models (after Burns et al. 2016); non-neutral taxa |

## Tests

```bash
Rscript -e 'testthat::test_dir("Code/tests/testthat")'
PYTHONNOUSERSITE=1 /home/j/miniconda3/envs/EEOS-env/bin/python -m pytest Code/tests -q
```

- **Unit tests** use small synthetic data and need no outputs.
- **`test-integration.R`** checks the real outputs and is skipped if they are absent.
- **`tests/compare_to_baseline.R`** is the golden-master check used during refactoring. It compares
  `Outputs/` against an archived baseline run.
