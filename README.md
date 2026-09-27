# National Tract-Level Cardiovascular–Kidney–Metabolic (CKM) Syndrome Index

Analysis code for a national study of all U.S. census tracts that (1) builds a
continuous CKM burden index from CDC PLACES indicators, aligned with the AHA 2023
CKM staging framework; (2) validates it against individual-level NHANES data; and
(3) models demographic, socioeconomic, behavioral, healthcare-engagement and
environmental correlates — focusing on population aging × environmental exposure
interactions — using spatial regression (INLA BYM2) and machine learning
(XGBoost with spatial cross-validation and TreeSHAP).

This repository accompanies the manuscript and exists so the results can be
reproduced.

## Contents

| Path | Description |
|------|-------------|
| `code/CKM_US_Main_public.R` | The main pipeline: downloads the data, builds the index, fits the models, and writes the tables and figures. Run it first. |
| `code/28_extended_bym2.R` | Refits the primary Model 1 BYM2 specification on the extended 7-indicator index (adds coronary heart disease and stroke); Supplemental §S3.9. Writes `28_extended_bym2_*.csv`. |
| `code/29_recompute_deposit_gaps.R` | Recomputes, from the cached fits (no model is refit), eight tables the manuscript reports that the main pipeline does not write: full-covariate OLS residual Moran's I, Southern and state summaries of the BYM2 random effects, the age distribution, behavior prevalence by burden quintile, refit drift against the published fits, and the baseline OLS coefficients. Writes `29_*.csv`. |
| `code/21_fara_null_check.py` | Checks how the USDA Food Access Research Atlas codes missing low-access population counts. Exploratory; not used in the article's results. Writes `21_fara_null_check.csv`. |
| `data/` | Empty in the repository; the main pipeline fills it on the first run. |

Input data are **not** redistributed here. All sources are public and are
downloaded by the script. Generated figures are not deposited in this
repository. The derived output tables — the 71 CSVs backing every reported
estimate — are archived on Zenodo at
<https://doi.org/10.5281/zenodo.22234206>; a full run regenerates both tables
and figures into `output/tables/` and `output/figures/`.

## Requirements

- **R ≥ 4.4.** Published results were produced on R 4.6.0 (2026-04-24),
  aarch64-apple-darwin23, with INLA 25.10.19.
- Internet access — the script downloads all input data.
- A free Census API key: <https://api.census.gov/data/key_signup.html>

`INLA` is not on CRAN. Install it from its own repository:

```r
install.packages("INLA",
  repos = c(INLA = "https://inla.r-inla-download.org/R/stable"),
  dep = TRUE)
```

## Running it

From the repository root (the folder that contains `code/` and `data/`):

```r
Sys.setenv(CENSUS_API_KEY = "your_key_here")
source("code/CKM_US_Main_public.R")
```

The script locates the project root by itself — the folder containing both
`code/` and `data/` — so `source()`, `Rscript` and RStudio's Source button all
work without `setwd()`. If it cannot find the root it stops with an explicit
message rather than guessing. It also stops immediately if the Census API key is
not set. **No key is stored in this repository.**

The three standalone stages read the main pipeline's cached objects, so run them
afterwards, also from the repository root:

```sh
Rscript code/28_extended_bym2.R          # one INLA BYM2 fit; expect 30-90 min
Rscript code/29_recompute_deposit_gaps.R # no refitting; a few minutes
python3 code/21_fara_null_check.py       # needs the openpyxl package
```

On the first run it downloads all input data and may install missing packages,
which takes a while. Model fits are cached — re-running reuses stored fits rather
than refitting, while figures and tables are always rewritten.

Outputs are written to `output/tables/` and `output/figures/`. Package versions
are written to `output/tables/sessionInfo.txt` at the end of the run.

## Two things to know before you compare numbers

**Model numbering.** *Model 1* is the behavior-**excluded** BYM2 — the total
association, and the primary specification the manuscript interprets. *Model 2* is
the behavior-**adjusted** fit, the controlled direct effect, retained as the
pre-specified contrast. The two were numbered the other way round before
2026-08-29, so filenames containing `model2` predate the swap and refer to what is
now Model 1. They were deliberately not renamed because they are cited by name in
the revision correspondence. The code avoids the ambiguity by naming fits
`_adj` / `_exc` rather than by number.

**INLA's information criteria are far less stable than its coefficients.**
Refitting these models on identical data, an identical model and an md5-identical
neighbor graph moved fixed-effect posterior means by at most 4 × 10⁻⁶ (Model 1)
and 1.2 × 10⁻³ (Model 2) — but moved DIC by roughly 1,700–1,800 and WAIC by up to
2,200. This follows from the empirical-Bayes integration strategy used throughout
(`control.inla`, `int.strategy = "eb"`): the hyperparameters are held at their
posterior mode and their dispersion comes from the numerical Hessian there, which
shifts between sessions. The effective-parameter penalties in DIC and WAIC are
sums over a 69,530-dimensional latent field and inherit that instability; the
fixed-effect posterior means do not.

**Verify reproduction on the posterior means, not on DIC or WAIC**, and do not
treat a DIC difference of order 10³ as evidence that one model fits better.

`12_bym2_refit_validation.csv` compares the stage-12 refits with the published
fits as they stood when that stage ran (2026-08-29). Both published models were
refit afterwards, so its `published` column no longer matches Table 3. Use
`29_refit_drift_terms.csv` and `29_refit_drift_summary.csv` for the comparison
against the fits reported in the manuscript.

## Manuscript figures

| Figure | File in `output/figures/` |
|--------|---------------------------|
| 1 | `04_nhanes_validation.png` |
| 2 | `03e_sdoh_panel.png` |
| 3 | `eda_choropleth_ckm_core.png` |
| 4 | `09_dumbbell_bym2_m1_m2.png` |
| 5 | `03d_choropleth_pm25.png` |
| 6 | `06h_bym2_spatial_random_effect_2panel.png` |
| 7 | `07_shap_summary.png` |
| 8 | `07_age_env_interactions.png` |

## Note on New Jersey

New Jersey is excluded from index construction because CDC PLACES 2022 publishes
no blood-pressure or cholesterol estimates there, leaving the five-indicator index
undefined. These tracts are shown in a distinct shade on the relevant maps and
affect no analytic result.

## Citation

If you use this code, please cite the accompanying manuscript.

## License

MIT — see [`LICENSE`](LICENSE).
