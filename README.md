# National Tract-Level Cardiovascular–Kidney–Metabolic (CKM) Syndrome Index

Analysis code for a national study of all U.S. census tracts that (1) builds a
continuous CKM burden index from CDC PLACES indicators, aligned with the AHA 2023
CKM staging framework; (2) validates it against individual-level NHANES data; and
(3) models demographic, socioeconomic, behavioral, healthcare-engagement and
environmental correlates — focusing on population aging × environmental exposure
interactions — using spatial regression (INLA BYM2) and machine learning
(XGBoost with spatial cross-validation and TreeSHAP).

This repository accompanies the article and exists so the results can be
reproduced.

## Contents

| Path | Description |
|------|-------------|
| `CKM_US_Main_public.R` | The complete pipeline. Run it top to bottom; it is the only analysis script. It also writes the extended-index refit (eResults in Supplement 1), the eight tables recomputed from the cached fits (`29_*.csv`), and the NaNDA food-store sensitivity analysis (eResults and eTable 5 in Supplement 1; `30_nanda_*.csv`). Section 11 prints six additional summary values: global Moran's I on the 69,854-tract index (0.657), Cronbach's α (0.917), the NHANES sample size (8,965), the upper boundary knot of the aging spline (92.2%), the number of tracts with no PLACES estimate (491), and the variance inflation factors of the Model 1 and Model 2 covariate sets. Section 12 draws the article's maps and the LILA-by-aging figure (Figures 1 and 3; eFigures 3 and 5) in `output/figures_article/`, with tracts that have no CDC PLACES data left blank. Both sections read saved files and fit no model. |
| `README.md` | This file. |
| `LICENSE` | MIT license. |

Input data are **not** redistributed here. All sources are public. The script
downloads most of them; three sets of files must be downloaded by hand (see
*Manual downloads* below). Generated figures are not deposited in this
repository. The derived output tables — the 62 CSVs backing every reported
estimate — are archived on Zenodo at
<https://doi.org/10.5281/zenodo.22234206>; a full run regenerates both tables
and figures into `output/tables/` and `output/figures/`.

## Requirements

- **R ≥ 4.4.** Reported results were produced on R 4.6.0 (2026-04-24),
  aarch64-apple-darwin23, with INLA 25.10.19.
- Internet access — the script downloads all input data except the files listed
  under *Manual downloads*.
- A free Census API key: <https://api.census.gov/data/key_signup.html>

`INLA` is not on CRAN. Install it from its own repository:

```r
install.packages("INLA",
  repos = c(INLA = "https://inla.r-inla-download.org/R/stable"),
  dep = TRUE)
```

## Running it

```r
Sys.setenv(CENSUS_API_KEY = "your_key_here")
source("CKM_US_Main_public.R")
```

The script locates the project root by itself, so `source()`, `Rscript` and
RStudio's Source button all work without `setwd()`, including for Section 10. If it cannot find the root it
stops with an explicit message rather than guessing. It also stops immediately if
the Census API key is not set. **No key is stored in this repository.**
It creates `data/` and `output/` next to itself on the first run.

Section 10 of the script (the post-pipeline analyses, which include one extra
INLA refit of 30–90 minutes and seven NaNDA refits of about 2 minutes each) can
be skipped with
`Rscript CKM_US_Main_public.R --skip-post`.

Sections 11 and 12 run at the end of the script and take a few seconds; they print the
six summary values described under Contents and redraw the article figures.

On the first run it downloads the input data listed above and may install missing
packages, which takes a while. Fits in Sections 6 to 8 and the NaNDA fits are cached,
so a re-run reuses them. Most other Section 10 stages refit each time. Figures and
tables are always rewritten, so run the code in a separate copy of any folder whose
outputs you want to keep unchanged.

Outputs are written to `output/tables/` and `output/figures/`. Package versions
are written to `output/tables/sessionInfo.txt` at the end of the run.

## Manual downloads

The script downloads most inputs itself. Save these three sets of files by hand in
`data/raw/` before you run it. All are free.

| Input | Where to get it | Save as | If it is missing |
|-------|-----------------|---------|------------------|
| EPA Downscaler PM₂.₅ and ozone, 2020, tract level | <https://www.epa.gov/hesc/rsig-related-downloadable-data-files> | `2020_pm25_daily_average_2010_census.txt.gz` and `2020_ozone_daily_8hour_maximum_2010_census.txt.gz` | The script stops with a message. |
| CDC PLACES tract file, 2022 release, GIS Friendly Format | <https://chronicdata.cdc.gov> (search for "PLACES Census Tract 2022 release (GIS Friendly Format)") | `PLACES__Census_Tract_Data_(GIS_Friendly_Format),_2022_release_20260311.csv` | The precision-weighted stages stop with a message, and so do the later stages that build on them (spline, sign-change, and regional stages). |
| NaNDA Grocery and Food Stores, version 2.1 (eTable 5 in Supplement 1) | <https://doi.org/10.3886/ICPSR209313.V2> (free ICPSR account) | `ICPSR_209313-V2.1/nanda_grocery_Tract10_1990-2022_01.dta` | Only that stage stops, with a message. |

## Things to know before you compare numbers

**Model numbering.** *Model 1* is the behavior-**excluded** BYM2 — the total
association, and the primary specification the article interprets. *Model 2* is
the behavior-**adjusted** fit, the controlled direct effect, retained as the
pre-specified contrast. Output files whose names contain `model2` in the
precision-weighted stage (`09b_precision_weighted_model2*`) hold Model 1; the
code refers to fits as `_adj` / `_exc` rather than by number.

**Compare models on log marginal likelihood, not DIC or WAIC.** DIC and WAIC moved
by thousands with the INLA starting values (about 10,500 in Model 1 and about
72,000 in Model 2), while the coefficients barely moved. The model cannot cleanly
separate residual noise from unstructured tract variation, and those criteria
depend on that split. The article reports log marginal likelihood instead. It
moved by up to about 7 across starting values, so smaller gaps are not interpreted
(eTable 4 in Supplement 1). Some output tables also list DIC and WAIC; model comparison
does not rest on them. Verify reproduction on the posterior means.

**Staten Island has its own intercept.** New Jersey has no PLACES data, so the 106
tracts of Richmond County, NY, form a separate connected part of the k = 8 neighbor
graph. INLA scales and sum-to-zero constrains the BYM2 term within each part, so
every BYM2 fit adds an `island` intercept (Freni-Sterrantino et al., 2018), through
the `.inla_isl` wrapper.

**Refit drift.** Refitting on identical data moved fixed-effect posterior means by
at most 2 × 10⁻⁴ (Model 1) and 8 × 10⁻³ (Model 2), the latter at the intercept. See
`29_refit_drift_summary.csv`. Stage 12 gates these differences at 0.1 posterior SD.
The largest Model 2 difference was 0.08 SD, so a rerun can stop at that gate without
any error in the data. Other reruns moved the means by less, and derived summaries
can change in the third decimal, so a rerun will not match every digit of the
deposited tables. The article reports the deposited run.

**Second starting values.** Stage 31 builds eTable 4 from `31_mlik_log.csv` (default
start) and `31_mlik_log_second.csv` (other starts). The second file is not rebuilt by
a default run, so copy it from the Zenodo deposit into `output/tables/` first. Each
run adds rows to `31_mlik_log.csv`, and stage 31 uses the earliest matching row, so
start with no log file to rebuild eTable 4 from a new run. To refit from the second
starting values, set `CKM_SECOND_START=1` and run the pipeline in a separate copy of
the project.

## Article figures

| Figure | File |
|--------|------|
| 1 | `output/figures_article/Figure_CKM_burden_map.png` (Section 12) |
| 2 | `output/figures/09_dumbbell_bym2_m1_m2.png` |
| 3 | `output/figures_article/Figure_LILA_by_aging.png` (Section 12) |
| eFigure 1 | `output/figures/S11_denominator_flow.png` |
| eFigure 2 | `output/figures/04_nhanes_validation.png` |
| eFigure 3 | `output/figures_article/eFigure3_ols_residuals.png` (Section 12) |
| eFigure 4 | `output/figures/07_shap_summary.png` |
| eFigure 5 | `output/figures_article/Figure_residual_spatial_effect.png` (Section 12) |

## Note on New Jersey

New Jersey is excluded from index construction because CDC PLACES 2022 publishes
no blood-pressure or cholesterol estimates there, leaving the five-indicator index
undefined. The pipeline's own maps show these tracts in a distinct shade; the figures from
Section 12 leave all tracts without PLACES data blank. Neither choice
affects any analytic result.

## Citation

If you use this code, please cite the accompanying article.

## License

MIT — see [`LICENSE`](LICENSE).

