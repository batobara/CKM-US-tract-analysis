# ============================================================
# CKM_US_Main_public.R
# US Tract-Level Cardiovascular-Kidney-Metabolic (CKM) Syndrome
#   Index + Aging × Environment Drivers
# Scope: ALL US census tracts
# Outcome: Continuous CKM burden index
# Validation: vs. NHANES individual-level co-occurrence
# Inference: INLA BYM2 + ML (XGBoost + spatial CV + SHAP)
# Effect of interest: % age 65+ × environmental exposure interactions
#
# Data sources (all open):
#   CDC PLACES         CDCPLACES package        diabetes, HTN, obesity, chol, CKD
#   ACS                tidycensus               age structure, education, income
#   TIGER tracts       tigris                   geometries
#   NHANES             nhanesA                  individual-level validation
#   AHRQ SDOH DB       httr2                    multi-domain SDOH
#   EJScreen           CSV download             environmental burden composite
#   PM2.5              EPA / Daymet derivative  annual mean
#   Heat               PRISM web service        warm-season (May-Sep) mean Tmax
#   NDVI               MODIS via terra/rgee     annual mean greenness
#   Walkability        EPA Walkability Index    CSV
#
# Run order: source this file top-to-bottom.
# Outputs: output/figures/  and  output/tables/
# ============================================================


# ==============================================================
# 0.  SETUP
# ==============================================================

# ---- 0a-pre. Pre-flight: foundation packages must meet minima -------
# Modern tidyverse / CDCPLACES / tidymodels require recent versions of
# rlang, cli, glue, vctrs, lifecycle, etc. On Windows, these packages'
# DLLs get locked by RStudio's own session, so install.packages() can
# silently fail to upgrade them. pak::pkg_install runs in a subprocess
# and CAN replace locked DLLs.
#
# Strategy:
#   (1) Define minimum versions for foundation packages
#   (2) Auto-detect a private Rlibs path (manual-delete fallback)
#   (3) Identify any packages below the minimum
#   (4) Use pak to upgrade them all in one subprocess install
#   (5) Force a clean R restart if anything was upgraded

PRIVATE_LIB <- "C:/Rlibs"

# Minimum versions for early-loaded foundation packages. These are the
# ones that commonly trip "namespace 'X' Y is loaded, but >= Z is required"
# errors when loading current CRAN tidyverse / tidymodels.
PREFLIGHT_MINS <- c(
  rlang     = "1.1.7",
  cli       = "3.6.6",
  glue      = "1.7.0",
  vctrs     = "0.6.5",
  lifecycle = "1.0.4",
  tibble    = "3.2.1",
  pillar    = "1.9.0",
  withr     = "3.0.0"
)

# (2) If a private library has newer copies, prepend it
if (dir.exists(PRIVATE_LIB)) {
  .libPaths(c(PRIVATE_LIB, .libPaths()))
  message("Prepended private library: ", PRIVATE_LIB)
}

# (3) Find packages below their minimum version
installed_now <- installed.packages()[, "Package"]
needs_upgrade <- vapply(names(PREFLIGHT_MINS), function(pkg) {
  if (!pkg %in% installed_now) return(TRUE)
  utils::packageVersion(pkg) < PREFLIGHT_MINS[[pkg]]
}, logical(1))

if (any(needs_upgrade)) {
  to_upgrade <- names(PREFLIGHT_MINS)[needs_upgrade]
  message("Foundation packages needing upgrade:")
  for (p in to_upgrade) {
    have <- if (p %in% installed_now)
              as.character(utils::packageVersion(p)) else "MISSING"
    message("  ", p, ": ", have, " -> ", PREFLIGHT_MINS[[p]], "+")
  }

  # Bootstrap pak if missing (pak install rarely hits DLL-lock issues)
  if (!"pak" %in% installed_now)
    install.packages("pak", repos = "https://cloud.r-project.org")

  # Upgrade all in one pak subprocess call — atomic, bypasses DLL locks
  tryCatch(
    pak::pkg_install(to_upgrade, upgrade = TRUE, ask = FALSE),
    error = function(e) message("pak install error: ", e$message)
  )

  # Re-check
  still_bad <- vapply(to_upgrade, function(pkg) {
    if (!pkg %in% installed.packages()[, "Package"]) return(TRUE)
    utils::packageVersion(pkg) < PREFLIGHT_MINS[[pkg]]
  }, logical(1))

  if (any(still_bad)) {
    failed <- to_upgrade[still_bad]
    message("\n========================================================")
    message("DLL lock not resolved by pak for: ",
            paste(failed, collapse = ", "))
    message("========================================================")
    message("  1. File > Quit Session AND close RStudio entirely")
    message("  2. Task Manager > End any of: rsession.exe, Rscript.exe, R.exe")
    message("  3. File Explorer > C:\\Users\\<your-username>\\AppData\\Local\\R\\")
    message("       win-library\\4.5\\ — DELETE these folders if present:")
    for (p in failed) message("       ", p)
    message("       and also: 00LOCK, 00LOCK-* folders")
    message("  4. Reopen RStudio (do NOT open the .R file yet); in console:")
    message("       install.packages(c(",
            paste0("'", failed, "'", collapse = ", "), "))")
    message("  5. Then source this script.")
    message("\nFallback: private library bypass")
    message("       dir.create('", PRIVATE_LIB, "', showWarnings = FALSE)")
    message("       .libPaths(c('", PRIVATE_LIB, "', .libPaths()))")
    message("       install.packages(c(",
            paste0("'", failed, "'", collapse = ", "),
            "), lib = '", PRIVATE_LIB, "')")
    message("========================================================\n")
    stop("Foundation package versions still too old. See above.",
         call. = FALSE)
  }

  stop("Upgraded: ", paste(to_upgrade, collapse = ", "),
       ". CLOSE this R session COMPLETELY, reopen R, then source again.\n",
       "(These packages load early and cannot reload in-place.)",
       call. = FALSE)
}
message("All foundation packages meet minimum versions — pre-flight OK.")

# ---- 0a. Install packages ------------------------------------
required_pkgs <- c(
  # Data acquisition
  "CDCPLACES",                                # CDC PLACES tract data
  "tidycensus", "tigris",                     # ACS + TIGER tract geometries
  "nhanesA",                                  # NHANES (Paper 2 validation)
  "httr2", "jsonlite", "curl",                # REST APIs + resilient downloads
  "daymetr",                                  # Daymet climate (heat)
  # Spatial core
  "sf", "terra", "stars",
  # Tidyverse
  "dplyr", "tidyr", "purrr", "readr", "tibble", "stringr", "lubridate",
  # Spatial methods
  "spdep", "spatialreg", "spmoran",           # Moran's I, SEM/SAR, eigenvector SF
  # INLA installed separately — see Section 0b
  # ML pillar
  # NB: fastshap removed — it has no CRAN build for R 4.6.0. SHAP is now
  # computed with xgboost's built-in exact TreeSHAP (predcontrib = TRUE) in
  # Section 7e, which needs no extra package.
  "xgboost", "tidymodels", "spatialsample", "vip",
  # NB: SpatialML (Geographic Random Forest) is archived from CRAN as of 2024.
  # Spatial heterogeneity in ML is covered by XGBoost + spatial-block CV in
  # Section 7. If GRF is needed later, install from GitHub:
  #   remotes::install_github("StefanosGeorganos/SpatialML")
  # Index construction & EDA
  "factoextra", "FactoMineR", "corrr", "ggcorrplot", "corrplot",
  # Visualization
  "ggplot2", "patchwork", "cowplot", "viridis", "scales", "tmap",
  # Misc
  "broom", "DescTools", "car",
  # NaNDA food-store file (stage 30) is a Stata .dta
  "haven"
)
new_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs))
  install.packages(new_pkgs, repos = "https://cloud.r-project.org")

# Source-install fallback: on a freshly-released R, CRAN may not yet have a
# compiled binary for some packages (e.g. fastshap on R 4.6.0), so the binary
# install above silently skips them with "not available for this version of R".
# Retry any still-missing packages from source. Requires compilers on macOS
# (xcode-select --install) / Rtools on Windows.
still_missing <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(still_missing)) {
  message("Binary unavailable for: ", paste(still_missing, collapse = ", "),
          " — retrying from source.")
  install.packages(still_missing, type = "source",
                   repos = "https://cloud.r-project.org")
}

# INLA is hosted off-CRAN; install only if missing
if (!"INLA" %in% installed.packages()[, "Package"]) {
  install.packages("INLA",
                   repos = c(getOption("repos"),
                             INLA = "https://inla.r-inla-download.org/R/stable"),
                   dep = TRUE)
}

# ---- 0b. Load packages ---------------------------------------
suppressPackageStartupMessages({
  library(CDCPLACES); library(tidycensus); library(tigris)
  library(nhanesA)
  library(httr2); library(jsonlite); library(daymetr)
  library(sf); library(terra); library(stars)
  library(dplyr); library(tidyr); library(purrr); library(readr)
  library(tibble); library(stringr); library(lubridate)
  library(spdep); library(spatialreg); library(spmoran)
  library(INLA)
  library(xgboost); library(tidymodels); library(spatialsample)
  # SHAP uses xgboost's native TreeSHAP (Section 7e) — no fastshap needed.
  library(vip)
  library(factoextra); library(FactoMineR); library(corrr)
  library(ggcorrplot); library(corrplot)
  library(ggplot2); library(patchwork); library(cowplot)
  library(viridis); library(scales); library(tmap)
  library(broom); library(DescTools); library(car)
})

# tigris caches geometries to disk to avoid repeated downloads
options(tigris_use_cache = TRUE)

# ---- 0b-bis. Census API credentials --------------------------
# Get a free key from https://api.census.gov/data/key_signup.html, then install
# it ONCE into your R environment:
#   tidycensus::census_api_key("YOUR_KEY", install = TRUE)
# which appends CENSUS_API_KEY to ~/.Renviron and reuses it across sessions.
# For a one-off run, export CENSUS_API_KEY in the shell environment instead.
#
# The key is deliberately NOT stored in this file: the script is distributed
# with the manuscript, and a literal key here would be published with it.
CENSUS_API_KEY <- Sys.getenv("CENSUS_API_KEY")
if (!nzchar(CENSUS_API_KEY)) {
  stop("Census API key not found. Run once, then restart R:\n",
       "  tidycensus::census_api_key('your_key', install = TRUE)\n",
       "or export CENSUS_API_KEY in your environment before running.")
}
census_api_key(CENSUS_API_KEY, overwrite = TRUE, install = FALSE)

# ---- 0c. Output / cache directories --------------------------

detect_root <- function() {
  is_root <- function(p) {
    dir.exists(file.path(p, "data")) && dir.exists(file.path(p, "code"))
  }
  # Climb at most `levels` parents. Stops at the filesystem root, where
  # dirname(p) == p, so this cannot loop.
  walk_up <- function(start, levels = 6L) {
    if (is.na(start) || !nzchar(start)) return(NA_character_)
    p <- suppressWarnings(normalizePath(start, mustWork = FALSE))
    for (i in seq_len(levels)) {
      if (is_root(p)) return(normalizePath(p))
      parent <- dirname(p)
      if (identical(parent, p)) break
      p <- parent
    }
    NA_character_
  }

  this_file <- function() {
    out <- character(0)
    for (i in rev(seq_len(sys.nframe()))) {
      of <- tryCatch(get("ofile", envir = sys.frame(i), inherits = FALSE),
                     error = function(e) NULL)
      if (is.character(of) && length(of) == 1L && nzchar(of)) out <- c(out, of)
    }
    a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

    if (length(a)) {
      out <- c(out, gsub("~+~", " ", sub("^--file=", "", a[[1L]]), fixed = TRUE))
    }
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE))) {
      p <- tryCatch(rstudioapi::getSourceEditorContext()$path,
                    error = function(e) NULL)
      if (is.character(p) && length(p) == 1L && nzchar(p)) out <- c(out, p)
    }
    out
  }
  r <- walk_up(getwd())
  if (!is.na(r)) return(r)
  for (f in this_file()) {
    r <- walk_up(dirname(suppressWarnings(normalizePath(f, mustWork = FALSE))))
    if (!is.na(r)) return(r)
  }
  # Flat layout (the public repository keeps this script at the top level,
  # without code/ or data/): use the folder that holds the script itself.
  for (f in this_file()) {
    d <- dirname(suppressWarnings(normalizePath(f, mustWork = FALSE)))
    if (dir.exists(d)) return(normalizePath(d))
  }
  NA_character_
}


root_candidates <- if (.Platform$OS.type == "windows") {
  "C:/path/to/CKM_US"                                                                                  # Windows (edit to your project root)
} else {
  "/path/to/CKM_US"                                                                                    # macOS/Linux (edit to your project root)
}

root <- detect_root()
if (is.na(root)) root <- root_candidates[dir.exists(root_candidates)][1]
if (is.na(root)) stop(
  "Could not locate the project root.\n",
  "  The project folder is the one containing both `code/` and `data/`.\n",
  "  It is normally found automatically, either from the working directory\n",
  "  or from this script's own location. Reaching this message usually means\n",
  "  the script was copied out of the project tree, or `data/` is missing.\n",
  "  Fix by setting the working directory explicitly, e.g.\n",
  "    setwd('/path/to/CKM_US'); source('code/CKM_US_Main_public.R')\n",
  "  or edit `root_candidates` above.")
message("Project root: ", root)
dir_data  <- file.path(root, "data")
dir_raw   <- file.path(dir_data, "raw")
dir_proc  <- file.path(dir_data, "processed")
dir_fig   <- file.path(root, "output", "figures")
dir_tbl   <- file.path(root, "output", "tables")
invisible(lapply(c(dir_raw, dir_proc, dir_fig, dir_tbl),
                 dir.create, recursive = TRUE, showWarnings = FALSE))


dir_graph <- file.path(tempdir(), "ckm_inla")
dir.create(dir_graph, recursive = TRUE, showWarnings = FALSE)

# ---- 0d. Constants -------------------------------------------
PLACES_RELEASE <- "2022"                      
                                            
ACS_YEAR       <- 2019                       
                                  
NHANES_CYCLES  <- c("2017-2018", "2017-2020")  # for index validation (Section 3)


CKM_CORE     <- c("DIABETES", "BPHIGH", "OBESITY", "HIGHCHOL", "KIDNEY")
CKM_EXTENDED <- c(CKM_CORE, "CHD", "STROKE")


PLACES_BEHAVIOR <- c("CSMOKING", "LPA", "ACCESS2")


PLACES_PREV <- c("CHECKUP")

CKM_MEASURES <- c(CKM_EXTENDED, PLACES_BEHAVIOR, PLACES_PREV)
                                                

ACS_B_VARS <- c(
  total_pop     = "B01003_001",   # Total population
  median_age    = "B01002_001",   # Median age (years)
  median_income = "B19013_001",   # Median household income (USD)
  pop_male_65_66   = "B01001_020", pop_male_67_69 = "B01001_021",
  pop_male_70_74   = "B01001_022", pop_male_75_79 = "B01001_023",
  pop_male_80_84   = "B01001_024", pop_male_85_plus = "B01001_025",
  pop_female_65_66 = "B01001_044", pop_female_67_69 = "B01001_045",
  pop_female_70_74 = "B01001_046", pop_female_75_79 = "B01001_047",
  pop_female_80_84 = "B01001_048", pop_female_85_plus = "B01001_049",
  pop_hispanic    = "B03003_003",
  pop_black       = "B02001_003",
  pop_white_nh    = "B03002_003",
  pop_25_plus     = "B15003_001", # denominator for education
  pop_25_bachelor = "B15003_022",
  pop_25_master   = "B15003_023",
  pop_25_prof     = "B15003_024",
  pop_25_doctor   = "B15003_025"
)

# Spatial CRS standard
CRS_LATLON <- 4326          # WGS84 for raw inputs
CRS_PROJECT <- 5070         # NAD83 / Conus Albers — equal-area, US-wide

# Reproducibility
set.seed(20260425)

# ---- 0e. Helper functions ------------------------------------
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

theme_ckm <- theme_bw(base_size = 11) +
  theme(strip.background = element_rect(fill = "grey92"),
        panel.grid.minor = element_blank(),
        legend.position  = "bottom")

# Save a tract-level data frame as both .rds (fast reload) and .csv (peek)
save_tract_df <- function(df, name) {
  saveRDS(df, file.path(dir_proc, paste0(name, ".rds")))
  if (nrow(df) <= 1e5)
    write_csv(df, file.path(dir_proc, paste0(name, ".csv")))
  invisible(df)
}

# ---- Pretty predictor labels (single source of truth) ----------

predictor_pretty <- c(
  pct_age_65_plus     = "% adults age 65+",
  pct_bachelor_plus   = "% adults with bachelor's+",
  pct_hispanic        = "% Hispanic",
  ice_income          = "ICE Income",
  ice_race            = "ICE Race",
  median_income       = "Median household income (USD)",
  pm25_annual         = "Annual mean PM₂.₅ (µg/m³)",
  o3_8hrmax_4thmax    = "Annual 4th-highest 8-hr O₃ (per 10 ppb)",
  tmax_warm           = "Warm-season (May–Sep) max temp (°C)",
  walk_index          = "EPA National Walkability Index (1–20)",
  LILATracts_1And10   = "Low-income low-access tract (1-mi/10-mi)",
  ruca_class          = "RUCA class",
  ruca_classMetro     = "RUCA: Metro",
  ruca_classMicropolitan = "RUCA: Micropolitan",
  `ruca_classSmall town` = "RUCA: Small town",
  ruca_classRural     = "RUCA: Rural"
)

.label_pred <- function(x) {
  vapply(as.character(x), function(s) {
    if (is.na(s) || s == "") return(s)
    if (grepl(":", s, fixed = TRUE)) {
      parts <- strsplit(s, ":", fixed = TRUE)[[1]]
      pretty <- ifelse(parts %in% names(predictor_pretty),
                        predictor_pretty[parts], parts)
      return(paste(pretty, collapse = " × "))
    }
    if (s %in% names(predictor_pretty)) predictor_pretty[[s]] else s
  }, character(1), USE.NAMES = FALSE)
}

message("Section 0 complete. Working dir = ", root)


# ==============================================================
# 1.  DATA ACQUISITION  (CDC PLACES + ACS + tract geometries)
# ==============================================================

# ---- 1a. CDC PLACES 2022 release, tract data -----------------

f_places <- file.path(dir_proc, "01_places_tract.rds")

need_pull_places <- TRUE
if (file.exists(f_places)) {
  cached_places <- readRDS(f_places)
  missing_meas <- setdiff(CKM_MEASURES, names(cached_places))
  if (length(missing_meas) == 0) {
    need_pull_places <- FALSE
  } else {
    message("PLACES cache missing measures: ",
            paste(missing_meas, collapse = ", "),
            "  → re-pulling.")
  }
}
if (need_pull_places) {

  CKM_MEASURES_PULL <- CKM_MEASURES
  dict_ok <- tryCatch({
    dict <- CDCPLACES::get_dictionary()
    valid_ids <- intersect(c("MeasureId", "Measure_Id", "measureid"),
                            names(dict))[1]
    if (!is.na(valid_ids)) {
      valid <- unique(as.character(dict[[valid_ids]]))
      missing_in_dict <- setdiff(CKM_MEASURES_PULL, valid)
      if (length(missing_in_dict) > 0) {
        message("  ! These measures not in PLACES dictionary, dropping: ",
                paste(missing_in_dict, collapse = ", "))
        CKM_MEASURES_PULL <- setdiff(CKM_MEASURES_PULL, missing_in_dict)
      }
    }
    TRUE
  }, error = function(e) {
    message("  (dictionary check skipped: ", e$message, ")"); TRUE
  })

  message("Pulling CDC PLACES ", PLACES_RELEASE, " tract data ",
          "(", length(CKM_MEASURES_PULL), " measures)...")
  places_long <- tryCatch(
    CDCPLACES::get_places(
      geography = "tract",          
      measure   = CKM_MEASURES_PULL,
      release   = PLACES_RELEASE
    ),
    error = function(e) {
      message("  ! PLACES API unreachable: ", e$message); NULL
    })


  if (is.null(places_long)) {
    
    candidate_csvs <- list.files(dir_raw,
      pattern = "PLACES.*Census_Tract.*GIS_Friendly.*2022.*\\.csv$",
      full.names = TRUE, ignore.case = TRUE)
    places_csv <- if (length(candidate_csvs) > 0) candidate_csvs[1] else
      file.path(dir_raw, paste0("places_", PLACES_RELEASE, "_tract.csv"))

    if (file.exists(places_csv) && file.size(places_csv) > 1e6) {
      message("  Manual CSV found: ", basename(places_csv),
              " (", round(file.size(places_csv) / 1e6, 1), " MB)")
    } else {
      message("  No manual CSV in ", dir_raw,
              "\n    Manual fallback: visit https://chronicdata.cdc.gov, search",
              "\n    'PLACES Census Tract 2022 release (GIS Friendly Format)',",
              "\n    Export → CSV, save to ", dir_raw)
    }
    if (file.exists(places_csv) && file.size(places_csv) > 1e6) {
      message("  reading PLACES CSV (", round(file.size(places_csv) / 1e6, 1),
              " MB)...")
      places_long <- tryCatch(
        readr::read_csv(places_csv, show_col_types = FALSE,
                        progress = FALSE, guess_max = 1e5),
        error = function(e) {
          message("  ! CSV parse failed: ", e$message); NULL
        })
      if (!is.null(places_long)) {

        geo_col_csv <- intersect(c("LocationID", "LocationName", "GEOID",
                                    "TractFIPS", "locationid"),
                                  names(places_long))[1]
        if (!is.na(geo_col_csv)) {
          sample_geoids <- stringr::str_pad(
            as.character(stats::na.omit(head(places_long[[geo_col_csv]], 200))),
            width = 11, side = "left", pad = "0")
          geoid_lens <- nchar(sample_geoids)
          if (length(geoid_lens) > 0 && stats::median(geoid_lens) != 11) {
            message("  ! Validation failed: median GEOID length is ",
                    stats::median(geoid_lens),
                    " (expected 11 for tract-level data). The downloaded ",
                    "file is NOT the 2022 Census Tract release ",
                    "(possibly the County or ZCTA release was returned). ",
                    "Discarding.")
            file.remove(places_csv)
            places_long <- NULL
          }
        }
      }
      if (!is.null(places_long)) {

        crude_cols <- grep("_CrudePrev$", names(places_long), value = TRUE)
        if (length(crude_cols) > 0) {
          message("  Wide-format CSV detected — converting to long format ",
                  "(", length(crude_cols), " *_CrudePrev measure columns).")
          places_long <- places_long |>
            dplyr::select(GEOID = !!geo_col_csv,
                          dplyr::all_of(crude_cols)) |>

            dplyr::mutate(GEOID = stringr::str_pad(
              as.character(GEOID), width = 11, side = "left", pad = "0")) |>
            tidyr::pivot_longer(-GEOID,
                                names_to  = "MeasureId",
                                values_to = "Data_Value") |>
            dplyr::mutate(MeasureId = sub("_CrudePrev$", "", MeasureId))
        }

        meas_col_csv <- intersect(c("MeasureId", "Measure_Id", "measureid"),
                                   names(places_long))[1]
        if (!is.na(meas_col_csv))
          places_long <- places_long |>
            dplyr::filter(.data[[meas_col_csv]] %in% CKM_MEASURES_PULL)

        geo_col_csv2 <- intersect(c("GEOID", "TractFIPS", "LocationID"),
                                   names(places_long))[1]
        message("  PLACES CSV download OK: ", nrow(places_long),
                " (tract × measure) rows; ",
                length(unique(places_long[[geo_col_csv2]])),
                " unique tracts; ",
                length(unique(places_long[[meas_col_csv]])),
                " measures")
      }
    } else {
      message("  ! Direct CSV download also failed.")
    }
  }


  if (is.null(places_long)) {
    if (file.exists(f_places)) {
      places_tract <- readRDS(f_places)
      message("  ! Falling back to cached PLACES file (",
              ncol(places_tract) - 1, " measures: ",
              paste(setdiff(names(places_tract), "GEOID"),
                    collapse = ", "), ")")
    } else {
      stop("PLACES API + direct CSV both failed and no cached file exists. ",
           "Manually download the 2022 PLACES Census Tract release from\n",
           "  https://chronicdata.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-Census-Tract-D/duw2-7jbt\n",
           "and save to:\n  ", f_places)
    }
  } else {

    val_col <- intersect(c("Data_Value", "Data_value", "data_value"),
                         names(places_long))[1]
    geo_col <- intersect(c("LocationID", "LocationName", "GEOID",
                           "TractFIPS", "locationid"),
                         names(places_long))[1]
    meas_col <- intersect(c("MeasureId", "Measure_Id", "measureid"),
                          names(places_long))[1]
    if (is.na(val_col) || is.na(geo_col) || is.na(meas_col))
      stop("CDCPLACES returned unexpected column names: ",
           paste(names(places_long), collapse = ", "))

    places_tract <- places_long |>
      dplyr::select(GEOID = !!geo_col,
                    MeasureId = !!meas_col,
                    Data_Value = !!val_col) |>
      dplyr::mutate(GEOID = as.character(GEOID)) |>
      tidyr::pivot_wider(id_cols = GEOID,
                         names_from = MeasureId,
                         values_from = Data_Value,
                         values_fn = mean)   
    save_tract_df(places_tract, "01_places_tract")
    message("  saved: ", f_places, " (", nrow(places_tract), " tracts)")
  }
} else {
  places_tract <- readRDS(f_places)
  message("  cached: ", f_places, " (", nrow(places_tract), " tracts)")
}

# ---- 1b. ACS 5-year tract data (loop over states) -----------

f_acs <- file.path(dir_proc, "01_acs_tract.rds")

required_acs_derived <- c("pct_age_65_plus", "pct_hispanic",
                           "pct_bachelor_plus", "median_income")
need_pull_acs <- TRUE
f_acs_csv <- file.path(dir_proc, "01_acs_tract.csv")

if (!file.exists(f_acs) && file.exists(f_acs_csv)) {
  message("ACS .rds missing — recovering from .csv backup at ", f_acs_csv)
  recovered <- readr::read_csv(f_acs_csv, show_col_types = FALSE,
                                progress = FALSE) |>
    dplyr::mutate(GEOID = stringr::str_pad(as.character(GEOID),
                                            width = 11, side = "left", pad = "0"))
  saveRDS(recovered, f_acs)
  message("  recovered: ", nrow(recovered), " tracts, ",
          ncol(recovered), " columns")
}
if (file.exists(f_acs)) {
  cached_acs <- readRDS(f_acs)
  missing_acs <- setdiff(required_acs_derived, names(cached_acs))
  if (length(missing_acs) == 0) {
    need_pull_acs <- FALSE
  } else {
    message("ACS cache missing variables: ",
            paste(missing_acs, collapse = ", "), "  → re-pulling.")
  }
}
if (need_pull_acs) {
  state_fips <- tigris::fips_codes |>
    dplyr::distinct(state, state_code) |>
    dplyr::filter(!state %in% c("PR", "AS", "GU", "MP", "UM", "VI")) |>
    dplyr::pull(state_code)

  message("Pulling ACS ", ACS_YEAR - 4, "-", ACS_YEAR,
          " 5-yr data for ", length(state_fips), " states/DC...")
  acs_tract <- purrr::map_dfr(state_fips, function(st) {
    tryCatch(
      tidycensus::get_acs(
        geography = "tract",
        variables = ACS_B_VARS,
        state     = st,
        year      = ACS_YEAR,
        survey    = "acs5",
        output    = "wide",
        cache_table = TRUE
      ),
      error = function(e) {
        message("  ! state ", st, " failed: ", e$message); NULL
      }
    )
  })

  # Derive analysis variables
  acs_tract <- acs_tract |>
    dplyr::mutate(
      pop_age_65_plus = pop_male_65_66E + pop_male_67_69E + pop_male_70_74E +
                        pop_male_75_79E + pop_male_80_84E + pop_male_85_plusE +
                        pop_female_65_66E + pop_female_67_69E + pop_female_70_74E +
                        pop_female_75_79E + pop_female_80_84E + pop_female_85_plusE,
      pct_age_65_plus = 100 * pop_age_65_plus / total_popE,
      pct_hispanic    = 100 * pop_hispanicE   / total_popE,
      pct_black       = 100 * pop_blackE      / total_popE,
      pct_white_nh    = 100 * pop_white_nhE   / total_popE,
      pct_bachelor_plus = 100 * (pop_25_bachelorE + pop_25_masterE +
                                 pop_25_profE + pop_25_doctorE) /
                          dplyr::na_if(pop_25_plusE, 0),
      median_age     = median_ageE,
      median_income  = median_incomeE,
      total_pop      = total_popE
    ) |>
    dplyr::select(GEOID, total_pop, median_age, median_income,
                  pct_age_65_plus, pct_hispanic, pct_black, pct_white_nh,
                  pct_bachelor_plus)

  save_tract_df(acs_tract, "01_acs_tract")
  message("  saved: ", f_acs, " (", nrow(acs_tract), " tracts)")
} else {
  acs_tract <- readRDS(f_acs)
  message("  cached: ", f_acs, " (", nrow(acs_tract), " tracts)")
}

# ---- 1c. Tract geometries (via tidycensus, guaranteed ACS-aligned) ----

# aligned to ACS_YEAR.
TIGRIS_YEAR <- ACS_YEAR    # geometries now align with ACS year by construction
f_geo <- file.path(dir_proc, "01_tracts_geo.rds")
if (!file.exists(f_geo)) {
  message("Pulling tract geometries via tidycensus state-by-state...")
  state_fips_geo <- tigris::fips_codes |>
    dplyr::distinct(state, state_code) |>
    dplyr::filter(!state %in% c("AS", "GU", "MP", "UM", "VI", "PR")) |>
    dplyr::pull(state_code)
  tracts_geo <- purrr::map_dfr(state_fips_geo, function(st) {
    tryCatch(
      tidycensus::get_acs(
        geography = "tract",
        variables = "B01003_001",     # total pop — only need 1 var to get geom
        state     = st,
        year      = ACS_YEAR,
        survey    = "acs5",
        geometry  = TRUE,
        progress_bar = FALSE
      ) |>
        dplyr::transmute(
          GEOID    = as.character(GEOID),
          total_pop_acs = estimate
        ),
      error = function(e) {
        message("  ! state ", st, " failed: ", e$message); NULL
      }
    )
  }) |>
    sf::st_as_sf() |>
    sf::st_transform(CRS_PROJECT)


  tracts_geo <- tracts_geo |>
    dplyr::mutate(
      ALAND  = as.numeric(sf::st_area(geometry)),
      AWATER = 0   
    ) |>
    dplyr::select(GEOID, ALAND, AWATER, geometry)

  saveRDS(tracts_geo, f_geo)
  message("  saved: ", f_geo, " (", nrow(tracts_geo), " features)")
} else {
  tracts_geo <- readRDS(f_geo)
  message("  cached: ", f_geo, " (", nrow(tracts_geo), " features)")
}

# ---- 1d. Join PLACES + ACS on GEOID --------------------------
analytic <- places_tract |>
  dplyr::inner_join(acs_tract, by = "GEOID")

cat("\n--- Analytic dataset summary ---\n")
cat("Tracts in PLACES:        ", nrow(places_tract), "\n")
cat("Tracts in ACS:           ", nrow(acs_tract), "\n")
cat("Tracts in joined set:    ", nrow(analytic), "\n")
cat("Tracts in TIGER geoms:   ", nrow(tracts_geo), "\n")
cat("Drop rate (PLACES → join):",
    round(100 * (1 - nrow(analytic) / nrow(places_tract)), 1), "%\n")


present_measures <- intersect(CKM_MEASURES, names(analytic))
missing_measures <- setdiff(CKM_MEASURES, names(analytic))
cat("CKM measures present:    ", paste(present_measures, collapse = ", "), "\n")
if (length(missing_measures))
  cat("CKM measures MISSING:    ", paste(missing_measures, collapse = ", "), "\n")


state_lookup <- tigris::fips_codes |>
  dplyr::distinct(state, state_code, state_name)
analytic_state <- analytic |>
  dplyr::mutate(state_code = substr(GEOID, 1, 2)) |>
  dplyr::count(state_code, name = "n_analytic")
acs_state <- acs_tract |>
  dplyr::mutate(state_code = substr(GEOID, 1, 2)) |>
  dplyr::count(state_code, name = "n_acs")
state_cov <- state_lookup |>
  dplyr::left_join(analytic_state, by = "state_code") |>
  dplyr::left_join(acs_state, by = "state_code") |>
  dplyr::mutate(coverage = round(100 * n_analytic / n_acs, 1)) |>
  dplyr::arrange(coverage)

cat("\n--- State-level coverage (lowest 10 states) ---\n")
print(head(state_cov[, c("state", "state_name", "n_acs", "n_analytic", "coverage")], 10))
cat("--- States with 0 analytic tracts (PLACES gap) ---\n")
gaps <- state_cov |> dplyr::filter(is.na(n_analytic) | n_analytic == 0)
if (nrow(gaps) > 0) print(gaps[, c("state", "state_name", "n_acs")]) else cat("  None.\n")

save_tract_df(analytic, "01_analytic_base")


# ==============================================================
# 1e. SDOH LAYERS  (ICE_Race, ICE_Income, RUCA)
# ==============================================================



# ---- 1e-i. ICE_Race + ICE_Income from ACS -------------------

f_ice <- file.path(dir_proc, "01e_ice_tract.rds")
if (!file.exists(f_ice)) {
  message("Computing ICE_Race + ICE_Income from ACS B-tables...")
  ICE_VARS <- c(
    pop_total_race    = "B03002_001",  # Total pop by race/ethnicity
    pop_white_nh      = "B03002_003",  # Non-Hispanic White
    pop_black_nh      = "B03002_004",  # Non-Hispanic Black
    hh_total          = "B19001_001",  # Total households (income denominator)
    # Low-income brackets: <$25,000 (sum 6 brackets <$25k)
    hh_inc_lt10k      = "B19001_002",
    hh_inc_10_15k     = "B19001_003",
    hh_inc_15_20k     = "B19001_004",
    hh_inc_20_25k     = "B19001_005",
    # High-income brackets: >=$100,000 (sum 5 brackets >=$100k)
    hh_inc_100_125k   = "B19001_014",
    hh_inc_125_150k   = "B19001_015",
    hh_inc_150_200k   = "B19001_016",
    hh_inc_200k_plus  = "B19001_017"
  )
  state_fips_codes <- tigris::fips_codes |>
    dplyr::distinct(state, state_code) |>
    dplyr::filter(!state %in% c("PR", "AS", "GU", "MP", "UM", "VI")) |>
    dplyr::pull(state_code)

  ice_raw <- purrr::map_dfr(state_fips_codes, function(st) {
    tryCatch(
      tidycensus::get_acs(geography = "tract", variables = ICE_VARS,
                          state = st, year = ACS_YEAR, survey = "acs5",
                          output = "wide", cache_table = TRUE),
      error = function(e) { message("  ! state ", st, ": ", e$message); NULL }
    )
  })

  ice_tract <- ice_raw |>
    dplyr::mutate(
      ice_race = (pop_white_nhE - pop_black_nhE) /
                  dplyr::na_if(pop_total_raceE, 0),
      hh_low   = hh_inc_lt10kE + hh_inc_10_15kE + hh_inc_15_20kE + hh_inc_20_25kE,
      hh_high  = hh_inc_100_125kE + hh_inc_125_150kE + hh_inc_150_200kE +
                 hh_inc_200k_plusE,
      ice_income = (hh_high - hh_low) / dplyr::na_if(hh_totalE, 0)
    ) |>
    dplyr::select(GEOID, ice_race, ice_income) |>
    dplyr::mutate(GEOID = as.character(GEOID))

  save_tract_df(ice_tract, "01e_ice_tract")
  message("  saved: ", f_ice, " (", nrow(ice_tract), " tracts)")
} else {
  ice_tract <- readRDS(f_ice)
  message("  cached: ", f_ice, " (", nrow(ice_tract), " tracts)")
}

# ---- 1e-iii. RUCA codes (USDA tract-level CSV) ---------------
# USDA 2010 RUCA codes — tract-level rural-urban classification.
# Source: https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes
# (Schema: 1-3 = Metro, 4-6 = Micropolitan, 7-9 = Small town, 10 = Rural)
f_ruca <- file.path(dir_proc, "01e_ruca_tract.rds")
if (!file.exists(f_ruca)) {
  ruca_url <- "https://www.ers.usda.gov/sites/default/files/_laserfiche/DataFiles/53241/ruca2010revised.xlsx"
  ruca_local <- file.path(dir_raw, "ruca2010revised.xlsx")
  if (!file.exists(ruca_local)) {
    message("Downloading USDA RUCA 2010 codes...")
    tryCatch(
      utils::download.file(ruca_url, ruca_local, mode = "wb", quiet = TRUE),
      error = function(e) {
        message("  ! RUCA download failed: ", e$message)
      }
    )
  }
  if (file.exists(ruca_local)) {
    if (!"readxl" %in% installed.packages()[, "Package"])
      install.packages("readxl", repos = "https://cloud.r-project.org")
    # Try multiple sheet names + skip values; USDA changes formatting
    ruca_raw <- NULL
    for (sheet_try in c("Data", 1)) {
      for (skip_try in c(1, 0, 2)) {
        ruca_raw <- tryCatch(
          readxl::read_excel(ruca_local, sheet = sheet_try, skip = skip_try),
          error = function(e) NULL
        )
        if (!is.null(ruca_raw) && ncol(ruca_raw) >= 4) break
      }
      if (!is.null(ruca_raw) && ncol(ruca_raw) >= 4) break
    }

    if (!is.null(ruca_raw) && ncol(ruca_raw) >= 4) {
      message("  RUCA columns detected: ",
              paste(head(names(ruca_raw), 8), collapse = " | "))

      nm <- tolower(names(ruca_raw))
      geoid_idx <- which(grepl("tract", nm) & grepl("fips|geoid", nm))[1]
      if (is.na(geoid_idx)) geoid_idx <- which(grepl("fips", nm))[1]
      ruca_idx  <- which(grepl("^primary", nm) | grepl("ruca.*2010", nm) |
                          (grepl("ruca", nm) & !grepl("secondary", nm)))[1]

      if (!is.na(geoid_idx) && !is.na(ruca_idx)) {
        ruca_tract <- tibble::tibble(
          GEOID = as.character(ruca_raw[[geoid_idx]]) |>
                  stringr::str_remove_all("[^0-9]"),
          ruca_primary = suppressWarnings(as.numeric(ruca_raw[[ruca_idx]]))
        ) |>
          dplyr::filter(nchar(GEOID) == 11, !is.na(ruca_primary)) |>
          dplyr::mutate(
            ruca_class = dplyr::case_when(
              ruca_primary %in% 1:3  ~ "Metro",
              ruca_primary %in% 4:6  ~ "Micropolitan",
              ruca_primary %in% 7:9  ~ "Small town",
              ruca_primary == 10     ~ "Rural",
              TRUE                   ~ NA_character_
            )
          )
        save_tract_df(ruca_tract, "01e_ruca_tract")
        message("  saved: ", f_ruca, " (", nrow(ruca_tract), " tracts)")
      } else {
        message("  ! Could not find FIPS/RUCA columns in RUCA file. Columns: ",
                paste(names(ruca_raw), collapse = " | "))
        ruca_tract <- tibble::tibble(GEOID = character(0),
                                     ruca_primary = numeric(0),
                                     ruca_class = character(0))
      }
    } else {
      message("  ! Could not parse RUCA xlsx; ruca_class will be NA.")
      ruca_tract <- tibble::tibble(GEOID = character(0),
                                   ruca_primary = numeric(0),
                                   ruca_class = character(0))
    }
  } else {
    message("  ! RUCA file not available; ruca_class will be NA.")
    ruca_tract <- tibble::tibble(GEOID = character(0),
                                 ruca_primary = numeric(0),
                                 ruca_class = character(0))
  }
} else {
  ruca_tract <- readRDS(f_ruca)
  message("  cached: ", f_ruca, " (", nrow(ruca_tract), " tracts)")
}

# ---- 1e-iv. CDC/ATSDR Social Vulnerability Index (SVI) ------

f_svi <- file.path(dir_proc, "01e_svi_tract.rds")
if (!file.exists(f_svi)) {
  svi_csvs <- list.files(dir_raw,
    pattern = "SVI.*2020.*\\.csv$",
    full.names = TRUE, ignore.case = TRUE)
  if (length(svi_csvs) > 0) {
    message("SVI 2020 CSV found: ", basename(svi_csvs[1]))
    svi_raw <- tryCatch(
      readr::read_csv(svi_csvs[1], show_col_types = FALSE,
                      progress = FALSE, guess_max = 1e5),
      error = function(e) {
        message("  ! SVI CSV parse failed: ", e$message); NULL
      })
    if (!is.null(svi_raw)) {
      svi_geo_col <- intersect(c("FIPS", "GEOID", "TRACT", "Tract", "fips"),
                                names(svi_raw))[1]
      svi_keep <- intersect(c("RPL_THEMES", "RPL_THEME1", "RPL_THEME2",
                              "RPL_THEME3", "RPL_THEME4"), names(svi_raw))
      if (!is.na(svi_geo_col) && length(svi_keep) > 0) {
        svi_tract <- svi_raw |>
          dplyr::select(GEOID = !!svi_geo_col,
                        dplyr::all_of(svi_keep)) |>
          # Pad GEOID and replace -999 sentinel with NA
          dplyr::mutate(GEOID = stringr::str_pad(
            as.character(GEOID), width = 11, side = "left", pad = "0")) |>
          dplyr::mutate(dplyr::across(dplyr::all_of(svi_keep),
            ~ ifelse(.x < 0, NA_real_, as.numeric(.x))))
        # Rename to lowercase analytic-friendly names
        rename_map <- c(svi_overall = "RPL_THEMES",
                        svi_ses     = "RPL_THEME1",
                        svi_house   = "RPL_THEME2",
                        svi_minor   = "RPL_THEME3",
                        svi_trans   = "RPL_THEME4")
        rename_map <- rename_map[rename_map %in% names(svi_tract)]
        svi_tract <- svi_tract |>
          dplyr::rename(!!!rlang::set_names(rename_map, names(rename_map)))
        save_tract_df(svi_tract, "01e_svi_tract")
        message("  saved: ", f_svi, " (", nrow(svi_tract), " tracts; ",
                length(svi_keep), " themes)")
      } else {
        message("  ! SVI CSV missing expected columns. Available: ",
                paste(head(names(svi_raw), 20), collapse = ", "))
        svi_tract <- tibble::tibble(GEOID = character(0))
      }
    } else {
      svi_tract <- tibble::tibble(GEOID = character(0))
    }
  } else {
    message("  ! No SVI 2020 CSV found in ", dir_raw,
            "\n    Download from https://www.atsdr.cdc.gov/place-health/php/svi/",
            "\n    (Census Tract → 2020 → United States → CSV) and save to data/raw/")
    svi_tract <- tibble::tibble(GEOID = character(0))
  }
} else {
  svi_tract <- readRDS(f_svi)
  message("  cached: ", f_svi, " (", nrow(svi_tract), " tracts)")
}

# ---- 1e-v. Join SDOH layers onto analytic --------------------
analytic <- analytic |>
  dplyr::left_join(ice_tract,  by = "GEOID") |>
  dplyr::left_join(ruca_tract |>
                     dplyr::select(GEOID, ruca_primary, ruca_class),
                   by = "GEOID") |>
  dplyr::left_join(svi_tract,  by = "GEOID")

cat("\n--- SDOH coverage on analytic dataset ---\n")
cat("  ICE_Race:      ",
    sum(!is.na(analytic$ice_race)),  " / ", nrow(analytic), "\n")
cat("  ICE_Income:    ",
    sum(!is.na(analytic$ice_income))," / ", nrow(analytic), "\n")
cat("  RUCA primary:  ",
    sum(!is.na(analytic$ruca_primary))," / ", nrow(analytic), "\n")
if ("svi_overall" %in% names(analytic))
  cat("  SVI overall:   ",
      sum(!is.na(analytic$svi_overall))," / ", nrow(analytic), "\n")

save_tract_df(analytic, "01e_analytic_with_sdoh")


# ==============================================================
# 1f. ENVIRONMENTAL LAYERS  (air, food)
# ==============================================================

bg_to_tract <- function(df, value_cols, geoid_col = "GEOID",
                        weight_col = "ACSTOTPOP") {
  df |>
    dplyr::mutate(tract_GEOID = substr(.data[[geoid_col]], 1, 11),
                  w = .data[[weight_col]]) |>
    dplyr::group_by(tract_GEOID) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(value_cols),
                                   ~ stats::weighted.mean(.x, w = w,
                                                          na.rm = TRUE)),
                     .groups = "drop") |>
    dplyr::rename(GEOID = tract_GEOID)
}


safe_download <- function(url, dest, timeout_s = 600) {
  if (file.exists(dest) && file.size(dest) > 1000) return(TRUE)
  ok <- tryCatch({
    curl::curl_download(url, dest, mode = "wb", quiet = FALSE,
                        handle = curl::new_handle(timeout = timeout_s,
                                                   followlocation = TRUE))
    TRUE
  }, error = function(e) {
    message("    curl failed: ", e$message, " — falling back to wininet")
    FALSE
  })
  if (!ok || !file.exists(dest) || file.size(dest) < 1000) {
    if (file.exists(dest)) file.remove(dest)
    ok <- tryCatch({
      old <- options(timeout = timeout_s)
      on.exit(options(old))
      utils::download.file(url, dest, mode = "wb", quiet = FALSE,
                           method = "wininet")
      TRUE
    }, error = function(e) {
      message("    wininet failed: ", e$message); FALSE
    })
  }
  ok && file.exists(dest) && file.size(dest) > 1000
}


download_first_working <- function(urls, dest, expect_ext = "zip",
                                   min_size_mb = 0.1) {
  if (file.exists(dest) && file.size(dest) > min_size_mb * 1e6)
    return(dest)
  for (u in urls) {
    message("  trying: ", u)
    ok <- tryCatch({
      utils::download.file(u, dest, mode = "wb", quiet = TRUE, timeout = 900)
      TRUE
    }, error = function(e) {
      message("    failed: ", e$message); FALSE
    }, warning = function(w) {
      message("    warning: ", conditionMessage(w))
      file.exists(dest) && file.size(dest) > min_size_mb * 1e6
    })
    if (ok && file.exists(dest) && file.size(dest) > min_size_mb * 1e6) {
      # Sanity-check that it's the expected file type
      first_bytes <- readBin(dest, "raw", n = 4)
      is_zip <- expect_ext == "zip" &&
                identical(first_bytes[1:2], as.raw(c(0x50, 0x4B)))
      is_xls <- expect_ext == "xlsx" &&
                identical(first_bytes[1:2], as.raw(c(0x50, 0x4B)))
      is_html <- identical(first_bytes[1:4], charToRaw("<!DO")) ||
                 identical(first_bytes[1:4], charToRaw("<htm")) ||
                 identical(first_bytes[1:4], charToRaw("<HTM"))
      if (is_html) {
        message("    got HTML (not data); trying next URL")
        file.remove(dest)
        next
      }
      if (expect_ext %in% c("zip", "xlsx") && !is_zip && !is_xls) {
        message("    got non-zip/xlsx file; trying next URL")
        file.remove(dest)
        next
      }
      message("    OK (", round(file.size(dest) / 1e6, 1), " MB)")
      return(dest)
    }
  }
  NA_character_
}

# ---- 1f-i. CDC/ATSDR SVI —

svi_tract <- tibble::tibble(GEOID = character(0))


AQ_YEAR <- 2020
f_aq    <- file.path(dir_proc, "01f_aq_tract.rds")


RSIG_PM25_FILE <- file.path(dir_raw,
  paste0(AQ_YEAR, "_pm25_daily_average_2010_census.txt.gz"))
RSIG_O3_FILE   <- file.path(dir_raw,
  paste0(AQ_YEAR, "_ozone_daily_8hour_maximum_2010_census.txt.gz"))
have_rsig <- file.exists(RSIG_PM25_FILE) && file.exists(RSIG_O3_FILE)


if (file.exists(f_aq)) {
  cached_aq <- tryCatch(readRDS(f_aq), error = function(e) NULL)
  invalidate_reason <- NULL
  if (!is.null(cached_aq) &&
      "pm25_annual" %in% names(cached_aq) &&
      "o3_8hrmax_4thmax" %in% names(cached_aq)) {
    pm25_cov <- mean(!is.na(cached_aq$pm25_annual))
    o3_cov   <- mean(!is.na(cached_aq$o3_8hrmax_4thmax))
    cached_method <- attr(cached_aq, "method") %||% "unknown"
    if (pm25_cov < 0.9 || o3_cov < 0.9) {
      invalidate_reason <- sprintf(
        "degenerate fit (PM2.5 cov = %.1f%%, O3 cov = %.1f%%)",
        100 * pm25_cov, 100 * o3_cov)
    } else if (have_rsig && !grepl("rsig", cached_method)) {
      invalidate_reason <- paste0(
        "RSIG Downscaler files now available; upgrading from ",
        "'", cached_method, "' to RSIG-based exposure surface.")
    }
  } else if (is.null(cached_aq)) {
    invalidate_reason <- "unreadable cache file"
  } else {
    invalidate_reason <- "missing expected columns"
  }
  if (!is.null(invalidate_reason)) {
    message("AQ cache invalidated (", invalidate_reason,
            "); deleting to trigger fresh build.")
    file.remove(f_aq)
    csv_path <- sub("\\.rds$", ".csv", f_aq)
    if (file.exists(csv_path)) file.remove(csv_path)
  }
}

# ---- 1f-ii.A. RSIG Downscaler (preferred path) ---------------

if (!file.exists(f_aq) && have_rsig) {
  for (p in c("data.table", "R.utils")) {     # R.utils = gzip backend for fread
    if (!p %in% installed.packages()[, "Package"])
      install.packages(p, repos = "https://cloud.r-project.org")
  }

  message("Reading RSIG Downscaler PM2.5 daily file (", AQ_YEAR, ")...")
  t0 <- Sys.time()
  pm25_dt <- data.table::fread(
    file = RSIG_PM25_FILE,
    select = c(2, 5, 6),
    col.names = c("FIPS", "pm25", "pm25_se"),
    showProgress = FALSE,
    colClasses = c(FIPS = "character"))
  message("  read ", nrow(pm25_dt), " rows in ",
          round(difftime(Sys.time(), t0, units = "secs"), 0), " sec")

  pm25_dt[, FIPS := stringr::str_pad(FIPS, 11, "left", "0")]
  pm25_tract_rsig <- pm25_dt[, .(
    pm25_annual = mean(pm25, na.rm = TRUE),
    pm25_se     = mean(pm25_se, na.rm = TRUE),
    n_days_pm25 = .N
  ), by = FIPS]
  data.table::setnames(pm25_tract_rsig, "FIPS", "GEOID")
  message("  PM2.5 aggregated to ", nrow(pm25_tract_rsig), " tracts ",
          "(typical n_days = ",
          stats::median(pm25_tract_rsig$n_days_pm25), ")")
  rm(pm25_dt); invisible(gc(verbose = FALSE))

  message("Reading RSIG Downscaler O3 daily file (", AQ_YEAR, ")...")
  t1 <- Sys.time()
  o3_dt <- data.table::fread(
    file = RSIG_O3_FILE,
    select = c(2, 5, 6),
    col.names = c("FIPS", "o3_ppb", "o3_se_ppb"),
    showProgress = FALSE,
    colClasses = c(FIPS = "character"))
  message("  read ", nrow(o3_dt), " rows in ",
          round(difftime(Sys.time(), t1, units = "secs"), 0), " sec")

  o3_dt[, FIPS := stringr::str_pad(FIPS, 11, "left", "0")]
  # 4th-highest daily 8-hr max (NAAQS form). Convert ppb → ppm.
  o3_tract_rsig <- o3_dt[, .(
    o3_8hrmax_4thmax = {
      v <- sort(o3_ppb[!is.na(o3_ppb)], decreasing = TRUE)
      if (length(v) >= 4) v[4] / 1000 else NA_real_
    },
    o3_se     = mean(o3_se_ppb, na.rm = TRUE) / 1000,
    n_days_o3 = .N
  ), by = FIPS]
  data.table::setnames(o3_tract_rsig, "FIPS", "GEOID")
  message("  O3 aggregated to ", nrow(o3_tract_rsig), " tracts ",
          "(typical n_days = ",
          stats::median(o3_tract_rsig$n_days_o3), ")")
  rm(o3_dt); invisible(gc(verbose = FALSE))

  aq_tract <- tibble::as_tibble(pm25_tract_rsig) |>
    dplyr::select(GEOID, pm25_annual, pm25_se) |>
    dplyr::full_join(
      tibble::as_tibble(o3_tract_rsig) |>
        dplyr::select(GEOID, o3_8hrmax_4thmax, o3_se),
      by = "GEOID"
    ) |>
    dplyr::mutate(GEOID = as.character(GEOID))

  attr(aq_tract, "method")     <- "rsig_downscaler"
  attr(aq_tract, "source")     <- "EPA RSIG Downscaler (CMAQ + monitor fusion)"
  attr(aq_tract, "ref")        <- "Berrocal, Gelfand & Holland, JABES 2010"
  attr(aq_tract, "units")      <- list(pm25 = "ug/m3", o3 = "ppm")
  attr(aq_tract, "year")       <- AQ_YEAR

  save_tract_df(aq_tract, "01f_aq_tract")
  message("  saved: ", f_aq, " (", nrow(aq_tract),
          " tracts | RSIG Downscaler)")
}

# ---- 1f-ii.B. AirData IDW (fallback path) --------------------
if (!file.exists(f_aq)) {
  message("Pulling EPA AirData annual monitor concentrations (year ",
          AQ_YEAR, ")...")

  ad_url <- sprintf(
    "https://aqs.epa.gov/aqsweb/airdata/annual_conc_by_monitor_%d.zip",
    AQ_YEAR)
  ad_zip <- file.path(dir_raw, sprintf("airdata_annual_%d.zip", AQ_YEAR))

  # Single small zip (~5 MB) → fast bandwidth-limited download
  if (!file.exists(ad_zip) || file.size(ad_zip) < 1e6) {
    message("  downloading ", ad_url)
    ok <- safe_download(ad_url, ad_zip, timeout_s = 600)
  } else {
    message("  cached: ", ad_zip, " (",
            round(file.size(ad_zip) / 1e6, 1), " MB)")
    ok <- TRUE
  }

  aq_tract <- tibble::tibble(GEOID = character(0))

  if (ok && file.exists(ad_zip) && file.size(ad_zip) > 1e6) {
    tmpdir <- tempfile("airdata_"); dir.create(tmpdir)
    files  <- utils::unzip(ad_zip, exdir = tmpdir)
    csv    <- files[grepl("\\.csv$", files, ignore.case = TRUE)][1]

    if (!is.na(csv)) {
      ad <- readr::read_csv(csv, show_col_types = FALSE,
                            progress = FALSE, guess_max = 1e5)
      message("  AirData rows: ", nrow(ad), " | cols: ",
              ncol(ad))

      # PM2.5 (88101) annual mean — keep monitors with full-year coverage

      pm25_mon <- ad |>
        dplyr::filter(`Parameter Code` == 88101,
                      `Sample Duration` == "24 HOUR" |
                      `Sample Duration` == "24-HR BLK AVG",
                      `Pollutant Standard` %in% c("PM25 Annual 2012",
                                                   "PM25 Annual 2024") |
                      grepl("Annual", `Pollutant Standard`),
                      `Observation Count` >= 50,
                      !is.na(`Arithmetic Mean`)) |>
        dplyr::transmute(
          monitor_id = paste(`State Code`, `County Code`,
                             `Site Num`,    `POC`, sep = "."),
          lon = Longitude, lat = Latitude,
          pm25 = as.numeric(`Arithmetic Mean`)) |>
        dplyr::group_by(monitor_id) |>
        dplyr::summarise(lon = mean(lon), lat = mean(lat),
                         pm25 = mean(pm25, na.rm = TRUE),
                         .groups = "drop") |>
        dplyr::filter(!is.na(pm25), is.finite(pm25))

      # O3 (44201) — 4th-highest daily 8-hr max is reported directly

      o3_mon <- ad |>
        dplyr::filter(`Parameter Code` == 44201,
                      grepl("8-hour", `Pollutant Standard`,
                            ignore.case = TRUE),
                      !is.na(`4th Max Value`)) |>
        dplyr::transmute(
          monitor_id = paste(`State Code`, `County Code`,
                             `Site Num`,    `POC`, sep = "."),
          lon = Longitude, lat = Latitude,
          o3_4thmax = as.numeric(`4th Max Value`)) |>
        dplyr::group_by(monitor_id) |>
        dplyr::summarise(lon = mean(lon), lat = mean(lat),
                         o3_4thmax = max(o3_4thmax, na.rm = TRUE),
                         .groups = "drop") |>
        dplyr::filter(!is.na(o3_4thmax), is.finite(o3_4thmax))

      message("  PM2.5 monitors (year ", AQ_YEAR, "): ", nrow(pm25_mon))
      message("  O3 monitors    (year ", AQ_YEAR, "): ", nrow(o3_mon))


      if (!"gstat" %in% installed.packages()[, "Package"])
        install.packages("gstat", repos = "https://cloud.r-project.org")


      tract_geo_clean <- tracts_geo |>
        dplyr::filter(!sf::st_is_empty(geometry))
      tract_centroids <- tract_geo_clean |>
        sf::st_transform(5070) |>          
        sf::st_centroid(of_largest_polygon = TRUE)
      tc_xy <- sf::st_coordinates(tract_centroids)
      tc_df <- tibble::tibble(GEOID = tract_centroids$GEOID,
                              x = tc_xy[, 1], y = tc_xy[, 2]) |>
        dplyr::filter(!is.na(x), !is.na(y), is.finite(x), is.finite(y))
      message("  tract centroids: ", nrow(tc_df), " valid (",
              nrow(tracts_geo) - nrow(tc_df), " dropped — empty/invalid)")

      project_pts <- function(df_lonlat) {
        sf::st_as_sf(df_lonlat, coords = c("lon", "lat"), crs = 4326) |>
          sf::st_transform(5070)
      }
      pm25_sf <- project_pts(pm25_mon)
      o3_sf   <- project_pts(o3_mon)

      tracts_sf <- sf::st_as_sf(tc_df, coords = c("x", "y"), crs = 5070)

      krige_to_tracts <- function(monitors_sf, value_col,
                                    nmax_krige = 50, nmax_idw = 10,
                                    coverage_thresh = 0.9) {
        message("  Interpolating ", value_col, " to tracts...")
        t0  <- Sys.time()
        fml <- stats::as.formula(paste(value_col, "~ 1"))

        # ---- Step 1:ordinary kriging ----

        vals <- monitors_sf[[value_col]]
        v0   <- stats::var(vals, na.rm = TRUE)
        v_init <- gstat::vgm(psill = 0.8 * v0, model = "Sph",
                              range = 500000, nugget = 0.2 * v0)
        krige_attempt <- suppressWarnings(tryCatch({
          vgm_emp <- gstat::variogram(fml, locations = monitors_sf)
          vgm_fit <- gstat::fit.variogram(vgm_emp, model = v_init,
                                           fit.method = 7,
                                           fit.ranges = TRUE,
                                           fit.sills = TRUE)

          if (any(vgm_fit$range > 5e6)) vgm_fit <- v_init
          pred <- gstat::krige(fml, locations = monitors_sf,
                                newdata = tracts_sf, model = vgm_fit,
                                nmax = nmax_krige, debug.level = 0)
          list(pred = pred$var1.pred, var = pred$var1.var,
               vgm_emp = vgm_emp, vgm_fit = vgm_fit)
        }, error = function(e) {
          message("    ! Kriging error: ", e$message)
          NULL
        }))

        # ---- Step 2: check coverage; 
        krige_cov <- if (is.null(krige_attempt)) 0 else
                       mean(!is.na(krige_attempt$pred))
        method_used <- "kriging (ordinary, spherical variogram)"

        if (krige_cov < coverage_thresh) {
          message("    Kriging coverage = ",
                  round(100 * krige_cov, 1),
                  "% < ", round(100 * coverage_thresh), "%; falling back to IDW.")
          idw_pred <- suppressWarnings(gstat::idw(
            fml, locations = monitors_sf, newdata = tracts_sf,
            nmax = nmax_idw, idp = 2, debug.level = 0))
          method_used <- "inverse-distance weighting (k=10, p=2)"
          out <- list(pred    = idw_pred$var1.pred,
                      var     = rep(NA_real_, length(idw_pred$var1.pred)),
                      vgm_emp = if (!is.null(krige_attempt))
                                  krige_attempt$vgm_emp else NULL,
                      vgm_fit = if (!is.null(krige_attempt))
                                  krige_attempt$vgm_fit else NULL,
                      method  = method_used)
        } else {
          out <- c(krige_attempt, list(method = method_used))
        }

        message("    method: ", method_used,
                " | n non-NA pred = ", sum(!is.na(out$pred)),
                " | done in ", round(difftime(Sys.time(), t0,
                                               units = "secs"), 1), " sec")
        out
      }

      pm25_kr <- krige_to_tracts(pm25_sf, "pm25")
      o3_kr   <- krige_to_tracts(o3_sf,   "o3_4thmax")

      # Save fitted variograms (or NULLs if IDW fallback) for manuscript Methods
      saveRDS(list(pm25 = list(emp = pm25_kr$vgm_emp,
                               fit = pm25_kr$vgm_fit,
                               method = pm25_kr$method),
                   o3   = list(emp = o3_kr$vgm_emp,
                               fit = o3_kr$vgm_fit,
                               method = o3_kr$method)),
              file.path(dir_proc, "01f_aq_variograms.rds"))

      aq_tract <- tibble::tibble(
        GEOID                = tc_df$GEOID,
        pm25_annual          = pm25_kr$pred,
        pm25_kriging_var     = pm25_kr$var,
        o3_8hrmax_4thmax     = o3_kr$pred,
        o3_kriging_var       = o3_kr$var
      )
      attr(aq_tract, "pm25_method") <- pm25_kr$method
      attr(aq_tract, "o3_method")   <- o3_kr$method
      save_tract_df(aq_tract, "01f_aq_tract")
      message("  saved: ", f_aq, " (", nrow(aq_tract), " tracts; ",
              sum(!is.na(aq_tract$pm25_annual)), " w/ PM2.5 [",
              pm25_kr$method, "]; ",
              sum(!is.na(aq_tract$o3_8hrmax_4thmax)), " w/ O3 [",
              o3_kr$method, "])")
    } else {
      message("  ! No CSV in AirData zip.")
    }
  } else {
    message("  ! AirData download failed.\n",
            "    Manual fallback: visit https://aqs.epa.gov/aqsweb/airdata/\n",
            "    download annual_conc_by_monitor_", AQ_YEAR,
            ".zip and place at:\n      ", ad_zip, "\n    then re-source.")
  }
} else {
  aq_tract <- readRDS(f_aq)
  message("  cached: ", f_aq, " (", nrow(aq_tract), " tracts)")
}

# ---- 1f-iii. Walkability (EPA Smart Location Database V3) --------

WALK_MIN_TRACTS <- 60000L
f_walk <- file.path(dir_proc, "01f_walk_tract.rds")
# Rebuild the cache if a saved RDS was built from a truncated source CSV.
if (file.exists(f_walk) && nrow(readRDS(f_walk)) < WALK_MIN_TRACTS) {
  message("  ! cached walkability RDS has only ", nrow(readRDS(f_walk)),
          " tracts (<", WALK_MIN_TRACTS, ") - rebuilding from source.")
  unlink(f_walk)
}
if (!file.exists(f_walk)) {
  message("Building EPA SLD walkability (block group -> tract, pop-weighted)...")
  walk_tract <- tryCatch({
    dir_sld  <- file.path(dir_raw, "epa_sld")
    dir.create(dir_sld, showWarnings = FALSE, recursive = TRUE)
    sld_csv  <- file.path(dir_sld, "SLD_V3.csv")
    sld_url  <- "https://edg.epa.gov/EPADataCommons/public/OA/EPA_SmartLocationDatabase_V3_Jan_2021_Final.csv"
    # The full CSV is ~200 MB; a truncated cache is far smaller. Purge and
    # re-download anything implausibly small so a partial file is never reused.
    if (file.exists(sld_csv) && file.info(sld_csv)$size < 5e7) {
      message("  ! cached SLD CSV only ", file.info(sld_csv)$size,
              " bytes (truncated) - deleting and re-downloading.")
      unlink(sld_csv)
    }

    old_timeout <- getOption("timeout")
    options(timeout = max(old_timeout, 1800))   # 30 min ceiling for 200 MB
    attempt <- 0L
    while ((!file.exists(sld_csv) || file.info(sld_csv)$size < 5e7) &&
           attempt < 3L) {
      attempt <- attempt + 1L
      try(utils::download.file(sld_url, sld_csv, mode = "wb", quiet = TRUE),
          silent = TRUE)
      if (file.exists(sld_csv) && file.info(sld_csv)$size < 5e7) {
        unlink(sld_csv); Sys.sleep(5 * attempt)
      }
    }
    options(timeout = old_timeout)   
    if (!file.exists(sld_csv) || file.info(sld_csv)$size < 5e7)
      stop("EPA SLD walkability CSV did not download completely after 3 ",
           "attempts. Re-run or fetch it manually to ", sld_csv,
           " . Walkability must not be silently truncated.")
    sld <- readr::read_csv(
      sld_csv,
      col_select = c("STATEFP", "COUNTYFP", "TRACTCE", "TotPop", "NatWalkInd"),
      col_types  = readr::cols(STATEFP = "i", COUNTYFP = "i", TRACTCE = "i",
                               TotPop = "d", NatWalkInd = "d"),
      progress = FALSE)
    wt <- sld |>
      dplyr::mutate(
        GEOID      = paste0(sprintf("%02d", STATEFP),
                            sprintf("%03d", COUNTYFP),
                            sprintf("%06d", TRACTCE)),
        NatWalkInd = ifelse(NatWalkInd < 0, NA_real_, NatWalkInd),
        TotPop     = ifelse(is.na(TotPop) | TotPop < 0, 0, TotPop)) |>
      dplyr::filter(!is.na(NatWalkInd)) |>
      dplyr::group_by(GEOID) |>
      dplyr::summarise(
        walk_index = if (sum(TotPop) > 0)
                       stats::weighted.mean(NatWalkInd, w = TotPop, na.rm = TRUE)
                     else mean(NatWalkInd, na.rm = TRUE),
        .groups = "drop") |>
      dplyr::mutate(walk_index = round(walk_index, 3))
    if (nrow(wt) < WALK_MIN_TRACTS) {
      unlink(sld_csv)   
      stop("EPA SLD walkability built only ", nrow(wt), " tracts (<",
           WALK_MIN_TRACTS, "); source CSV was truncated (now deleted). ",
           "Re-run to re-download. Walkability must not be silently truncated.")
    }
    save_tract_df(wt, "01f_walk_tract")
    message("  saved: ", f_walk, " (", nrow(wt), " tracts)")
    wt
  }, error = function(e) {
    message("  !!! SLD walkability build FAILED: ", e$message)
    message("  !!! WALKABILITY (walk_index) WILL BE MISSING/PARTIAL - this is a")
    message("  !!! DEGRADED run and will NOT reproduce the manuscript's n=69,530")
    message("  !!! results. Fix the SLD download and re-run before using numbers.")
    tibble::tibble(GEOID = character(0))
  })
} else {
  walk_tract <- readRDS(f_walk)
  message("  cached: ", f_walk, " (", nrow(walk_tract), " tracts)")
}

# ---- 1f-iv. Heat (PRISM warm-season Tmax, May-Sep 2019-2020) --------

f_heat <- file.path(dir_proc, "01f_heat_warm_tract.rds")
if (!file.exists(f_heat)) {
  message("Building PRISM warm-season (May-Sep) Tmax (2019-2020) -> tracts...")
  heat_tract <- tryCatch({
    dir_prism <- file.path(dir_raw, "prism")
    dir.create(dir_prism, showWarnings = FALSE, recursive = TRUE)
    # Helper: TRUE if 'zf' is a real, non-truncated zip archive. The NACSE
    # PRISM public service throttles repeated pulls and, when throttled,
    # returns a small HTML notice (a few hundred bytes) *with a .zip name*.
    # A valid 4-km CONUS Tmax zip is several MB and its central directory
    # lists >=1 .tif entry; either check fails on a poisoned/HTML file.
    .valid_zip <- function(zf) {
      if (!file.exists(zf) || file.info(zf)$size < 1e5) return(FALSE)
      ok <- tryCatch(nrow(utils::unzip(zf, list = TRUE)) > 0,
                     error = function(e) FALSE, warning = function(e) FALSE)
      isTRUE(ok) &&
        any(grepl("\\.(tif|bil)$",
                  tryCatch(utils::unzip(zf, list = TRUE)$Name,
                           error = function(e) character(0)),
                  ignore.case = TRUE))
    }
    # Warm season = May (05) through September (09), both study years.
    ym <- character(0)
    for (yr in c(2019, 2020))
      for (mo in 5:9) ym <- c(ym, sprintf("%d%02d", yr, mo))
    tif <- character(0)
    for (ymi in ym) {
      zf  <- file.path(dir_prism, paste0("tmax_", ymi, ".zip"))
      dst <- file.path(dir_prism, paste0("tmax_", ymi))

      if (file.exists(zf) && !.valid_zip(zf)) {
        message("  ! cached PRISM zip invalid (", file.info(zf)$size,
                " bytes) - deleting and re-downloading: ", basename(zf))
        unlink(zf)
      }
     
      attempt <- 0L
      while (!.valid_zip(zf) && attempt < 3L) {
        attempt <- attempt + 1L
        try(utils::download.file(
          sprintf("https://services.nacse.org/prism/data/get/us/4km/tmax/%s", ymi),
          zf, mode = "wb", quiet = TRUE), silent = TRUE)
        if (!.valid_zip(zf)) { unlink(zf); Sys.sleep(5 * attempt) }
      }
      if (!.valid_zip(zf))
        stop("PRISM ", ymi, " monthly Tmax download did not return a valid ",
             "zip after 3 attempts (NACSE throttles repeated pulls). Wait a ",
             "few hours and re-run, or download the ", ymi, " 4km monthly ",
             "Tmax zip manually to ", zf, " . Heat must not be silently dropped.")
      unlink(dst, recursive = TRUE)          # clean extract dir
      utils::unzip(zf, exdir = dst)
      mo_tif <- list.files(dst, pattern = "\\.(tif|bil)$",
                           full.names = TRUE, ignore.case = TRUE)
      if (!length(mo_tif))
        stop("PRISM ", ymi, " zip extracted but contained no raster (.tif/.bil).")
      tif <- c(tif, mo_tif[1])
    }

    tmax <- terra::app(terra::rast(tif), fun = mean, na.rm = TRUE)
    trv  <- terra::vect(sf::st_transform(tracts_geo, terra::crs(tmax)))
    pm   <- terra::extract(tmax, trv, fun = mean, na.rm = TRUE)[, 2]
    na_i <- which(is.na(pm))              # off-grid or sub-pixel tracts
    if (length(na_i)) {

      sub <- trv[na_i, ]
      has <- which(terra::expanse(sub, unit = "km") > 0)
      if (length(has)) {
        ex <- terra::extract(tmax, terra::centroids(sub[has, ]))
        pm[na_i[has]] <- ex[[2]][match(seq_along(has), ex[[1]])]
      }
    }
    ht <- tibble::tibble(GEOID = as.character(tracts_geo$GEOID),
                         tmax_warm = round(pm, 3))
    save_tract_df(ht, "01f_heat_warm_tract")
    message("  saved: ", f_heat, " (",
            sum(!is.na(ht$tmax_warm)), "/", nrow(ht), " tracts)")
    ht
  }, error = function(e) {
    message("  !!! PRISM heat build FAILED: ", e$message)
    message("  !!! HEAT (tmax_warm) WILL BE MISSING from this run.")
    message("  !!! This is a DEGRADED 4-exposure run and will NOT reproduce")
    message("  !!! the manuscript's 5-exposure / n=69,530 results (heat drops")
    message("  !!! out and list-wise deletion collapses N). Fix the PRISM")
    message("  !!! download and re-run before using any numbers.")
    tibble::tibble(GEOID = character(0))
  })
} else {
  heat_tract <- readRDS(f_heat)
  message("  cached: ", f_heat, " (", nrow(heat_tract), " tracts)")
}

# ---- 1f-v. USDA Food Access Research Atlas ------------------
f_food <- file.path(dir_proc, "01f_food_tract.rds")

FOOD_REQ <- c("GEOID", "LILATracts_1And10", "LowIncomeTracts",
              "LATracts_half", "LATracts1", "LATracts10", "LATracts20",
              "LILATracts_Vehicle")
food_stale <- !file.exists(f_food)
if (!food_stale) {
  food_miss  <- setdiff(FOOD_REQ, names(readRDS(f_food)))
  food_stale <- length(food_miss) > 0L
  if (food_stale)
    message("  ! 01f_food_tract.rds lacks ", paste(food_miss, collapse = ", "),
            " - rebuilding")
}
if (food_stale) {
  message("Downloading USDA Food Access Research Atlas 2019...")
  fara_url <- "https://www.ers.usda.gov/sites/default/files/_laserfiche/DataFiles/80591/FoodAccessResearchAtlasData2019.xlsx"
  fara_local <- file.path(dir_raw, "fara_2019.xlsx")
  if (!file.exists(fara_local)) {
    tryCatch(
      utils::download.file(fara_url, fara_local, mode = "wb", quiet = TRUE),
      error = function(e) message("  ! FARA download failed: ", e$message)
    )
  }
  if (file.exists(fara_local)) {
    food_raw <- tryCatch(
      readxl::read_excel(fara_local, sheet = "Food Access Research Atlas"),
      error = function(e) {
        message("  ! FARA parse failed: ", e$message); NULL
      })
    if (!is.null(food_raw)) {
      keep <- intersect(c("CensusTract", "LILATracts_1And10", "LILATracts_halfAnd10",
                          "LILATracts_1And20", "LILATracts_Vehicle",
                          "LowIncomeTracts", "PovertyRate",

                          "LATracts_half", "LATracts1", "LATracts10",
                          "LATracts20", "HUNVFlag",
                          "lapophalf", "lapop1", "lapop10",
                          "TractLOWI", "lalowihalf", "lalowi1"),
                        names(food_raw))

      keep_miss <- setdiff(setdiff(FOOD_REQ, "GEOID"), keep)
      if (length(keep_miss))
        stop("FARA release lacks required column(s): ",
             paste(keep_miss, collapse = ", "))
      food_tract <- food_raw |>
        dplyr::select(dplyr::all_of(keep)) |>
        dplyr::rename(GEOID = CensusTract) |>
        dplyr::mutate(GEOID = as.character(GEOID),
                      GEOID = ifelse(nchar(GEOID) == 10,
                                     paste0("0", GEOID), GEOID))
      save_tract_df(food_tract, "01f_food_tract")
      message("  saved: ", f_food, " (", nrow(food_tract), " tracts)")
    } else {
      food_tract <- tibble::tibble(GEOID = character(0))
    }
  } else {
    food_tract <- tibble::tibble(GEOID = character(0))
  }
} else {
  food_tract <- readRDS(f_food)
  message("  cached: ", f_food, " (", nrow(food_tract), " tracts)")
}


ahrq_tract <- tibble::tibble(GEOID = character(0))


env_layer_cols <- unique(c(
  setdiff(names(svi_tract),   "GEOID"),
  setdiff(names(aq_tract),    "GEOID"),
  setdiff(names(heat_tract),  "GEOID"),
  setdiff(names(walk_tract),  "GEOID"),
  setdiff(names(food_tract),  "GEOID"),
  setdiff(names(ahrq_tract),  "GEOID")
))
analytic <- analytic |>
  dplyr::select(-dplyr::any_of(env_layer_cols)) |>
  dplyr::left_join(svi_tract,  by = "GEOID") |>
  dplyr::left_join(aq_tract,   by = "GEOID") |>
  dplyr::left_join(heat_tract, by = "GEOID") |>
  dplyr::left_join(walk_tract, by = "GEOID") |>
  dplyr::left_join(food_tract, by = "GEOID") |>
  dplyr::left_join(ahrq_tract, by = "GEOID")

cat("\n--- Environmental coverage on analytic dataset ---\n")

env_cols_check <- list(
  "PM2.5 annual"     = "pm25_annual",
  "O3 4th-max 8hr"   = "o3_8hrmax_4thmax",
  "USDA LILA flag"   = "LILATracts_1And10",
  "Warm-season Tmax (C)" = "tmax_warm",
  "EPA Walkability"  = "walk_index"
)
for (lab in names(env_cols_check)) {
  cn <- env_cols_check[[lab]]
  if (cn %in% names(analytic)) {
    cat(sprintf("  %-20s: %6d / %d\n", lab,
                sum(!is.na(analytic[[cn]])), nrow(analytic)))
  } else {
    cat(sprintf("  %-20s: MISSING (column not pulled)\n", lab))
  }
}


MIN_ENV_COVERAGE_FRAC <- 0.90
env_gate_fail <- character(0)
for (lab in names(env_cols_check)) {
  cn <- env_cols_check[[lab]]
  if (!(cn %in% names(analytic))) {
    env_gate_fail <- c(env_gate_fail,
                       sprintf("%s (%s): column not present", lab, cn))
    next
  }
  frac <- sum(!is.na(analytic[[cn]])) / nrow(analytic)
  if (frac < MIN_ENV_COVERAGE_FRAC) {
    env_gate_fail <- c(env_gate_fail,
                       sprintf("%s (%s): only %.1f%% coverage (need >= %.0f%%)",
                               lab, cn, 100 * frac, 100 * MIN_ENV_COVERAGE_FRAC))
  }
}
if (length(env_gate_fail) > 0L) {
  stop(
    "\n",
    "==================================================================\n",
    " RUN HALTED — the exposure set is incomplete (degraded run).\n",
    "==================================================================\n",
    " The manuscript uses 5 environmental exposures. The following did\n",
    " not load with full coverage, so continuing would produce a model\n",
    " that does NOT match the manuscript:\n\n",
    paste0("   - ", env_gate_fail, collapse = "\n"), "\n\n",
    " Most common cause: the saved heat/walkability layers were not on\n",
    " this machine's disk, so the script tried to re-download them and\n",
    " the download failed (see the WALKABILITY / HEAT messages above).\n\n",
    " To fix:\n",
    "   1. Make sure these two files are fully downloaded (not cloud-only)\n",
    "      in data/processed/ :\n",
    "         01f_heat_warm_tract.rds\n",
    "         01f_walk_tract.rds\n",
    "   2. Restart R and source this script again from the top.\n\n",
    " A correct run shows Tmax and Walkability with real coverage above\n",
    " and, later, 'XGBoost design matrix: 69530 x 19 features'.\n",
    "==================================================================\n",
    call. = FALSE
  )
}
cat("  [gate] All 5 exposures present with full coverage — proceeding.\n")

save_tract_df(analytic, "01f_analytic_with_env")


# ==============================================================
# 2.  CKM INDEX CONSTRUCTION  (PCA + equal-weight composite)
# ==============================================================


core_avail <- intersect(CKM_CORE,     names(analytic))
ext_avail  <- intersect(CKM_EXTENDED, names(analytic))

if (length(core_avail) < 4) {
  warning("Core CKM index: only ", length(core_avail),
          " of 5 components available. Index will be unreliable. Components: ",
          paste(core_avail, collapse = ", "))
}


build_index <- function(df, cols, prefix) {
  X <- df |> dplyr::select(GEOID, dplyr::all_of(cols)) |>
    tidyr::drop_na()
  X_mat <- as.matrix(X[, cols, drop = FALSE])
  X_z   <- scale(X_mat)                                      
  pca   <- prcomp(X_z, center = FALSE, scale. = FALSE)       
 
  if ("DIABETES" %in% cols && pca$rotation["DIABETES", 1] < 0) {
    pca$x[, 1]        <- -pca$x[, 1]
    pca$rotation[, 1] <- -pca$rotation[, 1]
  }
  out <- tibble::tibble(GEOID = X$GEOID,
                        pca   = as.numeric(pca$x[, 1]),
                        zsum  = as.numeric(rowSums(X_z)))
  names(out)[2:3] <- paste0(prefix, c("_pca", "_zsum"))
  list(scores = out, pca_obj = pca, n = nrow(X), cols_used = cols)
}

idx_core <- build_index(analytic, core_avail, "ckm_core")
idx_ext  <- build_index(analytic, ext_avail,  "ckm_ext")


cat("\n--- CKM index PCA variance explained by PC1 ---\n")
cat("Core (n=", idx_core$n, ", k=", length(core_avail), "):  ",
    sprintf("%.1f%%", 100 * summary(idx_core$pca_obj)$importance[2, 1]), "\n")
cat("Extended (n=", idx_ext$n, ", k=", length(ext_avail), "): ",
    sprintf("%.1f%%", 100 * summary(idx_ext$pca_obj)$importance[2, 1]), "\n")


alpha_obj <- DescTools::CronbachAlpha(
  scale(analytic[, core_avail])[complete.cases(analytic[, core_avail]), ])
alpha_val <- if (is.list(alpha_obj)) alpha_obj$CronbachAlpha else as.numeric(alpha_obj[1])
cat("Core Cronbach's alpha:   ", sprintf("%.3f", alpha_val), "\n")


cat("\n--- Core PC1 loadings ---\n")
print(round(idx_core$pca_obj$rotation[, 1, drop = FALSE], 3))


analytic <- analytic |>
  dplyr::left_join(idx_core$scores, by = "GEOID") |>
  dplyr::left_join(idx_ext$scores,  by = "GEOID")

save_tract_df(analytic, "02_ckm_index")
cat("\nSaved: ", file.path(dir_proc, "02_ckm_index.rds"), "\n")




.t1_ruca <- c("Metro", "Micropolitan", "Small town", "Rural")


.t1_core_pred <- c("pct_age_65_plus", "pct_hispanic", "ice_income",
                   "pct_bachelor_plus", "ice_race", "ACCESS2", "CHECKUP",
                   "ruca_class", "CSMOKING", "LPA")
.t1_env_pred  <- c("pm25_annual", "o3_8hrmax_4thmax", "LILATracts_1And10",
                   "tmax_warm", "walk_index")
.t1_nonconus  <- c("02", "15", "60", "66", "69", "72", "78")

t1_sample <- analytic |>
  dplyr::left_join(
    tracts_geo |> sf::st_drop_geometry() |> dplyr::select(GEOID, ALAND),
    by = "GEOID") |>
  dplyr::filter(!substr(GEOID, 1, 2) %in% .t1_nonconus,
                ALAND > 0, !is.na(ckm_core_pca)) |>
  dplyr::mutate(ruca_class = factor(ruca_class, levels = .t1_ruca)) |>
  tidyr::drop_na(ckm_core_pca, pct_age_65_plus, ice_race, ice_income,
                 ruca_class, pct_hispanic, pct_black, pct_bachelor_plus,
                 median_income) |>
  tidyr::drop_na(tidyselect::all_of(c("ckm_core_pca", .t1_core_pred,
                                      .t1_env_pred)))


if (nrow(t1_sample) != 69530L) {
  message("  !!! Table 1 sample is ", nrow(t1_sample), " tracts, not 69,530.")
  message("  !!! This is a DEGRADED run. Table 1 must not go into the ",
          "manuscript until it reproduces 69,530.")
}


.t1_ms <- function(x, d = 2) {
  x <- suppressWarnings(as.numeric(as.character(x)))
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_character_)
  sprintf("%.*f (%.*f)", d, mean(x), d, sd(x))
}

.t1_np <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  sprintf("%d (%.1f%%)", sum(x == 1), 100 * mean(x == 1))
}

.t1_row <- function(df, label, f) {
  vals <- vapply(.t1_ruca,
                 function(k) f(df[which(df$ruca_class == k), , drop = FALSE]),
                 character(1))
  c(list(Variable = label), as.list(vals), list(Overall = f(df)))
}

tbl1_manuscript <- dplyr::bind_rows(
  .t1_row(t1_sample, "n (tracts)",
          function(d) format(nrow(d), big.mark = ",")),
  .t1_row(t1_sample, "% adults age 65+",
          function(d) .t1_ms(d$pct_age_65_plus)),
  .t1_row(t1_sample, "% Hispanic",
          function(d) .t1_ms(d$pct_hispanic)),
  .t1_row(t1_sample, "% adults with bachelor's+",
          function(d) .t1_ms(d$pct_bachelor_plus)),
  .t1_row(t1_sample, "Median household income (USD)",
          function(d) .t1_ms(d$median_income)),
  .t1_row(t1_sample, "ICE Income (Krieger)",
          function(d) .t1_ms(d$ice_income)),
  .t1_row(t1_sample, "ICE Race (Krieger)",
          function(d) .t1_ms(d$ice_race)),
  .t1_row(t1_sample, "Annual mean PM2.5 (ug/m3)",
          function(d) .t1_ms(d$pm25_annual)),
  .t1_row(t1_sample, "Annual 4th-highest 8-hr O3 (ppb)",
          function(d) .t1_ms(d$o3_8hrmax_4thmax * 1000)),
  .t1_row(t1_sample, "Low-income low-access tract (LILA)",
          function(d) .t1_np(d$LILATracts_1And10))
)

cat("\n--- Manuscript Table 1: sample characteristics by RUCA stratum ---\n")
print(as.data.frame(tbl1_manuscript))
cat("Table 1 describes the estimation sample: ",
    format(nrow(t1_sample), big.mark = ","), " tracts",
    "  (attribute-join universe: ", format(nrow(analytic), big.mark = ","),
    ")\n", sep = "")

cat("tracts with no RUCA class: ", sum(is.na(t1_sample$ruca_class)), "\n")
stopifnot(sum(vapply(.t1_ruca,
                     function(k) sum(t1_sample$ruca_class == k, na.rm = TRUE),
                     integer(1))) == nrow(t1_sample))
readr::write_csv(tbl1_manuscript,
                 file.path(dir_tbl, "Table1_sample_characteristics.csv"))


# ==============================================================
# 3.  EDA + GLOBAL MORAN'S I  
# ==============================================================

# Attach CKM index to geometries
analytic_geo <- tracts_geo |>
  dplyr::inner_join(
    dplyr::select(analytic, GEOID,
                  ckm_core_pca, ckm_ext_pca,
                  pct_age_65_plus),
    by = "GEOID"
  )


conus <- analytic_geo |>
  dplyr::filter(!substr(GEOID, 1, 2) %in% c("02", "15", "60", "66", "69", "72", "78")) |>
  dplyr::filter(ALAND > 0) |>
  dplyr::filter(!is.na(ckm_core_pca))

cat("\nCONUS analytic tracts with Core CKM index: ", nrow(conus), "\n")


geo_df <- tracts_geo |>
  sf::st_drop_geometry() |>
  dplyr::filter(!substr(GEOID, 1, 2) %in% c("60", "66", "69", "72", "78")) |>
  dplyr::filter(ALAND > 0) |>
  dplyr::mutate(GEOID = enc2utf8(as.character(GEOID)) |>
                          stringr::str_trim())

ckm_df <- analytic |>
  dplyr::select(GEOID, ckm_core_pca, ckm_ext_pca, pct_age_65_plus) |>
  dplyr::mutate(GEOID = enc2utf8(as.character(GEOID)) |>
                          stringr::str_trim())

joined_df <- dplyr::left_join(geo_df, ckm_df, by = "GEOID")

# Diagnostic: print join coverage by state
join_check <- joined_df |>
  dplyr::mutate(state_code = substr(GEOID, 1, 2)) |>
  dplyr::group_by(state_code) |>
  dplyr::summarise(n_total = dplyr::n(),
                   n_joined = sum(!is.na(ckm_core_pca)),
                   pct_joined = round(100 * n_joined / n_total, 1)) |>
  dplyr::arrange(pct_joined)
cat("\n--- Plain-DF join coverage (lowest 5 states) ---\n")
print(head(join_check, 5))

# Re-attach geometry by GEOID lookup
us50_all <- tracts_geo |>
  dplyr::mutate(GEOID = enc2utf8(as.character(GEOID)) |>
                          stringr::str_trim()) |>
  dplyr::filter(GEOID %in% joined_df$GEOID) |>
  dplyr::left_join(joined_df, by = "GEOID") |>
  sf::st_transform(4326) |>
  tigris::shift_geometry(geoid_column = "GEOID",
                         preserve_area = FALSE,
                         position = "below")


us50 <- us50_all |> dplyr::filter(!is.na(ckm_core_pca))


us50_suppressed <- us50_all |>
  dplyr::filter(is.na(ckm_core_pca)) |>
  dplyr::filter(substr(GEOID, 1, 2) != "34")   


nj_unavail <- tracts_geo |>
  dplyr::mutate(GEOID = enc2utf8(as.character(GEOID)) |> stringr::str_trim()) |>
  dplyr::filter(substr(GEOID, 1, 2) == "34", ALAND > 0) |>
  sf::st_transform(4326) |>
  tigris::shift_geometry(geoid_column = "GEOID",
                         preserve_area = FALSE, position = "below")
NJ_UNAVAIL_FILL <- "grey60"   

cat("US analytic tracts with CKM index: ", nrow(us50),
    "\nUS tracts suppressed by PLACES:    ", nrow(us50_suppressed),
    "\nNJ tracts (data unavailable):      ", nrow(nj_unavail), "\n")


suppress_by_state <- us50_suppressed |>
  sf::st_drop_geometry() |>
  dplyr::mutate(state_code = substr(GEOID, 1, 2)) |>
  dplyr::count(state_code, name = "n_suppressed")
total_by_state <- us50_all |>
  sf::st_drop_geometry() |>
  dplyr::mutate(state_code = substr(GEOID, 1, 2)) |>
  dplyr::count(state_code, name = "n_total")
suspicious_states <- total_by_state |>
  dplyr::left_join(suppress_by_state, by = "state_code") |>
  dplyr::mutate(n_suppressed = tidyr::replace_na(n_suppressed, 0L),
                pct_suppressed = round(100 * n_suppressed / n_total, 1)) |>
  dplyr::left_join(state_lookup, by = "state_code") |>
  dplyr::filter(pct_suppressed > 5) |>
  dplyr::arrange(dplyr::desc(pct_suppressed))

cat("\n--- States with >5% tract suppression (sorted) ---\n")
if (nrow(suspicious_states) > 0) {
  print(suspicious_states[, c("state", "state_name", "n_total",
                              "n_suppressed", "pct_suppressed")])
} else {
  cat("  None — all states <5% suppression. \n")
}

# ---- 3a. Choropleth — Core CKM index -------------------------

states_us <- tigris::states(cb = TRUE, year = TIGRIS_YEAR,
                            progress_bar = FALSE) |>
  dplyr::filter(!STATEFP %in% c("60", "66", "69", "72", "78")) |>
  tigris::shift_geometry(preserve_area = FALSE, position = "below")

states_conus <- states_us |>
  dplyr::filter(!STATEFP %in% c("02", "15"))


brks <- quantile(us50$ckm_core_pca, probs = seq(0, 1, 0.2), na.rm = TRUE)
us50$ckm_q <- cut(us50$ckm_core_pca, breaks = brks,
                  include.lowest = TRUE,
                  labels = c("Q1 (lowest 20%)", "Q2", "Q3", "Q4",
                             "Q5 (highest 20%)"))


n_na_q <- sum(is.na(us50$ckm_q))
if (n_na_q > 0) {
  message("  ", n_na_q, " tracts got NA after cut(); dropping them.")
  us50 <- us50 |> dplyr::filter(!is.na(ckm_q))
}


ckm_palette <- c("#FED976", "#FEB24C", "#FD8D3C", "#E31A1C", "#800026")


p_choro <- ggplot() +
  
  geom_sf(data = us50_suppressed,
          aes(fill = "Suppressed (no PLACES estimate)"),
          colour = NA) +
  
  geom_sf(data = nj_unavail,
          aes(fill = "Data unavailable (New Jersey)"),
          colour = NA) +
  
  geom_sf(data = us50, aes(fill = ckm_q), colour = NA) +
  geom_sf(data = states_us, fill = NA, colour = "grey25", linewidth = 0.25) +
  scale_fill_manual(
    values = c(setNames(ckm_palette,
                        c("Q1 (lowest 20%)", "Q2", "Q3", "Q4", "Q5 (highest 20%)")),
               "Suppressed (no PLACES estimate)" = "grey85",
               "Data unavailable (New Jersey)"   = NJ_UNAVAIL_FILL),
    name = "CKM Core burden",
    breaks = c("Q5 (highest 20%)", "Q4", "Q3", "Q2", "Q1 (lowest 20%)",
               "Suppressed (no PLACES estimate)",
               "Data unavailable (New Jersey)"),
    drop = FALSE,
    na.translate = FALSE
  ) +
  coord_sf(datum = NA) +
  theme_void(base_size = 11) +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey30")) +
  labs(
    title    = "Cardiovascular–Kidney–Metabolic Syndrome burden across U.S. census tracts",
    subtitle = paste0("Quintiles of the CKM Core PC1 index (",
                      length(core_avail),
                      "-component PCA: diabetes, hypertension, obesity, ",
                      "high cholesterol, chronic kidney disease)"),
    caption  = paste0("Data source: CDC PLACES ", PLACES_RELEASE,
                      " (BRFSS 2019–2020). N = ",
                      formatC(nrow(us50), big.mark = ","),
                      " tracts.  Alaska and Hawaii repositioned for display.")
  )
ggsave(file.path(dir_fig, "eda_choropleth_ckm_core.png"),
       p_choro, width = 11, height = 6.5, dpi = 220,
       bg = "white")   
cat("  saved: ", file.path(dir_fig, "eda_choropleth_ckm_core.png"), "\n")

# ---- 3b. Spatial weights matrix 

cat("\nBuilding spatial weights (k=8 nearest neighbors)...\n")
coords <- conus |> sf::st_centroid(of_largest_polygon = TRUE) |>
  sf::st_coordinates()
nb_knn <- spdep::knn2nb(spdep::knearneigh(coords, k = 8), sym = TRUE)
lw_knn <- spdep::nb2listw(nb_knn, style = "W", zero.policy = TRUE)

# ---- 3c. Global Moran's I — Core CKM index ------------------
cat("\nGlobal Moran's I on Core CKM PC1 (k-NN W, k=8)...\n")
mi_core <- spdep::moran.test(conus$ckm_core_pca, lw_knn,
                             zero.policy = TRUE, randomisation = TRUE)
cat("  I = ", sprintf("%.3f", mi_core$estimate["Moran I statistic"]),
    " | p = ", format.pval(mi_core$p.value, eps = 1e-16), "\n")

# Save Moran's I result for reporting
saveRDS(list(moran_core = mi_core, lw_knn_summary = summary(nb_knn)),
        file.path(dir_proc, "03_moran_core.rds"))
cat("  saved: ", file.path(dir_proc, "03_moran_core.rds"), "\n")

# ---- 3d. Air-quality exposure choropleths 

make_aq_choropleth <- function(col, palette, title, subtitle, legend_title,
                                breaks_q = NULL, conus_only = FALSE) {
  if (!col %in% names(analytic)) return(NULL)

  fips_excl <- c("60", "66", "69", "72", "78")
  if (conus_only) fips_excl <- c(fips_excl, "02", "15")

  aq_geo <- tracts_geo |>
    dplyr::mutate(GEOID = enc2utf8(as.character(GEOID)) |>
                            stringr::str_trim()) |>
    dplyr::left_join(
      analytic |>
        dplyr::select(GEOID, !!rlang::sym(col)) |>
        dplyr::mutate(GEOID = enc2utf8(as.character(GEOID)) |>
                                stringr::str_trim()),
      by = "GEOID"
    ) |>
    dplyr::filter(!substr(GEOID, 1, 2) %in% fips_excl,
                  ALAND > 0) |>
    sf::st_transform(4326)
  if (!conus_only)
    aq_geo <- tigris::shift_geometry(aq_geo, geoid_column = "GEOID",
                                       preserve_area = FALSE, position = "below")
  aq_geo <- aq_geo |> dplyr::filter(!is.na(.data[[col]]))
  if (nrow(aq_geo) == 0) return(NULL)


  states_layer <- if (conus_only)
    states_us |> dplyr::filter(!STATEFP %in% c("02", "15")) else states_us

  if (is.null(breaks_q))
    breaks_q <- quantile(aq_geo[[col]], probs = seq(0, 1, 0.2), na.rm = TRUE)
  aq_geo$bin <- cut(aq_geo[[col]], breaks = breaks_q,
                    include.lowest = TRUE,
                    labels = c("Q1 (lowest)", "Q2", "Q3", "Q4", "Q5 (highest)"))

  ggplot() +
    geom_sf(data = aq_geo, aes(fill = bin), colour = NA) +
    geom_sf(data = states_layer, fill = NA, colour = "grey25",
            linewidth = 0.25) +
    scale_fill_manual(values = setNames(palette,
                          c("Q1 (lowest)", "Q2", "Q3", "Q4", "Q5 (highest)")),
                       name = legend_title,
                       breaks = c("Q5 (highest)", "Q4", "Q3", "Q2", "Q1 (lowest)"),
                       drop = FALSE, na.translate = FALSE) +
    coord_sf(datum = NA) +
    theme_void(base_size = 11) +
    theme(legend.position = "right",
          plot.title    = element_text(face = "bold",
                                        margin = margin(b = 4)),
          plot.subtitle = element_text(colour = "grey30",
                                        size = 9.5,
                                        lineheight = 1.15,
                                        margin = margin(b = 8))) +
    labs(title = title, subtitle = subtitle)
}


.aq_method_attr <- attr(aq_tract, "method") %||% "unknown"
.is_rsig <- grepl("rsig", .aq_method_attr)

if (.is_rsig) {

  .pm25_subtitle <- paste0(
    "EPA RSIG Downscaler — fused CMAQ + monitor predictions at tract centroids\n",
    "2020 daily means → annual mean per tract")
  .o3_subtitle <- paste0(
    "EPA RSIG Downscaler — fused CMAQ + monitor predictions at tract centroids\n",
    "2020 4th-highest daily 8-hr max per tract (NAAQS form)")
} else {
  .aq_var <- tryCatch(
    readRDS(file.path(dir_proc, "01f_aq_variograms.rds")),
    error = function(e) list(pm25 = list(method = "IDW"),
                              o3   = list(method = "IDW")))
  .lab <- function(m) {
    if (is.null(m)) return("IDW fallback")
    if (grepl("^kriging", m)) "ordinary kriging" else
    if (grepl("^inverse", m)) "inverse-distance weighting (k = 10, p = 2)" else m
  }
  .pm25_subtitle <- paste0("Spatial interpolation of EPA AirData annual monitor concentrations to tract centroids (",
                            .lab(.aq_var$pm25$method), ")")
  .o3_subtitle   <- paste0("Spatial interpolation of EPA AirData (NAAQS form) to tract centroids (",
                            .lab(.aq_var$o3$method), ")")
}


p_pm25 <- make_aq_choropleth(
  col       = "pm25_annual",
  palette   = c("#FFFFCC", "#A1DAB4", "#41B6C4", "#2C7FB8", "#253494"),
  title     = if (.is_rsig)
    "Annual mean fine particulate matter (PM₂.₅) across the contiguous U.S."
  else
    "Annual mean fine particulate matter (PM₂.₅) across U.S. tracts",
  subtitle     = .pm25_subtitle,
  legend_title = "PM₂.₅ (µg/m³)\nquintile",
  conus_only   = .is_rsig)
if (!is.null(p_pm25)) {
  ggsave(file.path(dir_fig, "03d_choropleth_pm25.png"),
         p_pm25, width = 11, height = 6.5, dpi = 220, bg = "white")
  cat("  saved: ", file.path(dir_fig, "03d_choropleth_pm25.png"), "\n")
}

p_o3 <- make_aq_choropleth(
  col       = "o3_8hrmax_4thmax",
  palette   = c("#FFFFCC", "#FED976", "#FD8D3C", "#E31A1C", "#800026"),
  title     = if (.is_rsig)
    "Annual 4th-highest 8-hour maximum ozone across the contiguous U.S."
  else
    "Annual 4th-highest 8-hour maximum ozone across U.S. tracts",
  subtitle     = .o3_subtitle,
  legend_title = "O₃ (ppm)\nquintile",
  conus_only   = .is_rsig)
if (!is.null(p_o3)) {
  ggsave(file.path(dir_fig, "03d_choropleth_o3.png"),
         p_o3, width = 11, height = 6.5, dpi = 220, bg = "white")
  cat("  saved: ", file.path(dir_fig, "03d_choropleth_o3.png"), "\n")
}


p_tmax <- make_aq_choropleth(
  col       = "tmax_warm",
  palette   = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026"),
  title     = "Warm-season maximum temperature across the contiguous U.S.",
  subtitle  = paste0("PRISM Climate Group 4-km warm-season (May–Sep) Tmax, ",
                     "2019–2020 mean, aggregated to census tracts"),
  legend_title = "Tmax (°C)\nquintile",
  conus_only   = TRUE)
if (!is.null(p_tmax)) {
  ggsave(file.path(dir_fig, "03d_choropleth_tmax.png"),
         p_tmax, width = 11, height = 6.5, dpi = 220, bg = "white")
  cat("  saved: ", file.path(dir_fig, "03d_choropleth_tmax.png"), "\n")
}

p_walk <- make_aq_choropleth(
  col       = "walk_index",
  palette   = c("#FFFFCC", "#C2E699", "#78C679", "#31A354", "#006837"),
  title     = "EPA National Walkability Index across U.S. tracts",
  subtitle  = paste0("EPA Smart Location Database v3 — block-group NatWalkInd ",
                     "population-weighted to census tracts"),
  legend_title = "Walkability\nquintile",
  conus_only   = FALSE)
if (!is.null(p_walk)) {
  ggsave(file.path(dir_fig, "03d_choropleth_walk.png"),
         p_walk, width = 11, height = 6.5, dpi = 220, bg = "white")
  cat("  saved: ", file.path(dir_fig, "03d_choropleth_walk.png"), "\n")
}


make_sdoh_panel <- function(col, palette_dir, title_str) {
  if (!col %in% names(analytic)) return(NULL)
  sdoh_geo <- tracts_geo |>
    dplyr::mutate(GEOID = enc2utf8(as.character(GEOID)) |>
                            stringr::str_trim()) |>
    dplyr::left_join(
      analytic |>
        dplyr::select(GEOID, !!rlang::sym(col)) |>
        dplyr::mutate(GEOID = enc2utf8(as.character(GEOID)) |>
                                stringr::str_trim()),
      by = "GEOID"
    ) |>
    dplyr::filter(!substr(GEOID, 1, 2) %in% c("60", "66", "69", "72", "78"),
                  ALAND > 0) |>
    sf::st_transform(4326) |>
    tigris::shift_geometry(geoid_column = "GEOID",
                            preserve_area = FALSE, position = "below") |>
    dplyr::filter(!is.na(.data[[col]]))
  if (nrow(sdoh_geo) == 0) return(NULL)

  if (palette_dir == "div") {
    fill_scale <- scale_fill_gradient2(
      low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
      midpoint = 0, limits = c(-1, 1), oob = scales::squish,
      name = title_str)
  } else {

    upper_clip <- if (col == "pct_age_65_plus") 40 else
                  unname(quantile(sdoh_geo[[col]], 0.99, na.rm = TRUE))
    fill_scale <- scale_fill_viridis_c(name = title_str, option = "C",
                                        limits = c(0, upper_clip),
                                        oob = scales::squish)
  }
  ggplot() +
    geom_sf(data = sdoh_geo, aes(fill = .data[[col]]), colour = NA) +
    geom_sf(data = states_us, fill = NA, colour = "grey25", linewidth = 0.2) +
    fill_scale +
    coord_sf(datum = NA) +
    theme_void(base_size = 11) +
    theme(legend.position = "bottom",
          legend.key.width = unit(1.6, "cm"),
          legend.key.height = unit(0.4, "cm"),
          legend.title = element_text(size = 10),
          legend.text  = element_text(size = 9),
          plot.title = element_text(face = "bold", size = 12))
}

p_age   <- make_sdoh_panel("pct_age_65_plus", "seq",
                            "% age 65+") +
  labs(title = "(a) % adults aged ≥65")
p_inc   <- make_sdoh_panel("ice_income", "div",
                            "ICE Income\n(low → high)") +
  labs(title = "(b) ICE Income")
p_race  <- make_sdoh_panel("ice_race", "div",
                            "ICE Race\n(NH-Black → NH-White)") +
  labs(title = "(c) ICE Race")

if (!is.null(p_age) && !is.null(p_inc) && !is.null(p_race)) {
  p_sdoh <- (p_age | p_inc | p_race) +
    patchwork::plot_annotation(
      title    = "Geographic distribution of key predictors",
      subtitle = "Three predictors selected from the BYM2 model: aging, economic concentration (ICE), and racial concentration (ICE).",
      caption  = "ICE = Index of Concentration at the Extremes. Negative ICE = concentrated NH-Black or low-income; positive ICE = concentrated NH-White or high-income.",
      theme    = theme(plot.caption = element_text(size = 8,
                                                    colour = "grey40",
                                                    hjust = 0))
    )
  ggsave(file.path(dir_fig, "03e_sdoh_panel.png"),
         p_sdoh, width = 17, height = 7, dpi = 220, bg = "white")
  cat("  saved: ", file.path(dir_fig, "03e_sdoh_panel.png"), "\n")
}

cat("\n=== Section 3 complete. ===\n",
    "Verification checklist:\n",
    "  [", ifelse(file.exists(file.path(dir_proc, "01_places_tract.rds")), "x", " "),
    "] 01_places_tract.rds exists\n",
    "  [", ifelse(file.exists(file.path(dir_proc, "02_ckm_index.rds")), "x", " "),
    "] 02_ckm_index.rds exists\n",
    "  [", ifelse(file.exists(file.path(dir_fig,  "eda_choropleth_ckm_core.png")), "x", " "),
    "] eda_choropleth_ckm_core.png renders\n",
    "  [", ifelse(mi_core$p.value < 0.001 &&
                  mi_core$estimate["Moran I statistic"] > 0.3, "x", " "),
    "] Moran's I > 0.3 with p < 0.001 (spatial structure confirmed)\n",
    sep = "")


# ==============================================================
# 4.  NHANES VALIDATION  
# ==============================================================


cat("\n========================================================\n")
cat("Section 4 — NHANES validation of tract-level CKM index\n")
cat("========================================================\n")

f_nhanes <- file.path(dir_proc, "04_nhanes_individual.rds")
if (!file.exists(f_nhanes)) {
  message("Pulling NHANES 2017-2020 Pre-Pandemic files via nhanesA...")
  nhanes_files <- list(
    demo  = "P_DEMO",   # Demographics (age, sex, race/ethnicity, weights)
    bmx   = "P_BMX",    # Body measures (BMI for obesity)
    diq   = "P_DIQ",    # Diabetes questionnaire
    bpq   = "P_BPQ",    # BP + Cholesterol questionnaire
    kiq   = "P_KIQ_U"   # Kidney conditions
  )

  pulls <- purrr::map(nhanes_files, function(fn) {
    tryCatch(
      nhanesA::nhanes(fn, translated = FALSE),
      error = function(e) {
        message("  ! ", fn, " failed (translated=FALSE): ", e$message,
                "\n    retrying default call...")
        tryCatch(nhanesA::nhanes(fn),
                 error = function(e2) {
                   message("  ! ", fn, " failed: ", e2$message); NULL
                 })
      })
  })

  if (any(vapply(pulls, is.null, logical(1)))) {
    message("  ! One or more NHANES files unavailable. Validation will be partial.")
  }

  # Merge by SEQN (respondent ID)
  nhanes_ind <- pulls$demo |>
    dplyr::select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH3, WTMECPRP) |>
    dplyr::left_join(pulls$bmx |> dplyr::select(SEQN, BMXBMI), by = "SEQN") |>
    dplyr::left_join(pulls$diq |> dplyr::select(SEQN, DIQ010), by = "SEQN") |>
    dplyr::left_join(pulls$bpq |> dplyr::select(SEQN, BPQ020, BPQ080), by = "SEQN") |>
    dplyr::left_join(pulls$kiq |> dplyr::select(SEQN, KIQ022), by = "SEQN")


  yn <- function(x) {
    cx <- as.character(x)
    out <- rep(NA_integer_, length(x))
    out[cx == "1" | tolower(cx) == "yes"] <- 1L
    out[cx == "2" | tolower(cx) == "no"]  <- 0L
    out
  }

  # Restrict to adults (≥18, matching PLACES population definition)

  nhanes_ind <- nhanes_ind |>
    dplyr::filter(RIDAGEYR >= 18, !is.na(WTMECPRP), WTMECPRP > 0) |>
    dplyr::mutate(
      DIABETES_dx = yn(DIQ010),
      BPHIGH_dx   = yn(BPQ020),
      OBESITY_dx  = dplyr::case_when(BMXBMI >= 30 ~ 1L,
                                     BMXBMI <  30 ~ 0L,
                                     TRUE ~ NA_integer_),
      HIGHCHOL_dx = yn(BPQ080),
      KIDNEY_dx   = yn(KIQ022)
    ) |>
    dplyr::select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH3, WTMECPRP,
                  DIABETES_dx, BPHIGH_dx, OBESITY_dx, HIGHCHOL_dx, KIDNEY_dx)

  saveRDS(nhanes_ind, f_nhanes)
  message("  saved: ", f_nhanes, " (", nrow(nhanes_ind), " adults)")
} else {
  nhanes_ind <- readRDS(f_nhanes)
  message("  cached: ", f_nhanes, " (", nrow(nhanes_ind), " adults)")
}

# ---- 4a. National prevalence: NHANES vs. PLACES --------------
# NHANES: weighted mean of binary diagnoses, weighted by WTMECPRP
# PLACES: weighted mean of tract prevalences (already %), weighted by total_pop

w_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  sum(x[ok] * w[ok]) / sum(w[ok])
}

nhanes_prev <- tibble::tibble(
  Indicator = c("DIABETES", "BPHIGH", "OBESITY", "HIGHCHOL", "KIDNEY"),
  NHANES_pct = c(
    100 * w_mean(nhanes_ind$DIABETES_dx, nhanes_ind$WTMECPRP),
    100 * w_mean(nhanes_ind$BPHIGH_dx,   nhanes_ind$WTMECPRP),
    100 * w_mean(nhanes_ind$OBESITY_dx,  nhanes_ind$WTMECPRP),
    100 * w_mean(nhanes_ind$HIGHCHOL_dx, nhanes_ind$WTMECPRP),
    100 * w_mean(nhanes_ind$KIDNEY_dx,   nhanes_ind$WTMECPRP)
  )
)

places_prev <- tibble::tibble(
  Indicator = c("DIABETES", "BPHIGH", "OBESITY", "HIGHCHOL", "KIDNEY"),
  PLACES_pct = c(
    w_mean(analytic$DIABETES, analytic$total_pop),
    w_mean(analytic$BPHIGH,   analytic$total_pop),
    w_mean(analytic$OBESITY,  analytic$total_pop),
    w_mean(analytic$HIGHCHOL, analytic$total_pop),
    w_mean(analytic$KIDNEY,   analytic$total_pop)
  )
)

prev_compare <- nhanes_prev |>
  dplyr::left_join(places_prev, by = "Indicator") |>
  dplyr::mutate(diff_pct_pts = round(NHANES_pct - PLACES_pct, 2),
                NHANES_pct = round(NHANES_pct, 2),
                PLACES_pct = round(PLACES_pct, 2))

cat("\n--- (a) National prevalence: NHANES vs. PLACES (% adults) ---\n")
print(prev_compare)
write_csv(prev_compare, file.path(dir_tbl, "04_nhanes_vs_places_prevalence.csv"))

# ---- 4b. Pairwise correlations of the 5 Core indicators ------


# NHANES: complete-case + design-weighted correlation
nh_bin <- nhanes_ind |>
  dplyr::select(DIABETES_dx, BPHIGH_dx, OBESITY_dx, HIGHCHOL_dx, KIDNEY_dx) |>
  as.data.frame()
cc_nh <- stats::complete.cases(nh_bin)
cw_nh <- stats::cov.wt(nh_bin[cc_nh, , drop = FALSE],
                       wt  = nhanes_ind$WTMECPRP[cc_nh],
                       cor = TRUE)
nhanes_cor_mat <- cw_nh$cor
rownames(nhanes_cor_mat) <- colnames(nhanes_cor_mat) <- CKM_CORE

# PLACES: unweighted Pearson (matches the equal-per-tract index input)
places_cor_mat <- analytic |>
  dplyr::select(dplyr::all_of(CKM_CORE)) |>
  as.data.frame() |>
  stats::cor(use = "complete.obs", method = "pearson")

cat("\n--- (b) Design-weighted correlation of 5 indicators (Pearson/phi) ---\n")
cat("NHANES (individual level, WTMECPRP-weighted):\n")
print(round(nhanes_cor_mat, 3))
cat("\nPLACES (tract level, unweighted):\n")
print(round(places_cor_mat, 3))

# Mantel-style summary: correlation between the two correlation matrices
# (off-diagonal upper triangle only)
ut <- upper.tri(nhanes_cor_mat)
mantel_r <- stats::cor(nhanes_cor_mat[ut], places_cor_mat[ut])
cat("\nCorrelation of NHANES vs PLACES correlation matrices: r = ",
    round(mantel_r, 3), "\n", sep = "")

# ---- 4c. PCA on NHANES vs. tract PCA -------------------------

ev_nh   <- eigen(nhanes_cor_mat, symmetric = TRUE)
nh_load <- ev_nh$vectors[, 1]
names(nh_load) <- CKM_CORE

if (nh_load["DIABETES"] < 0) nh_load <- -nh_load
nh_var_pc1 <- ev_nh$values[1] / sum(ev_nh$values)


pca_nh <- list(rotation    = matrix(nh_load, ncol = 1,
                                    dimnames = list(CKM_CORE, "PC1")),
               eigenvalues = ev_nh$values,
               var_pc1     = nh_var_pc1,
               weighted    = TRUE,
               weight_var  = "WTMECPRP")

load_compare <- tibble::tibble(
  Indicator = CKM_CORE,
  NHANES_PC1 = round(as.numeric(nh_load), 3),
  PLACES_PC1 = round(idx_core$pca_obj$rotation[, 1], 3) 
)
cat("\n--- (c) PC1 loadings: NHANES (weighted) vs. PLACES ---\n")
print(load_compare)
cat("Variance explained by PC1 — NHANES (weighted): ",
    sprintf("%.1f%%", 100 * nh_var_pc1),
    " | PLACES: ",
    sprintf("%.1f%%", 100 * summary(idx_core$pca_obj)$importance[2, 1]), "\n")
loadings_r <- stats::cor(load_compare$NHANES_PC1, load_compare$PLACES_PC1)
cat("Correlation of PC1 loadings between sources: r = ",
    round(loadings_r, 3), "\n", sep = "")

write_csv(load_compare, file.path(dir_tbl, "04_pc1_loadings_compare.csv"))


.cond_label <- c(DIABETES = "Diabetes mellitus",
                 BPHIGH   = "Hypertension",
                 OBESITY  = "Obesity",
                 HIGHCHOL = "Hypercholesterolemia",
                 KIDNEY   = "Chronic kidney disease")
tbl2_manuscript <- load_compare |>
  dplyr::left_join(prev_compare, by = "Indicator") |>
  dplyr::transmute(
    `Condition`                = unname(.cond_label[Indicator]),
    `PC1 loading, NHANES`      = NHANES_PC1,
    `PC1 loading, PLACES`      = PLACES_PC1,
    `Prevalence, NHANES (%)`   = NHANES_pct,
    `Prevalence, PLACES (%)`   = PLACES_pct,
    `Difference (pct. points)` = diff_pct_pts   # NHANES minus PLACES
  )
cat("\n--- Manuscript Table 2: PLACES vs. NHANES (merged) ---\n")
print(tbl2_manuscript)
write_csv(tbl2_manuscript,
          file.path(dir_tbl, "04_table2_places_vs_nhanes.csv"))

# Persist correlation matrices + validation scalars to disk (provenance)
readr::write_csv(tibble::as_tibble(nhanes_cor_mat, rownames = "Indicator"),
                 file.path(dir_tbl, "04_nhanes_correlation_weighted.csv"))
readr::write_csv(tibble::as_tibble(places_cor_mat, rownames = "Indicator"),
                 file.path(dir_tbl, "04_places_correlation.csv"))
readr::write_csv(tibble::tibble(metric = c("matrix_r", "loadings_r",
                                           "nhanes_pc1_var", "places_pc1_var"),
                                value  = c(mantel_r, loadings_r, nh_var_pc1,
                                           summary(idx_core$pca_obj)$importance[2, 1])),
                 file.path(dir_tbl, "04_validation_summary.csv"))

# ---- 4d. Validation summary figure ---------------------------
p_prev <- ggplot(prev_compare, aes(x = PLACES_pct, y = NHANES_pct)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey50", linetype = "dashed") +
  geom_point(size = 3, colour = "#BD0026") +
  ggrepel::geom_text_repel(aes(label = Indicator), size = 3.5,
                           min.segment.length = 0) |>
  tryCatch(error = function(e) {
    geom_text(aes(label = Indicator), nudge_y = 0.5, size = 3.5)
  })

# Fall back if ggrepel not available
if (!"ggrepel" %in% installed.packages()[, "Package"]) {
  p_prev <- ggplot(prev_compare, aes(x = PLACES_pct, y = NHANES_pct)) +
    geom_abline(slope = 1, intercept = 0, colour = "grey50", linetype = "dashed") +
    geom_point(size = 3, colour = "#BD0026") +
    geom_text(aes(label = Indicator), nudge_y = 1.0, size = 3.5)
}

p_prev <- p_prev +
  labs(x = "PLACES tract-weighted national prevalence (%)",
       y = "NHANES individual-weighted national prevalence (%)",
       title = "Construct validation — PLACES vs. NHANES",
       subtitle = paste0("5 CKM Core indicators; matrix r = ",
                         round(mantel_r, 3),
                         "; PC1 loadings r = ",
                         round(loadings_r, 3))) +
  theme_ckm

ggsave(file.path(dir_fig, "04_nhanes_validation.png"),
       p_prev, width = 7, height = 6, dpi = 220, bg = "white")
cat("  saved: ", file.path(dir_fig, "04_nhanes_validation.png"), "\n")

# Save the validation summary
saveRDS(list(prev_compare = prev_compare,
             nhanes_cor   = nhanes_cor_mat,
             places_cor   = places_cor_mat,
             load_compare = load_compare,
             mantel_r     = mantel_r,
             loadings_r   = loadings_r,
             pca_nh       = pca_nh),
        file.path(dir_proc, "04_nhanes_validation.rds"))

cat("\n=== Section 4 complete. ===\n")
cat("  PLACES vs. NHANES national prevalences: see prev_compare\n")
cat("  Correlation-matrix similarity (Mantel-style r): ",
    round(mantel_r, 3), "\n")
cat("  PC1 loading similarity: r = ", round(loadings_r, 3), "\n")
cat("  >>> Manuscript Methods sentence: 'The tract-level CKM Core index\n",
    "      shows modest construct concordance with NHANES individual-level\n",
    "      data — directionally consistent but limited in quantitative\n",
    "      agreement (correlation-matrix similarity r = ",
    sprintf("%.2f", mantel_r),
    "; PC1 loading r = ",
    sprintf("%.2f", loadings_r), ").'\n", sep = "")


# ==============================================================
# 5.  OLS BASELINE + RESIDUAL SPATIAL DIAGNOSTICS
#     (Steps 4-5: OLS + VIF + residual Moran's I)
# ==============================================================
#   Step 4: VIF-screen predictors (drop > 10), fit OLS
#   Step 5: Diagnose residual spatial autocorrelation
#           - Moran's I on OLS residuals
#           - Residual choropleth
#           - Centroid semivariogram (defer to sensitivity in Sec. 8)
#
# Outcome: ckm_core_pca (Stage 1+2 cardiometabolic burden, Section 2)
# Predictors (3 blocks):
#   DEMOGRAPHIC: pct_age_65_plus, pct_hispanic, pct_black, pct_white_nh,
#                median_age, pct_bachelor_plus
#   SDOH:        ice_race, ice_income, ruca_class
#   ENVIRONMENT: assembled in Section 1f (PM2.5, ozone, warm-season Tmax,
#                walkability, food access) but DELIBERATELY EXCLUDED here.


cat("\n========================================================\n")
cat("Section 5 — OLS baseline + spatial residual diagnostics\n")
cat("========================================================\n")


stale_caches <- c("05_ols_baseline.rds",
                  "06_lm_tests.rds", "06_spmoran_resf.rds",
                  "06_inla_bym2.rds", "06_inla_graph.adj",
                  "06_6d_inla_bym2_wx.rds", "06_6g_inla_bym2_nobehav.rds",
                  "06_5c_moran_correlogram.rds", "06_5d_residual_variogram.rds",
                  "07_xgb_fit.rds", "07_cv_results.rds", "07_shap.rds",
                  "08_maup_county.rds", "08_alt_weights.rds",
                  "08_ruca_strata.rds", "08_svi_sensitivity.rds")
.f_inputs <- file.path(dir_proc,
                       c("01f_aq_tract.rds",   "01f_heat_warm_tract.rds",
                         "01f_walk_tract.rds", "01f_food_tract.rds"))
.input_mtimes <- file.info(.f_inputs)$mtime
.newest_input <- if (any(!is.na(.input_mtimes)))
  max(.input_mtimes, na.rm = TRUE) else NA
n_invalidated <- 0
for (f in stale_caches) {
  fp <- file.path(dir_proc, f)
  if (!file.exists(fp)) next
  cache_mtime <- file.info(fp)$mtime

  if (!is.na(.newest_input) && !is.na(cache_mtime) &&
      .newest_input > cache_mtime) {
    file.remove(fp)
    csv_path <- sub("\\.rds$", ".csv", fp)
    if (file.exists(csv_path)) file.remove(csv_path)
    n_invalidated <- n_invalidated + 1
  }
}
if (n_invalidated > 0)
  message("Section 5: invalidated ", n_invalidated,
          " downstream cache(s) due to newer upstream data.")


extra_cols <- c("ice_race", "ice_income", "ruca_class",
                "pct_hispanic", "pct_black", "pct_white_nh",
                "median_age", "pct_bachelor_plus", "median_income",
                # HP2030 SDOH additions
                "pct_no_vehicle", "pct_living_alone",
                "pct_unemployed",
                # PLACES behavioral / access
                "CSMOKING", "LPA", "ACCESS2",
                # PLACES healthcare-engagement
                "CHECKUP",
                # CDC/ATSDR SVI 2020
                "svi_overall", "svi_ses", "svi_house",
                "svi_minor", "svi_trans")
extra_cols <- intersect(extra_cols, names(analytic))

mod_data <- conus |>
  sf::st_drop_geometry() |>
  dplyr::left_join(
    analytic |> dplyr::select(GEOID, dplyr::all_of(extra_cols)),
    by = "GEOID"
  ) |>
  dplyr::mutate(
    ruca_class = factor(ruca_class,
                        levels = c("Metro", "Micropolitan",
                                   "Small town", "Rural"))
  ) |>
  tidyr::drop_na(ckm_core_pca, pct_age_65_plus,
                 ice_race, ice_income, ruca_class,
                 pct_hispanic, pct_black, pct_bachelor_plus, median_income)

cat("Modeling N (complete cases): ", nrow(mod_data), "\n")

# ---- 5a. VIF screening ---------------------------------------
# Standard threshold: VIF > 10 = problematic multicollinearity

ols_full <- lm(
  ckm_core_pca ~ pct_age_65_plus + ice_race + ice_income +
                 pct_hispanic + pct_bachelor_plus + ruca_class,
  data = mod_data
)

vif_vals <- car::vif(ols_full)
# car::vif returns a matrix for factor predictors (3 cols), reduce to a single
# value per predictor for display
if (is.matrix(vif_vals)) {
  vif_df <- tibble::tibble(
    predictor = rownames(vif_vals),
    GVIF      = vif_vals[, "GVIF"],
    GVIF_adj  = vif_vals[, "GVIF^(1/(2*Df))"]^2  # squared for VIF-equivalent
  )
} else {
  vif_df <- tibble::tibble(predictor = names(vif_vals),
                           VIF       = as.numeric(vif_vals))
}
cat("\n--- VIF table ---\n")
print(vif_df)

# ---- 5b. OLS results -----------------------------------------
ols_summary <- summary(ols_full)
cat("\n--- OLS coefficients (baseline, no env block) ---\n")
print(round(coef(ols_summary), 4))
cat("\nR-squared:        ", round(ols_summary$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(ols_summary$adj.r.squared, 4), "\n")
cat("F-statistic p:    ", format.pval(pf(ols_summary$fstatistic[1],
                                         ols_summary$fstatistic[2],
                                         ols_summary$fstatistic[3],
                                         lower.tail = FALSE)), "\n")


saveRDS(list(model = ols_full, summary = ols_summary, vif = vif_df,
             n = nrow(mod_data), GEOID = mod_data$GEOID),
        file.path(dir_proc, "05_ols_baseline.rds"))

# ---- 5c. Residual Moran's I   ----------------

mod_data$ols_resid <- residuals(ols_full)


mod_geoids <- mod_data$GEOID
conus_idx <- match(mod_geoids, conus$GEOID)
conus_idx <- conus_idx[!is.na(conus_idx)]

# k-NN weights on mod_data subset
mod_geo <- conus[conus_idx, ]
coords_mod <- mod_geo |> sf::st_centroid(of_largest_polygon = TRUE) |>
  sf::st_coordinates()
nb_mod <- spdep::knn2nb(spdep::knearneigh(coords_mod, k = 8), sym = TRUE)
lw_mod <- spdep::nb2listw(nb_mod, style = "W", zero.policy = TRUE)

cat("\n--- Moran's I on OLS residuals ---\n")
mi_resid <- spdep::moran.test(mod_data$ols_resid[match(mod_geo$GEOID, mod_data$GEOID)],
                              lw_mod,
                              zero.policy = TRUE, randomisation = TRUE)
cat("  I = ", sprintf("%.3f", mi_resid$estimate["Moran I statistic"]),
    " | p = ", format.pval(mi_resid$p.value, eps = 1e-16), "\n")

if (mi_resid$estimate["Moran I statistic"] > 0.1 && mi_resid$p.value < 0.001) {
  cat("\n  >>> RESIDUAL SPATIAL AUTOCORRELATION DETECTED.\n")
  cat("  >>> OLS standard errors are unreliable.\n")
  cat("  >>> Spatial regression (SEM/SAR/INLA) required — see Section 6.\n")
} else {
  cat("\n  Residuals appear spatially independent — OLS may be adequate.\n")
}

# ---- 5d. Residual choropleth   --------

resid_geo <- conus |>
  dplyr::inner_join(
    mod_data |> dplyr::select(GEOID, ols_resid),
    by = "GEOID"
  ) |>
  sf::st_transform(4326) |>
  tigris::shift_geometry(geoid_column = "GEOID",
                         preserve_area = FALSE,
                         position = "below")

# Diverging palette centered at 0 (red = under-predicted, blue = over-predicted)
resid_lim <- max(abs(quantile(resid_geo$ols_resid,
                              probs = c(0.02, 0.98), na.rm = TRUE)))

p_resid <- ggplot() +
  geom_sf(data = resid_geo, aes(fill = ols_resid), colour = NA) +
  # NJ — not modeled (no Core index; PLACES lacks BPHIGH & HIGHCHOL)
  geom_sf(data = nj_unavail, fill = NJ_UNAVAIL_FILL, colour = NA) +
  geom_sf(data = states_conus, fill = NA, colour = "grey25", linewidth = 0.25) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0,
    limits = c(-resid_lim, resid_lim),
    oob    = scales::squish,
    name   = "Residual\n(PC1 score units)"
  ) +
  coord_sf(datum = NA) +
  theme_void(base_size = 11) +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey30")) +
  labs(
    title    = "Spatial pattern of unexplained CKM burden",
    subtitle = paste0("OLS residuals after adjustment for SDOH + demographics. ",
                      "Red = under-prediction (excess CKM); blue = over-prediction. "),
    caption  = paste0("Moran's I = ",
                      sprintf("%.3f", mi_resid$estimate["Moran I statistic"]),
                      ", p < .001",
                      ". Strong residual clustering motivates the spatial random",
                      " effect added in Section 6.",
                      " New Jersey (grey) lacks PLACES Core-indicator data and",
                      " is not modeled.")
  )
ggsave(file.path(dir_fig, "05_ols_residuals_map.png"),
       p_resid, width = 11, height = 6.5, dpi = 220, bg = "white")
cat("  saved: ", file.path(dir_fig, "05_ols_residuals_map.png"), "\n")

# ---- 5e. Normality diagnostics  -------

cat("\n--- (5e) Outcome + OLS residual normality diagnostics ---\n")

# Skewness + kurtosis (excess) — quick numeric summary
.skew <- function(x) {
  x <- x[!is.na(x)]; n <- length(x); if (n < 3) return(NA)
  m <- mean(x); s <- sd(x)
  sum((x - m)^3) / ((n - 1) * s^3)
}
.kurt_excess <- function(x) {
  x <- x[!is.na(x)]; n <- length(x); if (n < 4) return(NA)
  m <- mean(x); s <- sd(x)
  sum((x - m)^4) / ((n - 1) * s^4) - 3
}

norm_diag <- tibble::tibble(
  series       = c("CKM Core PC1 (outcome)", "OLS residuals"),
  n            = c(sum(!is.na(mod_data$ckm_core_pca)),
                   sum(!is.na(mod_data$ols_resid))),
  mean         = c(mean(mod_data$ckm_core_pca, na.rm = TRUE),
                   mean(mod_data$ols_resid,    na.rm = TRUE)),
  sd           = c(sd(mod_data$ckm_core_pca, na.rm = TRUE),
                   sd(mod_data$ols_resid,    na.rm = TRUE)),
  skewness     = c(.skew(mod_data$ckm_core_pca),
                   .skew(mod_data$ols_resid)),
  excess_kurt  = c(.kurt_excess(mod_data$ckm_core_pca),
                   .kurt_excess(mod_data$ols_resid))
) |>
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))
print(norm_diag)
readr::write_csv(norm_diag,
                 file.path(dir_tbl, "05e_normality_diagnostics.csv"))

# Figure S9 — outcome distribution
p_hist <- ggplot(data.frame(x = mod_data$ckm_core_pca),
                 aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 60, fill = "#FED976", colour = "white",
                 linewidth = 0.2) +
  geom_density(colour = "#B2182B", linewidth = 0.8) +
  stat_function(fun = dnorm,
                args = list(mean = mean(mod_data$ckm_core_pca, na.rm = TRUE),
                            sd   = sd(mod_data$ckm_core_pca,   na.rm = TRUE)),
                colour = "#2166AC", linetype = "dashed", linewidth = 0.7) +
  labs(
    x = "CKM Core PC1 (PCA score units)",
    y = "Density",
    title    = "Outcome distribution — CKM Core PC1",
    subtitle = sprintf("Skewness = %.2f, excess kurtosis = %.2f, n = %s tracts. ",
                       .skew(mod_data$ckm_core_pca),
                       .kurt_excess(mod_data$ckm_core_pca),
                       formatC(sum(!is.na(mod_data$ckm_core_pca)),
                                big.mark = ",")),
    caption  = "Red curve = empirical density; blue dashed = best-fit normal."
  ) +
  theme_ckm
ggsave(file.path(dir_fig, "S9_outcome_normality.png"),
       p_hist, width = 8, height = 5, dpi = 220, bg = "white")
cat("  saved: ", file.path(dir_fig, "S9_outcome_normality.png"), "\n")

# Figure S10 — Q-Q plot of OLS residuals
p_qq <- ggplot(data.frame(r = mod_data$ols_resid),
               aes(sample = r)) +
  stat_qq(alpha = 0.15, colour = "#2166AC", size = 0.4) +
  stat_qq_line(colour = "#B2182B", linewidth = 0.7) +
  labs(
    x = "Theoretical quantiles (standard normal)",
    y = "Sample quantiles (OLS residuals)",
    title    = "Q–Q plot of OLS residuals vs. standard normal",
    subtitle = sprintf("Skewness = %.2f, excess kurtosis = %.2f, n = %s tracts.",
                       .skew(mod_data$ols_resid),
                       .kurt_excess(mod_data$ols_resid),
                       formatC(sum(!is.na(mod_data$ols_resid)),
                                big.mark = ",")),
    caption  = "Red line = identity (perfect normality). Curvature in tails ⇒ heavy/light tails."
  ) +
  theme_ckm
ggsave(file.path(dir_fig, "S10_ols_residual_qq.png"),
       p_qq, width = 7, height = 6, dpi = 220, bg = "white")
cat("  saved: ", file.path(dir_fig, "S10_ols_residual_qq.png"), "\n")

cat("\n=== Section 5 complete. ===\n")
cat("  OLS R²: ", round(ols_summary$r.squared, 3),
    " | Residual Moran's I: ", round(mi_resid$estimate["Moran I statistic"], 3),
    " | next: spatial regression (Section 6)\n")


# ==============================================================
# 6.  SPATIAL REGRESSION  (INLA BYM2 + LM tests + spatial-lag covariates)
# ==============================================================
# Step 6 (model-form selection): LM-err vs. LM-lag, robust variants.
# Step 7 (fit spatial model + interpret).


cat("\n========================================================\n")
cat("Section 6 — Spatial regression (LM tests + ESF + BYM2)\n")
cat("========================================================\n")

# ---- 6a. Reload Section 5 
# Allows sourcing Section 6 
if (!exists("ols_full") || !exists("mod_data") ||
    !exists("lw_mod") || !exists("mod_geo")) {
  ols_path <- file.path(dir_proc, "05_ols_baseline.rds")
  if (!file.exists(ols_path))
    stop("Section 5 has not been run. Source the script from the top.")
  ols_obj  <- readRDS(ols_path)
  ols_full <- ols_obj$model
  mod_data <- ols_full$model

  if (is.null(ols_obj$GEOID))
    stop("05_ols_baseline.rds was written before GEOID was persisted. ",
         "Delete data/processed/05_ols_baseline.rds and source from the top.")
  .rows <- as.integer(rownames(mod_data))
  stopifnot(!anyNA(.rows), max(.rows) <= length(ols_obj$GEOID))
  mod_data$GEOID     <- ols_obj$GEOID[.rows]
  stopifnot(!anyNA(mod_data$GEOID))
  mod_data$ols_resid <- residuals(ols_full)
  message("  Section 5 artifacts reloaded from cache.")
}

# ---- 6b. LM tests on baseline OLS  -------------

f_lm <- file.path(dir_proc, "06_lm_tests.rds")
if (!file.exists(f_lm)) {
  message("Running spdep Rao's-score / Lagrange-multiplier tests on OLS residuals...")

  rs_fn <- if (exists("lm.RStests", where = "package:spdep",
                       inherits = FALSE)) spdep::lm.RStests else
            spdep::lm.LMtests
  lm_tests <- tryCatch(
    rs_fn(ols_full, listw = lw_mod,
          test = c("RSerr", "RSlag", "adjRSerr", "adjRSlag", "SARMA"),
          zero.policy = TRUE),
    error = function(e) {
      message("  (new RS test names not accepted, using legacy LM names)")
      rs_fn(ols_full, listw = lw_mod,
            test = c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"),
            zero.policy = TRUE)
    })
  saveRDS(lm_tests, f_lm)
} else {
  lm_tests <- readRDS(f_lm)
  message("  RS tests cached.")
}

cat("\n--- Lagrange Multiplier tests (model-form selection) ---\n")
print(lm_tests)
lm_stats <- vapply(lm_tests, function(t) t$statistic, numeric(1))
lm_pvals <- vapply(lm_tests, function(t) t$p.value,    numeric(1))
lm_tab <- tibble::tibble(test = names(lm_tests),
                         statistic = lm_stats, p_value = lm_pvals) |>
  dplyr::arrange(p_value)
readr::write_csv(lm_tab, file.path(dir_tbl, "06_lm_tests.csv"))

cat("\nDecision rule (Anselin 1988):\n")

get_p <- function(...) {
  for (nm in c(...)) if (nm %in% names(lm_pvals)) return(lm_pvals[[nm]])
  NA_real_
}
sig_err  <- get_p("RSerr",   "LMerr")  < 0.001
sig_lag  <- get_p("RSlag",   "LMlag")  < 0.001
sig_rerr <- get_p("adjRSerr","RLMerr") < 0.001
sig_rlag <- get_p("adjRSlag","RLMlag") < 0.001
form_pick <- if (isTRUE(sig_rerr) && isTRUE(sig_rlag)) {
  "BOTH robust scores significant → SARMA / use full INLA BYM2"
} else if (isTRUE(sig_rerr)) {
  "Robust RSerr only → SEM (spatial error model) preferred"
} else if (isTRUE(sig_rlag)) {
  "Robust RSlag only → SAR / spatial lag model preferred"
} else if (isTRUE(sig_err) && isTRUE(sig_lag)) {
  "Both base scores sig but neither robust → check W spec (Section 8)"
} else {
  "Neither RSerr nor RSlag strongly significant — OLS may suffice"
}
cat("  ", form_pick, "\n\n")

# ---- 6c. Build the formula (defensive: includes env if present) ----

core_predictors_full <- c(
  # DEMO
  "pct_age_65_plus", "pct_hispanic",
  # ECON
  "ice_income", "pct_unemployed", "pct_bachelor_plus",
  # SOCIAL
  "ice_race", "pct_living_alone",
  # HEALTHCARE access + engagement
  "ACCESS2", "CHECKUP",
  # NEIGHBORHOOD / built env
  "ruca_class", "pct_no_vehicle",
  # BEHAVIORAL confounders
  "CSMOKING", "LPA"
)

core_predictors <- intersect(core_predictors_full, names(mod_data))
unavailable <- setdiff(core_predictors_full, core_predictors)
if (length(unavailable) > 0)
  message("  Dropping unavailable predictors: ",
          paste(unavailable, collapse = ", "))

# Environmental predictors 
env_candidates <- c("pm25_annual", "o3_8hrmax_4thmax",
                    "LILATracts_1And10", "tmax_warm", "walk_index")
env_predictors <- intersect(env_candidates, names(mod_data))
if (length(env_predictors) == 0 &&
    "analytic" %in% ls(envir = .GlobalEnv)) {

  env_predictors <- intersect(env_candidates, names(analytic))
  if (length(env_predictors) > 0) {
    mod_data <- mod_data |>
      dplyr::select(-dplyr::any_of(env_predictors)) |>
      dplyr::left_join(
        analytic |> dplyr::select(GEOID, dplyr::all_of(env_predictors)),
        by = "GEOID")
    message("  env predictors merged into mod_data: ",
            paste(env_predictors, collapse = ", "))
  }
}

# Aging × env interactions (paper's hypothesis)
aging_env_terms <- if (length(env_predictors) > 0)
  paste0("pct_age_65_plus:", env_predictors) else character(0)

all_predictors <- c(core_predictors, env_predictors, aging_env_terms)
fml_str <- paste("ckm_core_pca ~", paste(all_predictors, collapse = " + "))
fml <- stats::as.formula(fml_str)

cat("--- Spatial regression formula ---\n")
cat("  ", fml_str, "\n")
cat("  Predictors: ", length(core_predictors), " core + ",
    length(env_predictors), " env + ",
    length(aging_env_terms), " aging × env interactions\n", sep = "")
if (length(env_predictors) == 0)
  cat("  ! Env block empty — Section 1f-vii not yet run. Re-source after\n",
      "    env layers join to test the aging × env interactions.\n", sep = "")

# Drop rows with NA on any new env predictor
mod_data2 <- mod_data |> tidyr::drop_na(dplyr::all_of(c("ckm_core_pca",
                                                        core_predictors,
                                                        env_predictors)))


mod_data2 <- mod_data2 |>
  dplyr::mutate(dplyr::across(dplyr::any_of(env_predictors),
                              ~ suppressWarnings(as.numeric(.x))))

n_pre  <- nrow(mod_data2)
mod_data2 <- mod_data2 |>
  tidyr::drop_na(dplyr::all_of(env_predictors))
n_drop <- n_pre - nrow(mod_data2)
if (n_drop > 0)
  message("  ", n_drop, " rows dropped after numeric coercion of env block")

# Diagnostic: confirm column types before fitting
type_summary <- vapply(mod_data2[, c(core_predictors, env_predictors),
                                  drop = FALSE],
                       function(x) class(x)[1], character(1))
cat("  Predictor types:\n")
for (nm in names(type_summary))
  cat(sprintf("    %-22s: %s\n", nm, type_summary[[nm]]))

cat("  N modeling rows: ", nrow(mod_data2), " (", nrow(mod_data),
    " before env-NA drop)\n", sep = "")


if ("o3_8hrmax_4thmax" %in% names(mod_data2))
  mod_data2$o3_8hrmax_4thmax <- mod_data2$o3_8hrmax_4thmax * 100
interact_center <- intersect(
  c("pct_age_65_plus", "pm25_annual", "o3_8hrmax_4thmax",
    "tmax_warm", "walk_index"),
  names(mod_data2))

interact_means <- vapply(mod_data2[interact_center], mean, numeric(1),
                         na.rm = TRUE)
mod_data2 <- mod_data2 |>
  dplyr::mutate(dplyr::across(dplyr::all_of(interact_center),
                              ~ .x - mean(.x, na.rm = TRUE)))
cat("  Centered interacting predictors: ",
    paste(interact_center, collapse = ", "), "\n", sep = "")


ols_full2 <- lm(fml, data = mod_data2)

# ---- 6d. spmoran ESF — primary scalable spatial regression -----

f_esf <- file.path(dir_proc, "06_spmoran_resf.rds")

if (file.exists(f_esf)) {
  cached_esf <- tryCatch(readRDS(f_esf), error = function(e) NULL)
  esf_drift <- !is.null(cached_esf) && !is.null(cached_esf$formula) &&
               !identical(cached_esf$formula, fml_str)
  if (esf_drift) {
    message("ESF cache invalidated: predictor-set drift; refitting on ",
            length(c(core_predictors, env_predictors, aging_env_terms)),
            " predictors.")
    file.remove(f_esf)
  }
}
if (!file.exists(f_esf)) {
  if (!"spmoran" %in% installed.packages()[, "Package"]) {
    message("Installing spmoran...")
    install.packages("spmoran", repos = "https://cloud.r-project.org")
  }

  # Build coordinates aligned to mod_data2 row order
  esf_idx <- match(mod_data2$GEOID, mod_geo$GEOID)
  esf_idx <- esf_idx[!is.na(esf_idx)]
  coords_esf <- sf::st_centroid(mod_geo[esf_idx, ],
                                of_largest_polygon = TRUE) |>
    sf::st_coordinates()

  # Eigenvectors 
  message("Computing fast Moran eigenvectors (spmoran::meigen_f)...")
  t0 <- Sys.time()
  meig <- spmoran::meigen_f(coords = coords_esf, enum = 200)
  message("  meigen_f done in ", round(difftime(Sys.time(), t0,
                                                 units = "mins"), 1), " min")

  # Build numeric design matrix 
  mm <- model.matrix(fml, data = mod_data2)[, -1, drop = FALSE]
  y  <- mod_data2$ckm_core_pca

  message("Fitting spmoran::resf (random-effects spatial filter)...")
  t1 <- Sys.time()
  resf_fit <- spmoran::resf(y = y, x = mm, meig = meig)
  message("  resf done in ", round(difftime(Sys.time(), t1,
                                             units = "mins"), 1), " min")

  # Residual Moran's I after ESF correction
  esf_resid <- resf_fit$resid
  esf_lw    <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(
    coords_esf, k = 8), sym = TRUE), style = "W", zero.policy = TRUE)
  mi_esf <- spdep::moran.test(esf_resid, esf_lw, zero.policy = TRUE,
                               randomisation = TRUE)

  saveRDS(list(fit = resf_fit, meig_summary = list(n_eig = ncol(meig$sf)),
               residual_moran = mi_esf,
               n = nrow(mod_data2),
               formula = fml_str), f_esf)
  message("  saved: ", f_esf)
} else {
  resf_obj <- readRDS(f_esf)
  resf_fit <- resf_obj$fit
  mi_esf   <- resf_obj$residual_moran
  message("  spmoran cached: ", f_esf)
}

cat("\n--- spmoran ESF results ---\n")
cat("  N: ", nrow(mod_data2), "\n")

esf_stats <- stats::setNames(resf_fit$e[["stat"]], rownames(resf_fit$e))
.pick <- function(nm) if (nm %in% names(esf_stats)) esf_stats[[nm]] else NULL
adjR2_esf <- .pick("adjR2(cond)") %||% .pick("adjR2(GVC)") %||%
             .pick("adjR2") %||% NA_real_
cat("  Adjusted R² (conditional): ", round(adjR2_esf, 4), "\n")
cat("  Residual Moran's I (after ESF): ",
    sprintf("%.3f", mi_esf$estimate["Moran I statistic"]),
    " | p = ", format.pval(mi_esf$p.value, eps = 1e-16), "\n")
cat("\n  Coefficients (top of table):\n")
print(round(resf_fit$b[1:min(15, nrow(resf_fit$b)), ], 4))

# Save coefficient table for the manuscript
coef_esf <- tibble::as_tibble(resf_fit$b, rownames = "predictor")
readr::write_csv(coef_esf, file.path(dir_tbl, "06_coefficients_esf.csv"))

# ---- 6e. INLA BYM2 

f_bym2 <- file.path(dir_proc, "06_inla_bym2.rds")
have_inla <- requireNamespace("INLA", quietly = TRUE)

# ---- BYM2 cache completeness ------------------------------------------------

BYM2_REQUIRED <- c("summary_fixed", "summary_random",
                   "summary_hyperpar", "summary_fitted")

bym2_cache_missing <- function(x) {
  if (is.null(x)) return(character(0))
  setdiff(BYM2_REQUIRED, names(x)[!vapply(x, is.null, logical(1))])
}

warn_incomplete_bym2 <- function(x, path, label) {
  miss <- bym2_cache_missing(x)
  if (!length(miss)) return(invisible(TRUE))
  message("\n  !! ", label, " cache is INCOMPLETE (pre-2026-08-28 format).")
  message("     ", basename(path))
  message("     missing: ", paste(miss, collapse = ", "))
  message("     Nothing here is broken: this script's own outputs never read")
  message("     these fields, and 13_figure6_relabel.R needs only")
  message("     summary_random, which IS present. The one consumer of the")
  message("     missing fields, 12_bym2_posterior_residual.R, does not read")
  message("     this cache for them -- it refits both models itself and")
  message("     writes its own 12_bym2_refit_model*.rds.")
  message("     Do NOT delete this file to 'upgrade' it. A refit reproduces")
  message("     every posterior mean to ~2e-5 but returns a DIC/WAIC that")
  message("     differs from the published value by ~100 points (README,")
  message("     'INLA information criteria'), so deleting it would discard")
  message("     the exact fit the manuscript cites.")
  invisible(FALSE)
}


if (file.exists(f_bym2)) {
  cached_bym2 <- tryCatch(readRDS(f_bym2), error = function(e) NULL)
  if (!is.null(cached_bym2) && !is.null(cached_bym2$summary_fixed)) {
    cached_terms <- setdiff(rownames(cached_bym2$summary_fixed), "(Intercept)")

    current_terms <- tryCatch(
      colnames(model.matrix(fml, data = head(mod_data2, 5)))[-1],
      error = function(e) character(0))
    if (length(current_terms) > 0 &&
        !identical(sort(cached_terms), sort(current_terms))) {
      message("BYM2 cache invalidated: predictor-set drift (cached ",
              length(cached_terms), " fixed effects, current ",
              length(current_terms), "); will refit (~30-90 min).")
      file.remove(f_bym2)
    }
  }
  if (file.exists(f_bym2))
    warn_incomplete_bym2(cached_bym2, f_bym2, "BYM2 (Model 2, behavior-adjusted)")
}

if (!have_inla) {
  cat("\n  INLA not installed. To enable BYM2 modeling:\n",
      "    install.packages('INLA',\n",
      "      repos = c(getOption('repos'),\n",
      "                INLA = 'https://inla.r-inla-download.org/R/stable'),\n",
      "      dep = TRUE)\n", sep = "")
} else if (!file.exists(f_bym2)) {
  message("Fitting INLA BYM2 (this may take 30-90 min at this scale)...")

  # Convert nb_mod to INLA graph file (write once, reuse)
  graph_file <- file.path(dir_graph, "06_inla_graph.adj")
  if (!file.exists(graph_file))
    spdep::nb2INLA(graph_file, nb_mod)

  # Map each row of mod_data2 to a graph index
  mod_data2$idarea <- match(mod_data2$GEOID, mod_geo$GEOID)
  bym2_data <- mod_data2 |> dplyr::filter(!is.na(idarea))

  # BYM2 formula: fixed effects + spatial random effect
  fml_bym2 <- stats::as.formula(
    paste(fml_str,
          "+ f(idarea, model = 'bym2', graph = '", graph_file,
          "', scale.model = TRUE,",
          "    hyper = list(phi  = list(prior = 'pc',",
          "                              param = c(0.5, 0.5)),",
          "                 prec = list(prior = 'pc.prec',",
          "                              param = c(1, 0.01))))",
          sep = ""))

  t0 <- Sys.time()
  bym2_fit <- INLA::inla(
    fml_bym2, data = bym2_data, family = "gaussian",
    control.predictor = list(compute = TRUE),
    control.compute   = list(dic = TRUE, waic = TRUE,
                              return.marginals.predictor = FALSE),
    control.inla      = list(strategy = "adaptive",
                              int.strategy = "eb"),  
    verbose = FALSE
  )
  message("  INLA BYM2 done in ",
          round(difftime(Sys.time(), t0, units = "mins"), 1), " min")


  bym2_save <- list(
    summary_fixed    = bym2_fit$summary.fixed,
    summary_random   = bym2_fit$summary.random$idarea,
    summary_hyperpar = bym2_fit$summary.hyperpar,
    summary_fitted   = bym2_fit$summary.fitted.values,
    dic = bym2_fit$dic$dic,
    waic = bym2_fit$waic$waic,
    p_eff_dic = bym2_fit$dic$p.eff,
    p_eff_waic = bym2_fit$waic$p.eff,
    n = nrow(bym2_data),
    GEOID = bym2_data$GEOID,
    formula = paste(deparse(fml_bym2), collapse = " ")
  )
  saveRDS(bym2_save, f_bym2)
  cat("  saved: ", f_bym2, "\n")
} else {
  bym2_save <- readRDS(f_bym2)
  message("  BYM2 cached: ", f_bym2)
}

if (have_inla && file.exists(f_bym2)) {
  cat("\n--- INLA BYM2 fixed-effect coefficients ---\n")
  print(round(bym2_save$summary_fixed[, c("mean", "sd", "0.025quant",
                                           "0.975quant")], 4))
  cat("\n  DIC: ",  round(bym2_save$dic, 1),
      " | WAIC: ", round(bym2_save$waic, 1), "\n")

  bym2_fixed <- tibble::as_tibble(bym2_save$summary_fixed,
                                   rownames = "predictor")
  readr::write_csv(bym2_fixed, file.path(dir_tbl, "06_coefficients_bym2.csv"))
}

# ---- 6f. Coefficient-comparison table (OLS | ESF | BYM2) -------

cmp_ols <- broom::tidy(ols_full2) |>
  dplyr::transmute(predictor = term,
                   ols_estimate = estimate, ols_se = std.error,
                   ols_p = p.value)
cmp_esf <- coef_esf |>
  dplyr::transmute(predictor,
                   esf_estimate = .data[[grep("Estimate",
                                              names(coef_esf),
                                              value = TRUE)[1]]] %||% NA,
                   esf_p        = .data[[grep("p_value|Pr",
                                              names(coef_esf),
                                              value = TRUE)[1]]] %||% NA)

cmp <- dplyr::full_join(cmp_ols, cmp_esf, by = "predictor")
if (have_inla && file.exists(f_bym2)) {
  cmp_bym2 <- tibble::as_tibble(bym2_save$summary_fixed,
                                 rownames = "predictor") |>
    dplyr::transmute(predictor,
                     bym2_estimate = mean, bym2_2.5 = `0.025quant`,
                     bym2_97.5 = `0.975quant`)
  cmp <- dplyr::full_join(cmp, cmp_bym2, by = "predictor")
}
readr::write_csv(cmp, file.path(dir_tbl, "06_coefficient_comparison.csv"))
cat("\n  Coefficient-comparison table saved: ",
    file.path(dir_tbl, "06_coefficient_comparison.csv"), "\n")

# ---- 6g. Residual Moran's I progression ------------------------
# Visualize how spatial models reduce residual autocorrelation.
ols_obj_cached <- readRDS(file.path(dir_proc, "05_ols_baseline.rds"))
mi_ols <- spdep::moran.test(residuals(ols_obj_cached$model), lw_mod,
                            zero.policy = TRUE, randomisation = TRUE)
mi_progression <- tibble::tibble(
  model   = c("OLS baseline (S5)", "spmoran ESF (6d)"),
  moran_I = c(unname(mi_ols$estimate[1]), unname(mi_esf$estimate[1])),
  p_value = c(mi_ols$p.value,             mi_esf$p.value)
)
print(mi_progression)
readr::write_csv(mi_progression,
                 file.path(dir_tbl, "06_residual_moran_progression.csv"))

# ---- 6h. BYM2 spatial random-effect map -----------------------

if (have_inla && file.exists(f_bym2) &&
    !is.null(bym2_save$summary_random)) {
  message("Building BYM2 spatial random-effect map...")

  re_df <- tibble::as_tibble(bym2_save$summary_random)
  if (!"idarea" %in% names(re_df) && "ID" %in% names(re_df))
    names(re_df)[match("ID", names(re_df))] <- "idarea"
  n_tracts_bym2 <- nrow(mod_geo)
  re_df <- head(re_df, n_tracts_bym2)
  re_df$GEOID <- mod_geo$GEOID

  re_geo <- tracts_geo |>
    dplyr::mutate(GEOID = enc2utf8(as.character(GEOID)) |>
                            stringr::str_trim()) |>
    dplyr::inner_join(
      re_df |> dplyr::select(GEOID, mean) |>
        dplyr::mutate(GEOID = enc2utf8(as.character(GEOID)) |>
                                stringr::str_trim()),
      by = "GEOID") |>
    dplyr::filter(!substr(GEOID, 1, 2) %in% c("60", "66", "69", "72", "78"),
                  ALAND > 0) |>
    sf::st_transform(4326) |>
    tigris::shift_geometry(geoid_column = "GEOID",
                            preserve_area = FALSE, position = "below")

  re_lim <- max(abs(quantile(re_geo$mean, c(0.02, 0.98), na.rm = TRUE)))

  p_bym2_map <- ggplot() +
    geom_sf(data = re_geo, aes(fill = mean), colour = NA) +

    geom_sf(data = nj_unavail, fill = NJ_UNAVAIL_FILL, colour = NA) +
    geom_sf(data = states_conus, fill = NA, colour = "grey25", linewidth = 0.25) +
    scale_fill_gradient2(
      low = "#2166AC", mid = "white", high = "#B2182B",
      midpoint = 0,
      limits = c(-re_lim, re_lim), oob = scales::squish,
      name = "Spatial\nrandom effect\n(PC1 score units)") +
    coord_sf(datum = NA) +
    theme_void(base_size = 11) +
    theme(legend.position = "right",
          plot.title    = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey30")) +
    labs(
      title    = "Unexplained spatial structure in CKM burden after full adjustment",
      subtitle = paste0("BYM2 spatial random effect after adjusting for ",
                        "demographic, SDOH, behavioral, healthcare-engagement, ",
                        "and environmental predictors. ",
                        "Red = excess burden beyond what predictors explain."),
      caption  = paste0("Hierarchical Bayesian INLA BYM2 model. ",
                        "Persistent red clusters reveal regions where additional ",
                        "unmeasured factors drive CKM burden. ",
                        "New Jersey (grey) lacks PLACES Core-indicator data and ",
                        "is not modeled.")
    )
  ggsave(file.path(dir_fig, "06h_bym2_spatial_random_effect.png"),
         p_bym2_map, width = 11, height = 6.5, dpi = 220, bg = "white")
  cat("  saved: ", file.path(dir_fig, "06h_bym2_spatial_random_effect.png"),
      "\n")
}

cat("\n=== Section 6 complete. ===\n")
cat("  LM-test verdict: ", form_pick, "\n", sep = "")
cat("  Residual I: OLS=", sprintf("%.3f", mi_progression$moran_I[1]),
    " → ESF=", sprintf("%.3f", mi_progression$moran_I[2]), "\n", sep = "")
cat("  Next: lecture-canonical diagnostics (Section 6.5) → ML pillar (Section 7)\n")


# ==============================================================
# 6.5  CANONICAL DIAGNOSTICS  (LISA, Gi*, correlogram, variogram, AIC)
# ==============================================================
#  diagnostics that the
# OLS → ESF → BYM2 progression doesn't produce by default.
#
#   6.5a  Local Moran's I (LISA) cluster map         → Figure S5
#   6.5b  Getis-Ord Gi* hotspot map                  → Figure S6
#   6.5c  Moran's I correlogram (autocorr by lag)    → Figure S7
#   6.5d  Residual semivariogram on tract centroids  → Figure S8
#   6.5e  AIC / DIC / WAIC comparison table          → Table 7

cat("\n========================================================\n")
cat("Section 6.5 — Lecture-canonical diagnostics\n")
cat("========================================================\n")

# ---- 6.5a. Local Moran's I (LISA) ----------------------------

f_lisa <- file.path(dir_proc, "06_5a_lisa.rds")
if (!file.exists(f_lisa)) {
  message("Computing Local Moran's I (LISA) on CKM Core index...")
  lisa_vals <- spdep::localmoran(conus$ckm_core_pca, lw_knn,
                                 zero.policy = TRUE)
  pcol <- intersect(c("Pr(z != E(Ii))", "Pr(z > 0)", "Pr(z < 0)"),
                    colnames(lisa_vals))[1]
  if (is.na(pcol)) pcol <- grep("^Pr", colnames(lisa_vals), value = TRUE)[1]

  # Spatial-lag standardized x for HH/LL/HL/LH classification
  x_std  <- as.numeric(scale(conus$ckm_core_pca))
  wx_std <- spdep::lag.listw(lw_knn, x_std, zero.policy = TRUE)

  lisa_df <- tibble::tibble(
    GEOID   = conus$GEOID,
    Ii      = lisa_vals[, "Ii"],
    p       = lisa_vals[, pcol],
    z       = lisa_vals[, "Z.Ii"],
    cluster = dplyr::case_when(
      is.na(lisa_vals[, pcol]) | lisa_vals[, pcol] >= 0.05 ~ "Not significant",
      x_std > 0  & wx_std > 0  ~ "HH (high-high)",
      x_std < 0  & wx_std < 0  ~ "LL (low-low)",
      x_std > 0  & wx_std < 0  ~ "HL (high-low outlier)",
      x_std < 0  & wx_std > 0  ~ "LH (low-high outlier)",
      TRUE                     ~ "Not significant"
    )
  )
  saveRDS(lisa_df, f_lisa)
  message("  saved: ", f_lisa)
} else {
  lisa_df <- readRDS(f_lisa)
  message("  cached: ", f_lisa)
}

cat("\n--- LISA classification (CKM Core, k=8 W) ---\n")
print(table(lisa_df$cluster, useNA = "ifany"))

lisa_geo <- conus |>
  dplyr::inner_join(lisa_df, by = "GEOID") |>
  sf::st_transform(4326) |>
  tigris::shift_geometry(geoid_column = "GEOID",
                         preserve_area = FALSE,
                         position = "below")

p_lisa <- ggplot() +
  geom_sf(data = lisa_geo, aes(fill = cluster), colour = NA) +

  geom_sf(data = nj_unavail, aes(fill = "Data unavailable (New Jersey)"),
          colour = NA) +
  geom_sf(data = states_conus, fill = NA, colour = "grey25", linewidth = 0.25) +
  scale_fill_manual(
    values = c("HH (high-high)"        = "#B2182B",
               "LL (low-low)"          = "#2166AC",
               "HL (high-low outlier)" = "#F4A582",
               "LH (low-high outlier)" = "#92C5DE",
               "Not significant"       = "grey90",
               "Data unavailable (New Jersey)" = NJ_UNAVAIL_FILL),
    name = "LISA cluster",
    breaks = c("HH (high-high)", "LL (low-low)",
               "HL (high-low outlier)", "LH (low-high outlier)",
               "Not significant", "Data unavailable (New Jersey)"),
    drop = FALSE) +
  coord_sf(datum = NA) +
  theme_void(base_size = 11) +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey30")) +
  labs(
    title    = "Local Moran's I (LISA) cluster map of CKM Core burden",
    subtitle = "Significant local autocorrelation patterns (p < .05). LISA identifies local autocorrelation, not statistically confirmed disease clusters.",
    caption  = paste0("HH = high-burden tract surrounded by high-burden neighbors; LL = low-low; HL/LH = spatial outliers. ",
                      "k = 8 nearest-neighbor weights, n = ",
                      formatC(nrow(lisa_geo), big.mark = ","),
                      " CONUS tracts.")
  )
ggsave(file.path(dir_fig, "06_5a_lisa_cluster_map.png"),
       p_lisa, width = 11, height = 6.5, dpi = 220, bg = "white")
cat("  saved: ", file.path(dir_fig, "06_5a_lisa_cluster_map.png"), "\n")

# ---- 6.5b. Getis-Ord Gi* hotspot statistic --------------------

f_gi <- file.path(dir_proc, "06_5b_getis_ord.rds")
if (!file.exists(f_gi)) {
  message("Computing Getis-Ord Gi* hotspot statistic...")
  lw_self <- spdep::nb2listw(spdep::include.self(nb_knn),
                             style = "W", zero.policy = TRUE)
  gi_vals <- spdep::localG(conus$ckm_core_pca, lw_self,
                            zero.policy = TRUE)
  gi_df <- tibble::tibble(GEOID   = conus$GEOID,
                          Gi_star = as.numeric(gi_vals))
  saveRDS(gi_df, f_gi)
  message("  saved: ", f_gi)
} else {
  gi_df <- readRDS(f_gi)
  message("  cached: ", f_gi)
}
cat("\n--- Getis-Ord Gi* z-score range ---\n")
cat("  [", sprintf("%.2f", min(gi_df$Gi_star, na.rm = TRUE)),
    ", ", sprintf("%.2f", max(gi_df$Gi_star, na.rm = TRUE)), "]",
    "  (|z|>2.58 ⇒ p<0.01)\n", sep = "")
cat("  Hot spots (z > 2.58):  ", sum(gi_df$Gi_star >  2.58, na.rm = TRUE), "\n")
cat("  Cold spots (z <-2.58): ", sum(gi_df$Gi_star < -2.58, na.rm = TRUE), "\n")

gi_geo <- conus |>
  dplyr::inner_join(gi_df, by = "GEOID") |>
  sf::st_transform(4326) |>
  tigris::shift_geometry(geoid_column = "GEOID",
                         preserve_area = FALSE,
                         position = "below")

p_gi <- ggplot() +
  geom_sf(data = gi_geo, aes(fill = Gi_star), colour = NA) +
  # NJ — not analyzed (no Core index; PLACES lacks BPHIGH & HIGHCHOL)
  geom_sf(data = nj_unavail, fill = NJ_UNAVAIL_FILL, colour = NA) +
  geom_sf(data = states_conus, fill = NA, colour = "grey25", linewidth = 0.25) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, limits = c(-6, 6), oob = scales::squish,
    name = "Gi* z-score") +
  coord_sf(datum = NA) +
  theme_void(base_size = 11) +
  theme(legend.position = "right",
        plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey30")) +
  labs(
    title    = "Getis-Ord Gi* hotspot map of CKM Core burden",
    subtitle = "Red = significant burden hotspots (z > 0); blue = cold spots (z < 0).",
    caption  = paste0("Gi* with self in neighbor set, k = 8, n = ",
                      formatC(nrow(gi_geo), big.mark = ","),
                      " CONUS tracts. New Jersey (grey) lacks PLACES ",
                      "Core-indicator data and is not analyzed.")
  )
ggsave(file.path(dir_fig, "06_5b_getis_ord_gi.png"),
       p_gi, width = 11, height = 6.5, dpi = 220, bg = "white")
cat("  saved: ", file.path(dir_fig, "06_5b_getis_ord_gi.png"), "\n")

# ---- 6.5c. Moran's I correlogram ------------------------------

f_correl <- file.path(dir_proc, "06_5c_moran_correlogram.rds")
if (!file.exists(f_correl)) {
  message("Computing Moran's I correlogram on 5,000-tract subsample...")
  set.seed(20260426)
  sub_idx    <- sample(seq_len(nrow(conus)), min(5000, nrow(conus)))
  conus_sub  <- conus[sub_idx, ]
  coords_sub <- conus_sub |>
    sf::st_centroid(of_largest_polygon = TRUE) |>
    sf::st_coordinates()
  nb_sub <- spdep::knn2nb(spdep::knearneigh(coords_sub, k = 8), sym = TRUE)

  correl_res <- spdep::sp.correlogram(
    nb_sub, conus_sub$ckm_core_pca,
    order = 6, method = "I",
    style = "W", zero.policy = TRUE)
  saveRDS(correl_res, f_correl)
  message("  saved: ", f_correl)
} else {
  correl_res <- readRDS(f_correl)
  message("  cached: ", f_correl)
}

cat("\n--- Moran's I correlogram (lag orders 1-6) ---\n")
correl_I <- correl_res$res[, 1]
correl_df <- tibble::tibble(lag = seq_along(correl_I), I = correl_I)
print(round(correl_df, 3))

p_correl <- ggplot(correl_df, aes(x = lag, y = I)) +
  geom_hline(yintercept = 0, colour = "grey50", linetype = "dashed") +
  geom_line(colour = "#FD8D3C", linewidth = 1) +
  geom_point(size = 3, colour = "#FD8D3C") +
  geom_text(aes(label = sprintf("%.3f", I)), vjust = -0.8, size = 3.2) +
  scale_x_continuous(breaks = correl_df$lag) +
  labs(
    x = "Spatial lag order (neighbors of neighbors of …)",
    y = "Moran's I",
    title    = "Moran's I correlogram — autocorrelation decays with lag order",
    subtitle = "5,000-tract systematic subsample, k = 8 nearest-neighbor weights"
  ) +
  theme_ckm
ggsave(file.path(dir_fig, "06_5c_moran_correlogram.png"),
       p_correl, width = 8, height = 5, dpi = 220, bg = "white")
cat("  saved: ", file.path(dir_fig, "06_5c_moran_correlogram.png"), "\n")

# ---- 6.5d. Residual semivariogram on tract centroids -----------

f_var <- file.path(dir_proc, "06_5d_residual_variogram.rds")
if (!file.exists(f_var)) {
  message("Computing residual semivariogram on OLS residuals...")
  if (!"gstat" %in% installed.packages()[, "Package"])
    install.packages("gstat", repos = "https://cloud.r-project.org")

  # Use mod_geo + mod_data2 OLS residuals (already computed in §5)
  variog_input <- mod_data2 |>
    dplyr::transmute(GEOID, ols_resid = residuals(ols_full2)) |>
    dplyr::inner_join(
      mod_geo |> dplyr::select(GEOID),
      by = "GEOID"
    )
  variog_sf <- mod_geo |>
    dplyr::filter(GEOID %in% variog_input$GEOID) |>
    dplyr::left_join(variog_input, by = "GEOID") |>
    sf::st_centroid(of_largest_polygon = TRUE) |>
    dplyr::filter(!is.na(ols_resid))

  # Sample for tractability (full variogram on 50k+ centroids is heavy)
  set.seed(20260426)
  variog_sub <- variog_sf[sample(seq_len(nrow(variog_sf)),
                                  min(5000, nrow(variog_sf))), ]
  variog_emp <- gstat::variogram(ols_resid ~ 1, variog_sub,
                                  cutoff = 1500000,    # 1500 km
                                  width  = 50000)      # 50 km bins
  saveRDS(variog_emp, f_var)
  message("  saved: ", f_var)
} else {
  variog_emp <- readRDS(f_var)
  message("  cached: ", f_var)
}
cat("\n--- Residual semivariogram (OLS residuals) ---\n")
cat("  bins: ", nrow(variog_emp), " | distance range: 0-",
    round(max(variog_emp$dist) / 1000), " km\n", sep = "")

p_var <- ggplot(variog_emp, aes(x = dist / 1000, y = gamma)) +
  geom_point(aes(size = np / 1000), alpha = 0.7, colour = "#2166AC") +
  geom_line(colour = "#2166AC", linewidth = 0.5) +
  scale_size_continuous(name = "Pairs\n(thousands)",
                        trans  = "sqrt",
                        range  = c(1.5, 5),
                        breaks = c(50, 150, 250)) +
  labs(
    x = "Distance between tract centroids (km)",
    y = expression("Semivariance " * gamma * "(h)"),
    title    = "Residual semivariogram on OLS residuals (tract centroids)",
    subtitle = paste0("Geostatistical diagnostic on tract centroids. ",
                       "5,000-tract subsample.")
  ) +
  theme_ckm
ggsave(file.path(dir_fig, "06_5d_residual_semivariogram.png"),
       p_var, width = 8, height = 5, dpi = 220, bg = "white")
cat("  saved: ", file.path(dir_fig, "06_5d_residual_semivariogram.png"), "\n")

# ---- 6.5e. AIC / DIC / WAIC fit-comparison table --------------

cat("\n--- Model fit comparison: OLS / ESF / BYM2 ---\n")

ols_obj_fit  <- readRDS(file.path(dir_proc, "05_ols_baseline.rds"))
esf_obj_fit  <- readRDS(file.path(dir_proc, "06_spmoran_resf.rds"))
bym2_obj_fit <- readRDS(file.path(dir_proc, "06_inla_bym2.rds"))

# OLS AIC — both the simple and the full (post-env) OLS
aic_ols_simple <- AIC(ols_obj_fit$model)
aic_ols_full   <- AIC(ols_full2)

# ESF: spmoran reports an information criterion in fit$e if available
aic_esf <- tryCatch({
  esf_e <- esf_obj_fit$fit$e
  ic_nm <- intersect(c("AIC", "aic", "BIC", "bic"), names(esf_e))[1]
  if (!is.na(ic_nm)) as.numeric(esf_e[[ic_nm]]) else NA_real_
}, error = function(e) NA_real_)

# BYM2: DIC and WAIC stored in cached object
dic_bym2  <- bym2_obj_fit$dic
waic_bym2 <- bym2_obj_fit$waic

fit_compare <- tibble::tibble(
  model      = c("OLS baseline (§5)",
                  "OLS full (post-env, §6 fml)",
                  "spmoran ESF (§6d)",
                  "INLA BYM2 (§6e)"),
  n          = c(ols_obj_fit$n,
                  nrow(mod_data2),
                  esf_obj_fit$n %||% NA_real_,
                  bym2_obj_fit$n),
  AIC        = c(round(aic_ols_simple, 1),
                  round(aic_ols_full, 1),
                  if (is.na(aic_esf)) NA else round(aic_esf, 1),
                  NA),
  DIC        = c(NA, NA, NA, round(dic_bym2, 1)),
  WAIC       = c(NA, NA, NA, round(waic_bym2, 1)),
  p_eff_dic  = c(NA, NA, NA,
                 round(bym2_obj_fit$p_eff_dic  %||% NA_real_, 1)),
  p_eff_waic = c(NA, NA, NA,
                 round(bym2_obj_fit$p_eff_waic %||% NA_real_, 1))
)
if (is.na(fit_compare$p_eff_dic[4]))
  message("  NOTE: p.eff absent from the cached Model 2 fit. ",
          "Delete the BYM2 cache and refit to populate it.")
print(fit_compare)
readr::write_csv(fit_compare, file.path(dir_tbl, "06_5e_fit_comparison.csv"))
cat("  saved: ", file.path(dir_tbl, "06_5e_fit_comparison.csv"), "\n")

cat("\n=== Section 6.5 complete. ===\n")
cat("  Lecture-canonical diagnostics added:\n")
cat("    LISA cluster map         → 06_5a_lisa_cluster_map.png\n")
cat("    Getis-Ord Gi* hotspot    → 06_5b_getis_ord_gi.png\n")
cat("    Moran's I correlogram    → 06_5c_moran_correlogram.png\n")
cat("    Residual semivariogram   → 06_5d_residual_semivariogram.png\n")
cat("    Fit-comparison table     → 06_5e_fit_comparison.csv\n")


# ==============================================================
# 6.6  Wx FOCAL-VS-NEIGHBORHOOD-EFFECTS DECOMPOSITION
# ==============================================================



cat("\n========================================================\n")
cat("Section 6.6 — Wx (focal-vs-neighborhood) decomposition\n")
cat("========================================================\n")

# ---- 6.6a. Choose focal predictors for Wx augmentation --------

WX_PREDICTORS <- intersect(
  c("pct_age_65_plus", "ice_income", "ice_race",
    "pct_bachelor_plus", "pct_hispanic",
    "pm25_annual", "o3_8hrmax_4thmax", "tmax_warm", "walk_index"),
  names(mod_data2)
)
cat("Wx predictors (", length(WX_PREDICTORS), "): ",
    paste(WX_PREDICTORS, collapse = ", "), "\n", sep = "")

# ---- 6.6b. Compute Wx terms on the WIDER analytic universe ----

wx_input <- conus |>
  sf::st_drop_geometry() |>
  dplyr::select(GEOID) |>
  dplyr::mutate(GEOID = as.character(GEOID)) |>
  dplyr::left_join(
    analytic |>
      dplyr::select(GEOID, dplyr::any_of(WX_PREDICTORS)) |>
      dplyr::mutate(GEOID = as.character(GEOID)) |>
      dplyr::mutate(dplyr::across(dplyr::any_of(WX_PREDICTORS),
                                   ~ suppressWarnings(as.numeric(.x)))),
    by = "GEOID"
  )
wx_input_idx <- seq_len(nrow(wx_input))   # left_join preserves conus order


if ("o3_8hrmax_4thmax" %in% names(wx_input))
  wx_input$o3_8hrmax_4thmax <- wx_input$o3_8hrmax_4thmax * 100

cat("  Computing Wx on wider universe (n = ", nrow(wx_input),
    " CONUS tracts) ...\n", sep = "")

for (pred in WX_PREDICTORS) {
  if (!pred %in% names(wx_input)) {
    cat("    ! ", pred, " not in wx_input; skipping\n")
    next
  }
  x_vec <- wx_input[[pred]][wx_input_idx]   # align to lw_knn order (= conus)

  # (1) Zero-fill NAs for the numerator
  x_zeroed <- x_vec
  x_zeroed[is.na(x_zeroed)] <- 0

  # (2) Indicator for "neighbor has a non-NA value"
  na_indicator <- as.numeric(!is.na(x_vec))

  # (3) Multiply both through W (no NAs now → no NA propagation)
  wx_numer <- spdep::lag.listw(lw_knn, x_zeroed,     zero.policy = TRUE)
  wx_wt    <- spdep::lag.listw(lw_knn, na_indicator, zero.policy = TRUE)

  # (4) Renormalize: average over non-NA neighbors
  wx_wt[wx_wt == 0] <- NA_real_   # avoid div-by-zero (zero non-NA neighbors)
  wx_vec <- wx_numer / wx_wt

  wx_input[[paste0("wx_", pred)]] <-
    wx_vec[match(wx_input$GEOID, conus$GEOID)]
}

mod_data2 <- mod_data2 |> dplyr::select(-dplyr::any_of(starts_with("wx_")))
wx_join <- wx_input |>
  dplyr::select(GEOID, dplyr::starts_with("wx_")) |>
  dplyr::mutate(GEOID = as.character(GEOID))
mod_data2 <- mod_data2 |>
  dplyr::mutate(GEOID = as.character(GEOID)) |>
  dplyr::left_join(wx_join, by = "GEOID")
cat("  Wx columns merged into mod_data2: ",
    sum(grepl("^wx_", names(mod_data2))), "\n")


n_pre_wx <- nrow(mod_data2)
mod_data2_wx <- mod_data2 |>
  tidyr::drop_na(dplyr::all_of(paste0("wx_", WX_PREDICTORS)))
n_drop_wx <- n_pre_wx - nrow(mod_data2_wx)
cat("  Rows dropped due to NA Wx values: ", n_drop_wx, " of ", n_pre_wx,
    " (", round(100 * n_drop_wx / n_pre_wx, 1), "%)\n", sep = "")

# ---- 6.6c. Refit OLS with focal + Wx -------------------------

fml_wx_str <- paste(
  "ckm_core_pca ~",
  paste(c(core_predictors, env_predictors, aging_env_terms,
          paste0("wx_", WX_PREDICTORS)), collapse = " + ")
)
fml_wx <- stats::as.formula(fml_wx_str)
cat("  OLS focal+Wx — fitting...\n")
ols_wx <- lm(fml_wx, data = mod_data2_wx)
r2_wx       <- summary(ols_wx)$r.squared
r2_focal    <- summary(ols_full2)$r.squared
cat(sprintf("    R² (focal+Wx)    = %.4f\n", r2_wx))
cat(sprintf("    R² (focal-only)  = %.4f\n", r2_focal))
cat(sprintf("    ΔR²              = +%.4f\n", r2_wx - r2_focal))

# Residual Moran's I after Wx

mod_geo_wx <- mod_geo[match(mod_data2_wx$GEOID, mod_geo$GEOID), ]
coords_wx  <- mod_geo_wx |>
  sf::st_centroid(of_largest_polygon = TRUE) |>
  sf::st_coordinates()
nb_wx <- spdep::knn2nb(spdep::knearneigh(coords_wx, k = 8), sym = TRUE)
lw_wx <- spdep::nb2listw(nb_wx, style = "W", zero.policy = TRUE)
mi_wx <- spdep::moran.test(residuals(ols_wx), lw_wx,
                            zero.policy = TRUE, randomisation = TRUE)
cat(sprintf("    Residual Moran's I (focal+Wx) = %.3f\n",
            mi_wx$estimate["Moran I statistic"]))
cat(sprintf("    Residual Moran's I (focal-only) = %.3f\n",
            mi_resid$estimate["Moran I statistic"]))
cat(sprintf("    ΔI                        = %+.3f\n",
            mi_wx$estimate["Moran I statistic"] -
            mi_resid$estimate["Moran I statistic"]))

# ---- 6.6d. Refit BYM2 with focal + Wx ------------------------
f_bym2_wx <- file.path(dir_proc, "06_6d_inla_bym2_wx.rds")
have_inla <- requireNamespace("INLA", quietly = TRUE)

# Auto-invalidate the BYM2-Wx cache when:

if (file.exists(f_bym2_wx)) {
  cached_wx <- tryCatch(readRDS(f_bym2_wx), error = function(e) NULL)
  invalidate_wx <- FALSE
  invalidate_reason <- ""
  if (!is.null(cached_wx) &&
      abs(cached_wx$n - nrow(mod_data2_wx)) > 1000) {
    invalidate_wx     <- TRUE
    invalidate_reason <- paste0(
      "n drift (cache n=", cached_wx$n,
      ", current Wx n=", nrow(mod_data2_wx), ")")
  }
  aq_mtime <- file.info(file.path(dir_proc, "01f_aq_tract.rds"))$mtime
  wx_mtime <- file.info(f_bym2_wx)$mtime
  if (!is.na(aq_mtime) && !is.na(wx_mtime) && aq_mtime > wx_mtime) {
    invalidate_wx     <- TRUE
    invalidate_reason <- paste0(
      "AQ surface (01f_aq_tract.rds) is newer than BYM2-Wx cache; ",
      "upstream env predictors changed")
  }
  # Predictor-set drift: cached fixed-effect names != current expected
  if (!invalidate_wx && !is.null(cached_wx) &&
      !is.null(cached_wx$summary_fixed)) {
    cached_terms_wx <- setdiff(rownames(cached_wx$summary_fixed), "(Intercept)")
    current_terms_wx <- tryCatch(
      colnames(model.matrix(fml_wx, data = head(mod_data2_wx, 5)))[-1],
      error = function(e) character(0))
    if (length(current_terms_wx) > 0 &&
        !identical(sort(cached_terms_wx), sort(current_terms_wx))) {
      invalidate_wx     <- TRUE
      invalidate_reason <- paste0(
        "predictor-set drift (cached ", length(cached_terms_wx),
        " fixed effects, current ", length(current_terms_wx), ")")
    }
  }
  if (invalidate_wx) {
    message("BYM2-Wx cache invalidated: ", invalidate_reason,
            "; refitting BYM2 with current focal+Wx predictors.")
    file.remove(f_bym2_wx)
  }
}

if (have_inla && !file.exists(f_bym2_wx)) {
  message("Fitting BYM2 with focal + Wx (this may take 10-20 min)...")
  graph_file <- file.path(dir_graph, "06_inla_graph.adj")
  if (!file.exists(graph_file))
    spdep::nb2INLA(graph_file, nb_mod)

  mod_data2_wx$idarea <- match(mod_data2_wx$GEOID, mod_geo$GEOID)
  bym2_data_wx <- mod_data2_wx |> dplyr::filter(!is.na(idarea))

  fml_bym2_wx <- stats::as.formula(
    paste(fml_wx_str,
          "+ f(idarea, model = 'bym2', graph = '", graph_file,
          "', scale.model = TRUE,",
          "    hyper = list(phi  = list(prior = 'pc',",
          "                              param = c(0.5, 0.5)),",
          "                 prec = list(prior = 'pc.prec',",
          "                              param = c(1, 0.01))))",
          sep = ""))

  t0 <- Sys.time()
  bym2_wx_fit <- INLA::inla(
    fml_bym2_wx, data = bym2_data_wx, family = "gaussian",
    control.predictor = list(compute = TRUE),
    control.compute   = list(dic = TRUE, waic = TRUE,
                              return.marginals.predictor = FALSE),
    control.inla      = list(strategy = "adaptive",
                              int.strategy = "eb"),
    verbose = FALSE
  )
  message("  BYM2-Wx done in ",
          round(difftime(Sys.time(), t0, units = "mins"), 1), " min")

  bym2_wx_save <- list(
    summary_fixed  = bym2_wx_fit$summary.fixed,
    summary_random = bym2_wx_fit$summary.random$idarea,
    dic            = bym2_wx_fit$dic$dic,
    waic           = bym2_wx_fit$waic$waic,
    n              = nrow(bym2_data_wx),
    formula        = paste(deparse(fml_bym2_wx), collapse = " ")
  )
  saveRDS(bym2_wx_save, f_bym2_wx)
  cat("  saved: ", f_bym2_wx, "\n")
} else if (file.exists(f_bym2_wx)) {
  bym2_wx_save <- readRDS(f_bym2_wx)
  message("  BYM2-Wx cached: ", f_bym2_wx)
} else {
  message("  INLA not installed; BYM2-Wx skipped.")
  bym2_wx_save <- NULL
}

# ---- 6.6e. Coefficient comparison: focal-only vs. focal+Wx ----
if (have_inla && !is.null(bym2_wx_save)) {
  bym2_focal_df <- tibble::as_tibble(bym2_save$summary_fixed,
                                      rownames = "predictor") |>
    dplyr::transmute(predictor,
                     focal_only_mean = mean,
                     focal_only_2.5  = `0.025quant`,
                     focal_only_97.5 = `0.975quant`)
  bym2_wx_df <- tibble::as_tibble(bym2_wx_save$summary_fixed,
                                   rownames = "predictor") |>
    dplyr::transmute(predictor,
                     focal_wx_mean = mean,
                     focal_wx_2.5  = `0.025quant`,
                     focal_wx_97.5 = `0.975quant`)
  bym2_compare <- dplyr::full_join(bym2_focal_df, bym2_wx_df,
                                    by = "predictor") |>
    dplyr::mutate(
      pct_change = ifelse(!is.na(focal_only_mean) & focal_only_mean != 0,
                          round(100 * (focal_wx_mean - focal_only_mean) /
                                  abs(focal_only_mean), 1),
                          NA_real_)
    )
  readr::write_csv(bym2_compare,
                    file.path(dir_tbl, "06_6e_bym2_focal_vs_wx.csv"))
  cat("  saved: ", file.path(dir_tbl, "06_6e_bym2_focal_vs_wx.csv"), "\n")

  # Wx-only effects (the new neighborhood terms)
  wx_only <- bym2_wx_df |>
    dplyr::filter(grepl("^wx_", predictor)) |>
    dplyr::mutate(focal_predictor = sub("^wx_", "", predictor)) |>
    dplyr::transmute(focal_predictor,
                     wx_mean = round(focal_wx_mean, 4),
                     wx_2.5  = round(focal_wx_2.5, 4),
                     wx_97.5 = round(focal_wx_97.5, 4),
                     credible = ifelse(focal_wx_2.5 > 0 |
                                       focal_wx_97.5 < 0, "Yes", "No"))
  cat("\n--- BYM2 Wx (neighborhood) effects ---\n")
  print(wx_only)
  readr::write_csv(wx_only,
                    file.path(dir_tbl,
                              "06_6e_bym2_wx_neighborhood_effects.csv"))

  # Summary of changes in focal effects after Wx augmentation
  focal_drift <- bym2_compare |>
    dplyr::filter(!grepl("^wx_", predictor),
                  predictor != "(Intercept)",
                  !is.na(pct_change)) |>
    dplyr::arrange(dplyr::desc(abs(pct_change)))
  cat("\n--- Focal-effect drift after Wx augmentation (top 10 by |%|) ---\n")
  print(head(focal_drift |>
               dplyr::select(predictor, focal_only_mean,
                              focal_wx_mean, pct_change), 10))
}

# ---- 6.6f. Fit-improvement summary table ----------------------
fit_summary_wx <- tibble::tibble(
  model = c("OLS focal-only",
            "OLS focal+Wx",
            "BYM2 focal-only",
            "BYM2 focal+Wx"),
  n     = c(nrow(mod_data2),
            nrow(mod_data2_wx),
            bym2_save$n,
            if (!is.null(bym2_wx_save)) bym2_wx_save$n else NA_real_),
  R2_or_DIC = c(
    sprintf("R² = %.4f", r2_focal),
    sprintf("R² = %.4f", r2_wx),
    sprintf("DIC = %.0f, WAIC = %.0f", bym2_save$dic, bym2_save$waic),
    if (!is.null(bym2_wx_save))
      sprintf("DIC = %.0f, WAIC = %.0f",
              bym2_wx_save$dic, bym2_wx_save$waic) else "skipped"
  ),
  resid_moranI = c(
    # NOTE 2026-09-23: mi_resid is the six-predictor BASELINE OLS (section 5c),
    # not the focal-only full-covariate OLS this row is labelled with. The
    # manuscript reports the focal-only value (0.518) from
    # 29_recompute_deposit_gaps.R -> 29_residual_moran_full_ols.csv.
    sprintf("%.3f", mi_resid$estimate["Moran I statistic"]),
    sprintf("%.3f", mi_wx$estimate["Moran I statistic"]),
    "—", "—"
  )
)
cat("\n--- Section 6.6 fit-improvement summary ---\n")
print(fit_summary_wx)
readr::write_csv(fit_summary_wx,
                  file.path(dir_tbl, "06_6f_wx_fit_summary.csv"))
cat("  saved: ", file.path(dir_tbl, "06_6f_wx_fit_summary.csv"), "\n")

cat("\n=== Section 6.6 complete. ===\n")
cat("  Decision rule (Curriero / Jennings 2013):\n")
cat("    If a Wx coefficient's CrI excludes 0, the neighborhood effect\n")
cat("    is credibly different from the focal effect — meaning what\n")
cat("    happens NEXT DOOR matters above and beyond what happens IN\n")
cat("    the tract. If multiple Wx terms are credible AND residual\n")
cat("    Moran's I drops substantially, the BYM2 random structure is\n")
cat("    absorbing less of the spatial signal — interpretation moves\n")
cat("    from 'unmeasured confounders' to 'spatial spillover'.\n")


# ==============================================================
# 6.7  BEHAVIORAL-DROP BYM2 SENSITIVITY 
# ==============================================================

behav_drop <- c("CSMOKING", "LPA", "ACCESS2", "CHECKUP")
present_behav <- intersect(behav_drop, core_predictors)
reduced_core  <- setdiff(core_predictors, behav_drop)
reduced_predictors <- c(reduced_core, env_predictors, aging_env_terms)
fml_str_red <- paste("ckm_core_pca ~",
                     paste(reduced_predictors, collapse = " + "))
fml_red     <- stats::as.formula(fml_str_red)

cat("\n=== Section 6.7  Behavioral-drop BYM2 sensitivity ===\n")
cat("  Dropping behavioral/access block: ",
    paste(present_behav, collapse = ", "),
    " (", length(present_behav), " of 4 present)\n", sep = "")
cat("  Reduced model: ", length(reduced_core), " core + ",
    length(env_predictors), " env + ",
    length(aging_env_terms), " aging x env terms\n", sep = "")

f_bym2_nb <- file.path(dir_proc, "06_6g_inla_bym2_nobehav.rds")
# Reuse the have_inla flag set upstream in Section 6e/6.6.

# Auto-invalidate on predictor-set drift, same discipline as the focal fit.
if (file.exists(f_bym2_nb)) {
  cached_nb <- tryCatch(readRDS(f_bym2_nb), error = function(e) NULL)
  if (!is.null(cached_nb) && !is.null(cached_nb$summary_fixed)) {
    cached_terms_nb  <- setdiff(rownames(cached_nb$summary_fixed), "(Intercept)")
    current_terms_nb <- tryCatch(
      colnames(model.matrix(fml_red, data = head(mod_data2, 5)))[-1],
      error = function(e) character(0))
    if (length(current_terms_nb) > 0 &&
        !identical(sort(cached_terms_nb), sort(current_terms_nb))) {
      message("BYM2 behavioral-drop cache invalidated: predictor-set drift ",
              "(cached ", length(cached_terms_nb), " fixed effects, current ",
              length(current_terms_nb), "); will refit (~30-90 min).")
      file.remove(f_bym2_nb)
    }
  }
  if (file.exists(f_bym2_nb))
    warn_incomplete_bym2(cached_nb, f_bym2_nb,
                         "BYM2 (Model 1, behavior-excluded, primary)")
}

if (!have_inla) {
  cat("  INLA not installed; behavioral-drop BYM2 skipped.\n")
  bym2_nb_save <- NULL
} else if (!file.exists(f_bym2_nb)) {
  message("Fitting behavioral-drop BYM2 (this may take 30-90 min)...")

 
  graph_file <- file.path(dir_graph, "06_inla_graph.adj")
  if (!file.exists(graph_file))
    spdep::nb2INLA(graph_file, nb_mod)
  mod_data2$idarea <- match(mod_data2$GEOID, mod_geo$GEOID)
  bym2_data_nb <- mod_data2 |> dplyr::filter(!is.na(idarea))

  fml_bym2_nb <- stats::as.formula(
    paste(fml_str_red,
          "+ f(idarea, model = 'bym2', graph = '", graph_file,
          "', scale.model = TRUE,",
          "    hyper = list(phi  = list(prior = 'pc',",
          "                              param = c(0.5, 0.5)),",
          "                 prec = list(prior = 'pc.prec',",
          "                              param = c(1, 0.01))))",
          sep = ""))

  t0 <- Sys.time()
  bym2_nb_fit <- INLA::inla(
    fml_bym2_nb, data = bym2_data_nb, family = "gaussian",
    control.predictor = list(compute = TRUE),
    control.compute   = list(dic = TRUE, waic = TRUE,
                              return.marginals.predictor = FALSE),
    control.inla      = list(strategy = "adaptive",
                              int.strategy = "eb"),
    verbose = FALSE
  )
  message("  behavioral-drop BYM2 done in ",
          round(difftime(Sys.time(), t0, units = "mins"), 1), " min")


  bym2_nb_save <- list(
    summary_fixed    = bym2_nb_fit$summary.fixed,
    summary_random   = bym2_nb_fit$summary.random$idarea,
    summary_hyperpar = bym2_nb_fit$summary.hyperpar,
    summary_fitted   = bym2_nb_fit$summary.fitted.values,
    dic     = bym2_nb_fit$dic$dic,
    waic    = bym2_nb_fit$waic$waic,
    p_eff_dic  = bym2_nb_fit$dic$p.eff,
    p_eff_waic = bym2_nb_fit$waic$p.eff,
    n       = nrow(bym2_data_nb),
    GEOID   = bym2_data_nb$GEOID,
    dropped = present_behav,
    formula = paste(deparse(fml_bym2_nb), collapse = " ")
  )
  saveRDS(bym2_nb_save, f_bym2_nb)
  cat("  saved: ", f_bym2_nb, "\n")
} else {
  bym2_nb_save <- readRDS(f_bym2_nb)
  message("  behavioral-drop BYM2 cached: ", f_bym2_nb)
}

# ---- 6.7a. Reduced fixed-effect table + with/without shift -------
if (have_inla && !is.null(bym2_nb_save)) {
  cat("\n--- behavioral-drop BYM2 fixed-effect coefficients ---\n")
  print(round(bym2_nb_save$summary_fixed[, c("mean", "sd", "0.025quant",
                                              "0.975quant")], 4))
  cat("\n  DIC: ",  round(bym2_nb_save$dic, 1),
      " | WAIC: ", round(bym2_nb_save$waic, 1),
      " | n = ", bym2_nb_save$n, "\n", sep = "")

  bym2_nb_fixed <- tibble::as_tibble(bym2_nb_save$summary_fixed,
                                      rownames = "predictor")
  readr::write_csv(bym2_nb_fixed,
                    file.path(dir_tbl, "06_6g_coefficients_bym2_nobehav.csv"))
  cat("  saved: ",
      file.path(dir_tbl, "06_6g_coefficients_bym2_nobehav.csv"), "\n")


  bym2_nb_fit <- tibble::tibble(
    model = "bym2_nobehav",
    dic   = round(bym2_nb_save$dic, 1),
    waic  = round(bym2_nb_save$waic, 1),
    p_eff_dic  = round(bym2_nb_save$p_eff_dic  %||% NA_real_, 1),
    p_eff_waic = round(bym2_nb_save$p_eff_waic %||% NA_real_, 1),
    n     = bym2_nb_save$n)
  if (is.na(bym2_nb_fit$p_eff_dic))
    message("  NOTE: p.eff absent from the cached Model 1 fit (", f_bym2_nb,
            "). Delete that cache and refit to populate it.")
  readr::write_csv(bym2_nb_fit,
                    file.path(dir_tbl, "06_6g_fit_nobehav.csv"))
  cat("  saved: ",
      file.path(dir_tbl, "06_6g_fit_nobehav.csv"), "\n")


  if (!is.null(bym2_save) && !is.null(bym2_save$summary_random)) {
    message("Building two-panel BYM2 spatial random-effect map (Figure 6)...")


    .re_geo_of <- function(sv) {
      rd <- tibble::as_tibble(sv$summary_random)
      if (!"idarea" %in% names(rd) && "ID" %in% names(rd))
        names(rd)[match("ID", names(rd))] <- "idarea"
      rd <- head(rd, nrow(mod_geo))
      rd$GEOID <- mod_geo$GEOID
      tracts_geo |>
        dplyr::mutate(GEOID = stringr::str_trim(enc2utf8(as.character(GEOID)))) |>
        dplyr::inner_join(
          rd |> dplyr::select(GEOID, mean) |>
            dplyr::mutate(GEOID = stringr::str_trim(enc2utf8(as.character(GEOID)))),
          by = "GEOID") |>
        dplyr::filter(!substr(GEOID, 1, 2) %in% c("60", "66", "69", "72", "78"),
                      ALAND > 0) |>
        sf::st_transform(4326) |>
        tigris::shift_geometry(geoid_column = "GEOID",
                               preserve_area = FALSE, position = "below")
    }

    re_adj <- .re_geo_of(bym2_save)      # Model 2: behavior-adjusted
    re_exc <- .re_geo_of(bym2_nb_save)   # Model 1: behavior-excluded

    re_lim2 <- max(abs(quantile(re_adj$mean, c(0.02, 0.98), na.rm = TRUE)),
                   abs(quantile(re_exc$mean, c(0.02, 0.98), na.rm = TRUE)))


    .bym2_panel <- function(d, tag, ttl, sub) {
      ggplot() +
        geom_sf(data = d, aes(fill = mean), colour = NA) +
        geom_sf(data = nj_unavail, fill = NJ_UNAVAIL_FILL, colour = NA) +
        geom_sf(data = states_conus, fill = NA, colour = "grey25",
                linewidth = 0.25) +
        scale_fill_gradient2(
          low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
          limits = c(-re_lim2, re_lim2), oob = scales::squish,
          name = "Spatial\nrandom effect\n(PC1 score units)") +
        coord_sf(datum = NA) +
        theme_void(base_size = 19) +
        theme(plot.title    = element_text(face = "bold", size = 19),
              plot.subtitle = element_text(colour = "grey30", size = 14),
              plot.tag      = element_text(face = "bold", size = 22),
              legend.title  = element_text(size = 17),
              legend.text   = element_text(size = 15)) +
        labs(tag = tag, title = ttl, subtitle = sub)
    }

    p_fig6 <- patchwork::wrap_plots(
      .bym2_panel(re_exc, "A", "Model 1 - behavior-excluded",
                  "Behavioral/healthcare-engagement block removed"),
      .bym2_panel(re_adj, "B", "Model 2 - behavior-adjusted",
                  "Behavioral/healthcare-engagement block included"),
      nrow = 1, guides = "collect")

    ggsave(file.path(dir_fig, "06h_bym2_spatial_random_effect_2panel.png"),
           p_fig6, width = 13, height = 5.6, dpi = 220, bg = "white")
    cat("  saved: ",
        file.path(dir_fig, "06h_bym2_spatial_random_effect_2panel.png"), "\n")


    .south_fips <- c("01", "05", "10", "11", "12", "13", "21", "22", "24",
                     "28", "37", "40", "45", "47", "48", "51", "54")
    for (.nm in c("Model 1", "Model 2")) {
      .dd  <- sf::st_drop_geometry(if (.nm == "Model 1") re_exc else re_adj)
      .iss <- substr(.dd$GEOID, 1, 2) %in% .south_fips
      cat(sprintf(
        "  %s  South %+.3f (%.1f%% pos) | non-South %+.3f (%.1f%% pos)\n",
        .nm, mean(.dd$mean[.iss]),  100 * mean(.dd$mean[.iss]  > 0),
             mean(.dd$mean[!.iss]), 100 * mean(.dd$mean[!.iss] > 0)))
    }

    
    .sm <- sf::st_drop_geometry(re_exc)
    .sm$state <- substr(.sm$GEOID, 1, 2)
    .sm <- aggregate(mean ~ state, .sm, base::mean)
    .sm <- .sm[order(-.sm$mean), ][1:8, ]
    .sm$south <- ifelse(.sm$state %in% .south_fips, "South", "")
    cat("\n  Model 1 - highest state mean random effects:\n")
    print(.sm, row.names = FALSE, digits = 3)
  }

  # WITH-behavioral (bym2_save, Section 6e) vs WITHOUT (bym2_nb_save)
  with_df <- tibble::as_tibble(bym2_save$summary_fixed,
                               rownames = "predictor") |>
    dplyr::transmute(predictor,
                     with_mean = mean,
                     with_2.5  = `0.025quant`,
                     with_97.5 = `0.975quant`)
  without_df <- bym2_nb_fixed |>
    dplyr::transmute(predictor,
                     without_mean = mean,
                     without_2.5  = `0.025quant`,
                     without_97.5 = `0.975quant`)
  behav_shift <- dplyr::full_join(with_df, without_df, by = "predictor") |>
    dplyr::mutate(
      abs_change = without_mean - with_mean,
      pct_change = ifelse(!is.na(with_mean) & with_mean != 0,
                          round(100 * (without_mean - with_mean) /
                                  abs(with_mean), 1),
                          NA_real_),
      sign_flip  = ifelse(!is.na(with_mean) & !is.na(without_mean) &
                          sign(with_mean) != sign(without_mean) &
                          with_mean != 0 & without_mean != 0, "Yes", "No")
    )
  readr::write_csv(behav_shift,
                    file.path(dir_tbl, "06_6g_behavioral_drop_shift.csv"))
  cat("  saved: ",
      file.path(dir_tbl, "06_6g_behavioral_drop_shift.csv"), "\n")


  focus_terms <- c("ice_race", "pct_hispanic", "ice_income")
  behav_focus <- behav_shift |>
    dplyr::filter(predictor %in% focus_terms)
  cat("\n--- Concern B focus: ICE_Race / %Hispanic / ICE_Income",
      " (WITH vs WITHOUT behavioral block) ---\n", sep = "")
  print(behav_focus |>
          dplyr::transmute(predictor,
                           with_mean = round(with_mean, 4),
                           without_mean = round(without_mean, 4),
                           abs_change = round(abs_change, 4),
                           pct_change, sign_flip))
  readr::write_csv(behav_focus,
                    file.path(dir_tbl, "06_6g_behavioral_drop_focus.csv"))
}

# ==============================================================
# 6.8  KIDNEY LOADING ASYMMETRY  
# ==============================================================

f_load_cmp <- file.path(dir_tbl, "04_pc1_loadings_compare.csv")
if (file.exists(f_load_cmp)) {
  load_cmp <- readr::read_csv(f_load_cmp, show_col_types = FALSE)
  if (all(c("NHANES_PC1", "PLACES_PC1") %in% names(load_cmp))) {
    load_asym <- load_cmp |>
      dplyr::mutate(
        loading_gap = PLACES_PC1 - NHANES_PC1,
        rel_gap_pct = ifelse(NHANES_PC1 != 0,
                             round(100 * (PLACES_PC1 - NHANES_PC1) /
                                     abs(NHANES_PC1), 1), NA_real_)
      ) |>
      dplyr::arrange(dplyr::desc(abs(loading_gap)))
    readr::write_csv(load_asym,
                      file.path(dir_tbl, "04_pc1_loading_asymmetry.csv"))
    cat("\n=== Section 6.8  PC1 loading asymmetry (PLACES - NHANES) ===\n")
    print(load_asym)
    cat("  saved: ",
        file.path(dir_tbl, "04_pc1_loading_asymmetry.csv"), "\n")
    kid <- load_asym |> dplyr::filter(toupper(Indicator) == "KIDNEY")
    if (nrow(kid) == 1)
      cat(sprintf(
        "  KIDNEY: PLACES loading %.3f vs NHANES %.3f (gap %+.3f, %+.0f%%)\n",
        kid$PLACES_PC1, kid$NHANES_PC1, kid$loading_gap, kid$rel_gap_pct))
  }
} else {
  cat("\n  [6.8] ", f_load_cmp, " not found; run Section 4 first.\n", sep = "")
}

# ==============================================================
# 6.9  MAUP COUNTY RE-AGGREGATION  
# ==============================================================

f_maup <- file.path(dir_tbl, "08_maup_county.csv")
if (file.exists(f_maup))
  cat("\n  [6.9] County MAUP re-aggregation available for main-text ",
      "elevation: ", f_maup, "\n", sep = "")


# ==============================================================
# 7.  ML PILLAR  (XGBoost + spatial block CV + SHAP)
# ==============================================================

# Output files:
#   data/processed/07_xgb_fit.rds      — final XGBoost model + hyperparams
#   data/processed/07_cv_results.rds   — fold-by-fold metrics, both schemes
#   data/processed/07_shap.rds         — SHAP values matrix (50k × 14)
#   output/tables/07_shap_global.csv   — global SHAP importance ranking
#   output/figures/07_shap_summary.png — beeswarm-style importance plot
#   output/figures/07_age_pm25_interaction.png — partial-dependence interaction

cat("\n========================================================\n")
cat("Section 7 — ML pillar (XGBoost + spatial CV + SHAP)\n")
cat("========================================================\n")

# ---- 7a. Reload Section 6 modeling data + geometry ------------
if (!exists("mod_data2") || !exists("mod_geo") ||
    !exists("env_predictors")) {
  message("  Reloading Section 6 modeling data + geometry...")
  if (!file.exists(file.path(dir_proc, "06_spmoran_resf.rds")))
    stop("Section 6 has not been run. Source from the top.")
  # Rebuild mod_data2 from analytic + 02_ckm_index
  ckm_idx <- readRDS(file.path(dir_proc, "02_ckm_index.rds"))
  env_predictors <- intersect(c("pm25_annual", "o3_8hrmax_4thmax",
                                "LILATracts_1And10", "tmax_warm",
                                "walk_index"),
                              names(ckm_idx))

  # Section-7 run changes the XGBoost/BYM2 feature set 
  core_predictors <- c(
    "pct_age_65_plus", "pct_hispanic",
    "ice_income", "pct_unemployed", "pct_bachelor_plus",
    "ice_race", "pct_living_alone",
    "ACCESS2", "CHECKUP",
    "ruca_class", "pct_no_vehicle",
    "CSMOKING", "LPA"
  )

  core_predictors <- intersect(core_predictors, names(ckm_idx))
  mod_data2 <- ckm_idx |>
    dplyr::select(GEOID, ckm_core_pca,
                  dplyr::all_of(c(core_predictors, env_predictors))) |>
    dplyr::mutate(
      ruca_class = factor(ruca_class,
                          levels = c("Metro", "Micropolitan",
                                     "Small town", "Rural")),
      dplyr::across(dplyr::any_of(env_predictors),
                    ~ suppressWarnings(as.numeric(.x)))) |>
    tidyr::drop_na()
  mod_geo <- readRDS(file.path(dir_proc, "01_tracts_geo.rds")) |>
    dplyr::filter(!substr(GEOID, 1, 2) %in% c("02", "15", "60", "66",
                                                "69", "72", "78"))
}

# ---- 7b. Build the design matrix + train/full split ------------

xgb_predictors <- c(core_predictors, env_predictors)
xgb_formula    <- stats::as.formula(
  paste("~", paste(xgb_predictors, collapse = " + "), "- 1")
)
X_full <- model.matrix(xgb_formula, data = mod_data2)
y_full <- mod_data2$ckm_core_pca

cat("  XGBoost design matrix: ", nrow(X_full), " × ",
    ncol(X_full), " features\n", sep = "")

# ---- 7c. Hyperparameter tuning — manual CV via xgb.train ------

f_xgb  <- file.path(dir_proc, "07_xgb_fit.rds")
f_cv   <- file.path(dir_proc, "07_cv_results.rds")
f_shap <- file.path(dir_proc, "07_shap.rds")

if (file.exists(f_xgb)) {
  cached_xgb <- tryCatch(readRDS(f_xgb), error = function(e) NULL)
  bad_tuning <- !is.null(cached_xgb) &&
                "cv_grid" %in% names(cached_xgb) &&
                (all(is.na(cached_xgb$cv_grid$best_rmse)) ||
                 isTRUE(cached_xgb$params$best_iter %in% c(0, NA)))

  feature_drift <- !is.null(cached_xgb) &&
                   ("predictors" %in% names(cached_xgb)) &&
                   !identical(sort(cached_xgb$predictors),
                              sort(colnames(X_full)))
  if (bad_tuning || feature_drift) {
    reason <- if (bad_tuning) "broken tuning state" else
              paste0("predictor-set drift (cached ",
                     length(cached_xgb$predictors), " features, current ",
                     ncol(X_full), " features)")
    message("XGBoost cache invalidated: ", reason,
            "; cascade-invalidating f_xgb, f_cv, f_shap to re-tune cleanly.")
    for (fp in c(f_xgb, f_cv, f_shap))
      if (file.exists(fp)) file.remove(fp)
  }
}
if (!file.exists(f_xgb)) {
  message("Tuning XGBoost via manual 5-fold CV with early stopping...")
  set.seed(20260425)

  param_grid <- expand.grid(
    eta       = c(0.05, 0.1),
    max_depth = c(4, 6, 8)
  )
  K <- 5
  fold_id <- sample(rep(seq_len(K), length.out = nrow(X_full)))

  cv_one_setting <- function(eta, max_depth) {
    fold_metrics <- vapply(seq_len(K), function(k) {
      idx_tr <- which(fold_id != k)
      idx_va <- which(fold_id == k)
      dtr <- xgboost::xgb.DMatrix(data = X_full[idx_tr, , drop = FALSE],
                                   label = y_full[idx_tr])
      dva <- xgboost::xgb.DMatrix(data = X_full[idx_va, , drop = FALSE],
                                   label = y_full[idx_va])

      eval_arg <- if ("evals" %in% names(formals(xgboost::xgb.train)))
        list(evals = list(val = dva)) else list(watchlist = list(val = dva))
      fit <- do.call(xgboost::xgb.train, c(
        list(params = list(objective = "reg:squarederror",
                            eval_metric = "rmse",
                            eta = eta, max_depth = max_depth,
                            subsample = 0.8, colsample_bytree = 0.8),
             data    = dtr,
             nrounds = 1500,
             early_stopping_rounds = 25,
             verbose = 0),
        eval_arg))

      bi_raw <- fit$best_iteration %||% fit$best_iter %||% fit$niter %||%
                tryCatch(nrow(as.data.frame(fit$evaluation_log)),
                          error = function(e) NA_integer_)
      bi <- suppressWarnings(as.integer(bi_raw[1]))
      if (length(bi) == 0 || is.na(bi) || bi < 1) bi <- 100L
      yhat_va  <- predict(fit, dva)
      rmse_val <- sqrt(mean((y_full[idx_va] - yhat_va)^2, na.rm = TRUE))
      c(best_iter = as.numeric(bi),
        best_rmse = rmse_val)
    }, numeric(2))
    list(best_iter = round(median(fold_metrics["best_iter", ])),
         best_rmse = mean(fold_metrics["best_rmse", ]))
  }

  cv_results <- purrr::map_dfr(seq_len(nrow(param_grid)), function(i) {
    p <- param_grid[i, ]
    res <- cv_one_setting(p$eta, p$max_depth)
    tibble::tibble(eta = p$eta, max_depth = p$max_depth,
                   best_iter = res$best_iter, best_rmse = res$best_rmse)
  })
  cat("\n  Hyperparameter-grid CV results:\n")
  print(cv_results |> dplyr::arrange(best_rmse))

  best <- cv_results |> dplyr::arrange(best_rmse) |> dplyr::slice(1)
  message("  Best params: eta=", best$eta, " max_depth=", best$max_depth,
          " (", best$best_iter, " rounds, RMSE=",
          round(best$best_rmse, 4), ")")


  message("Fitting final XGBoost model...")
  dfull <- xgboost::xgb.DMatrix(data = X_full, label = y_full)
  xgb_fit <- xgboost::xgb.train(
    params = list(objective = "reg:squarederror",
                  eval_metric = "rmse",
                  eta = best$eta, max_depth = best$max_depth,
                  subsample = 0.8, colsample_bytree = 0.8),
    data    = dfull,
    nrounds = best$best_iter,
    verbose = 0
  )

  saveRDS(list(model = xgb_fit, params = best,
               cv_grid = cv_results, predictors = colnames(X_full)),
          f_xgb)
  message("  saved: ", f_xgb)
} else {
  xgb_obj <- readRDS(f_xgb)
  xgb_fit <- xgb_obj$model
  best    <- xgb_obj$params
  message("  XGBoost cached: ", f_xgb)
}

cat("\n--- XGBoost final model ---\n")
cat("  eta=",       best$eta, "\n")
cat("  max_depth=", best$max_depth, "\n")
cat("  nrounds=",   best$best_iter, "\n")
cat("  CV RMSE: ",  round(best$best_rmse, 4), "\n")


y_pred_full <- predict(xgb_fit, X_full)
r2_insample <- 1 - sum((y_full - y_pred_full)^2) /
                   sum((y_full - mean(y_full))^2)
cat("  In-sample R²: ", round(r2_insample, 4),
    "  (use spatial CV R² in 7d for generalization)\n")

# ---- 7d. Random k-fold vs. spatial block CV --------------------

f_cv <- file.path(dir_proc, "07_cv_results.rds")
if (!file.exists(f_cv)) {
  if (!"spatialsample" %in% installed.packages()[, "Package"])
    install.packages("spatialsample", repos = "https://cloud.r-project.org")

  message("Computing random k-fold CV...")
  set.seed(20260425)
  rand_folds <- sample(rep(1:5, length.out = nrow(X_full)))

  fit_predict_one_fold <- function(idx_train, idx_test) {
    dtr <- xgboost::xgb.DMatrix(data = X_full[idx_train, , drop = FALSE],
                                 label = y_full[idx_train])
    fit <- xgboost::xgb.train(
      params = list(objective = "reg:squarederror",
                    eval_metric = "rmse",
                    eta = best$eta, max_depth = best$max_depth,
                    subsample = 0.8, colsample_bytree = 0.8),
      data    = dtr,
      nrounds = best$best_iter,
      verbose = 0
    )
    yhat <- predict(fit, X_full[idx_test, , drop = FALSE])
    list(rmse = sqrt(mean((y_full[idx_test] - yhat)^2)),
         r2   = 1 - sum((y_full[idx_test] - yhat)^2) /
                    sum((y_full[idx_test] - mean(y_full[idx_test]))^2),
         n_test = length(idx_test))
  }

  rand_results <- purrr::map_dfr(1:5, function(k) {
    idx_test  <- which(rand_folds == k)
    idx_train <- which(rand_folds != k)
    res <- fit_predict_one_fold(idx_train, idx_test)
    tibble::tibble(scheme = "random", fold = k, !!!res)
  })

  message("Computing spatial block CV (this is slower, fits 5 models)...")

  cv_sf <- mod_data2 |>
    dplyr::select(GEOID) |>
    dplyr::left_join(mod_geo, by = "GEOID") |>
    sf::st_as_sf() |>
    sf::st_centroid(of_largest_polygon = TRUE)

  set.seed(20260425)
  block_folds <- spatialsample::spatial_block_cv(
    cv_sf, v = 5, n = c(8, 8))   

  spat_results <- purrr::map_dfr(seq_along(block_folds$splits), function(k) {
    sp <- block_folds$splits[[k]]
    idx_train <- sp$in_id
    idx_test  <- setdiff(seq_len(nrow(X_full)), idx_train)
    if (length(idx_test) < 100) return(NULL)
    res <- fit_predict_one_fold(idx_train, idx_test)
    tibble::tibble(scheme = "spatial_block", fold = k, !!!res)
  })

  cv_results <- dplyr::bind_rows(rand_results, spat_results)
  cv_summary <- cv_results |>
    dplyr::group_by(scheme) |>
    dplyr::summarise(rmse_mean = mean(rmse), r2_mean = mean(r2),
                     n_folds = dplyr::n(), .groups = "drop")

  saveRDS(list(per_fold = cv_results, summary = cv_summary), f_cv)
  message("  saved: ", f_cv)
} else {
  cv_obj <- readRDS(f_cv)
  cv_results <- cv_obj$per_fold
  cv_summary <- cv_obj$summary
  message("  CV results cached: ", f_cv)
}

cat("\n--- Cross-validation: random vs. spatial block ---\n")
print(cv_summary)
readr::write_csv(cv_summary, file.path(dir_tbl, "07_cv_summary.csv"))


readr::write_csv(
  tibble::tibble(scheme = "in_sample", rmse_mean = NA_real_,
                 r2_mean = r2_insample, n_folds = NA_integer_),
  file.path(dir_tbl, "07_cv_summary.csv"), append = TRUE)
cv_gap <- cv_summary$r2_mean[cv_summary$scheme == "random"] -
          cv_summary$r2_mean[cv_summary$scheme == "spatial_block"]
cat("\n  Spatial-leakage gap (random R² − spatial R²): ",
    sprintf("%.3f", cv_gap), "\n",
    "  (Larger gap = more spatial autocorrelation leakage.\n",
    "   Manuscript framing: 'Honest out-of-area performance is\n",
    "   captured by the spatial block CV R².')\n", sep = "")

# ---- 7e. SHAP — global + local explanations --------------------


f_shap <- file.path(dir_proc, "07_shap.rds")
if (!file.exists(f_shap)) {
  message("Computing exact TreeSHAP via xgboost predcontrib...")
  set.seed(20260425)
  t0 <- Sys.time()
  shap_raw <- predict(xgb_fit, X_full, predcontrib = TRUE)

  if (is.null(colnames(shap_raw)))
    colnames(shap_raw) <- c(colnames(X_full), "BIAS")

  shap_vals <- shap_raw[, colnames(X_full), drop = FALSE]
  message("  SHAP done in ",
          round(difftime(Sys.time(), t0, units = "secs"), 1), " sec")
  saveRDS(list(shap = shap_vals, predictors = colnames(X_full)), f_shap)
  message("  saved: ", f_shap)
} else {
  shap_obj <- readRDS(f_shap)
  shap_vals <- shap_obj$shap
  message("  SHAP cached: ", f_shap)
}


shap_vals <- shap_vals[, intersect(colnames(X_full), colnames(shap_vals)),
                       drop = FALSE]

# ---- 7f. Global SHAP importance + ranking ----------------------
shap_global <- tibble::tibble(
  feature  = colnames(shap_vals),
  mean_abs = apply(abs(shap_vals), 2, mean),
  sd_abs   = apply(abs(shap_vals), 2, sd)
) |>
  dplyr::arrange(dplyr::desc(mean_abs)) |>
  dplyr::mutate(rank = dplyr::row_number(),
                pct_total = round(100 * mean_abs / sum(mean_abs), 1))

cat("\n--- Global SHAP importance (top 15) ---\n")
print(head(shap_global, 15))
readr::write_csv(shap_global, file.path(dir_tbl, "07_shap_global.csv"))

# Block-level importance: SDOH vs. ENV vs. DEMO
block_lookup <- function(feat) {
  if (feat %in% c("pct_age_65_plus", "pct_hispanic")) "DEMO"
  else if (grepl("ruca_class", feat) ||
           feat %in% c("ice_race", "ice_income", "pct_bachelor_plus")) "SDOH"
  else if (feat %in% c("pm25_annual", "o3_8hrmax_4thmax",
                        "LILATracts_1And10", "tmax_warm",
                        "walk_index")) "ENV"
  else "OTHER"
}
shap_global$block <- vapply(shap_global$feature, block_lookup, character(1))
block_imp <- shap_global |>
  dplyr::group_by(block) |>
  dplyr::summarise(total_imp = sum(mean_abs),
                   pct_total = round(100 * total_imp /
                                      sum(shap_global$mean_abs), 1),
                   n_features = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(total_imp))
cat("\n--- SHAP importance by predictor block ---\n")
print(block_imp)
readr::write_csv(block_imp, file.path(dir_tbl, "07_shap_block_importance.csv"))

# ---- 7g. SHAP summary plot (top-10 beeswarm-equivalent) -------
top10_feats <- head(shap_global$feature, 10)
shap_long <- as.data.frame(shap_vals[, top10_feats, drop = FALSE]) |>
  tibble::rowid_to_column("row_id") |>
  tidyr::pivot_longer(-row_id, names_to = "feature", values_to = "shap")

# Add the actual feature value as a color encoding (high vs. low)
feat_vals <- as.data.frame(X_full[, top10_feats, drop = FALSE]) |>
  tibble::rowid_to_column("row_id") |>
  tidyr::pivot_longer(-row_id, names_to = "feature", values_to = "value") |>
  dplyr::group_by(feature) |>
  dplyr::mutate(value_q = (value - min(value, na.rm = TRUE)) /
                          diff(range(value, na.rm = TRUE))) |>
  dplyr::ungroup()


shap_plot_df <- dplyr::left_join(shap_long, feat_vals,
                                  by = c("row_id", "feature")) |>
  dplyr::mutate(feature = factor(feature, levels = rev(top10_feats)))

p_shap <- ggplot(shap_plot_df,
                 aes(x = shap, y = feature, colour = value_q)) +
  geom_vline(xintercept = 0, colour = "grey50", linetype = "dashed") +
  geom_jitter(height = 0.3, alpha = 0.15, size = 0.4) +
  scale_y_discrete(labels = .label_pred) +
  scale_colour_gradient(
    low = "#2166AC", high = "#B2182B",
    name   = "Predictor value",
    breaks = c(0, 1),
    labels = c("Low", "High")) +
  labs(
    x = "SHAP value  —  contribution to tract CKM burden index",
    y = NULL,
    title    = "Predictor importance from XGBoost SHAP analysis",
    subtitle = paste0("Top-10 predictors ranked by mean |SHAP|.\n",
                      "Each point = one tract; colour = predictor value; ",
                      "horizontal spread = effect magnitude."),
    caption  = paste0("N = ", formatC(nrow(X_full), big.mark = ","),
                      " tracts.  Spatial block CV R² = ",
                      sprintf("%.2f", cv_summary$r2_mean[
                        cv_summary$scheme == "spatial_block"]), ".")
  ) +
  theme_ckm +
  theme(plot.subtitle = element_text(lineheight = 1.1))
ggsave(file.path(dir_fig, "07_shap_summary.png"),
       p_shap, width = 10, height = 5.5, dpi = 220, bg = "white")
cat("  saved: ", file.path(dir_fig, "07_shap_summary.png"), "\n")

# ---- 7h. Aging × env interaction visualization ----------------

shap_age <- shap_vals[, "pct_age_65_plus"]
env_in_X <- intersect(env_predictors, colnames(X_full))


.to_native <- function(v, x) {
  if (!exists("interact_means") || !v %in% names(interact_means)) return(x)
  x <- x + interact_means[[v]]
  if (v == "o3_8hrmax_4thmax") x <- x * 10   # 10-ppb units -> ppb
  x
}
age_native <- .to_native("pct_age_65_plus", X_full[, "pct_age_65_plus"])

interaction_df <- purrr::map_dfr(env_in_X, function(env_var) {
  vals <- X_full[, env_var]
  is_binary <- length(unique(vals[!is.na(vals)])) <= 2
  tibble::tibble(
    env_var    = env_var,
    env_value  = .to_native(env_var, vals),
    shap_age   = shap_age,
    age_value  = age_native,
    is_binary  = is_binary
  )
})

# Figure-8 facet labels: 
fig8_pretty <- c(predictor_pretty,
                 o3_8hrmax_4thmax = "Annual 4th-highest 8-hr O₃ (ppb)")
.label_fig8 <- function(x) unname(fig8_pretty[as.character(x)])

# Continuous env predictors → scatter + loess (non-binary panels)
cont_df  <- interaction_df |> dplyr::filter(!is_binary)
# Binary predictors (e.g. LILATracts_1And10 = 0/1) → boxplot of SHAP(age)

bin_df   <- interaction_df |> dplyr::filter(is_binary)



p_cont <- ggplot(cont_df,
                 aes(x = env_value, y = shap_age, colour = age_value)) +
  geom_point(alpha = 0.2, size = 0.4) +
  geom_smooth(colour = "black", se = FALSE,
              method = "loess", formula = y ~ x, span = 0.5) +
  facet_wrap(~ env_var, scales = "free_x", ncol = 2,
             labeller = ggplot2::as_labeller(.label_fig8)) +
  scale_colour_viridis_c(name = "% age 65+") +
  labs(x = "Environmental predictor value (original measurement scale)",
       y = "SHAP value for % adults age 65+",
       subtitle = "Continuous exposures — loess trend") +
  theme_ckm

if (nrow(bin_df) > 0) {
  p_bin <- ggplot(bin_df,
                  aes(x = factor(env_value, levels = c("0", "1"),
                                  labels = c("Not LILA", "LILA")),
                      y = shap_age,
                      fill = factor(env_value))) +
    geom_violin(alpha = 0.4, scale = "width", colour = NA) +
    geom_boxplot(width = 0.15, outlier.alpha = 0.05, fill = "white") +
    facet_wrap(~ env_var, ncol = 2,
               labeller = ggplot2::as_labeller(.label_fig8)) +
    scale_fill_manual(values = c("0" = "#2166AC", "1" = "#B2182B"),
                       guide = "none") +
    labs(x = "Tract food-access status",
         y = "SHAP value for % adults age 65+",
         subtitle = "Binary exposure — violin + boxplot") +
    theme_ckm
  p_interact <- p_cont / p_bin +
    patchwork::plot_layout(heights = c(2, 1)) +
    patchwork::plot_annotation(
      title = "Dependence of the aging SHAP attribution on environmental predictors",
     
      subtitle = paste0("Descriptive dependence of the aging SHAP contribution on each exposure.\n",
                        "A non-flat trend is consistent with an interaction but does not isolate one:\n",
                        "it can also arise from predictor correlation or main-effect nonlinearity.")
    )
} else {
  p_interact <- p_cont +
    labs(title = "Dependence of the aging SHAP attribution on environmental predictors")
}

ggsave(file.path(dir_fig, "07_age_env_interactions.png"),
       p_interact, width = 9, height = 8, dpi = 220, bg = "white")
cat("  saved: ", file.path(dir_fig, "07_age_env_interactions.png"), "\n")

# ---- Figure 8 companion data ------------------------------------------

fig8_cont_data <- purrr::map_dfr(unique(cont_df$env_var), function(v) {
  d <- cont_df |>
    dplyr::filter(env_var == v, is.finite(env_value), is.finite(shap_age))
  fit <- stats::loess(shap_age ~ env_value, data = d, span = 0.5)
  gx  <- seq(min(d$env_value), max(d$env_value), length.out = 25)
  tibble::tibble(
    panel       = "continuous",
    env_var     = v,
    label       = unname(fig8_pretty[v]),
    x           = gx,
    level       = NA_character_,
    n_panel     = nrow(d),
    n_at_or_below_x = vapply(gx, function(g) sum(d$env_value <= g), integer(1)),
    loess_shap  = as.numeric(stats::predict(fit,
                    newdata = data.frame(env_value = gx))),
    median_shap = NA_real_, q25_shap = NA_real_, q75_shap = NA_real_)
})

fig8_bin_data <- if (nrow(bin_df) > 0) {
  bin_df |>
    dplyr::filter(is.finite(shap_age)) |>
    dplyr::group_by(env_var, env_value) |>
    dplyr::summarise(
      n_panel     = dplyr::n(),
      median_shap = stats::median(shap_age),
      q25_shap    = stats::quantile(shap_age, 0.25),
      q75_shap    = stats::quantile(shap_age, 0.75),
      .groups     = "drop") |>
    dplyr::transmute(
      panel = "binary", env_var,
      label = unname(fig8_pretty[env_var]),
      x = NA_real_,
      level = ifelse(env_value == 1, "LILA", "Not LILA"),
      n_panel, n_at_or_below_x = NA_integer_, loess_shap = NA_real_,
      median_shap, q25_shap, q75_shap)
} else NULL

fig8_data <- dplyr::bind_rows(fig8_cont_data, fig8_bin_data)
readr::write_csv(fig8_data,
                 file.path(dir_tbl, "07_age_env_interaction_loess.csv"))
cat("  saved: ", file.path(dir_tbl, "07_age_env_interaction_loess.csv"), "\n")

cat("\n=== Section 7 complete. ===\n")
cat("  In-sample R²: ",        round(r2_insample, 4), "\n")
cat("  Random CV R²: ",        round(cv_summary$r2_mean[cv_summary$scheme == "random"], 4), "\n")
cat("  Spatial CV R²: ",       round(cv_summary$r2_mean[cv_summary$scheme == "spatial_block"], 4), "\n")
cat("  Spatial-leakage gap: ", sprintf("%.3f", cv_gap), "\n")
cat("  Top-3 SHAP features: ", paste(head(shap_global$feature, 3),
                                      collapse = ", "), "\n")
cat("  Block ranking: ", paste(block_imp$block, collapse = " > "), "\n")


# ==============================================================
# 8.  SENSITIVITY  (MAUP, alt weights, RUCA strata)
# ==============================================================

#
# Output:
#   data/processed/08_*.rds
#   output/tables/08_*.csv
#   output/figures/08_alt_weights_moranI.png

cat("\n========================================================\n")
cat("Section 8 — Sensitivity analyses (MAUP, alt W, strata)\n")
cat("========================================================\n")


.s8_sig_cols <- intersect(
  c("o3_8hrmax_4thmax", "pm25_annual", "tmax_warm",
    "walk_index", "pct_age_65_plus", "ckm_core_pca"),
  names(mod_data2))
.s8_sig <- paste(vapply(.s8_sig_cols, function(cc) {
  v <- mod_data2[[cc]]
  sprintf("%s:sd=%.6g", cc, stats::sd(v, na.rm = TRUE))
}, character(1)), collapse = "|")
.s8_sig_file <- file.path(dir_proc, "08_datascale.sig")
.s8_caches <- file.path(dir_proc,
  c("08_maup_county.rds", "08_ruca_strata.rds", "08_svi_sensitivity.rds"))
.s8_prev_sig <- if (file.exists(.s8_sig_file))
  readLines(.s8_sig_file, warn = FALSE)[1] else NA_character_
if (!is.na(.s8_prev_sig) && !identical(.s8_prev_sig, .s8_sig)) {
  message("Section 8 data-scale signature changed since caches were built.")
  message("  stored : ", .s8_prev_sig)
  message("  current: ", .s8_sig)
  for (.f in .s8_caches) if (file.exists(.f)) {
    file.rename(.f, paste0(.f, ".stale_", format(Sys.time(), "%Y%m%d_%H%M%S")))
    message("  invalidated ", basename(.f))
  }
}
writeLines(.s8_sig, .s8_sig_file)

# ---- 8a. MAUP: county-scale re-fit ------------------------------

f_maup <- file.path(dir_proc, "08_maup_county.rds")


maup_code_version <- "2026-08-28_county-only-groupby_popweighted-lm"

maup_cache_ok <- FALSE
if (file.exists(f_maup)) {
  maup_obj <- readRDS(f_maup)
  maup_cache_ok <- identical(maup_obj$code_version, maup_code_version)
  if (!maup_cache_ok)
    message("  MAUP cache was built by an older code version; recomputing.")
}

if (!maup_cache_ok) {
  message("Aggregating tract values to county (population-weighted)...")

  # Pull total_pop from `analytic` if available; else use uniform weights.
  if (exists("analytic", envir = .GlobalEnv) &&
      "total_pop" %in% names(analytic)) {
    pop_lookup <- analytic |>
      dplyr::select(GEOID, total_pop) |>
      dplyr::mutate(GEOID = as.character(GEOID))
    mod_data2 <- mod_data2 |>
      dplyr::mutate(GEOID = as.character(GEOID)) |>
      dplyr::left_join(pop_lookup, by = "GEOID")
    mod_data2$w <- ifelse(is.na(mod_data2$total_pop) | mod_data2$total_pop == 0,
                          1, mod_data2$total_pop)
    message("  using ACS total_pop weights")
  } else {
    mod_data2$w <- 1
    message("  total_pop unavailable; using uniform tract weights")
  }


  maup_core <- c("ckm_core_pca", "pct_age_65_plus", "ice_race", "ice_income",
                 "pct_hispanic", "pct_bachelor_plus")
  maup_env  <- intersect(
    c("pm25_annual", "o3_8hrmax_4thmax", "LILATracts_1And10",
      "tmax_warm", "walk_index"),
    names(mod_data2)
  )


  ruca_dom <- mod_data2 |>
    dplyr::mutate(county_fips = substr(GEOID, 1, 5)) |>
    dplyr::group_by(county_fips, ruca_class) |>
    dplyr::summarise(wsum = sum(w, na.rm = TRUE), .groups = "drop_last") |>
    dplyr::slice_max(wsum, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(county_fips, ruca_class)

  county_data <- mod_data2 |>
    dplyr::mutate(county_fips = substr(GEOID, 1, 5)) |>
    dplyr::group_by(county_fips) |>              # county only: one row per county
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(c(maup_core, maup_env)),
        ~ stats::weighted.mean(.x, w = w, na.rm = TRUE)
      ),
      pop      = sum(w, na.rm = TRUE),   # county population = regression weight
      n_tracts = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::left_join(ruca_dom, by = "county_fips") |>
    tidyr::drop_na()

  stopifnot(nrow(county_data) == dplyr::n_distinct(county_data$county_fips))
  cat("  Counties (post-aggregation): ", nrow(county_data), "\n")
  if (length(maup_env) < 5)
    message("  NOTE: MAUP using ", length(maup_env), "/5 env predictors (",
            paste(maup_env, collapse = ", "), ")")

  maup_inter <- if (length(maup_env))
    paste0("pct_age_65_plus:", maup_env) else character(0)
  maup_rhs <- paste(c("pct_age_65_plus", "ice_race", "ice_income",
                      "pct_hispanic", "pct_bachelor_plus",
                      maup_env, maup_inter), collapse = " + ")
  maup_fml <- stats::as.formula(paste("ckm_core_pca ~", maup_rhs))


  ols_county    <- lm(maup_fml, data = county_data, weights = pop)
  ols_county_un <- lm(maup_fml, data = county_data)

  maup_obj <- list(model = ols_county, n = nrow(county_data),
                   summary = summary(ols_county),
                   model_unweighted   = ols_county_un,
                   summary_unweighted = summary(ols_county_un),
                   weighting = "population (w = total_pop); unweighted retained as sensitivity",
                   code_version = maup_code_version)
  saveRDS(maup_obj, f_maup)
  message("  saved: ", f_maup)
} else {
  ols_county    <- maup_obj$model
  ols_county_un <- maup_obj$model_unweighted
  message("  MAUP cached (code version ", maup_code_version, ").")
}

cat("\n--- (8a) County-scale OLS coefficients (population-weighted) ---\n")
print(round(coef(summary(ols_county))[, c("Estimate", "Std. Error",
                                            "Pr(>|t|)")], 4))
cat("\n  N counties: ", maup_obj$n, "\n")
cat("  R² (weighted):   ", round(summary(ols_county)$r.squared, 3), "\n")
cat("  R² (unweighted): ", round(summary(ols_county_un)$r.squared, 3), "\n")


maup_compare <- broom::tidy(ols_county) |>
  dplyr::transmute(predictor = term,
                   county_estimate = round(estimate, 4),
                   county_p        = round(p.value, 4)) |>
  dplyr::left_join(
    broom::tidy(ols_county_un) |>
      dplyr::transmute(predictor = term,
                       county_estimate_unweighted = round(estimate, 4),
                       county_p_unweighted        = round(p.value, 4)),
    by = "predictor")
readr::write_csv(maup_compare, file.path(dir_tbl, "08_maup_county.csv"))
cat("  saved: ", file.path(dir_tbl, "08_maup_county.csv"), "\n")

# ---- 8b. Alternative spatial weights ----------------------------

f_altw <- file.path(dir_proc, "08_alt_weights.rds")


altw_code_version <- "2026-08-27_alt-weights-v1"

altw_cache_ok <- FALSE
if (file.exists(f_altw)) {
  altw_obj <- readRDS(f_altw)
  altw_cache_ok <- identical(altw_obj$code_version, altw_code_version)
  if (!altw_cache_ok)
    message("  Alt-weights cache was built by an older code version; recomputing.")
}

if (!altw_cache_ok) {
  message("Recomputing Moran's I under alternative spatial weights...")

  ols_resid <- residuals(ols_full)


  nb_k5  <- spdep::knn2nb(spdep::knearneigh(coords_mod, k =  5), sym = TRUE)
  nb_k20 <- spdep::knn2nb(spdep::knearneigh(coords_mod, k = 20), sym = TRUE)
  lw_k5  <- spdep::nb2listw(nb_k5,  style = "W", zero.policy = TRUE)
  lw_k20 <- spdep::nb2listw(nb_k20, style = "W", zero.policy = TRUE)

  mi_k5  <- spdep::moran.test(ols_resid, lw_k5,  zero.policy = TRUE,
                              randomisation = TRUE)
  mi_k20 <- spdep::moran.test(ols_resid, lw_k20, zero.policy = TRUE,
                              randomisation = TRUE)


  mi_queen <- tryCatch({
    message("  Building queen contiguity (slow, may take 5-15 min)...")
    nb_q  <- spdep::poly2nb(mod_geo, queen = TRUE)
    lw_q  <- spdep::nb2listw(nb_q, style = "W", zero.policy = TRUE)
    spdep::moran.test(ols_resid, lw_q, zero.policy = TRUE,
                      randomisation = TRUE)
  }, error = function(e) {
    message("  ! Queen contiguity skipped: ", e$message)
    NULL
  })

  alt_w_tbl <- tibble::tibble(
    weights = c("k=5", "k=8 (primary)", "k=20",
                if (!is.null(mi_queen)) "Queen contig" else character(0)),
    moran_I = c(unname(mi_k5$estimate[1]),
                unname(mi_resid$estimate[1]),
                unname(mi_k20$estimate[1]),
                if (!is.null(mi_queen)) unname(mi_queen$estimate[1])
                  else numeric(0)),
    p_value = c(mi_k5$p.value, mi_resid$p.value, mi_k20$p.value,
                if (!is.null(mi_queen)) mi_queen$p.value else numeric(0))
  )

  saveRDS(list(table = alt_w_tbl,
               k5 = mi_k5, k20 = mi_k20, queen = mi_queen,
               code_version = altw_code_version), f_altw)
  message("  saved: ", f_altw)
} else {
  alt_w_tbl <- altw_obj$table
  message("  Alt-weights cached (code version ", altw_code_version, ").")
}

cat("\n--- (8b) Residual Moran's I across spatial weights ---\n")
print(alt_w_tbl)
readr::write_csv(alt_w_tbl, file.path(dir_tbl, "08_alt_weights.csv"))

p_altw <- ggplot(alt_w_tbl,
                 aes(x = reorder(weights, moran_I), y = moran_I)) +
  geom_col(fill = "#FD8D3C") +
  geom_text(aes(label = sprintf("%.3f", moran_I)),
            hjust = -0.1, size = 3) +
  coord_flip(ylim = c(0, max(alt_w_tbl$moran_I) * 1.15)) +
  labs(x = NULL, y = "Moran's I on OLS residuals",
       title = "Residual spatial autocorrelation across W specifications",
       subtitle = "Stable across k = 5/8/20 + queen ⇒ robust spatial signal") +
  theme_ckm
ggsave(file.path(dir_fig, "08_alt_weights_moranI.png"),
       p_altw, width = 8, height = 4.5, dpi = 220, bg = "white")
cat("  saved: ", file.path(dir_fig, "08_alt_weights_moranI.png"), "\n")

# ---- 8c. RUCA-stratified OLS ------------------------------------

f_strat <- file.path(dir_proc, "08_ruca_strata.rds")


strat_code_version <- "2026-08-27_ruca-strata-v1"

strat_cache_ok <- FALSE
if (file.exists(f_strat)) {
  strat_obj <- readRDS(f_strat)
  strat_cache_ok <- identical(strat_obj$code_version, strat_code_version)
  if (!strat_cache_ok)
    message("  Strata cache was built by an older code version; recomputing.")
}

if (!strat_cache_ok) {
  message("Fitting RUCA-stratified OLS models...")


  mod_data2 <- mod_data2 |>
    dplyr::mutate(
      ruca_strat = dplyr::case_when(
        ruca_class == "Metro"        ~ "Metro",
        ruca_class == "Micropolitan" ~ "Micropolitan",
        TRUE                         ~ "SmallTown+Rural"
      )
    )


  strat_env   <- intersect(
    c("pm25_annual", "o3_8hrmax_4thmax", "LILATracts_1And10",
      "tmax_warm", "walk_index"),
    names(mod_data2)
  )
  strat_inter <- if (length(strat_env))
    paste0("pct_age_65_plus:", strat_env) else character(0)
  strat_rhs   <- paste(c("pct_age_65_plus", "ice_race", "ice_income",
                         "pct_hispanic", "pct_bachelor_plus",
                         strat_env, strat_inter), collapse = " + ")
  strat_fml   <- stats::as.formula(paste("ckm_core_pca ~", strat_rhs))

  strata_fits <- mod_data2 |>
    dplyr::group_by(ruca_strat) |>
    dplyr::group_split() |>
    purrr::map(function(df) {
      list(stratum = df$ruca_strat[1], n = nrow(df),
           model = lm(strat_fml, data = df))
    })

  saveRDS(list(fits = strata_fits, code_version = strat_code_version), f_strat)
  message("  saved: ", f_strat)
} else {
  strata_fits <- strat_obj$fits
  message("  Strata-fits cached (code version ", strat_code_version, ").")
}


focal_terms <- c("pct_age_65_plus:pm25_annual",
                 "pct_age_65_plus:o3_8hrmax_4thmax",
                 "pct_age_65_plus:LILATracts_1And10",
                 "pct_age_65_plus:tmax_warm",
                 "pct_age_65_plus:walk_index")
strata_compare <- purrr::map_dfr(strata_fits, function(s) {
  td <- broom::tidy(s$model)
  td |>
    dplyr::filter(term %in% focal_terms) |>
    dplyr::transmute(stratum = s$stratum, n = s$n,
                     term, estimate = round(estimate, 5),
                     se = round(std.error, 5),
                     p = round(p.value, 4))
})
cat("\n--- (8c) Aging × env interactions across RUCA strata ---\n")
print(strata_compare)
readr::write_csv(strata_compare, file.path(dir_tbl, "08_ruca_strata.csv"))
cat("  saved: ", file.path(dir_tbl, "08_ruca_strata.csv"), "\n")

cat("\n=== Section 8 complete. ===\n")
cat("  MAUP (county) R²:    ", round(summary(ols_county)$r.squared, 3), "\n")
cat("  Moran's I range across W: [",
    sprintf("%.3f", min(alt_w_tbl$moran_I)), ", ",
    sprintf("%.3f", max(alt_w_tbl$moran_I)), "]\n", sep = "")
cat("  Strata fit:          ",
    paste(vapply(strata_fits, function(s)
                  paste0(s$stratum, " (n=", s$n, ")"), character(1)),
          collapse = " | "), "\n")


# ---- 8d. SVI-replacement sensitivity (OLS form) -----------------

f_svi_sens <- file.path(dir_proc, "08_svi_sensitivity.rds")
svi_terms <- c("svi_ses", "svi_minor", "svi_trans")
have_svi <- all(svi_terms %in% names(mod_data2))
if (have_svi && !file.exists(f_svi_sens)) {
  cat("\n---- 8d. SVI-replacement sensitivity (OLS) ----\n")
  ice_terms <- c("ice_race", "ice_income", "pct_unemployed", "pct_bachelor_plus")
  preds_no_ice <- setdiff(core_predictors, ice_terms)
  preds_svi    <- c(preds_no_ice, svi_terms)
  fml_svi <- stats::as.formula(paste("ckm_core_pca ~",
    paste(c(preds_svi, env_predictors, aging_env_terms), collapse = " + ")))
  d_svi <- tidyr::drop_na(mod_data2,
                          dplyr::all_of(c("ckm_core_pca", svi_terms,
                                          preds_no_ice, env_predictors)))
  ols_svi <- stats::lm(fml_svi, data = d_svi)
  ols_ice <- stats::lm(fml, data = mod_data2)   
  svi_sens <- list(
    ols_svi = ols_svi, ols_ice_ref = ols_ice,
    n_svi   = nrow(d_svi),
    aging_env_svi = broom::tidy(ols_svi) |>
      dplyr::filter(grepl("pct_age_65_plus:", term)),
    aging_env_ice = broom::tidy(ols_ice) |>
      dplyr::filter(grepl("pct_age_65_plus:", term))
  )
  saveRDS(svi_sens, f_svi_sens)
  cat("  n (SVI specification): ", nrow(d_svi), "\n", sep = "")
  cat("  Aging × env interactions — ICE-based (primary) ─────────\n")
  print(svi_sens$aging_env_ice |>
          dplyr::transmute(term, est = round(estimate, 5),
                           se = round(std.error, 5),
                           p = round(p.value, 4)))
  cat("\n  Aging × env interactions — SVI-based (sensitivity) ─────\n")
  print(svi_sens$aging_env_svi |>
          dplyr::transmute(term, est = round(estimate, 5),
                           se = round(std.error, 5),
                           p = round(p.value, 4)))
  readr::write_csv(
    dplyr::bind_rows(
      dplyr::mutate(svi_sens$aging_env_ice, spec = "ICE (primary)"),
      dplyr::mutate(svi_sens$aging_env_svi, spec = "SVI (sensitivity)")),
    file.path(dir_tbl, "08_svi_sensitivity.csv"))
  cat("  saved: ", file.path(dir_tbl, "08_svi_sensitivity.csv"), "\n")
} else if (have_svi) {
  message("  SVI sensitivity: cached.")
} else {
  message("  SVI sensitivity: skipped (SVI columns not in mod_data2 — ",
          "download SVI 2020 CSV to data/raw/ to enable).")
}


# ---- 8e. CKD-excluded (4-indicator) CKM index sensitivity -------


f_ckd_sens <- file.path(dir_proc, "08_ckd_excluded.rds")
core4 <- setdiff(intersect(CKM_CORE, names(analytic)), "KIDNEY")

if (!exists("build_index") || !exists("analytic")) {
  message("  8e CKD-excluded index: skipped (Section 2 objects not in scope).")
  ckd_sens <- NULL
} else if (length(core4) < 4) {
  message("  8e CKD-excluded index: skipped (only ", length(core4),
          " non-kidney Core indicators present).")
  ckd_sens <- NULL
} else if (file.exists(f_ckd_sens)) {
  ckd_sens <- readRDS(f_ckd_sens)
  message("  8e CKD-excluded index: cached.")
} else {
  cat("\n---- 8e. CKD-excluded (4-indicator) CKM index ----\n")
  cat("  Indicators: ", paste(core4, collapse = ", "), "\n", sep = "")


  idx_core4 <- build_index(analytic, core4, "ckm_core4")
  var4      <- summary(idx_core4$pca_obj)$importance[2, 1]
  a4_obj    <- DescTools::CronbachAlpha(
    scale(analytic[, core4])[complete.cases(analytic[, core4]), ])
  a4        <- if (is.list(a4_obj)) a4_obj$CronbachAlpha else as.numeric(a4_obj[1])

  cat("  PC1 variance explained: ", sprintf("%.1f%%", 100 * var4), "\n", sep = "")
  cat("  Cronbach's alpha:       ", sprintf("%.3f", a4), "\n", sep = "")
  cat("  PC1 loadings:\n")
  print(round(idx_core4$pca_obj$rotation[, 1, drop = FALSE], 3))


  mod_data2 <- dplyr::left_join(mod_data2, idx_core4$scores, by = "GEOID")
  ok <- stats::complete.cases(mod_data2$ckm_core_pca, mod_data2$ckm_core4_pca)
  r_pearson  <- stats::cor(mod_data2$ckm_core_pca[ok], mod_data2$ckm_core4_pca[ok])
  r_spearman <- stats::cor(mod_data2$ckm_core_pca[ok], mod_data2$ckm_core4_pca[ok],
                           method = "spearman")
  cat("  Correlation with 5-indicator index: Pearson r = ",
      sprintf("%.4f", r_pearson), ", Spearman rho = ",
      sprintf("%.4f", r_spearman), " (n = ", sum(ok), ")\n", sep = "")

  readr::write_csv(
    tibble::tibble(
      quantity = c("n_tracts", "pc1_variance_explained", "cronbach_alpha",
                   "pearson_r_vs_5indicator", "spearman_rho_vs_5indicator"),
      value    = c(idx_core4$n, var4, a4, r_pearson, r_spearman)),
    file.path(dir_tbl, "08_ckd_excluded_index.csv"))
  cat("  saved: ", file.path(dir_tbl, "08_ckd_excluded_index.csv"), "\n")

  # ---- BYM2 refit on the 4-indicator outcome, identical RHS ----
  ckd_bym2 <- NULL
  if (!have_inla) {
    cat("  INLA not installed; CKD-excluded BYM2 refit skipped.\n")
  } else {
    message("Fitting CKD-excluded BYM2 (this may take 30-90 min)...")
    graph_file <- file.path(dir_graph, "06_inla_graph.adj")
    if (!file.exists(graph_file)) spdep::nb2INLA(graph_file, nb_mod)
    mod_data2$idarea <- match(mod_data2$GEOID, mod_geo$GEOID)
    d4 <- mod_data2 |>
      dplyr::filter(!is.na(idarea), !is.na(ckm_core4_pca))

    fml_str4 <- paste("ckm_core4_pca ~",
                      paste(c(core_predictors, env_predictors,
                              aging_env_terms), collapse = " + "))
    fml_bym2_4 <- stats::as.formula(
      paste(fml_str4,
            "+ f(idarea, model = 'bym2', graph = '", graph_file,
            "', scale.model = TRUE,",
            "    hyper = list(phi  = list(prior = 'pc',",
            "                              param = c(0.5, 0.5)),",
            "                 prec = list(prior = 'pc.prec',",
            "                              param = c(1, 0.01))))",
            sep = ""))

    t0 <- Sys.time()
    fit4 <- INLA::inla(
      fml_bym2_4, data = d4, family = "gaussian",
      control.predictor = list(compute = TRUE),
      control.compute   = list(dic = TRUE, waic = TRUE,
                               return.marginals.predictor = FALSE),
      control.inla      = list(strategy = "adaptive", int.strategy = "eb"),
      verbose = FALSE)
    message("  CKD-excluded BYM2 done in ",
            round(difftime(Sys.time(), t0, units = "mins"), 1), " min")

    ckd_bym2 <- list(summary_fixed = fit4$summary.fixed,
                     dic  = fit4$dic$dic,
                     waic = fit4$waic$waic,
                     n    = nrow(d4))

    # Side-by-side against the 5-indicator primary fit
    cmp <- dplyr::full_join(
      tibble::as_tibble(bym2_save$summary_fixed, rownames = "predictor") |>
        dplyr::transmute(predictor,
                         mean5  = mean,
                         lo5    = `0.025quant`,
                         hi5    = `0.975quant`),
      tibble::as_tibble(ckd_bym2$summary_fixed, rownames = "predictor") |>
        dplyr::transmute(predictor,
                         mean4  = mean,
                         lo4    = `0.025quant`,
                         hi4    = `0.975quant`),
      by = "predictor") |>
      dplyr::mutate(
        sign_agree   = sign(mean5) == sign(mean4),
        both_credible = (lo5 * hi5 > 0) & (lo4 * hi4 > 0),
        pct_change   = 100 * (mean4 - mean5) / abs(mean5))

    readr::write_csv(cmp, file.path(dir_tbl, "08_ckd_excluded_coefficients.csv"))
    cat("  saved: ", file.path(dir_tbl, "08_ckd_excluded_coefficients.csv"), "\n")
    cat("  Sign agreement across all fixed effects: ",
        sum(cmp$sign_agree, na.rm = TRUE), "/", nrow(cmp), "\n", sep = "")
  }

  ckd_sens <- list(index = idx_core4, alpha = a4, pc1_var = var4,
                   r_pearson = r_pearson, r_spearman = r_spearman,
                   bym2 = ckd_bym2)
  saveRDS(ckd_sens, f_ckd_sens)
  cat("  saved: ", f_ckd_sens, "\n")
}


# ---- 8f. Standardized (per-SD / per-IQR) effect sizes ------------


cat("\n---- 8f. Standardized (per-SD / per-IQR) effect sizes ----\n")


.std_frame <- mod_data2
.std_frame$.idarea <- match(.std_frame$GEOID, mod_geo$GEOID)
.std_frame <- dplyr::filter(.std_frame, !is.na(.idarea))

.scale_for <- function(term, dat, what = c("SD", "IQR")) {
  what  <- match.arg(what)
  fun   <- if (what == "SD") stats::sd else stats::IQR
  parts <- strsplit(term, ":", fixed = TRUE)[[1]]
  fac   <- 1
  types <- character(0)
  for (p in parts) {
    if (!p %in% names(dat) || !is.numeric(dat[[p]])) {
      types <- c(types, "unit"); next
    }
    v <- dat[[p]]
    u <- unique(v[!is.na(v)])
    if (length(u) <= 2 && all(u %in% c(0, 1))) {
      types <- c(types, "0->1"); next
    }
    fac   <- fac * fun(v, na.rm = TRUE)
    types <- c(types, what)
  }
  list(factor = fac, type = paste(types, collapse = " x "))
}

.standardize <- function(sf, dat, label, outcome) {
  y_sd <- stats::sd(dat[[outcome]], na.rm = TRUE)
  d    <- tibble::as_tibble(sf, rownames = "predictor")
  d    <- d[d$predictor != "(Intercept)", , drop = FALSE]
  sdi  <- lapply(d$predictor, .scale_for, dat = dat, what = "SD")
  iqi  <- lapply(d$predictor, .scale_for, dat = dat, what = "IQR")
  sd_f <- vapply(sdi, `[[`, numeric(1),   "factor")
  typ  <- vapply(sdi, `[[`, character(1), "type")
  iq_f <- vapply(iqi, `[[`, numeric(1),   "factor")
  tibble::tibble(
    model          = label,
    predictor      = d$predictor,
    raw_mean       = d$mean,
    raw_lower      = d$`0.025quant`,
    raw_upper      = d$`0.975quant`,
    scale_type     = typ,
    sd_factor      = sd_f,
    iqr_factor     = iq_f,
    per_sd_mean    = d$mean         * sd_f,
    per_sd_lower   = d$`0.025quant` * sd_f,
    per_sd_upper   = d$`0.975quant` * sd_f,
    per_iqr_mean   = d$mean         * iq_f,
    pct_outcome_sd = 100 * d$mean * sd_f / y_sd)
}

std_tbl <- .standardize(bym2_save$summary_fixed, .std_frame,
                        "Model 2 (fully adjusted)", "ckm_core_pca")
if (exists("bym2_nb_save") && !is.null(bym2_nb_save))
  std_tbl <- dplyr::bind_rows(
    .standardize(bym2_nb_save$summary_fixed, .std_frame,
                 "Model 1 (behavioral block excluded)", "ckm_core_pca"),
    std_tbl)

readr::write_csv(std_tbl, file.path(dir_tbl, "08_standardized_effects.csv"))
cat("  n (estimation frame): ", nrow(.std_frame), "\n", sep = "")
cat("  outcome SD:           ",
    sprintf("%.4f", stats::sd(.std_frame$ckm_core_pca, na.rm = TRUE)),
    "\n", sep = "")
cat("  saved: ", file.path(dir_tbl, "08_standardized_effects.csv"), "\n")

.preview_model <- if ("Model 1 (behavioral block excluded)" %in% std_tbl$model)
  "Model 1 (behavioral block excluded)" else std_tbl$model[1]
cat("  top predictors, ", .preview_model, ":\n", sep = "")
print(std_tbl |>
        dplyr::filter(model == .preview_model) |>
        dplyr::arrange(dplyr::desc(abs(pct_outcome_sd))) |>
        dplyr::transmute(predictor,
                         per_sd   = round(per_sd_mean, 4),
                         pct_outSD = round(pct_outcome_sd, 1)) |>
        head(12))


# ---- 8g. South-minus-non-South decomposition of the Model 2 predictor ----
 

cat("\n---- 8g. South-minus-non-South decomposition (Model 2) ----\n")


if (!exists("bym2_save") || is.null(bym2_save) ||
    is.null(bym2_save$summary_random)) {
  message("  8g decomposition: skipped (Model 2 BYM2 fit not in scope).")
  south_decomp <- NULL
} else {


  .dec <- mod_data2
  .dec$.idarea <- match(.dec$GEOID, mod_geo$GEOID)
  .dec <- dplyr::filter(.dec, !is.na(.idarea))

 
  .rd <- tibble::as_tibble(bym2_save$summary_random)
  .rd <- head(.rd, nrow(mod_geo))
  .rd$GEOID <- mod_geo$GEOID
  .dec <- dplyr::left_join(.dec, .rd |> dplyr::select(GEOID, .re = mean),
                           by = "GEOID")

  stopifnot(nrow(.dec) == bym2_save$n, !anyNA(.dec$.re),
            !anyNA(.dec$ckm_core_pca))

  .b <- setNames(as.numeric(bym2_save$summary_fixed$mean),
                 rownames(bym2_save$summary_fixed))
  .rc <- as.character(.dec$ruca_class)
  .need <- c("pct_age_65_plus", "pct_hispanic", "ice_income", "ice_race",
             "pct_bachelor_plus", "LPA", "CSMOKING", "CHECKUP", "ACCESS2",
             "pm25_annual", "o3_8hrmax_4thmax", "tmax_warm", "walk_index",
             "LILATracts_1And10", "ruca_classMicropolitan",
             "ruca_classSmall town", "ruca_classRural",
             "pct_age_65_plus:pm25_annual", "pct_age_65_plus:o3_8hrmax_4thmax",
             "pct_age_65_plus:LILATracts_1And10", "pct_age_65_plus:tmax_warm",
             "pct_age_65_plus:walk_index")
  stopifnot(all(.need %in% names(.b)))

  .li <- as.numeric(.dec$LILATracts_1And10)     
  .a  <- .dec$pct_age_65_plus                   

  .blocks <- list(
    Demography  = .b["pct_age_65_plus"] * .a +
                   .b["pct_hispanic"] * .dec$pct_hispanic,
    SES         = .b["ice_income"] * .dec$ice_income +
                   .b["ice_race"] * .dec$ice_race +
                   .b["pct_bachelor_plus"] * .dec$pct_bachelor_plus,
    Behavioral  = .b["LPA"] * .dec$LPA + .b["CSMOKING"] * .dec$CSMOKING +
                   .b["CHECKUP"] * .dec$CHECKUP + .b["ACCESS2"] * .dec$ACCESS2,
    Environment = .b["pm25_annual"] * .dec$pm25_annual +
                   .b["o3_8hrmax_4thmax"] * .dec$o3_8hrmax_4thmax +
                   .b["tmax_warm"] * .dec$tmax_warm +
                   .b["walk_index"] * .dec$walk_index +
                   .b["LILATracts_1And10"] * .li,
    Rurality    = .b["ruca_classMicropolitan"] * (.rc == "Micropolitan") +
                   .b["ruca_classSmall town"] * (.rc == "Small town") +
                   .b["ruca_classRural"] * (.rc == "Rural"),
    Interaction = .b["pct_age_65_plus:pm25_annual"] * .a * .dec$pm25_annual +
                   .b["pct_age_65_plus:o3_8hrmax_4thmax"] * .a *
                     .dec$o3_8hrmax_4thmax +
                   .b["pct_age_65_plus:LILATracts_1And10"] * .a * .li +
                   .b["pct_age_65_plus:tmax_warm"] * .a * .dec$tmax_warm +
                   .b["pct_age_65_plus:walk_index"] * .a * .dec$walk_index,
    RandomEff   = .dec$.re)


  .south_fips <- c("01", "05", "10", "11", "12", "13", "21", "22", "24",
                   "28", "37", "40", "45", "47", "48", "51", "54")
  .iss <- substr(.dec$GEOID, 1, 2) %in% .south_fips

  south_decomp <- tibble::tibble(
    block    = names(.blocks),
    mean_south     = vapply(.blocks, function(v) mean(v[.iss],  na.rm = TRUE), 0),
    mean_non_south = vapply(.blocks, function(v) mean(v[!.iss], na.rm = TRUE), 0))
  south_decomp$gap <- south_decomp$mean_south - south_decomp$mean_non_south

  .obs <- mean(.dec$ckm_core_pca[.iss]) - mean(.dec$ckm_core_pca[!.iss])

  cat("  South n = ", format(sum(.iss), big.mark = ","),
      " | non-South n = ", format(sum(!.iss), big.mark = ","), "\n", sep = "")
  cat("  Block contribution to the linear predictor, South minus non-South:\n")
  for (i in seq_len(nrow(south_decomp)))
    cat(sprintf("    %-12s %+8.3f  %+12.6f\n", south_decomp$block[i],
                south_decomp$gap[i], south_decomp$gap[i]))
  cat(sprintf("    %-12s %+8.3f  %+12.6f\n", "SUM(all)",
              sum(south_decomp$gap), sum(south_decomp$gap)))
  cat(sprintf("    %-12s %+8.3f  %+12.6f\n", "OBSERVED", .obs, .obs))
  cat(sprintf("    %-12s %+8.3f  %+12.6f\n", "residual",
              .obs - sum(south_decomp$gap), .obs - sum(south_decomp$gap)))

  south_decomp <- dplyr::bind_rows(
    south_decomp,
    tibble::tibble(block = c("SUM_ALL_BLOCKS", "OBSERVED_GAP"),
                   mean_south = NA_real_, mean_non_south = NA_real_,
                   gap = c(sum(south_decomp$gap), .obs)))
  readr::write_csv(south_decomp,
                   file.path(dir_tbl, "08_south_decomposition.csv"))
  cat("  saved: ", file.path(dir_tbl, "08_south_decomposition.csv"), "\n")


  .g <- setNames(south_decomp$gap, south_decomp$block)
  .named_in_text <- c("Behavioral", "Demography", "SES", "Environment",
                      "Rurality", "Interaction", "RandomEff")
  if (!isTRUE(all.equal(sum(.g[.named_in_text]), unname(.g["SUM_ALL_BLOCKS"]),
                        tolerance = 1e-8)))
    warning("8g: enumerated blocks do not sum to the reported total; ",
            "Results 3.9 must list every block it adds up.")

  rm(.dec, .rd, .b, .rc, .li, .a, .blocks, .iss, .obs, .g, .need,
     .south_fips, .named_in_text)
}


# ==============================================================
# 9.  FIGURES + TABLES
# ==============================================================

# Outputs:
#   output/figures/09_forest_bym2.png         — main effects + CrIs, Model 2
#                                               (comparator view)
#   output/figures/09_forest_bym2_primary.png  — Model 1 forest, single-
#                                               specification view
#   output/figures/09_dumbbell_bym2_m1_m2.png  — manuscript Figure 4: Model 1 vs
#                                               Model 2, paired by predictor
#   output/figures/09_residual_moran_progression.png
#   output/tables/09_main_results_table.csv   — combined OLS|ESF|BYM2|SHAP
#   output/tables/09_table3_bym2.csv          — BYM2 Model 2 coefficients; these
#                                               are the Model 2 column of Table 3
#   output/tables/09_tableS7_ols_esf.csv      — manuscript Table S7 (OLS/ESF)

cat("\n========================================================\n")
cat("Section 9 — Manuscript figures + tables\n")
cat("========================================================\n")

# ---- 9a. Forest plot of BYM2 95% CrIs ---------------------------
bym2_save <- readRDS(file.path(dir_proc, "06_inla_bym2.rds"))
forest_df <- tibble::as_tibble(bym2_save$summary_fixed,
                                rownames = "predictor") |>
  dplyr::filter(predictor != "(Intercept)") |>
  dplyr::mutate(
    block = dplyr::case_when(
      grepl(":", predictor)                                ~ "Aging × Env",
      predictor %in% c("pm25_annual", "o3_8hrmax_4thmax",
                       "LILATracts_1And10", "tmax_warm",
                       "walk_index")                        ~ "Environment",
      predictor %in% c("CSMOKING", "LPA", "ACCESS2",
                       "CHECKUP")                          ~ "Behavioral",
      grepl("^ruca", predictor) | predictor %in%
        c("ice_race", "ice_income", "pct_bachelor_plus",
          "median_income", "pct_hispanic")                 ~ "SDOH",
      TRUE                                                  ~ "Demography"
    ),
    block = factor(block, levels = c("Aging × Env", "Environment",
                                      "SDOH", "Behavioral", "Demography"))
  ) |>
  dplyr::arrange(block, mean) |>
  dplyr::mutate(predictor = factor(predictor, levels = predictor))

p_forest <- ggplot(forest_df,
                   aes(x = mean, y = predictor, colour = block)) +
  geom_vline(xintercept = 0, colour = "grey50", linetype = "dashed") +
  geom_errorbar(aes(xmin = `0.025quant`, xmax = `0.975quant`),
                width = 0, linewidth = 0.6, orientation = "y") +
  geom_point(size = 2.4) +
  scale_y_discrete(labels = .label_pred) +
  scale_colour_manual(values = c("Aging × Env" = "#B2182B",
                                  "Environment" = "#FD8D3C",
                                  "SDOH"        = "#2166AC",
                                  "Behavioral"  = "#9970AB",
                                  "Demography"  = "#5AAE61")) +
  facet_wrap(~ block, ncol = 1, scales = "free", strip.position = "top") +
  labs(
    x = "Posterior mean coefficient (95% credible interval)",
    y = NULL,
    colour = NULL,
    title    = "Adjusted associations of SDOH and environmental exposures with CKM burden",
    subtitle = paste0("Bayesian hierarchical spatial regression (INLA BYM2). ",
                      "Vertical reference line at 0 = no association."),
    caption  = paste0("N = ", formatC(bym2_save$n, big.mark = ","),
                      " tracts.  DIC = ", round(bym2_save$dic, 0),
                      ".  Each panel uses an independent x-axis scale because",
                      " effect-size magnitudes differ by predictor block.")
  ) +
  theme_ckm +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 11),
        strip.background = element_rect(fill = "grey92", colour = NA),
        panel.spacing.y = unit(0.8, "lines"),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(size = 8, colour = "grey40",
                                     hjust = 0, lineheight = 1.1),
        plot.margin = margin(8, 14, 8, 8))
ggsave(file.path(dir_fig, "09_forest_bym2.png"),
       p_forest, width = 12, height = 9, dpi = 220, bg = "white")
cat("  saved: ", file.path(dir_fig, "09_forest_bym2.png"), "\n")

# ---- 9a2. Forest plot of the PRIMARY specification (Model 1) ----

forest2_df <- readr::read_csv(
    file.path(dir_tbl, "06_6g_coefficients_bym2_nobehav.csv"),
    show_col_types = FALSE) |>
  dplyr::filter(predictor != "(Intercept)") |>
  dplyr::mutate(
    block = dplyr::case_when(
      grepl(":", predictor)                                ~ "Aging × Env",
      predictor %in% c("pm25_annual", "o3_8hrmax_4thmax",
                       "LILATracts_1And10", "tmax_warm",
                       "walk_index")                        ~ "Environment",
      grepl("^ruca", predictor) | predictor %in%
        c("ice_race", "ice_income", "pct_bachelor_plus",
          "median_income", "pct_hispanic")                 ~ "SDOH",
      TRUE                                                  ~ "Demography"
    ),
    block = factor(block, levels = c("Aging × Env", "Environment",
                                      "SDOH", "Demography"))
  ) |>
  dplyr::arrange(block, mean) |>
  dplyr::mutate(predictor = factor(predictor, levels = predictor))


stopifnot(!any(c("CSMOKING", "LPA", "ACCESS2", "CHECKUP") %in%
                 as.character(forest2_df$predictor)))
stopifnot(nrow(forest2_df) == 18L)

fit2 <- readr::read_csv(file.path(dir_tbl, "06_6g_fit_nobehav.csv"),
                        show_col_types = FALSE)

p_forest2 <- ggplot(forest2_df,
                    aes(x = mean, y = predictor, colour = block)) +
  geom_vline(xintercept = 0, colour = "grey50", linetype = "dashed") +
  geom_errorbar(aes(xmin = `0.025quant`, xmax = `0.975quant`),
                width = 0, linewidth = 0.6, orientation = "y") +
  geom_point(size = 2.4) +
  scale_y_discrete(labels = .label_pred) +
  scale_colour_manual(values = c("Aging × Env" = "#B2182B",
                                  "Environment" = "#FD8D3C",
                                  "SDOH"        = "#2166AC",
                                  "Demography"  = "#5AAE61")) +
  facet_wrap(~ block, ncol = 1, scales = "free", strip.position = "top") +
  labs(
    x = "Posterior mean coefficient (95% credible interval)",
    y = NULL,
    colour = NULL,
    title    = "Adjusted associations of SDOH and environmental exposures with CKM burden",

    subtitle = paste0("Bayesian hierarchical spatial regression (INLA BYM2), ",
                      "behavior-excluded primary specification (Model 1).\n",
                      "Vertical reference line at 0 = no association."),

    caption  = paste0("N = ", formatC(as.integer(fit2$n[1]),
                                       format = "d", big.mark = ","),
                      " tracts.  DIC = ", formatC(as.integer(round(fit2$dic[1], 0)),
                                                   format = "d", big.mark = ","),
                      ".  Each panel uses an independent x-axis scale because",
                      " effect-size magnitudes differ by predictor block.")
  ) +
  theme_ckm +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 11),
        strip.background = element_rect(fill = "grey92", colour = NA),
        panel.spacing.y = unit(0.8, "lines"),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(size = 8, colour = "grey40",
                                     hjust = 0, lineheight = 1.1),
        plot.margin = margin(8, 14, 8, 8))

ggsave(file.path(dir_fig, "09_forest_bym2_primary.png"),
       p_forest2, width = 12, height = 9, dpi = 220, bg = "white")
cat("  saved: ", file.path(dir_fig, "09_forest_bym2_primary.png"),
    "  (Model 1 forest; superseded as manuscript Figure 4 by the dumbbell below)\n")

m1_dumb <- readr::read_csv(
    file.path(dir_tbl, "06_6g_coefficients_bym2_nobehav.csv"),
    show_col_types = FALSE) |>
  dplyr::filter(predictor != "(Intercept)") |>
  dplyr::transmute(predictor, m1 = mean,
                   m1lo = `0.025quant`, m1hi = `0.975quant`)
m2_dumb <- readr::read_csv(
    file.path(dir_tbl, "06_coefficient_comparison.csv"),
    show_col_types = FALSE) |>
  dplyr::filter(predictor != "(Intercept)") |>
  dplyr::transmute(predictor, m2 = bym2_estimate,
                   m2lo = `bym2_2.5`, m2hi = `bym2_97.5`)

dumb_df <- dplyr::inner_join(m1_dumb, m2_dumb, by = "predictor") |>
  dplyr::mutate(
    block = dplyr::case_when(
      grepl(":", predictor)                                ~ "Aging \u00d7 Env",
      predictor %in% c("pm25_annual", "o3_8hrmax_4thmax",
                       "LILATracts_1And10", "tmax_warm",
                       "walk_index")                        ~ "Environment",
      grepl("^ruca", predictor) | predictor %in%
        c("ice_race", "ice_income", "pct_bachelor_plus",
          "median_income", "pct_hispanic")                 ~ "SDOH",
      TRUE                                                  ~ "Demography"
    ),
    block = factor(block, levels = c("Aging \u00d7 Env", "Environment",
                                      "SDOH", "Demography")),
    flips = sign(m1) != sign(m2)
  ) |>
  dplyr::arrange(block, m1) |>
  dplyr::mutate(predictor = factor(predictor, levels = predictor))


stopifnot(nrow(dumb_df) == 18L)
stopifnot(!any(c("CSMOKING", "LPA", "ACCESS2", "CHECKUP") %in%
                 as.character(dumb_df$predictor)))
stopifnot(sum(dumb_df$flips) == 7L)

dumb_long <- dplyr::bind_rows(
  dumb_df |> dplyr::transmute(predictor, block, est = m1, lo = m1lo, hi = m1hi,
                              Model = "Model 1 (behavior-excluded, primary)"),
  dumb_df |> dplyr::transmute(predictor, block, est = m2, lo = m2lo, hi = m2hi,
                              Model = "Model 2 (behavior-adjusted)"))

p_dumb <- ggplot(dumb_df) +
  geom_vline(xintercept = 0, colour = "grey50", linetype = "dashed") +
  geom_segment(aes(x = m1, xend = m2, y = predictor, yend = predictor,
                   colour = flips), linewidth = 0.9, lineend = "round") +
  geom_errorbar(data = dumb_long, aes(xmin = lo, xmax = hi, y = predictor),
                width = 0, linewidth = 0.4, colour = "grey35") +
  geom_point(data = dumb_long, aes(x = est, y = predictor, fill = Model),
             shape = 21, size = 2.8, colour = "grey20", stroke = 0.4) +
  scale_y_discrete(labels = .label_pred) +
  scale_colour_manual(values = c(`FALSE` = "grey70", `TRUE` = "#B2182B"),
                      guide = "none") +
  scale_fill_manual(values = c("Model 1 (behavior-excluded, primary)" = "#2166AC",
                               "Model 2 (behavior-adjusted)"          = "#F4A582")) +
  facet_wrap(~ block, ncol = 1, scales = "free", strip.position = "top") +
  labs(
    x = "Posterior mean coefficient (95% credible interval)", y = NULL, fill = NULL,
    title = "Adjusted associations with CKM burden under two adjustment sets",

    subtitle = paste0("Bayesian hierarchical spatial regression (INLA BYM2). ",
                      "Each pair joins one predictor's posterior\nmean under ",
                      "Model 1 and Model 2; red connectors mark coefficients ",
                      "that change sign.\nVertical reference line at 0 = no association."),
    caption = paste0("N = 69,530 tracts. Each panel uses an independent x-axis ",
                     "scale because effect-size magnitudes differ by predictor block.\n",
                     "Model 1 excludes the behavioral and healthcare-engagement ",
                     "block by construction, so four predictor domains are plotted ",
                     "rather than five.")
  ) +
  theme_ckm +
  theme(strip.text = element_text(face = "bold", size = 11),
        strip.background = element_rect(fill = "grey92", colour = NA),
        panel.spacing.y = unit(0.8, "lines"),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(size = 8, colour = "grey40",
                                     hjust = 0, lineheight = 1.1),
        legend.position = "bottom",
        plot.margin = margin(8, 14, 8, 8))


dumb_nrow <- as.integer(table(dumb_df$block)[levels(dumb_df$block)])
stopifnot(sum(dumb_nrow) == 18L)
gt_dumb <- ggplotGrob(p_dumb)
dumb_prow <- gt_dumb$layout$t[grepl("^panel", gt_dumb$layout$name)]
stopifnot(length(dumb_prow) == length(dumb_nrow))
gt_dumb$heights[dumb_prow] <- grid::unit(dumb_nrow, "null")

ggsave(file.path(dir_fig, "09_dumbbell_bym2_m1_m2.png"), gt_dumb,
       width = 12, height = 8.2, dpi = 220, bg = "white")
cat("  saved: ", file.path(dir_fig, "09_dumbbell_bym2_m1_m2.png"),
    "  (manuscript Figure 4)\n")


# ---- 9b. Residual Moran's I progression bar chart --------------
mi_prog <- readr::read_csv(
  file.path(dir_tbl, "06_residual_moran_progression.csv"),
  show_col_types = FALSE)
p_mi <- ggplot(mi_prog,
               aes(x = reorder(model, -moran_I), y = moran_I)) +
  geom_col(fill = "#FD8D3C") +
  geom_text(aes(label = sprintf("%.3f", moran_I)),
            vjust = -0.4, size = 3.5) +
  labs(x = NULL, y = "Residual Moran's I",
       title = "Residual spatial autocorrelation: model progression",
       subtitle = sprintf("OLS → spmoran ESF reduces residual I from %.3f → %.3f",
                          max(mi_prog$moran_I, na.rm = TRUE),
                          min(mi_prog$moran_I, na.rm = TRUE))) +
  theme_ckm
ggsave(file.path(dir_fig, "09_residual_moran_progression.png"),
       p_mi, width = 7, height = 4.5, dpi = 220, bg = "white")
cat("  saved: ", file.path(dir_fig, "09_residual_moran_progression.png"), "\n")

# ---- 9c. Combined main-results table (OLS | ESF | BYM2 | SHAP) -

ols_coefs  <- readr::read_csv(file.path(dir_tbl, "06_coefficient_comparison.csv"),
                              show_col_types = FALSE) |>
  dplyr::transmute(predictor,
                   ols_estimate = round(ols_estimate, 4),
                   ols_se       = signif(ols_se, 3),
                   ols_p        = signif(ols_p, 3))
esf_coefs  <- readr::read_csv(file.path(dir_tbl, "06_coefficients_esf.csv"),
                              show_col_types = FALSE) |>
  dplyr::transmute(predictor,
                   esf_estimate = round(Estimate, 4),
                   esf_p        = signif(p_value, 3))
bym2_coefs <- tibble::as_tibble(bym2_save$summary_fixed,
                                 rownames = "predictor") |>
  dplyr::transmute(predictor,
                   bym2_mean = round(mean, 4),
                   bym2_2.5  = round(`0.025quant`, 4),
                   bym2_97.5 = round(`0.975quant`, 4))
shap_rank  <- readr::read_csv(file.path(dir_tbl, "07_shap_global.csv"),
                              show_col_types = FALSE) |>
  dplyr::transmute(predictor = feature, shap_rank = rank,
                   shap_pct  = pct_total)

main_results <- ols_coefs |>
  dplyr::full_join(esf_coefs,  by = "predictor") |>
  dplyr::full_join(bym2_coefs, by = "predictor") |>
  dplyr::full_join(shap_rank,  by = "predictor")
readr::write_csv(main_results, file.path(dir_tbl, "09_main_results_table.csv"))
cat("  saved: ", file.path(dir_tbl, "09_main_results_table.csv"), "\n")

# ---- 9d. Split into the BYM2 export and manuscript Table S7 ----------------


.pred_label <- c(
  `(Intercept)`                       = "Intercept",
  pct_age_65_plus                     = "% Adults aged ≥65 years",
  pct_hispanic                        = "% Hispanic or Latino",
  ice_income                          = "ICE (Income)",
  pct_bachelor_plus                   = "% Adults with bachelor's degree or higher",
  ice_race                            = "ICE (Race)",
  ACCESS2                             = "Lacking health insurance (ages 18-64)",
  CHECKUP                             = "Routine checkup in past year",
  ruca_classMicropolitan              = "RUCA: Micropolitan",
  `ruca_classSmall town`              = "RUCA: Small town",
  ruca_classRural                     = "RUCA: Rural",
  CSMOKING                            = "Current smoking",
  LPA                                 = "No leisure-time physical activity",
  pm25_annual                         = "Annual mean PM₂.₅",
  o3_8hrmax_4thmax                    = "4th-highest 8-hr maximum O₃",
  LILATracts_1And10                   = "Low-income low-access tract",
  tmax_warm                           = "Warm-season mean daily maximum temperature (°C)",
  walk_index                          = "EPA National Walkability Index",
  `pct_age_65_plus:pm25_annual`       = "% Age ≥65 × PM₂.₅",
  `pct_age_65_plus:o3_8hrmax_4thmax`  = "% Age ≥65 × O₃",
  `pct_age_65_plus:LILATracts_1And10` = "% Age ≥65 × LILA",
  `pct_age_65_plus:tmax_warm`         = "% Age ≥65 × Heat",
  `pct_age_65_plus:walk_index`        = "% Age ≥65 × Walkability")

.label_check <- function(x) {
  miss <- setdiff(x, names(.pred_label))
  if (length(miss)) warning("No manuscript label for predictor(s): ",
                            paste(miss, collapse = ", "), call. = FALSE)
  ifelse(x %in% names(.pred_label), unname(.pred_label[x]), x)
}

tbl3_manuscript <- tibble::as_tibble(bym2_save$summary_fixed,
                                     rownames = "predictor") |>
  dplyr::transmute(Predictor             = .label_check(predictor),
                   `BYM2 posterior mean` = mean,
                   `95% CrI lower`       = `0.025quant`,
                   `95% CrI upper`       = `0.975quant`)
readr::write_csv(tbl3_manuscript, file.path(dir_tbl, "09_table3_bym2.csv"))
cat("  saved: ", file.path(dir_tbl, "09_table3_bym2.csv"), "\n")

tblS7_manuscript <- readr::read_csv(
    file.path(dir_tbl, "06_coefficient_comparison.csv"), show_col_types = FALSE) |>
  dplyr::filter(!is.na(ols_estimate)) |>
  dplyr::transmute(Predictor = .label_check(predictor),
                   `OLS β`  = ols_estimate,
                   `OLS SE` = ols_se,
                   `OLS p`  = ols_p,
                   `ESF β`  = esf_estimate,
                   `ESF p`  = esf_p)
readr::write_csv(tblS7_manuscript, file.path(dir_tbl, "09_tableS7_ols_esf.csv"))
cat("  saved: ", file.path(dir_tbl, "09_tableS7_ols_esf.csv"), "\n")

cat("\n=== Section 9 complete. ===\n")
cat("  Manuscript-ready outputs assembled at:\n")
cat("    figures: ", dir_fig, "\n")
cat("    tables:  ", dir_tbl, "\n")
cat("\n  Recommended figure order for manuscript:\n")
cat("    Fig 1: eda_choropleth_ckm_core.png        (national CKM burden map)\n")
cat("    Fig 2: 03e_sdoh_panel.png                 (SDOH predictor geography — 3 panels)\n")
cat("    Fig 3: 03d_choropleth_pm25.png            (PM2.5 exposure map)\n")
cat("    Fig 4: 04_nhanes_validation.png           (construct validation)\n")
cat("    Fig 5: 09_dumbbell_bym2_m1_m2.png         (HEADLINE — Model 1 vs Model 2\n")
cat("           BYM2 coefficients; see the FIG5_DUMBBELL block in Sec. 9a2)\n")
cat("           09_forest_bym2_primary.png is the Model 1 forest plot, superseded\n")
cat("           as manuscript Figure 4 by the dumbbell; 09_forest_bym2.png is the\n")
cat("           Model 2 comparator. Neither is a manuscript figure.\n")
cat("    Fig 6: 06h_bym2_spatial_random_effect_2panel.png\n")
cat("           (residual spatial pattern, Model 2 | Model 1 — see Sec. 6.7b)\n")
cat("    Fig 7: 07_shap_summary.png                (ML feature importance)\n")
cat("    Fig 8: 07_age_env_interactions.png        (aging SHAP dependence)\n")
cat("    Sup1:  05_ols_residuals_map.png           (OLS residual diagnostic)\n")
cat("    Sup2:  09_residual_moran_progression.png  (spatial-correction progression)\n")
cat("    Sup3:  08_alt_weights_moranI.png          (W-robustness)\n")
cat("    Sup4:  03d_choropleth_o3.png              (O3 exposure map)\n")
cat("    Sup5:  03d_choropleth_tmax.png            (heat exposure map)\n")
cat("    Sup6:  03d_choropleth_walk.png            (walkability exposure map)\n")
cat("    Sup7:  06_5a_lisa_cluster_map.png         (LISA local-cluster map)\n")
cat("    Sup8:  06_5b_getis_ord_gi.png             (Getis-Ord Gi* hotspots)\n")
cat("    Sup9:  06_5c_moran_correlogram.png        (Moran's I correlogram)\n")
cat("    Sup10: 06_5d_residual_semivariogram.png   (residual semivariogram)\n")
cat("    Sup11: S9_outcome_normality.png           (outcome normality check)\n")
cat("    Sup12: S10_ols_residual_qq.png            (OLS residual Q-Q plot)\n")

# ==============================================================
# 10.  POST-PIPELINE ANALYSES
# ==============================================================


cat("\n", strrep("=", 74), "\n", sep = "")
cat("Section 10 - post-pipeline analyses\n")
cat(strrep("=", 74), "\n")

if (!exists("RUN_POST"))
  RUN_POST <- !("--skip-post" %in% commandArgs(TRUE))

.post_fail <- character(0)

.post_n    <- 0L
.run_stage <- function(label, fn) {
  if (!RUN_POST) return(invisible(NULL))
  .post_n <<- .post_n + 1L
  cat("\n  --- ", label, " ---\n", sep = "")
  t0 <- Sys.time()
  res <- tryCatch({ fn(); TRUE },
                  error = function(e) { message("  STAGE FAILED: ",
                                                conditionMessage(e)); FALSE })
  m <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  if (isTRUE(res)) cat(sprintf("  done %s (%.1f min)\n", label, m))
  else .post_fail <<- c(.post_fail, label)
  invisible(res)
}


.ckm_env <- new.env(parent = globalenv())
eval(quote({

  # =============================================================================
  suppressMessages({library(dplyr); library(tidyr); library(sf)})

  CORE_PRED <- c("pct_age_65_plus", "pct_hispanic", "ice_income",
                 "pct_bachelor_plus", "ice_race", "ACCESS2", "CHECKUP",
                 "ruca_class", "CSMOKING", "LPA")
  ENV_PRED  <- c("pm25_annual", "o3_8hrmax_4thmax", "LILATracts_1And10",
                 "tmax_warm", "walk_index")
  CENTER    <- c("pct_age_65_plus", "pm25_annual", "o3_8hrmax_4thmax",
                 "tmax_warm", "walk_index")
  BEHAV     <- c("ACCESS2", "CHECKUP", "CSMOKING", "LPA")
  # Model 1 is the PRIMARY specification: the behavioural block is excluded.
  M1_PRED   <- setdiff(CORE_PRED, BEHAV)

  ckm_root <- function() {
    root <- getwd()
    if (!dir.exists(file.path(root, "data")) &&
        dir.exists(file.path(root, "..", "data")))
      root <- normalizePath(file.path(root, ".."))
    stopifnot(dir.exists(file.path(root, "data", "processed")))
    root
  }

  ckm_sample <- function(verbose = TRUE) {
    root     <- ckm_root()
    dir_proc <- file.path(root, "data", "processed")

    an <- readRDS(file.path(dir_proc, "02_ckm_index.rds"))
    if (inherits(an, "sf")) an <- st_drop_geometry(an)
    geo <- readRDS(file.path(dir_proc, "01_tracts_geo.rds"))
    d   <- an |> left_join(st_drop_geometry(geo)[, c("GEOID", "ALAND")],
                           by = "GEOID")

    conus <- d |>
      filter(!substr(GEOID, 1, 2) %in%
               c("02", "15", "60", "66", "69", "72", "78")) |>
      filter(ALAND > 0, !is.na(ckm_core_pca))

    mod <- conus |>
      mutate(ruca_class = factor(ruca_class,
               levels = c("Metro", "Micropolitan", "Small town", "Rural"))) |>
      drop_na(ckm_core_pca, pct_age_65_plus, ice_race, ice_income, ruca_class,
              pct_hispanic, pct_black, pct_bachelor_plus, median_income) |>
      drop_na(all_of(c("ckm_core_pca", CORE_PRED, ENV_PRED)))


    ref <- readRDS(file.path(dir_proc, "12_bym2_refit_model1.rds"))
    if (!identical(as.character(mod$GEOID), as.character(ref$GEOID)))
      stop("SAMPLE DRIFT: rebuild does not match the validated Model 1 refit ",
           "row-for-row (n_rebuild = ", nrow(mod), ", n_ref = ", length(ref$GEOID),
           "). Refusing to estimate anything.", call. = FALSE)
    stopifnot(nrow(mod) == 69530L)


    mod$o3_8hrmax_4thmax <- mod$o3_8hrmax_4thmax * 100
    for (v in CENTER) mod[[v]] <- mod[[v]] - mean(mod[[v]], na.rm = TRUE)
    stopifnot(!"LILATracts_1And10" %in% CENTER)

    mod$idarea <- seq_len(nrow(mod))
    mod$state  <- substr(mod$GEOID, 1, 2)

    graph_file <- file.path(dir_proc, "12_bym2_graph.adj")
    stopifnot(file.exists(graph_file))
    n_nodes <- as.integer(readLines(graph_file, n = 1L))
    if (!identical(n_nodes, nrow(mod)))
      stop("graph/node mismatch: ", n_nodes, " vs ", nrow(mod), call. = FALSE)

    mod_geo <- geo[match(mod$GEOID, geo$GEOID), ]
    stopifnot(identical(as.character(mod_geo$GEOID), as.character(mod$GEOID)))

    if (verbose) {
      message(sprintf("sample OK: n = %d, matches validated Model 1 refit",
                      nrow(mod)))
      message(sprintf("graph: %s (%d nodes)", basename(graph_file), n_nodes))
      message(sprintf("states: %d", dplyr::n_distinct(mod$state)))
    }
    list(mod = mod, graph_file = graph_file, geo = mod_geo, ref = ref)
  }


  m1_published <- function() {
    ref <- readRDS(file.path(ckm_root(), "data", "processed",
                             "12_bym2_refit_model1.rds"))
    ref$summary_fixed
  }

}), .ckm_env)
stopifnot(is.function(get("ckm_sample", .ckm_env)))


.stage <- function() {
  # =============================================================================
  # 09_precision_weighted_sensitivity.R
 
  # INPUTS   data/processed/02_ckm_index.rds
  #          data/processed/01_tracts_geo.rds
  #          data/processed/06_inla_bym2.rds        (primary fit, for validation)
  #          data/raw/PLACES__..._2022_release_20260311.csv
  # OUTPUTS  data/processed/09_precision_weighted.rds
  #          output/tables/09_precision_weighted_sensitivity.csv
  #          output/tables/09_precision_weighted_validation.csv
  # =============================================================================

  suppressMessages({
    library(dplyr); library(tidyr); library(data.table)
    library(sf); library(spdep); library(INLA)
  })

  root <- getwd()
  if (!dir.exists(file.path(root, "data")) &&
      dir.exists(file.path(root, "..", "data"))) root <- normalizePath(file.path(root, ".."))
  stopifnot(dir.exists(file.path(root, "data", "processed")))
  dir_proc <- file.path(root, "data", "processed")
  dir_tab  <- file.path(root, "output", "tables")
  f_raw    <- file.path(root, "data", "raw",
    "PLACES__Census_Tract_Data_(GIS_Friendly_Format),_2022_release_20260311.csv")

  CORE <- c("DIABETES", "BPHIGH", "OBESITY", "HIGHCHOL", "KIDNEY")
  CORE_PRED <- c("pct_age_65_plus", "pct_hispanic", "ice_income",
                 "pct_bachelor_plus", "ice_race", "ACCESS2", "CHECKUP",
                 "ruca_class", "CSMOKING", "LPA")
  ENV_PRED  <- c("pm25_annual", "o3_8hrmax_4thmax", "LILATracts_1And10",
                 "tmax_warm", "walk_index")
  CENTER    <- c("pct_age_65_plus", "pm25_annual", "o3_8hrmax_4thmax",
                 "tmax_warm", "walk_index")
  pad11 <- function(x) formatC(as.numeric(x), width = 11, format = "f",
                               digits = 0, flag = "0")

  # ---------------------------------------------------------------------------
  # 1. Reproduce the PCA standardization constants and PC1 loadings

  # ---------------------------------------------------------------------------
  an <- readRDS(file.path(dir_proc, "02_ckm_index.rds"))
  if (inherits(an, "sf")) an <- st_drop_geometry(an)

  Xc  <- an[complete.cases(an[, CORE]), c("GEOID", CORE)]
  Xz  <- scale(as.matrix(Xc[, CORE]))
  sd_j <- attr(Xz, "scaled:scale"); names(sd_j) <- CORE

  pca <- prcomp(Xz, center = FALSE, scale. = FALSE)
  if (pca$rotation["DIABETES", 1] < 0) pca$rotation[, 1] <- -pca$rotation[, 1]
  loadings <- pca$rotation[, 1]
  var_pc1_total <- var(pca$x[, 1])

  message(sprintf("PCA reproduced: n = %d, PC1 explains %.1f%% of variance",
                  nrow(Xc), 100 * summary(pca)$importance[2, 1]))
  message("PC1 loadings: ",
          paste(sprintf("%s=%.3f", CORE, loadings[CORE]), collapse = "  "))

  # ---------------------------------------------------------------------------
  # 2. Recover per-tract indicator SEs from the published 95% CIs
  # ---------------------------------------------------------------------------
  raw <- fread(f_raw)
  raw[, k := pad11(TractFIPS)]
  se_dt <- data.table(k = raw$k)
  for (v in CORE) {
    s  <- raw[[paste0(v, "_Crude95CI")]]
    nn <- regmatches(s, gregexpr("[0-9.]+", s))
    ok <- lengths(nn) == 2
    lo <- hi <- rep(NA_real_, length(s))
    lo[ok] <- as.numeric(vapply(nn[ok], `[`, "", 1))
    hi[ok] <- as.numeric(vapply(nn[ok], `[`, "", 2))
    se_dt[[v]] <- (hi - lo) / (2 * qnorm(0.975))
  }

  wdt <- merge(data.table(GEOID = Xc$GEOID, k = pad11(Xc$GEOID)),
               se_dt, by = "k")
  stopifnot(nrow(wdt) == nrow(Xc))
  var_pc1 <- rowSums(vapply(CORE, function(v)
    (loadings[[v]]^2) * (wdt[[v]] / sd_j[[v]])^2, numeric(nrow(wdt))))
  stopifnot(!anyNA(var_pc1), all(var_pc1 > 0))
  wdt[, ckm_pc1_var := var_pc1]

  err_share <- mean(var_pc1) / var_pc1_total
  message(sprintf(
    "Outcome measurement-error variance = %.2f%% of total between-tract PC1 variance",
    100 * err_share))

  # ---------------------------------------------------------------------------
  # 3. Rebuild the estimation sample (CKM_US_Main.R Sections 5-6)
  # ---------------------------------------------------------------------------
  geo <- readRDS(file.path(dir_proc, "01_tracts_geo.rds"))
  d   <- an |> left_join(st_drop_geometry(geo)[, c("GEOID", "ALAND")], by = "GEOID")

  conus <- d |>
    filter(!substr(GEOID, 1, 2) %in% c("02", "15", "60", "66", "69", "72", "78")) |>
    filter(ALAND > 0, !is.na(ckm_core_pca))
  message(sprintf("CONUS tracts with index: %d", nrow(conus)))

  mod <- conus |>
    mutate(ruca_class = factor(ruca_class,
             levels = c("Metro", "Micropolitan", "Small town", "Rural"))) |>
    drop_na(ckm_core_pca, pct_age_65_plus, ice_race, ice_income, ruca_class,
            pct_hispanic, pct_black, pct_bachelor_plus, median_income) |>
    drop_na(all_of(c("ckm_core_pca", CORE_PRED, ENV_PRED)))

  prim <- readRDS(file.path(dir_proc, "06_inla_bym2.rds"))
  message(sprintf("Estimation sample: n = %d (primary fit n = %d)",
                  nrow(mod), prim$n))
  stopifnot(nrow(mod) == prim$n)

  # Match the primary model's variable scaling exactly.
  mod$o3_8hrmax_4thmax <- mod$o3_8hrmax_4thmax * 100          # ppm -> per-10-ppb
  for (v in CENTER) mod[[v]] <- mod[[v]] - mean(mod[[v]], na.rm = TRUE)

  # Attach weights, scaled to mean 1 over the estimation sample.
  mod <- mod |> left_join(as.data.frame(wdt[, .(GEOID, ckm_pc1_var)]), by = "GEOID")
  stopifnot(!anyNA(mod$ckm_pc1_var))
  mod$ckm_wt <- (1 / mod$ckm_pc1_var) / mean(1 / mod$ckm_pc1_var)
  message(sprintf("Weights: p1=%.3f med=%.3f p99=%.3f (%.1f-fold p99/p1)",
                  quantile(mod$ckm_wt, .01), median(mod$ckm_wt),
                  quantile(mod$ckm_wt, .99),
                  quantile(mod$ckm_wt, .99) / quantile(mod$ckm_wt, .01)))

  # ---------------------------------------------------------------------------
  # 4. Rebuild the k = 8 symmetric neighbour graph on this sample's ordering
  # ---------------------------------------------------------------------------
  mod_geo <- geo[match(mod$GEOID, geo$GEOID), ]
  stopifnot(identical(mod_geo$GEOID, mod$GEOID))
  coords <- suppressWarnings(
    st_coordinates(st_centroid(mod_geo, of_largest_polygon = TRUE)))
  nb <- knn2nb(knearneigh(coords, k = 8), sym = TRUE)
  graph_file <- file.path(dir_proc, "09_pw_graph.adj")

  nb2INLA_fast <- function(file, nb) {
    crd <- spdep::card(nb)
    writeLines(c(as.character(length(nb)),
      vapply(seq_along(nb), function(i)
        if (crd[i] == 0L) paste(i, 0)
        else paste(c(i, crd[i], nb[[i]]), collapse = " "),
        character(1))), file)
  }
  nb2INLA_fast(graph_file, nb)
  mod$idarea <- seq_len(nrow(mod))
  message(sprintf("Neighbour graph: %d nodes, mean %.2f neighbours",
                  length(nb), mean(lengths(nb))))

  # ---------------------------------------------------------------------------
  # 5. Fit unweighted and precision-weighted BYM2 on the identical sample+graph
  # ---------------------------------------------------------------------------
  fml_str <- paste("ckm_core_pca ~",
    paste(c(CORE_PRED, ENV_PRED,
            paste0("pct_age_65_plus:", ENV_PRED)), collapse = " + "))
  fml <- as.formula(paste0(
    fml_str, " + f(idarea, model = 'bym2', graph = '", graph_file, "',",
    " scale.model = TRUE,",
    " hyper = list(phi  = list(prior = 'pc', param = c(0.5, 0.5)),",
    "              prec = list(prior = 'pc.prec', param = c(1, 0.01))))"))

 
  run <- function(label, scale_vec) {
    message(sprintf("\nFitting %s BYM2 (n = %d)...", label, nrow(mod)))
    stopifnot(length(scale_vec) == nrow(mod),
              all(is.finite(scale_vec)), all(scale_vec > 0))
    d <- mod
    d$scale_wt <- scale_vec        
    t0 <- Sys.time()
    f <- withCallingHandlers(
      INLA::inla(fml, family = "gaussian", data = d, scale = scale_wt,
                 control.compute   = list(dic = TRUE, waic = TRUE),
                 control.inla      = list(int.strategy = "eb"),
                 control.predictor = list(compute = FALSE)),
      warning = function(w) {
        if (grepl("expanded to NULL", conditionMessage(w), fixed = TRUE))
          stop("INLA ignored `scale=` for the ", label, " fit: ",
               conditionMessage(w), call. = FALSE)
     
      })
    message(sprintf("  done in %.1f min", as.numeric(
      difftime(Sys.time(), t0, units = "mins"))))
    f
  }

  fit_u <- run("unweighted",          rep(1, nrow(mod)))
  fit_w <- run("precision-weighted",  mod$ckm_wt)


  stopifnot(!isTRUE(all.equal(fit_u$summary.fixed$mean,
                              fit_w$summary.fixed$mean, tolerance = 1e-12)))
  message("weights confirmed applied: weighted and unweighted fits differ")

  # ---------------------------------------------------------------------------
  # 6a. Validation: does the unweighted refit reproduce the published primary?
  # ---------------------------------------------------------------------------
  pf <- prim$summary_fixed
  uf <- fit_u$summary.fixed
  shared <- intersect(rownames(pf), rownames(uf))
  val <- data.frame(
    Term        = shared,
    Published   = pf[shared, "mean"],
    Refit       = uf[shared, "mean"],
    Abs_diff    = uf[shared, "mean"] - pf[shared, "mean"],
    row.names = NULL)
  message("\n--- Validation: unweighted refit vs published primary fit ---")
  print(val, digits = 4, row.names = FALSE)
  message(sprintf("max |diff| = %.5f   r = %.5f",
                  max(abs(val$Abs_diff)), cor(val$Published, val$Refit)))
  write.csv(val, file.path(dir_tab, "09_precision_weighted_validation.csv"),
            row.names = FALSE)

  # ---------------------------------------------------------------------------
  # 6b. Primary comparison: weighted vs unweighted, same sample and graph
  # ---------------------------------------------------------------------------
  wf  <- fit_w$summary.fixed
  trm <- rownames(uf)
  cred <- function(lo, hi) sign(lo) == sign(hi)
  cmp <- data.frame(
    Term            = trm,
    Unweighted_mean = uf[trm, "mean"],
    Unweighted_lo   = uf[trm, "0.025quant"],
    Unweighted_hi   = uf[trm, "0.975quant"],
    Weighted_mean   = wf[trm, "mean"],
    Weighted_lo     = wf[trm, "0.025quant"],
    Weighted_hi     = wf[trm, "0.975quant"],
    row.names = NULL, check.names = FALSE)
  cmp$Abs_change     <- cmp$Weighted_mean - cmp$Unweighted_mean
  cmp$Pct_change     <- 100 * cmp$Abs_change / abs(cmp$Unweighted_mean)
  cmp$Credible_unwt  <- cred(cmp$Unweighted_lo, cmp$Unweighted_hi)
  cmp$Credible_wt    <- cred(cmp$Weighted_lo,   cmp$Weighted_hi)
  cmp$Sign_flip      <- sign(cmp$Weighted_mean) != sign(cmp$Unweighted_mean)

  message("\n--- Precision-weighted vs unweighted BYM2 fixed effects ---")
  print(cmp[, c("Term", "Unweighted_mean", "Weighted_mean", "Pct_change",
                "Credible_unwt", "Credible_wt")], digits = 4, row.names = FALSE)
  message(sprintf(
    "\nmax |%% change| = %.2f%% | median |%% change| = %.2f%% | credibility changes: %d | sign flips: %d",
    max(abs(cmp$Pct_change), na.rm = TRUE),
    median(abs(cmp$Pct_change), na.rm = TRUE),
    sum(cmp$Credible_unwt != cmp$Credible_wt), sum(cmp$Sign_flip)))
  message(sprintf("correlation of posterior means: r = %.5f",
                  cor(cmp$Unweighted_mean, cmp$Weighted_mean)))
  message(sprintf("DIC  unweighted %.1f  weighted %.1f", fit_u$dic$dic,  fit_w$dic$dic))
  message(sprintf("WAIC unweighted %.1f  weighted %.1f", fit_u$waic$waic, fit_w$waic$waic))

  dir.create(dir_tab, showWarnings = FALSE, recursive = TRUE)
  write.csv(cmp, file.path(dir_tab, "09_precision_weighted_sensitivity.csv"),
            row.names = FALSE)
  saveRDS(list(comparison = cmp, validation = val,
               weights = mod[, c("GEOID", "ckm_pc1_var", "ckm_wt")],
               err_share = err_share, loadings = loadings, sd_j = sd_j,
               summary_fixed_unwt = uf, summary_fixed_wt = wf,
               dic  = c(unweighted = fit_u$dic$dic,   weighted = fit_w$dic$dic),
               waic = c(unweighted = fit_u$waic$waic, weighted = fit_w$waic$waic),
               formula = fml_str, n = nrow(mod)),
          file.path(dir_proc, "09_precision_weighted.rds"))
  message("\nSaved: 09_precision_weighted.rds, 09_precision_weighted_sensitivity.csv")

}
environment(.stage) <- new.env(parent = globalenv())
.run_stage("09_precision_weighted_sensitivity.R", .stage)

# ---- stage: 09b_precision_weighted_model2.R ----
.stage <- function() {
  # =============================================================================
  # 09b_precision_weighted_model2.R

  # INPUTS   data/processed/02_ckm_index.rds
  #          data/processed/01_tracts_geo.rds
  #          data/processed/06_inla_bym2.rds                  (for the n check)
  #          output/tables/06_6g_coefficients_bym2_nobehav.csv (published Model 1)
  #          data/raw/PLACES__..._2022_release_20260311.csv
  # OUTPUTS  data/processed/09b_precision_weighted_model2.rds
  #          output/tables/09b_precision_weighted_model2_sensitivity.csv
  #          output/tables/09b_precision_weighted_model2_validation.csv
  # =============================================================================

  suppressMessages({
    library(dplyr); library(tidyr); library(data.table)
    library(sf); library(spdep); library(INLA)
  })

  root <- getwd()
  if (!dir.exists(file.path(root, "data")) &&
      dir.exists(file.path(root, "..", "data"))) root <- normalizePath(file.path(root, ".."))
  stopifnot(dir.exists(file.path(root, "data", "processed")))
  dir_proc <- file.path(root, "data", "processed")
  dir_tab  <- file.path(root, "output", "tables")
  f_raw    <- file.path(root, "data", "raw",
    "PLACES__Census_Tract_Data_(GIS_Friendly_Format),_2022_release_20260311.csv")

  CORE <- c("DIABETES", "BPHIGH", "OBESITY", "HIGHCHOL", "KIDNEY")


  BEHAV <- c("ACCESS2", "CHECKUP", "CSMOKING", "LPA")

  # Model 2 predictor set, kept intact for the complete-case filter.
  CORE_PRED <- c("pct_age_65_plus", "pct_hispanic", "ice_income",
                 "pct_bachelor_plus", "ice_race", "ACCESS2", "CHECKUP",
                 "ruca_class", "CSMOKING", "LPA")

  # Model 1 predictor set: Model 2 minus the behavioral / care-engagement block.
  CORE_PRED_EXC <- setdiff(CORE_PRED, BEHAV)
  stopifnot(length(CORE_PRED_EXC) == 6L, !any(BEHAV %in% CORE_PRED_EXC))

  ENV_PRED  <- c("pm25_annual", "o3_8hrmax_4thmax", "LILATracts_1And10",
                 "tmax_warm", "walk_index")
  CENTER    <- c("pct_age_65_plus", "pm25_annual", "o3_8hrmax_4thmax",
                 "tmax_warm", "walk_index")
  pad11 <- function(x) formatC(as.numeric(x), width = 11, format = "f",
                               digits = 0, flag = "0")

  message("Model 1 (behavior-excluded) precision-weighted sensitivity")
  message("  dropped from the linear predictor: ", paste(BEHAV, collapse = ", "))

  # ---------------------------------------------------------------------------
  # 1. Reproduce the PCA standardization constants and PC1 loadings

  # ---------------------------------------------------------------------------
  an <- readRDS(file.path(dir_proc, "02_ckm_index.rds"))
  if (inherits(an, "sf")) an <- st_drop_geometry(an)

  Xc  <- an[complete.cases(an[, CORE]), c("GEOID", CORE)]
  Xz  <- scale(as.matrix(Xc[, CORE]))
  sd_j <- attr(Xz, "scaled:scale"); names(sd_j) <- CORE

  pca <- prcomp(Xz, center = FALSE, scale. = FALSE)
  if (pca$rotation["DIABETES", 1] < 0) pca$rotation[, 1] <- -pca$rotation[, 1]
  loadings <- pca$rotation[, 1]
  var_pc1_total <- var(pca$x[, 1])

  message(sprintf("PCA reproduced: n = %d, PC1 explains %.1f%% of variance",
                  nrow(Xc), 100 * summary(pca)$importance[2, 1]))
  message("PC1 loadings: ",
          paste(sprintf("%s=%.3f", CORE, loadings[CORE]), collapse = "  "))

  # ---------------------------------------------------------------------------
  # 2. Recover per-tract indicator SEs from the published 95% CIs
  # ---------------------------------------------------------------------------
  raw <- fread(f_raw)
  raw[, k := pad11(TractFIPS)]
  se_dt <- data.table(k = raw$k)
  for (v in CORE) {
    s  <- raw[[paste0(v, "_Crude95CI")]]
    nn <- regmatches(s, gregexpr("[0-9.]+", s))
    ok <- lengths(nn) == 2
    lo <- hi <- rep(NA_real_, length(s))
    lo[ok] <- as.numeric(vapply(nn[ok], `[`, "", 1))
    hi[ok] <- as.numeric(vapply(nn[ok], `[`, "", 2))
    se_dt[[v]] <- (hi - lo) / (2 * qnorm(0.975))
  }

  wdt <- merge(data.table(GEOID = Xc$GEOID, k = pad11(Xc$GEOID)),
               se_dt, by = "k")
  stopifnot(nrow(wdt) == nrow(Xc))
  var_pc1 <- rowSums(vapply(CORE, function(v)
    (loadings[[v]]^2) * (wdt[[v]] / sd_j[[v]])^2, numeric(nrow(wdt))))
  stopifnot(!anyNA(var_pc1), all(var_pc1 > 0))
  wdt[, ckm_pc1_var := var_pc1]

  err_share <- mean(var_pc1) / var_pc1_total
  message(sprintf(
    "Outcome measurement-error variance = %.2f%% of total between-tract PC1 variance",
    100 * err_share))

  # ---------------------------------------------------------------------------
  # 3. Rebuild the estimation sample (CKM_US_Main.R Sections 5-6)

  # ---------------------------------------------------------------------------
  geo <- readRDS(file.path(dir_proc, "01_tracts_geo.rds"))
  d   <- an |> left_join(st_drop_geometry(geo)[, c("GEOID", "ALAND")], by = "GEOID")

  conus <- d |>
    filter(!substr(GEOID, 1, 2) %in% c("02", "15", "60", "66", "69", "72", "78")) |>
    filter(ALAND > 0, !is.na(ckm_core_pca))
  message(sprintf("CONUS tracts with index: %d", nrow(conus)))

  mod <- conus |>
    mutate(ruca_class = factor(ruca_class,
             levels = c("Metro", "Micropolitan", "Small town", "Rural"))) |>
    drop_na(ckm_core_pca, pct_age_65_plus, ice_race, ice_income, ruca_class,
            pct_hispanic, pct_black, pct_bachelor_plus, median_income) |>
    drop_na(all_of(c("ckm_core_pca", CORE_PRED, ENV_PRED)))

  prim <- readRDS(file.path(dir_proc, "06_inla_bym2.rds"))
  message(sprintf("Estimation sample: n = %d (primary fit n = %d)",
                  nrow(mod), prim$n))
  stopifnot(nrow(mod) == prim$n)


  mod$o3_8hrmax_4thmax <- mod$o3_8hrmax_4thmax * 100         
  for (v in CENTER) mod[[v]] <- mod[[v]] - mean(mod[[v]], na.rm = TRUE)

 
  mod <- mod |> left_join(as.data.frame(wdt[, .(GEOID, ckm_pc1_var)]), by = "GEOID")
  stopifnot(!anyNA(mod$ckm_pc1_var))
  mod$ckm_wt <- (1 / mod$ckm_pc1_var) / mean(1 / mod$ckm_pc1_var)
  message(sprintf("Weights: p1=%.3f med=%.3f p99=%.3f (%.1f-fold p99/p1)",
                  quantile(mod$ckm_wt, .01), median(mod$ckm_wt),
                  quantile(mod$ckm_wt, .99),
                  quantile(mod$ckm_wt, .99) / quantile(mod$ckm_wt, .01)))

  # ---------------------------------------------------------------------------
  # 4. Rebuild the k = 8 symmetric neighbour graph on this sample's ordering
  # ---------------------------------------------------------------------------
  mod_geo <- geo[match(mod$GEOID, geo$GEOID), ]
  stopifnot(identical(mod_geo$GEOID, mod$GEOID))
  coords <- suppressWarnings(
    st_coordinates(st_centroid(mod_geo, of_largest_polygon = TRUE)))
  nb <- knn2nb(knearneigh(coords, k = 8), sym = TRUE)
  graph_file <- file.path(dir_proc, "09b_pw_m2_graph.adj")

  nb2INLA_fast <- function(file, nb) {
    crd <- spdep::card(nb)
    writeLines(c(as.character(length(nb)),
      vapply(seq_along(nb), function(i)
        if (crd[i] == 0L) paste(i, 0)
        else paste(c(i, crd[i], nb[[i]]), collapse = " "),
        character(1))), file)
  }
  nb2INLA_fast(graph_file, nb)
  mod$idarea <- seq_len(nrow(mod))
  message(sprintf("Neighbour graph: %d nodes, mean %.2f neighbours",
                  length(nb), mean(lengths(nb))))

  # ---------------------------------------------------------------------------
  # 5. Fit unweighted and precision-weighted Model 1 BYM2 on the identical
  #    sample + graph
  # ---------------------------------------------------------------------------
  fml_str <- paste("ckm_core_pca ~",
    paste(c(CORE_PRED_EXC, ENV_PRED,
            paste0("pct_age_65_plus:", ENV_PRED)), collapse = " + "))


  stopifnot(!any(vapply(BEHAV, function(v) grepl(v, fml_str, fixed = TRUE),
                        logical(1))))
  message("Model 1 formula: ", fml_str)

  fml <- as.formula(paste0(
    fml_str, " + f(idarea, model = 'bym2', graph = '", graph_file, "',",
    " scale.model = TRUE,",
    " hyper = list(phi  = list(prior = 'pc', param = c(0.5, 0.5)),",
    "              prec = list(prior = 'pc.prec', param = c(1, 0.01))))"))


  run <- function(label, scale_vec) {
    message(sprintf("\nFitting %s Model 1 BYM2 (n = %d)...", label, nrow(mod)))
    stopifnot(length(scale_vec) == nrow(mod),
              all(is.finite(scale_vec)), all(scale_vec > 0))
    d <- mod
    d$scale_wt <- scale_vec         
    t0 <- Sys.time()
    f <- withCallingHandlers(
      INLA::inla(fml, family = "gaussian", data = d, scale = scale_wt,
                 control.compute   = list(dic = TRUE, waic = TRUE),
                 control.inla      = list(int.strategy = "eb"),
                 control.predictor = list(compute = FALSE)),
      warning = function(w) {
        if (grepl("expanded to NULL", conditionMessage(w), fixed = TRUE))
          stop("INLA ignored `scale=` for the ", label, " fit: ",
               conditionMessage(w), call. = FALSE)

      })
    message(sprintf("  done in %.1f min", as.numeric(
      difftime(Sys.time(), t0, units = "mins"))))
    f
  }

  fit_u <- run("unweighted",          rep(1, nrow(mod)))
  fit_w <- run("precision-weighted",  mod$ckm_wt)


  stopifnot(!isTRUE(all.equal(fit_u$summary.fixed$mean,
                              fit_w$summary.fixed$mean, tolerance = 1e-12)))
  message("weights confirmed applied: weighted and unweighted fits differ")

  # ---------------------------------------------------------------------------
  # 6a. Validation: does the unweighted refit reproduce the published Model 1?
  # ---------------------------------------------------------------------------
  pub <- read.csv(file.path(dir_tab, "06_6g_coefficients_bym2_nobehav.csv"),
                  check.names = FALSE, stringsAsFactors = FALSE)
  pf  <- setNames(pub$mean, pub$predictor)
  uf  <- fit_u$summary.fixed
  shared <- intersect(names(pf), rownames(uf))
  stopifnot(length(shared) == nrow(uf))          
  val <- data.frame(
    Term      = shared,
    Published = as.numeric(pf[shared]),
    Refit     = uf[shared, "mean"],
    row.names = NULL)
  val$Abs_diff <- val$Refit - val$Published
  message("\n--- Validation: unweighted Model 1 refit vs published Model 1 fit ---")
  print(val, digits = 6, row.names = FALSE)
  message(sprintf("max |diff| = %.6f   r = %.6f",
                  max(abs(val$Abs_diff)), cor(val$Published, val$Refit)))
  write.csv(val, file.path(dir_tab, "09b_precision_weighted_model2_validation.csv"),
            row.names = FALSE)

  # ---------------------------------------------------------------------------
  # 6b. Primary comparison: weighted vs unweighted, same sample and graph
  # ---------------------------------------------------------------------------
  wf  <- fit_w$summary.fixed
  trm <- rownames(uf)
  cred <- function(lo, hi) sign(lo) == sign(hi)
  cmp <- data.frame(
    Term            = trm,
    Unweighted_mean = uf[trm, "mean"],
    Unweighted_lo   = uf[trm, "0.025quant"],
    Unweighted_hi   = uf[trm, "0.975quant"],
    Weighted_mean   = wf[trm, "mean"],
    Weighted_lo     = wf[trm, "0.025quant"],
    Weighted_hi     = wf[trm, "0.975quant"],
    row.names = NULL, check.names = FALSE)
  cmp$Abs_change     <- cmp$Weighted_mean - cmp$Unweighted_mean
  cmp$Pct_change     <- 100 * cmp$Abs_change / abs(cmp$Unweighted_mean)
  cmp$Credible_unwt  <- cred(cmp$Unweighted_lo, cmp$Unweighted_hi)
  cmp$Credible_wt    <- cred(cmp$Weighted_lo,   cmp$Weighted_hi)
  cmp$Sign_flip      <- sign(cmp$Weighted_mean) != sign(cmp$Unweighted_mean)

  message("\n--- Precision-weighted vs unweighted Model 1 BYM2 fixed effects ---")
  print(cmp[, c("Term", "Unweighted_mean", "Weighted_mean", "Pct_change",
                "Credible_unwt", "Credible_wt")], digits = 4, row.names = FALSE)
  message(sprintf(
    "\nmax |%% change| = %.2f%% | median |%% change| = %.2f%% | credibility changes: %d | sign flips: %d",
    max(abs(cmp$Pct_change), na.rm = TRUE),
    median(abs(cmp$Pct_change), na.rm = TRUE),
    sum(cmp$Credible_unwt != cmp$Credible_wt), sum(cmp$Sign_flip)))
  message(sprintf("correlation of posterior means: r = %.5f",
                  cor(cmp$Unweighted_mean, cmp$Weighted_mean)))
  message(sprintf("DIC  unweighted %.1f  weighted %.1f", fit_u$dic$dic,  fit_w$dic$dic))
  message(sprintf("WAIC unweighted %.1f  weighted %.1f", fit_u$waic$waic, fit_w$waic$waic))


  for (v in c("pct_age_65_plus", "pct_age_65_plus:walk_index",
              "pct_age_65_plus:LILATracts_1And10", "pm25_annual")) {
    r <- cmp[cmp$Term == v, ]
    if (nrow(r) == 1)
      message(sprintf("  %-36s unwt %+.5f [%+.5f, %+.5f]  wt %+.5f [%+.5f, %+.5f]",
                      v, r$Unweighted_mean, r$Unweighted_lo, r$Unweighted_hi,
                      r$Weighted_mean, r$Weighted_lo, r$Weighted_hi))
  }

  dir.create(dir_tab, showWarnings = FALSE, recursive = TRUE)
  write.csv(cmp, file.path(dir_tab, "09b_precision_weighted_model2_sensitivity.csv"),
            row.names = FALSE)
  saveRDS(list(model = "Model 1 (behavior-excluded)",
               dropped = BEHAV,
               comparison = cmp, validation = val,
               weights = mod[, c("GEOID", "ckm_pc1_var", "ckm_wt")],
               err_share = err_share, loadings = loadings, sd_j = sd_j,
               summary_fixed_unwt = uf, summary_fixed_wt = wf,
               dic  = c(unweighted = fit_u$dic$dic,   weighted = fit_w$dic$dic),
               waic = c(unweighted = fit_u$waic$waic, weighted = fit_w$waic$waic),
               formula = fml_str, n = nrow(mod)),
          file.path(dir_proc, "09b_precision_weighted_model2.rds"))
  message("\nSaved: 09b_precision_weighted_model2.rds, ",
          "09b_precision_weighted_model2_sensitivity.csv")
  message("sessionInfo() follows, for the archived log:")
  print(sessionInfo())

}
environment(.stage) <- new.env(parent = globalenv())
.run_stage("09b_precision_weighted_model2.R", .stage)

# ---- stage: 10_denominator_flow.R ----
.stage <- function() {
  # =============================================================================
  # 10_denominator_flow.R

  # =============================================================================

  suppressMessages({library(dplyr); library(ggplot2); library(sf)})

  root <- getwd()
  if (!dir.exists(file.path(root, "data")) &&
      dir.exists(file.path(root, "..", "data"))) root <- normalizePath(file.path(root, ".."))
  stopifnot(dir.exists(file.path(root, "data", "processed")))
  dir_proc <- file.path(root, "data", "processed")
  dir_fig  <- file.path(root, "output", "figures")
  dir_tab  <- file.path(root, "output", "tables")
  dir.create(dir_fig, showWarnings = FALSE, recursive = TRUE)
  dir.create(dir_tab, showWarnings = FALSE, recursive = TRUE)

  CORE      <- c("DIABETES", "BPHIGH", "OBESITY", "HIGHCHOL", "KIDNEY")
  CORE_PRED <- c("pct_age_65_plus", "pct_hispanic", "ice_income",
                 "pct_bachelor_plus", "ice_race", "ACCESS2", "CHECKUP",
                 "ruca_class", "CSMOKING", "LPA")
  ENV_PRED  <- c("pm25_annual", "o3_8hrmax_4thmax", "LILATracts_1And10",
                 "tmax_warm", "walk_index")
  NONCONUS  <- c("02", "15", "60", "66", "69", "72", "78")

  an  <- readRDS(file.path(dir_proc, "02_ckm_index.rds"))
  if (inherits(an, "sf")) an <- st_drop_geometry(an)
  geo <- readRDS(file.path(dir_proc, "01_tracts_geo.rds")) |> st_drop_geometry()

  n_states <- function(g) length(unique(substr(g, 1, 2)))

  # --- Step 1: attribute join -------------------------------------------------
  n1 <- nrow(an); s1 <- n_states(an$GEOID)

  # --- Step 2: index universe (drop tracts missing any Core indicator) --------
  idx  <- an[complete.cases(an[, CORE]), ]
  n2   <- nrow(idx); s2 <- n_states(idx$GEOID)
  drop_idx <- an[!complete.cases(an[, CORE]), ]
  n_nj <- sum(substr(drop_idx$GEOID, 1, 2) == "34")
  stopifnot(n1 - n2 == nrow(drop_idx))

  # --- Step 3: contiguous US --------------------------------------------------
  conus <- idx |>
    left_join(geo[, c("GEOID", "ALAND")], by = "GEOID") |>
    filter(!substr(GEOID, 1, 2) %in% NONCONUS, ALAND > 0, !is.na(ckm_core_pca))
  n3 <- nrow(conus); s3 <- n_states(conus$GEOID)
  n_ak <- sum(substr(idx$GEOID, 1, 2) == "02")
  n_hi <- sum(substr(idx$GEOID, 1, 2) == "15")

  # --- Step 4: estimation sample ---------------------------------------------
  est <- conus |>
    mutate(ruca_class = factor(ruca_class,
             levels = c("Metro", "Micropolitan", "Small town", "Rural"))) |>
    tidyr::drop_na(ckm_core_pca, pct_age_65_plus, ice_race, ice_income,
                   ruca_class, pct_hispanic, pct_black, pct_bachelor_plus,
                   median_income) |>
    tidyr::drop_na(tidyselect::all_of(c("ckm_core_pca", CORE_PRED, ENV_PRED)))
  n4 <- nrow(est); s4 <- n_states(est$GEOID)

  # --- Arithmetic must close --------------------------------------------------
  stopifnot(n1 - n_nj == n2)                 # NJ is the whole of the first drop
  stopifnot(n2 - (n_ak + n_hi) == n3)        # AK + HI is the whole of the second
  message(sprintf("%d -(%d NJ)-> %d -(%d AK + %d HI)-> %d -(%d covariates)-> %d",
                  n1, n_nj, n2, n_ak, n_hi, n3, n3 - n4, n4))
  message(sprintf("state-equivalents: %d -> %d -> %d -> %d", s1, s2, s3, s4))

  flow <- data.frame(
    step = 1:4,
    stage = c("PLACES 2022 and ACS attribute join",
              "CKM burden index universe",
              "Contiguous United States",
              # BYM2 results are reported in Section 3.3; 3.4 is the XGBoost /
              # SHAP section. This one string feeds BOTH the flow CSV and the
              # Figure S13 box, so correcting it here corrects both.
              "Estimation sample (OLS 3.2; BYM2 3.3)"),
    n = c(n1, n2, n3, n4),
    state_equivalents = c(s1, s2, s3, s4),
    excluded_before = c(NA, n1 - n2, n2 - n3, n3 - n4))
  write.csv(flow, file.path(dir_tab, "10_denominator_flow.csv"), row.names = FALSE)

  # ---------------------------------------------------------------------------
  # Figure
  # ---------------------------------------------------------------------------
  fmt <- function(x) formatC(x, big.mark = ",", format = "d")

  boxes <- data.frame(
    y = c(4, 3, 2, 1),
    title = flow$stage,
    body = c(
      sprintf("%s tracts  |  %d states + DC", fmt(n1), s1 - 1L),
      sprintf("%s tracts  |  %d states + DC", fmt(n2), s2 - 1L),
      sprintf("%s tracts  |  %d states + DC", fmt(n3), s3 - 1L),
      sprintf("%s tracts  |  %d states + DC", fmt(n4), s4 - 1L)))

  excl <- data.frame(
    y = c(3.5, 2.5, 1.5),
    label = c(
      sprintf("Excluded %s New Jersey tracts:\nCDC PLACES publishes no high-blood-pressure\nor high-cholesterol estimate, so the five-indicator\nindex cannot be computed",
              fmt(n_nj)),
      sprintf("Excluded %s tracts (Alaska %s, Hawaii %s):\nEPA Downscaler and PRISM exposure surfaces\ndo not extend beyond the contiguous US",
              fmt(n_ak + n_hi), fmt(n_ak), fmt(n_hi)),
      sprintf("Excluded %s tracts with incomplete\ndemographic or socioeconomic covariates\n(environmental, heat, walkability and food-access\nblocks contribute no further attrition)",
              fmt(n3 - n4))))

  p <- ggplot() +
    # main boxes (left column)
    geom_rect(data = boxes,
              aes(xmin = 0, xmax = 4.1, ymin = y - 0.32, ymax = y + 0.32),
              fill = "grey97", colour = "grey25", linewidth = 0.5) +
    geom_text(data = boxes, aes(x = 0.16, y = y + 0.12, label = title),
              hjust = 0, fontface = "bold", size = 3.5) +
    geom_text(data = boxes, aes(x = 0.16, y = y - 0.13, label = body),
              hjust = 0, size = 3.2, colour = "grey20") +
    # vertical connectors
    geom_segment(data = data.frame(y = c(4, 3, 2)),
                 aes(x = 1.1, xend = 1.1, y = y - 0.32, yend = y - 0.68),
                 arrow = arrow(length = unit(0.16, "cm"), type = "closed"),
                 linewidth = 0.45, colour = "grey25") +
    # elbow into exclusion boxes
    geom_segment(data = excl, aes(x = 1.1, xend = 4.7, y = y, yend = y),
                 linewidth = 0.4, colour = "grey55", linetype = "22") +
    geom_rect(data = excl,
              aes(xmin = 4.7, xmax = 10.6, ymin = y - 0.30, ymax = y + 0.30),
              fill = "grey93", colour = "grey60", linewidth = 0.4) +
    geom_text(data = excl, aes(x = 4.85, y = y, label = label),
              hjust = 0, size = 2.75, colour = "grey15", lineheight = 1.05) +
    scale_x_continuous(limits = c(-0.1, 10.8)) +
    scale_y_continuous(limits = c(0.55, 4.45)) +
    theme_void(base_size = 11) +
    theme(plot.margin = margin(8, 8, 8, 8))

  ggsave(file.path(dir_fig, "S11_denominator_flow.png"), p,
         width = 11, height = 5.2, dpi = 400, bg = "white")
  ggsave(file.path(dir_fig, "S11_denominator_flow.pdf"), p,
         width = 11, height = 5.2, bg = "white")
  message("Saved: output/figures/S11_denominator_flow.png / .pdf")

}
environment(.stage) <- new.env(parent = globalenv())
.run_stage("10_denominator_flow.R", .stage)

# ---- stage: 11_maup_county_weighted.R ----
.stage <- function() {
  # =============================================================================
  # 11_maup_county_weighted.R

  # OUTPUTS  output/tables/08_maup_county.csv   (weighted primary + unweighted)
  #          data/processed/08_maup_county.rds  (both fits; cache-compatible)
  # =============================================================================

  suppressMessages({library(dplyr); library(tidyr); library(sf); library(broom)})

  root <- getwd()
  if (!dir.exists(file.path(root, "data")) &&
      dir.exists(file.path(root, "..", "data"))) root <- normalizePath(file.path(root, ".."))
  stopifnot(dir.exists(file.path(root, "data", "processed")))
  dir_proc <- file.path(root, "data", "processed")
  dir_tbl  <- file.path(root, "output", "tables")


  maup_code_version <- "2026-08-28_county-only-groupby_popweighted-lm"

  CORE_PRED <- c("pct_age_65_plus", "pct_hispanic", "ice_income",
                 "pct_bachelor_plus", "ice_race", "ACCESS2", "CHECKUP",
                 "ruca_class", "CSMOKING", "LPA")
  ENV_PRED  <- c("pm25_annual", "o3_8hrmax_4thmax", "LILATracts_1And10",
                 "tmax_warm", "walk_index")
  CENTER    <- c("pct_age_65_plus", "pm25_annual", "o3_8hrmax_4thmax",
                 "tmax_warm", "walk_index")
  NONCONUS  <- c("02", "15", "60", "66", "69", "72", "78")
  MAUP_CORE <- c("ckm_core_pca", "pct_age_65_plus", "ice_race", "ice_income",
                 "pct_hispanic", "pct_bachelor_plus")


  an <- readRDS(file.path(dir_proc, "02_ckm_index.rds"))
  if (inherits(an, "sf")) an <- st_drop_geometry(an)
  geo <- readRDS(file.path(dir_proc, "01_tracts_geo.rds"))
  if (inherits(geo, "sf")) geo <- st_drop_geometry(geo)

  mod <- an |>
    left_join(geo[, c("GEOID", "ALAND")], by = "GEOID") |>
    filter(!substr(GEOID, 1, 2) %in% NONCONUS, ALAND > 0, !is.na(ckm_core_pca)) |>
    mutate(ruca_class = factor(ruca_class,
             levels = c("Metro", "Micropolitan", "Small town", "Rural"))) |>
    drop_na(ckm_core_pca, pct_age_65_plus, ice_race, ice_income, ruca_class,
            pct_hispanic, pct_black, pct_bachelor_plus, median_income) |>
    drop_na(all_of(c("ckm_core_pca", CORE_PRED, ENV_PRED)))

  stopifnot(nrow(mod) == 69530)
  message("tract n = ", nrow(mod))


  mod$o3_8hrmax_4thmax <- mod$o3_8hrmax_4thmax * 100
  for (v in CENTER) mod[[v]] <- mod[[v]] - mean(mod[[v]], na.rm = TRUE)
  mod$w <- ifelse(is.na(mod$total_pop) | mod$total_pop == 0, 1, mod$total_pop)


  ruca_dom <- mod |>
    mutate(county_fips = substr(GEOID, 1, 5)) |>
    group_by(county_fips, ruca_class) |>
    summarise(wsum = sum(w, na.rm = TRUE), .groups = "drop_last") |>
    slice_max(wsum, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(county_fips, ruca_class)

  county_data <- mod |>
    mutate(county_fips = substr(GEOID, 1, 5)) |>
    group_by(county_fips) |>
    summarise(across(all_of(c(MAUP_CORE, ENV_PRED)),
                     ~ stats::weighted.mean(.x, w = w, na.rm = TRUE)),
              pop      = sum(w, na.rm = TRUE),
              n_tracts = dplyr::n(),
              .groups = "drop") |>
    left_join(ruca_dom, by = "county_fips") |>
    drop_na()

  stopifnot(nrow(county_data) == n_distinct(county_data$county_fips))
  message("county n = ", nrow(county_data))

  maup_rhs <- paste(c("pct_age_65_plus", "ice_race", "ice_income", "pct_hispanic",
                      "pct_bachelor_plus", ENV_PRED,
                      paste0("pct_age_65_plus:", ENV_PRED)), collapse = " + ")
  maup_fml <- stats::as.formula(paste("ckm_core_pca ~", maup_rhs))

  ols_county_un <- lm(maup_fml, data = county_data)                  
  ols_county    <- lm(maup_fml, data = county_data, weights = pop)   

 
  pub_f <- file.path(dir_tbl, "08_maup_county.csv")
  if (file.exists(pub_f)) {
    pub <- read.csv(pub_f, stringsAsFactors = FALSE)
    if (!"county_estimate_unweighted" %in% names(pub)) {   
      got <- round(coef(ols_county_un), 4)
      chk <- data.frame(predictor = names(got), refit = unname(got)) |>
        inner_join(pub[, c("predictor", "county_estimate")], by = "predictor")
      stopifnot(nrow(chk) == length(got))
      bad <- chk[abs(chk$refit - chk$county_estimate) > 1e-4, ]
      if (nrow(bad)) {
        print(bad)
        stop("Reconstruction does not reproduce the published unweighted fit; ",
             "refusing to overwrite outputs.")
      }
      message("provenance guard PASSED: reproduced all ", nrow(chk),
              " published coefficients to 4 dp")
    } else {
      message("published CSV already carries corrected columns; guard skipped")
    }
  }

  # ---- Report ----------------------------------------------------------------
  cat("\n--- County-scale OLS (population-weighted, primary) ---\n")
  print(round(coef(summary(ols_county))[, c("Estimate", "Std. Error",
                                            "Pr(>|t|)")], 4))
  cat("\n  N counties: ", nrow(county_data), "\n")
  cat("  R² (weighted):   ", round(summary(ols_county)$r.squared, 4), "\n")
  cat("  R² (unweighted): ", round(summary(ols_county_un)$r.squared, 4), "\n")

  cat("\n--- Leverage imbalance under the unweighted fit ---\n")
  p_small <- 100 * sum(sort(county_data$pop)[1:500]) / sum(county_data$pop)
  p_large <- 100 * sum(sort(county_data$pop, decreasing = TRUE)[1:100]) /
    sum(county_data$pop)
  cat(sprintf("  smallest 500 counties: %5.2f%% of population, %4.1f%% of rows\n",
              p_small, 100 * 500 / nrow(county_data)))
  cat(sprintf("  largest  100 counties: %5.2f%% of population, %4.1f%% of rows\n",
              p_large, 100 * 100 / nrow(county_data)))

 
  ols_county_nt <- lm(maup_fml, data = county_data, weights = n_tracts)
  key <- c("LILATracts_1And10", "pm25_annual", "ice_income", "ice_race",
           "pct_age_65_plus:pm25_annual",
           "pct_age_65_plus:o3_8hrmax_4thmax",
           "pct_age_65_plus:LILATracts_1And10",
           "pct_age_65_plus:tmax_warm",
           "pct_age_65_plus:walk_index")
  cat("\n--- Key coefficients under three weightings ---\n")
  print(data.frame(
    term       = key,
    unweighted = sprintf("%+.4f (p=%.3f)", coef(ols_county_un)[key],
                         summary(ols_county_un)$coefficients[key, 4]),
    pop        = sprintf("%+.4f (p=%.3f)", coef(ols_county)[key],
                         summary(ols_county)$coefficients[key, 4]),
    n_tracts   = sprintf("%+.4f (p=%.3f)", coef(ols_county_nt)[key],
                         summary(ols_county_nt)$coefficients[key, 4])),
    row.names = FALSE)


  maup_obj <- list(model = ols_county, n = nrow(county_data),
                   summary = summary(ols_county),
                   model_unweighted   = ols_county_un,
                   summary_unweighted = summary(ols_county_un),
                   weighting = "population (w = total_pop); unweighted retained as sensitivity",
                   code_version = maup_code_version)
  saveRDS(maup_obj, file.path(dir_proc, "08_maup_county.rds"))

  maup_compare <- broom::tidy(ols_county) |>
    transmute(predictor = term,
              county_estimate = round(estimate, 4),
              county_p        = round(p.value, 4)) |>
    left_join(broom::tidy(ols_county_un) |>
                transmute(predictor = term,
                          county_estimate_unweighted = round(estimate, 4),
                          county_p_unweighted        = round(p.value, 4)),
              by = "predictor")
  write.csv(maup_compare, file.path(dir_tbl, "08_maup_county.csv"),
            row.names = FALSE)

  message("\nSaved: output/tables/08_maup_county.csv")
  message("Saved: data/processed/08_maup_county.rds")

}
environment(.stage) <- new.env(parent = globalenv())
.run_stage("11_maup_county_weighted.R", .stage)

# ---- stage: 12_bym2_posterior_residual.R ----
.stage <- function() {
  # =============================================================================
  # 12_bym2_posterior_residual.R

  # OUTPUTS
  #   data/processed/12_bym2_refit_model1.rds   full summaries incl. fitted+hyper
  #   data/processed/12_bym2_refit_model2.rds
  #   output/tables/12_bym2_posterior_residual.csv     manuscript-facing numbers
  #   output/tables/12_bym2_refit_validation.csv       refit vs published
  #   output/logs/12_bym2_posterior_residual.log       captured console output
  #

  # =============================================================================
  suppressMessages({
    library(dplyr); library(tidyr); library(sf); library(spdep)
  })
  stopifnot(requireNamespace("INLA", quietly = TRUE))


  root <- getwd()
  if (!dir.exists(file.path(root, "data")) &&
      dir.exists(file.path(root, "..", "data"))) root <- normalizePath(file.path(root, ".."))
  stopifnot(dir.exists(file.path(root, "data", "processed")))
  dir_proc <- file.path(root, "data", "processed")
  dir_tab  <- file.path(root, "output", "tables")
  dir_log  <- file.path(root, "output", "logs")
  dir.create(dir_log, recursive = TRUE, showWarnings = FALSE)
  stopifnot(dir.exists(dir_proc), dir.exists(dir_tab))

  CORE_PRED <- c("pct_age_65_plus", "pct_hispanic", "ice_income",
                 "pct_bachelor_plus", "ice_race", "ACCESS2", "CHECKUP",
                 "ruca_class", "CSMOKING", "LPA")
  ENV_PRED  <- c("pm25_annual", "o3_8hrmax_4thmax", "LILATracts_1And10",
                 "tmax_warm", "walk_index")
  CENTER    <- c("pct_age_65_plus", "pm25_annual", "o3_8hrmax_4thmax",
                 "tmax_warm", "walk_index")
  BEHAV     <- c("ACCESS2", "CHECKUP", "CSMOKING", "LPA")

  # ---------------------------------------------------------------------------
  # 1. Rebuild the estimation sample 
  # ---------------------------------------------------------------------------
  an <- readRDS(file.path(dir_proc, "02_ckm_index.rds"))
  if (inherits(an, "sf")) an <- st_drop_geometry(an)
  geo <- readRDS(file.path(dir_proc, "01_tracts_geo.rds"))
  d   <- an |> left_join(st_drop_geometry(geo)[, c("GEOID", "ALAND")], by = "GEOID")

  conus <- d |>
    filter(!substr(GEOID, 1, 2) %in% c("02", "15", "60", "66", "69", "72", "78")) |>
    filter(ALAND > 0, !is.na(ckm_core_pca))
  message(sprintf("CONUS tracts with index: %d", nrow(conus)))

  mod <- conus |>
    mutate(ruca_class = factor(ruca_class,
             levels = c("Metro", "Micropolitan", "Small town", "Rural"))) |>
    drop_na(ckm_core_pca, pct_age_65_plus, ice_race, ice_income, ruca_class,
            pct_hispanic, pct_black, pct_bachelor_plus, median_income) |>
    drop_na(all_of(c("ckm_core_pca", CORE_PRED, ENV_PRED)))

  prim <- readRDS(file.path(dir_proc, "06_inla_bym2.rds"))
  m1p  <- readRDS(file.path(dir_proc, "06_6g_inla_bym2_nobehav.rds"))
  stopifnot(nrow(mod) == prim$n, nrow(mod) == m1p$n)
  message(sprintf("Estimation sample: n = %d", nrow(mod)))

  mod$o3_8hrmax_4thmax <- mod$o3_8hrmax_4thmax * 100     
  for (v in CENTER) mod[[v]] <- mod[[v]] - mean(mod[[v]], na.rm = TRUE)

  # ---------------------------------------------------------------------------
  # 2. Neighbour graph on this sample's ordering
  # ---------------------------------------------------------------------------
  mod_geo <- geo[match(mod$GEOID, geo$GEOID), ]
  stopifnot(identical(mod_geo$GEOID, mod$GEOID))
  coords <- suppressWarnings(
    st_coordinates(st_centroid(mod_geo, of_largest_polygon = TRUE)))
  nb <- knn2nb(knearneigh(coords, k = 8), sym = TRUE)
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  graph_file <- file.path(dir_proc, "12_bym2_graph.adj")

  nb2INLA_fast <- function(file, nb) {
    crd <- spdep::card(nb)
    writeLines(c(as.character(length(nb)),
      vapply(seq_along(nb), function(i)
        if (crd[i] == 0L) paste(i, 0)
        else paste(c(i, crd[i], nb[[i]]), collapse = " "),
        character(1))), file)
  }
  nb2INLA_fast(graph_file, nb)
  mod$idarea <- seq_len(nrow(mod))
  message(sprintf("Neighbour graph: %d nodes, mean %.2f neighbours",
                  length(nb), mean(lengths(nb))))

  y <- mod$ckm_core_pca
  message(sprintf("Outcome: sd = %.4f, mean = %.4f", sd(y), mean(y)))

  # ---------------------------------------------------------------------------
  # 3. Fit
  # ---------------------------------------------------------------------------
 
  fit_one <- function(label, preds) {
    rhs <- paste(c(preds, ENV_PRED, paste0("pct_age_65_plus:", ENV_PRED)),
                 collapse = " + ")
    fml <- as.formula(paste0(
      "ckm_core_pca ~ ", rhs,
      " + f(idarea, model = 'bym2', graph = '", graph_file, "',",
      " scale.model = TRUE,",
      " hyper = list(phi  = list(prior = 'pc', param = c(0.5, 0.5)),",
      "              prec = list(prior = 'pc.prec', param = c(1, 0.01))))"))
    message(sprintf("\nFitting %s (n = %d)...", label, nrow(mod)))
    t0 <- Sys.time()
    f <- INLA::inla(
      fml, data = mod, family = "gaussian",
      control.predictor = list(compute = TRUE),
      control.compute   = list(dic = TRUE, waic = TRUE,
                               return.marginals.predictor = FALSE),
      control.inla      = list(strategy = "adaptive", int.strategy = "eb"),
      verbose = FALSE)
    message(sprintf("  done in %.1f min",
                    as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    f
  }

  # ---------------------------------------------------------------------------
  # 4. Validate the refit against the published fit 
  # ---------------------------------------------------------------------------
  TOL <- 1e-3   
  validate <- function(label, fit, published) {
    pf <- published$summary_fixed
    rf <- fit$summary.fixed
    stopifnot(setequal(rownames(pf), rownames(rf)))
    sh <- rownames(pf)
    v <- data.frame(model = label, term = sh,
                    published = pf[sh, "mean"], refit = rf[sh, "mean"],
                    abs_diff = rf[sh, "mean"] - pf[sh, "mean"],
                    row.names = NULL)
    mx <- max(abs(v$abs_diff))
    message(sprintf("--- %s: refit vs published, max |diff| = %.3g, r = %.6f ---",
                    label, mx, cor(v$published, v$refit)))
    print(v[order(-abs(v$abs_diff)), ], digits = 5, row.names = FALSE)
    if (mx > TOL)
      stop(sprintf(paste("%s refit does NOT reproduce the published fit",
                         "(max |diff| = %.4g > %.4g). Refusing to report",
                         "derived quantities."), label, mx, TOL), call. = FALSE)
    v
  }

  # ---------------------------------------------------------------------------
  # 5. Derived quantities,
  # ---------------------------------------------------------------------------
  hp_row <- function(fit, pattern) {
    h <- fit$summary.hyperpar
    i <- grep(pattern, rownames(h))
    stopifnot(length(i) == 1)
    h[i, ]
  }

 
  sigma_from_tau <- function(fit, pattern) {
    nm <- grep(pattern, names(fit$marginals.hyperpar), value = TRUE)
    stopifnot(length(nm) == 1)
    m <- fit$marginals.hyperpar[[nm]]
    s <- INLA::inla.tmarginal(function(x) 1 / sqrt(x), m)
    c(mean = INLA::inla.emarginal(function(x) x, s),
      lo   = INLA::inla.qmarginal(0.025, s),
      hi   = INLA::inla.qmarginal(0.975, s))
  }

  summarise_fit <- function(label, fit, preds) {
    n <- nrow(mod)
    fv <- fit$summary.fitted.values
    stopifnot(nrow(fv) >= n)
    eta <- fv$mean[seq_len(n)]          

  
    rhs <- paste(c(preds, ENV_PRED, paste0("pct_age_65_plus:", ENV_PRED)),
                 collapse = " + ")
    X <- model.matrix(as.formula(paste("~", rhs)), data = mod)
    b <- fit$summary.fixed
    stopifnot(setequal(colnames(X), rownames(b)))
    Xb <- as.vector(X[, rownames(b), drop = FALSE] %*% b$mean)

    r_post <- y - eta
    r_fix  <- y - Xb

    
    if (sd(r_post) > sd(r_fix))
      stop(sprintf("%s: posterior residual SD (%.4f) exceeds fixed-only (%.4f)",
                   label, sd(r_post), sd(r_fix)), call. = FALSE)

    mi_post <- moran.test(r_post, lw, zero.policy = TRUE)
    mi_fix  <- moran.test(r_fix,  lw, zero.policy = TRUE)
    mi_y    <- moran.test(y,      lw, zero.policy = TRUE)

    phi  <- hp_row(fit, "^Phi for idarea$")
    sg   <- sigma_from_tau(fit, "Precision for the Gaussian observations")
    sfld <- sigma_from_tau(fit, "^Precision for idarea$")

    cat(sprintf("\n=== %s ===\n", label))
    cat(sprintf("  Moran's I, outcome                    : %.4f\n", mi_y$estimate[[1]]))
    cat(sprintf("  Moran's I, fixed-effects-only residual: %.4f (p = %.3g)\n",
                mi_fix$estimate[[1]], mi_fix$p.value))
    cat(sprintf("  Moran's I, POSTERIOR residual         : %.4f (p = %.3g)\n",
                mi_post$estimate[[1]], mi_post$p.value))
    cat(sprintf("  reduction, fixed-only -> posterior    : %.1f%%\n",
                100 * (1 - mi_post$estimate[[1]] / mi_fix$estimate[[1]])))
    cat(sprintf("  residual SD, empirical (y - eta)      : %.4f  [outcome SD %.4f]\n",
                sd(r_post), sd(y)))
    cat(sprintf("  residual SD, posterior sigma          : %.4f [%.4f, %.4f]\n",
                sg[["mean"]], sg[["lo"]], sg[["hi"]]))
    cat(sprintf("  phi (spatial share of random effect)  : %.4f [%.4f, %.4f]\n",
                phi[["mean"]], phi[["0.025quant"]], phi[["0.975quant"]]))
    cat(sprintf("  marginal SD of the random effect      : %.4f [%.4f, %.4f]\n",
                sfld[["mean"]], sfld[["lo"]], sfld[["hi"]]))
    cat(sprintf("  var(eta)/var(y)                       : %.4f\n", var(eta) / var(y)))
    cat(sprintf("  DIC = %.1f   WAIC = %.1f\n", fit$dic$dic, fit$waic$waic))

    data.frame(
      model                  = label,
      n                      = n,
      moran_outcome          = mi_y$estimate[[1]],
      moran_resid_fixedonly  = mi_fix$estimate[[1]],
      moran_resid_posterior  = mi_post$estimate[[1]],
      moran_resid_post_p     = mi_post$p.value,
      resid_sd_empirical     = sd(r_post),
      resid_sd_posterior     = sg[["mean"]],
      resid_sd_lo            = sg[["lo"]],
      resid_sd_hi            = sg[["hi"]],
      phi                    = phi[["mean"]],
      phi_lo                 = phi[["0.025quant"]],
      phi_hi                 = phi[["0.975quant"]],
      field_sd               = sfld[["mean"]],
      field_sd_lo            = sfld[["lo"]],
      field_sd_hi            = sfld[["hi"]],
      outcome_sd             = sd(y),
      dic                    = fit$dic$dic,
      waic                   = fit$waic$waic,
      row.names = NULL)
  }

  save_fit <- function(fit, path, extra = list()) {
    saveRDS(c(list(
      summary_fixed     = fit$summary.fixed,
      summary_random    = fit$summary.random$idarea,
      summary_hyperpar  = fit$summary.hyperpar,
      summary_fitted    = fit$summary.fitted.values,
      dic  = fit$dic$dic, waic = fit$waic$waic,
      n    = nrow(mod),
      GEOID = mod$GEOID,
      formula = paste(deparse(fit$.args$formula), collapse = " ")), extra), path)
    message("  saved: ", path)
  }

  # ---- Model 2 (fully adjusted, behavioural block included) ------------------
  f2 <- fit_one("Model 2 (fully adjusted)", CORE_PRED)
  v2 <- validate("Model 2", f2, prim)
  s2 <- summarise_fit("Model 2 (fully adjusted, with behavioural block)",
                      f2, CORE_PRED)
  save_fit(f2, file.path(dir_proc, "12_bym2_refit_model2.rds"))

  # ---- Model 1 (primary, behaviour-excluded) ---------------------------------
  f1 <- fit_one("Model 1 (behaviour-excluded)", setdiff(CORE_PRED, BEHAV))
  v1 <- validate("Model 1", f1, m1p)
  s1 <- summarise_fit("Model 1 (primary, behaviour-excluded)",
                      f1, setdiff(CORE_PRED, BEHAV))
  save_fit(f1, file.path(dir_proc, "12_bym2_refit_model1.rds"),
           list(dropped = BEHAV))

  # ---------------------------------------------------------------------------
  # 6. outputs
  # ---------------------------------------------------------------------------
  out <- rbind(s1, s2)
  write.csv(out, file.path(dir_tab, "12_bym2_posterior_residual.csv"),
            row.names = FALSE)
  write.csv(rbind(v1, v2), file.path(dir_tab, "12_bym2_refit_validation.csv"),
            row.names = FALSE)
  cat("\n--- written ---\n")
  cat("  ", file.path(dir_tab, "12_bym2_posterior_residual.csv"), "\n")
  cat("  ", file.path(dir_tab, "12_bym2_refit_validation.csv"), "\n")
  print(out, digits = 4, row.names = FALSE)

  
  cat("\nAlready archived elsewhere, for the §3.3 sentence:\n")
  cat("  OLS baseline residual Moran's I = 0.470\n")
  cat("  ESF residual Moran's I          = 0.448\n")
  cat("\ndone\n")

}
environment(.stage) <- new.env(parent = globalenv())
.run_stage("12_bym2_posterior_residual.R", .stage)

# ---- stage: 13_figure6_relabel.R ----
.stage <- function() {
  


  suppressPackageStartupMessages({
    library(sf); library(dplyr); library(ggplot2); library(stringr)
    library(tigris); library(patchwork); library(scales)
  })
  options(tigris_use_cache = TRUE)
  sf::sf_use_s2(FALSE)


  ROOT <- getwd()
  if (!dir.exists(file.path(ROOT, "data")) &&
      dir.exists(file.path(ROOT, "..", "data"))) ROOT <- normalizePath(file.path(ROOT, ".."))
  stopifnot(dir.exists(file.path(ROOT, "data", "processed")))
  dir_proc <- file.path(ROOT, "data/processed")
  dir_fig  <- file.path(ROOT, "output/figures")
  OUT      <- file.path(dir_fig, "06h_bym2_spatial_random_effect_2panel.png")

  TIGRIS_YEAR     <- 2019
  NJ_UNAVAIL_FILL <- "grey60"
  LEGEND          <- "Spatial\nrandom effect\n(PC1 score units)"


  TARGET_W <- 2860L
  TARGET_H <- 1232L

  stopifnot(!grepl("SD units", LEGEND))

  # ---- 1. cached published fits ------------------------------------------------
  f_adj <- file.path(dir_proc, "06_inla_bym2.rds")            # Model 2
  f_exc <- file.path(dir_proc, "06_6g_inla_bym2_nobehav.rds") # Model 1
  stopifnot(file.exists(f_adj), file.exists(f_exc))
  bym2_save    <- readRDS(f_adj)
  bym2_nb_save <- readRDS(f_exc)

  # ---- 2. GEOID vector in the CACHED fits' node order --------------------------
 
  # 3.3 Southern mean from +0.297 to +0.167; the checks in step 3 catch that.
  r1 <- readRDS(file.path(dir_proc, "12_bym2_refit_model1.rds"))
  r2 <- readRDS(file.path(dir_proc, "12_bym2_refit_model2.rds"))
  stopifnot(identical(r1$GEOID, r2$GEOID))

  tracts_geo <- readRDS(file.path(dir_proc, "01_tracts_geo.rds"))
  tg <- str_trim(enc2utf8(as.character(st_drop_geometry(tracts_geo)$GEOID)))
  stopifnot(!anyDuplicated(tg))
  GEO <- tg[tg %in% r1$GEOID]
  N   <- length(GEO)
  stopifnot(N == 69530, !anyDuplicated(GEO), setequal(GEO, r1$GEOID))
  
  stopifnot(!identical(GEO, r1$GEOID))

 
  re_mean <- function(sv) {
    rd <- as.data.frame(sv$summary_random)
    stopifnot(nrow(rd) == 2 * N)
    head(rd$mean, N)
  }
  m_exc <- re_mean(bym2_nb_save)   # Model 1
  m_adj <- re_mean(bym2_save)      # Model 2

  # ---- 3. PROVE the pairing against the numbers Section 3.3 quotes -------------
  SOUTH <- c("01","05","10","11","12","13","21","22","24",
             "28","37","40","45","47","48","51","54")
  chk <- function(mv, label, exp_s, exp_n, exp_p) {
    iss <- substr(GEO, 1, 2) %in% SOUTH
    s <- round(mean(mv[iss]), 3); n <- round(mean(mv[!iss]), 3)
    p <- round(100 * mean(mv[iss] > 0), 1)
    cat(sprintf("%-8s South %+.3f (pub %+.3f) | non-South %+.3f (pub %+.3f) | %%pos %.1f (pub %.1f)\n",
                label, s, exp_s, n, exp_n, p, exp_p))
    stopifnot(s == exp_s, n == exp_n, p == exp_p)
  }
  chk(m_exc, "Model 1", 0.297, -0.176, 63.8)
  chk(m_adj, "Model 2", -0.124, 0.073, 36.7)

 
  st <- tapply(m_exc, substr(GEO, 1, 2), mean)
  top6 <- head(sort(st, decreasing = TRUE), 6)
  cat("top 6 state means (Model 1):",
      paste(sprintf("%s %+.2f", names(top6), top6), collapse = "  "), "\n")
  stopifnot(identical(names(top6), c("54","21","01","40","47","05")),
            all(round(unname(top6), 2) == c(1.54, 1.26, 0.89, 0.86, 0.77, 0.74)),
            all(names(top6) %in% SOUTH))
  cat("PAIRING VERIFIED: cached fits + refit GEOID reproduce Section 3.3 exactly\n\n")

  # ---- 4. geometry -------------------------------------------------------------
  .re_geo_of <- function(mv) {
    rd <- data.frame(GEOID = GEO, mean = mv, stringsAsFactors = FALSE)
    tracts_geo |>
      mutate(GEOID = str_trim(enc2utf8(as.character(GEOID)))) |>
      inner_join(rd |> mutate(GEOID = str_trim(enc2utf8(GEOID))), by = "GEOID") |>
      filter(!substr(GEOID, 1, 2) %in% c("60","66","69","72","78"), ALAND > 0) |>
      st_transform(4326) |>
      tigris::shift_geometry(geoid_column = "GEOID",
                             preserve_area = FALSE, position = "below")
  }
  re_exc <- .re_geo_of(m_exc)
  re_adj <- .re_geo_of(m_adj)
  cat(sprintf("joined tracts: Model 1 %d, Model 2 %d (of %d)\n",
              nrow(re_exc), nrow(re_adj), N))
  stopifnot(nrow(re_exc) == nrow(re_adj), nrow(re_exc) > 0.99 * N)

  nj_unavail <- tracts_geo |>
    mutate(GEOID = str_trim(enc2utf8(as.character(GEOID)))) |>
    filter(substr(GEOID, 1, 2) == "34", ALAND > 0) |>
    st_transform(4326) |>
    tigris::shift_geometry(geoid_column = "GEOID",
                           preserve_area = FALSE, position = "below")

  states_conus <- tigris::states(cb = TRUE, year = TIGRIS_YEAR,
                                 progress_bar = FALSE) |>
    filter(!STATEFP %in% c("60","66","69","72","78")) |>
    tigris::shift_geometry(preserve_area = FALSE, position = "below") |>
    filter(!STATEFP %in% c("02","15"))

  # ---- 5. draw (verbatim from CKM_US_Main.R 6.7b, legend string excepted) ------
  re_lim2 <- max(abs(quantile(re_adj$mean, c(0.02, 0.98), na.rm = TRUE)),
                 abs(quantile(re_exc$mean, c(0.02, 0.98), na.rm = TRUE)))
  cat(sprintf("colour limit re_lim2 = %.6f\n", re_lim2))

  .bym2_panel <- function(d, tag, ttl, sub) {
    ggplot() +
      geom_sf(data = d, aes(fill = mean), colour = NA) +
      geom_sf(data = nj_unavail, fill = NJ_UNAVAIL_FILL, colour = NA) +
      geom_sf(data = states_conus, fill = NA, colour = "grey25", linewidth = 0.25) +
      scale_fill_gradient2(
        low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
        limits = c(-re_lim2, re_lim2), oob = scales::squish,
        name = LEGEND) +
      coord_sf(datum = NA) +
      theme_void(base_size = 19) +
      theme(plot.title    = element_text(face = "bold", size = 19),
            plot.subtitle = element_text(colour = "grey30", size = 14),
            plot.tag      = element_text(face = "bold", size = 22),
            legend.title  = element_text(size = 17),
            legend.text   = element_text(size = 15)) +
      labs(tag = tag, title = ttl, subtitle = sub)
  }

  p_fig6 <- patchwork::wrap_plots(
    .bym2_panel(re_exc, "A", "Model 1 - behavior-excluded",
                "Behavioral/healthcare-engagement block removed"),
    .bym2_panel(re_adj, "B", "Model 2 - behavior-adjusted",
                "Behavioral/healthcare-engagement block included"),
    nrow = 1, guides = "collect")

  ggsave(OUT, p_fig6, width = 13, height = 5.6, dpi = 220, bg = "white")

  # ---- 6. the PNG must drop into the existing <wp:extent> unchanged ------------
  d <- readBin(OUT, "raw", 40)
  stopifnot(identical(as.integer(d[1:8]), c(137L,80L,78L,71L,13L,10L,26L,10L)))
  rdint <- function(b) sum(as.integer(b) * 256^(3:0))
  w <- rdint(d[17:20]); h <- rdint(d[21:24])
  cat(sprintf("wrote %s : %d x %d px\n", basename(OUT), w, h))
  stopifnot(w == TARGET_W, h == TARGET_H)
  cat("Figure 6 re-rendered with corrected legend; pixel dimensions unchanged.\n")

}
environment(.stage) <- new.env(parent = globalenv())
.run_stage("13_figure6_relabel.R", .stage)

# ---- stage: 14_figure8_rerender.R ----
.stage <- function() {
  # =============================================================================
  # 14_figure8_rerender.R 


  suppressPackageStartupMessages({
    library(sf); library(dplyr); library(tidyr); library(tibble)
    library(purrr); library(ggplot2); library(patchwork); library(readr)
  })


  ROOT <- getwd()
  if (!dir.exists(file.path(ROOT, "data")) &&
      dir.exists(file.path(ROOT, "..", "data"))) ROOT <- normalizePath(file.path(ROOT, ".."))
  stopifnot(dir.exists(file.path(ROOT, "data", "processed")))
  dir_proc <- file.path(ROOT, "data/processed")
  dir_fig  <- file.path(ROOT, "output/figures")
  dir_tbl  <- file.path(ROOT, "output/tables")

  F_PNG  <- file.path(dir_fig, "07_age_env_interactions.png")
  F_CSV  <- file.path(dir_tbl, "07_age_env_interaction_loess.csv")
  WRITE  <- "--write" %in% commandArgs(trailingOnly = TRUE)

  ENV_PRED <- c("pm25_annual", "o3_8hrmax_4thmax", "LILATracts_1And10",
                "tmax_warm", "walk_index")


  predictor_pretty <- c(
    pct_age_65_plus     = "% adults age 65+",
    pct_bachelor_plus   = "% adults with bachelor's+",
    pct_hispanic        = "% Hispanic",
    ice_income          = "ICE Income",
    ice_race            = "ICE Race",
    pm25_annual         = "Annual mean PM₂.₅ (µg/m³)",
    o3_8hrmax_4thmax    = "Annual 4th-highest 8-hr O₃ (per 10 ppb)",
    tmax_warm           = "Warm-season (May–Sep) max temp (°C)",
    walk_index          = "EPA National Walkability Index (1–20)",
    LILATracts_1And10   = "Low-income low-access tract (1-mi/10-mi)")


  fig8_pretty <- c(predictor_pretty,
                   o3_8hrmax_4thmax = "Annual 4th-highest 8-hr O₃ (ppb)")
  fig8_pretty <- fig8_pretty[!duplicated(names(fig8_pretty), fromLast = TRUE)]
  stopifnot(fig8_pretty[["o3_8hrmax_4thmax"]] ==
              "Annual 4th-highest 8-hr O₃ (ppb)")
  .label_fig8 <- function(x) unname(fig8_pretty[as.character(x)])

  theme_ckm <- theme_bw(base_size = 11) +
    theme(strip.background = element_rect(fill = "grey92"),
          panel.grid.minor = element_blank(),
          legend.position  = "bottom")

  # ---------------------------------------------------------------------------
  # 1. The cached TreeSHAP matrix -- the published one, not a recomputation
  # ---------------------------------------------------------------------------
  shap_obj <- readRDS(file.path(dir_proc, "07_shap.rds"))
  shap_vals <- shap_obj$shap
  stopifnot(identical(shap_obj$predictors, colnames(shap_vals)),
            nrow(shap_vals) == 69530L, ncol(shap_vals) == 18L,
            "pct_age_65_plus" %in% colnames(shap_vals))
  shap_age <- shap_vals[, "pct_age_65_plus"]


  g <- read_csv(file.path(dir_tbl, "07_shap_global.csv"), show_col_types = FALSE)
  ma <- apply(abs(shap_vals), 2, mean)
  stopifnot(nrow(g) == ncol(shap_vals))
  stopifnot(max(abs(ma[g$feature] - g$mean_abs)) < 1e-12)
  message(sprintf("SHAP cache verified against Table S7 (max |diff| %.2e)",
                  max(abs(ma[g$feature] - g$mean_abs))))

  # ---------------------------------------------------------------------------
  # 2. Rebuild the estimation frame IN THE ORDER THE SHAP ROWS ASSUME
  # ---------------------------------------------------------------------------
  an <- readRDS(file.path(dir_proc, "02_ckm_index.rds"))
  if (inherits(an, "sf")) an <- sf::st_drop_geometry(an)
  geo <- readRDS(file.path(dir_proc, "01_tracts_geo.rds"))
  gdf <- sf::st_drop_geometry(geo)[, c("GEOID", "ALAND")]

  conus <- gdf |>
    dplyr::inner_join(an, by = "GEOID") |>
    dplyr::filter(!substr(GEOID, 1, 2) %in%
                    c("02", "15", "60", "66", "69", "72", "78")) |>
    dplyr::filter(ALAND > 0, !is.na(ckm_core_pca))


  CORE <- intersect(c("pct_age_65_plus", "pct_hispanic", "ice_income",
                      "pct_unemployed", "pct_bachelor_plus", "ice_race",
                      "pct_living_alone", "ACCESS2", "CHECKUP", "ruca_class",
                      "pct_no_vehicle", "CSMOKING", "LPA"), names(an))
  ENV <- intersect(ENV_PRED, names(an))

  mod <- conus |>
    dplyr::mutate(ruca_class = factor(ruca_class,
                    levels = c("Metro", "Micropolitan", "Small town", "Rural"))) |>

    tidyr::drop_na(ckm_core_pca, pct_age_65_plus, ice_race, ice_income,
                   ruca_class, pct_hispanic, pct_black, pct_bachelor_plus,
                   median_income) |>
    tidyr::drop_na(dplyr::all_of(c("ckm_core_pca", CORE, ENV)))

  X_full <- model.matrix(
    stats::as.formula(paste("~", paste(c(CORE, ENV), collapse = " + "), "- 1")),
    data = mod)
  stopifnot(nrow(X_full) == nrow(shap_vals),
            identical(colnames(X_full), colnames(shap_vals)))
  message(sprintf("Estimation frame rebuilt: n = %d, %d features, column order identical",
                  nrow(X_full), ncol(X_full)))

  # ---------------------------------------------------------------------------
  # 3. Native measurement scale for the axes
  # ---------------------------------------------------------------------------
  to_native <- function(v, x) if (v == "o3_8hrmax_4thmax") x * 1000 else x

  interaction_df <- purrr::map_dfr(ENV, function(v) {
    vals <- mod[[v]]
    tibble::tibble(env_var   = v,
                   env_value = to_native(v, vals),
                   shap_age  = shap_age,
                   age_value = mod$pct_age_65_plus,
                   is_binary = length(unique(vals[!is.na(vals)])) <= 2)
  })
  cont_df <- dplyr::filter(interaction_df, !is_binary)
  bin_df  <- dplyr::filter(interaction_df,  is_binary)
  stopifnot(nrow(bin_df) > 0,
            identical(sort(unique(bin_df$env_var)), "LILATracts_1And10"))

  o3r <- range(cont_df$env_value[cont_df$env_var == "o3_8hrmax_4thmax"])
  stopifnot(o3r[1] > 20, o3r[2] < 200)

  # ---------------------------------------------------------------------------
  # 4. THE GATE: every drawn summary must reproduce the archived companion data
  # ---------------------------------------------------------------------------
  arch <- read_csv(F_CSV, show_col_types = FALSE)
  worst <- 0
  for (v in unique(cont_df$env_var)) {
    d <- cont_df |> dplyr::filter(env_var == v, is.finite(env_value),
                                  is.finite(shap_age))
    fit <- stats::loess(shap_age ~ env_value, data = d, span = 0.5)
    gx  <- seq(min(d$env_value), max(d$env_value), length.out = 25)
    ls  <- as.numeric(stats::predict(fit, newdata = data.frame(env_value = gx)))
    a   <- arch |> dplyr::filter(env_var == v) |> dplyr::arrange(x)
    stopifnot(nrow(a) == 25L, a$n_panel[1] == nrow(d))
    dx <- max(abs(gx - a$x)); dl <- max(abs(ls - a$loess_shap))
    worst <- max(worst, dx, dl)
    message(sprintf("  %-18s n=%5d  max|dx|=%.2e  max|d loess|=%.2e",
                    v, nrow(d), dx, dl))
    stopifnot(dx < 1e-8, dl < 1e-8)
  }
  ab <- arch |> dplyr::filter(panel == "binary")
  db <- bin_df |> dplyr::filter(is.finite(shap_age)) |>
    dplyr::group_by(env_value) |>
    dplyr::summarise(n = dplyr::n(), med = stats::median(shap_age),
                     q25 = stats::quantile(shap_age, .25),
                     q75 = stats::quantile(shap_age, .75), .groups = "drop")
  for (i in seq_len(nrow(ab))) {
    r <- db |> dplyr::filter(env_value == ifelse(ab$level[i] == "LILA", 1, 0))
    stopifnot(nrow(r) == 1L, r$n == ab$n_panel[i])
    dd <- max(abs(c(r$med - ab$median_shap[i], r$q25 - ab$q25_shap[i],
                    r$q75 - ab$q75_shap[i])))
    worst <- max(worst, dd)
    message(sprintf("  %-18s n=%5d  max|d box|=%.2e", ab$level[i], r$n, dd))
    stopifnot(dd < 1e-8)
  }
  message(sprintf("GATE PASSED: figure reproduces its archived companion data to %.2e",
                  worst))

  # ---------------------------------------------------------------------------
  # 5. Draw, with the corrected title, subtitle and ozone label
  # ---------------------------------------------------------------------------
  NEW_TITLE <- "Dependence of the aging SHAP attribution on environmental predictors"
  NEW_SUB <- paste0(
    "Descriptive dependence of the aging SHAP contribution on each exposure.\n",
    "A non-flat trend is consistent with an interaction but does not isolate one:\n",
    "it can also arise from predictor correlation or main-effect nonlinearity.")
  
  stopifnot(!grepl("⇒", NEW_SUB), !grepl("=>", NEW_SUB, fixed = TRUE),
            grepl("does not isolate one", NEW_SUB, fixed = TRUE))

  for (ln in strsplit(NEW_SUB, "\n", fixed = TRUE)[[1]])
    stopifnot(nchar(ln) <= 90)

  p_cont <- ggplot(cont_df, aes(x = env_value, y = shap_age, colour = age_value)) +
    geom_point(alpha = 0.2, size = 0.4) +
    geom_smooth(colour = "black", se = FALSE, method = "loess",
                formula = y ~ x, span = 0.5) +
    facet_wrap(~ env_var, scales = "free_x", ncol = 2,
               labeller = ggplot2::as_labeller(.label_fig8)) +
    scale_colour_viridis_c(name = "% age 65+") +
    labs(x = "Environmental predictor value (original measurement scale)",
         y = "SHAP value for % adults age 65+",
         subtitle = "Continuous exposures — loess trend") +
    theme_ckm

  p_bin <- ggplot(bin_df,
                  aes(x = factor(env_value, levels = c("0", "1"),
                                 labels = c("Not LILA", "LILA")),
                      y = shap_age, fill = factor(env_value))) +
    geom_violin(alpha = 0.4, scale = "width", colour = NA) +
    geom_boxplot(width = 0.15, outlier.alpha = 0.05, fill = "white") +
    facet_wrap(~ env_var, ncol = 2,
               labeller = ggplot2::as_labeller(.label_fig8)) +
    scale_fill_manual(values = c("0" = "#2166AC", "1" = "#B2182B"),
                      guide = "none") +
    labs(x = "Tract food-access status",
         y = "SHAP value for % adults age 65+",
         subtitle = "Binary exposure — violin + boxplot") +
    theme_ckm

  p_interact <- p_cont / p_bin +
    patchwork::plot_layout(heights = c(2, 1)) +
    patchwork::plot_annotation(title = NEW_TITLE, subtitle = NEW_SUB)

  # ---------------------------------------------------------------------------
  # 6. Write. Dimensions are pinned
  # ---------------------------------------------------------------------------
  if (!WRITE) {
    message("\nDRY RUN -- nothing written. Re-run with --write.")
    return(invisible(NULL))
  }

  old_png <- readBin(F_PNG, "raw", file.info(F_PNG)$size)
  bak <- file.path(dir_fig, sprintf("07_age_env_interactions.bak_item1_%s.png",
                                    format(Sys.time(), "%Y%m%d_%H%M%S")))
  writeBin(old_png, bak)

  ggsave(F_PNG, p_interact, width = 9, height = 8, dpi = 220, bg = "white")

  d <- png::readPNG(F_PNG)
  stopifnot(dim(d)[2] == 1980L, dim(d)[1] == 1760L)


  ink <- d[, , 1] < 0.5
  margin_hits <- which(apply(ink[, (dim(d)[2] - 11):dim(d)[2], drop = FALSE], 1, any))
  if (length(margin_hits))
    stop(sprintf("subtitle/label clipped at right edge: ink in final 12 px on %d row(s), first at y=%d",
                 length(margin_hits), margin_hits[1]))
  message(sprintf("right-margin check clear (rightmost ink at x=%d of %d)",
                  max(which(apply(ink, 2, any))), dim(d)[2]))
  message(sprintf("\nwrote %s (%d x %d px)", basename(F_PNG), dim(d)[2], dim(d)[1]))
  message(sprintf("backup %s", basename(bak)))

 
  arch2 <- arch |>
    dplyr::mutate(label = unname(fig8_pretty[env_var]))
  stopifnot(nrow(arch2) == nrow(arch),
            identical(arch2$x, arch$x),
            identical(arch2$loess_shap, arch$loess_shap),
            identical(arch2$median_shap, arch$median_shap),
            all(arch2$label[arch2$env_var == "o3_8hrmax_4thmax"] ==
                  "Annual 4th-highest 8-hr O₃ (ppb)"))
  readr::write_csv(arch2, F_CSV)
  message(sprintf("relabelled %s (values byte-identical, ozone label corrected)",
                  basename(F_CSV)))

}
environment(.stage) <- new.env(parent = globalenv())
.run_stage("14_figure8_rerender.R", .stage)

# ---- stage: 15_verify_b70_moran.R ----
.stage <- function() {
 
  suppressPackageStartupMessages({
    library(sf); library(dplyr); library(tidyr); library(spdep)
  })
  sf::sf_use_s2(FALSE)

  ROOT <- getwd()
  if (!dir.exists(file.path(ROOT, "data")) &&
      dir.exists(file.path(ROOT, "..", "data"))) ROOT <- normalizePath(file.path(ROOT, ".."))
  dir_proc <- file.path(ROOT, "data/processed")

  CORE_PRED <- c("pct_age_65_plus", "pct_hispanic", "ice_income",
                 "pct_bachelor_plus", "ice_race", "ACCESS2", "CHECKUP",
                 "ruca_class", "CSMOKING", "LPA")
  ENV_PRED  <- c("pm25_annual", "o3_8hrmax_4thmax", "LILATracts_1And10",
                 "tmax_warm", "walk_index")
  CENTER    <- c("pct_age_65_plus", "pm25_annual", "o3_8hrmax_4thmax",
                 "tmax_warm", "walk_index")
  BEHAV     <- c("ACCESS2", "CHECKUP", "CSMOKING", "LPA")

  an <- readRDS(file.path(dir_proc, "02_ckm_index.rds"))
  if (inherits(an, "sf")) an <- st_drop_geometry(an)
  geo <- readRDS(file.path(dir_proc, "01_tracts_geo.rds"))
  d   <- an |> left_join(st_drop_geometry(geo)[, c("GEOID", "ALAND")], by = "GEOID")

  conus <- d |>
    filter(!substr(GEOID, 1, 2) %in% c("02", "15", "60", "66", "69", "72", "78")) |>
    filter(ALAND > 0, !is.na(ckm_core_pca))

  mod <- conus |>
    mutate(ruca_class = factor(ruca_class,
             levels = c("Metro", "Micropolitan", "Small town", "Rural"))) |>
    drop_na(ckm_core_pca, pct_age_65_plus, ice_race, ice_income, ruca_class,
            pct_hispanic, pct_black, pct_bachelor_plus, median_income) |>
    drop_na(all_of(c("ckm_core_pca", CORE_PRED, ENV_PRED)))

  mod$o3_8hrmax_4thmax <- mod$o3_8hrmax_4thmax * 100
  for (v in CENTER) mod[[v]] <- mod[[v]] - mean(mod[[v]], na.rm = TRUE)
  cat(sprintf("n = %d\n", nrow(mod)))

  mod_geo <- geo[match(mod$GEOID, geo$GEOID), ]
  stopifnot(identical(mod_geo$GEOID, mod$GEOID))
  coords <- suppressWarnings(
    st_coordinates(st_centroid(mod_geo, of_largest_polygon = TRUE)))
  nb <- knn2nb(knearneigh(coords, k = 8), sym = TRUE)
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

  y <- mod$ckm_core_pca
  cat(sprintf("outcome Moran I = %.6f   (sd = %.6f)\n",
              moran.test(y, lw, zero.policy = TRUE)$estimate[[1]], sd(y)))

  fixed_only_moran <- function(rds, preds, label) {
    f <- readRDS(file.path(dir_proc, rds))
    stopifnot(identical(f$GEOID, mod$GEOID))   
    rhs <- paste(c(preds, ENV_PRED, paste0("pct_age_65_plus:", ENV_PRED)),
                 collapse = " + ")
    X <- model.matrix(as.formula(paste("~", rhs)), data = mod)
    b <- f$summary_fixed
    stopifnot(setequal(colnames(X), rownames(b)))
    X <- X[, rownames(b), drop = FALSE]
    Xb <- as.vector(X %*% b$mean)
    mi <- moran.test(y - Xb, lw, zero.policy = TRUE)$estimate[[1]]
    cat(sprintf("%-9s fixed-effects-only residual Moran I = %.6f  -> rounds to %.2f\n",
                label, mi, round(mi, 2)))
  
    cat(sprintf("%-9s   sd(Xb) = %.4f  (sd(y) = %.4f)   Moran(Xb) = %.4f   cor(Xb,y) = %.4f\n",
                label, sd(Xb), sd(y),
                moran.test(Xb, lw, zero.policy = TRUE)$estimate[[1]], cor(Xb, y)))
    list(moran = mi, sd_fit = sd(Xb), X = X, b = b)
  }

  m1 <- fixed_only_moran("12_bym2_refit_model1.rds", setdiff(CORE_PRED, BEHAV), "Model 1")
  m2 <- fixed_only_moran("12_bym2_refit_model2.rds", CORE_PRED,                 "Model 2")

 
  bcols <- intersect(rownames(m2$b), BEHAV)
  stopifnot(length(bcols) == length(BEHAV))
  contrib <- as.vector(m2$X[, bcols, drop = FALSE] %*% m2$b[bcols, "mean"])
  cat(sprintf("behav block  sd = %.4f   Moran = %.4f   cor with y = %+.4f\n",
              sd(contrib),
              moran.test(contrib, lw, zero.policy = TRUE)$estimate[[1]],
              cor(contrib, y)))

  cat(sprintf("\nSECTION 3.3 QUOTED VALUES\n"))
  cat(sprintf("  residual Moran I  Model 1 = %.2f   Model 2 = %.2f\n",
              round(m1$moran, 2), round(m2$moran, 2)))
  cat(sprintf("  outcome Moran I            = %.2f\n",
              round(moran.test(y, lw, zero.policy = TRUE)$estimate[[1]], 2)))
  cat(sprintf("  fitted-surface SD Model 2  = %.2f   outcome SD = %.2f\n",
              round(m2$sd_fit, 2), round(sd(y), 2)))
  stopifnot(round(m1$moran, 2) == 0.51, round(m2$moran, 2) == 0.93,
            round(m2$sd_fit, 2) == 2.32, round(sd(y), 2) == 1.93)
  cat("all §3.3 values reproduce\n")

  # ---- write the fitted-surface SDs to a SHIPPED table -------------------------
 
  dir_tab <- file.path(ROOT, "output", "tables")
  dir.create(dir_tab, showWarnings = FALSE, recursive = TRUE)
  out <- data.frame(
    model = c("Model 1 (primary, behaviour-excluded)",
              "Model 2 (fully adjusted, with behavioural block)"),
    n = nrow(mod),
    fit_sd_fixedonly = c(m1$sd_fit, m2$sd_fit),
    fit_moran_fixedonly = c(
      moran.test(as.vector(m1$X %*% m1$b$mean), lw, zero.policy = TRUE)$estimate[[1]],
      moran.test(as.vector(m2$X %*% m2$b$mean), lw, zero.policy = TRUE)$estimate[[1]]),
    moran_resid_fixedonly = c(m1$moran, m2$moran),
    outcome_sd = sd(y),
    moran_outcome = moran.test(y, lw, zero.policy = TRUE)$estimate[[1]],
    behav_block_sd = c(NA_real_, sd(contrib)),
    behav_block_moran = c(NA_real_,
      moran.test(contrib, lw, zero.policy = TRUE)$estimate[[1]]),
    behav_block_cor_y = c(NA_real_, cor(contrib, y)),
    stringsAsFactors = FALSE
  )
  f_out <- file.path(dir_tab, "15_b70_fitted_surface.csv")
  write.csv(out, f_out, row.names = FALSE)
  cat(sprintf("wrote %s\n", basename(f_out)))
 
  pr <- read.csv(file.path(dir_tab, "12_bym2_posterior_residual.csv"),
                 stringsAsFactors = FALSE)
  stopifnot(nrow(pr) == 2,
            all(abs(pr$moran_resid_fixedonly - out$moran_resid_fixedonly) < 1e-6),
            all(abs(pr$outcome_sd - out$outcome_sd) < 1e-6))
  cat("cross-check vs 12_bym2_posterior_residual.csv: agrees to <1e-6\n")

}
environment(.stage) <- new.env(parent = globalenv())
.run_stage("15_verify_b70_moran.R", .stage)

# ---- stage: 16_spline_lila_interaction.R ----
.stage <- function() {
  # =============================================================================
  # 16_spline_lila_interaction.R
  # INPUTS   data/processed/02_ckm_index.rds
  #          data/processed/01_tracts_geo.rds
  #          data/processed/09b_precision_weighted_model2.rds   (sample check)
  #          data/processed/09b_pw_m2_graph.adj                 (graph check)
  #          output/tables/06_6g_coefficients_bym2_nobehav.csv  (published M1)
  # OUTPUTS  data/processed/16_spline_lila_interaction.rds
  #          output/tables/16_spline_lila_contrasts.csv
  #          output/tables/16_spline_lila_modelfit.csv
  #          output/tables/16_spline_lila_validation.csv
  # =============================================================================

  suppressMessages({
    library(dplyr); library(tidyr); library(sf); library(spdep)
    library(splines); library(INLA)
  })

  root <- getwd()
  if (!dir.exists(file.path(root, "data")) &&
      dir.exists(file.path(root, "..", "data"))) root <- normalizePath(file.path(root, ".."))
  stopifnot(dir.exists(file.path(root, "data", "processed")))
  dir_proc <- file.path(root, "data", "processed")
  dir_tab  <- file.path(root, "output", "tables")
  dir.create(dir_tab, showWarnings = FALSE, recursive = TRUE)

  BEHAV     <- c("ACCESS2", "CHECKUP", "CSMOKING", "LPA")
  CORE_PRED <- c("pct_age_65_plus", "pct_hispanic", "ice_income",
                 "pct_bachelor_plus", "ice_race", "ACCESS2", "CHECKUP",
                 "ruca_class", "CSMOKING", "LPA")
  CORE_PRED_EXC <- setdiff(CORE_PRED, BEHAV)
  ENV_PRED  <- c("pm25_annual", "o3_8hrmax_4thmax", "LILATracts_1And10",
                 "tmax_warm", "walk_index")
  CENTER    <- c("pct_age_65_plus", "pm25_annual", "o3_8hrmax_4thmax",
                 "tmax_warm", "walk_index")
  DF_SPLINE <- 4L
  PCTS      <- c(.10, .25, .50, .75, .90, .95)

  t_start <- Sys.time()
  message("=== 16  Spline / categorical test of the aging x LILA interaction ===")

  # ---------------------------------------------------------------------------
  # 1. Rebuild the estimation sample (identical path to 09b)
  # ---------------------------------------------------------------------------
  an <- readRDS(file.path(dir_proc, "02_ckm_index.rds"))
  if (inherits(an, "sf")) an <- st_drop_geometry(an)
  geo <- readRDS(file.path(dir_proc, "01_tracts_geo.rds"))
  d   <- an |> left_join(st_drop_geometry(geo)[, c("GEOID", "ALAND")], by = "GEOID")

  conus <- d |>
    filter(!substr(GEOID, 1, 2) %in% c("02", "15", "60", "66", "69", "72", "78")) |>
    filter(ALAND > 0, !is.na(ckm_core_pca))

  mod <- conus |>
    mutate(ruca_class = factor(ruca_class,
             levels = c("Metro", "Micropolitan", "Small town", "Rural"))) |>
    drop_na(ckm_core_pca, pct_age_65_plus, ice_race, ice_income, ruca_class,
            pct_hispanic, pct_black, pct_bachelor_plus, median_income) |>
    drop_na(all_of(c("ckm_core_pca", CORE_PRED, ENV_PRED)))

  # G1
  message(sprintf("G1  n = %d", nrow(mod)))
  stopifnot(nrow(mod) == 69530L)

  # G2 -- same tracts, same order, as the validated 09b run
  pw <- readRDS(file.path(dir_proc, "09b_precision_weighted_model2.rds"))
  stopifnot(identical(as.character(mod$GEOID), as.character(pw$weights$GEOID)))
  message("G2  GEOID vector and row order identical to validated 09b run")

  
  age_mean_raw <- mean(mod$pct_age_65_plus, na.rm = TRUE)
  mod$o3_8hrmax_4thmax <- mod$o3_8hrmax_4thmax * 100
  for (v in CENTER) mod[[v]] <- mod[[v]] - mean(mod[[v]], na.rm = TRUE)
  stopifnot(all(mod$LILATracts_1And10 %in% c(0, 1)))

  # ---------------------------------------------------------------------------
  # 2. Neighbour graph on this sample's ordering
  # ---------------------------------------------------------------------------
  mod_geo <- geo[match(mod$GEOID, geo$GEOID), ]
  stopifnot(identical(mod_geo$GEOID, mod$GEOID))
  coords <- suppressWarnings(
    st_coordinates(st_centroid(mod_geo, of_largest_polygon = TRUE)))
  nb <- knn2nb(knearneigh(coords, k = 8), sym = TRUE)

 
  nb2INLA_fast <- function(file, nb) {
    crd <- spdep::card(nb)
    writeLines(c(as.character(length(nb)),
      vapply(seq_along(nb), function(i)
        if (crd[i] == 0L) paste(i, 0)
        else paste(c(i, crd[i], nb[[i]]), collapse = " "),
        character(1))), file)
  }
  graph_file <- file.path(dir_proc, "16_spline_graph.adj")
  nb2INLA_fast(graph_file, nb)

  # G3 -- byte-identical to the graph 09b validated on
  stopifnot(identical(tools::md5sum(graph_file)[[1]],
                      tools::md5sum(file.path(dir_proc, "09b_pw_m2_graph.adj"))[[1]]))
  message("G3  neighbour graph byte-identical to validated 09b graph")
  mod$idarea <- seq_len(nrow(mod))

  # ---------------------------------------------------------------------------
  # 3. Explicit design columns

  age  <- mod$pct_age_65_plus                    # centered
  lila <- mod$LILATracts_1And10

  # 3a. 
  bas <- ns(age, df = DF_SPLINE)
  NS  <- sprintf("age_ns%d", seq_len(DF_SPLINE))
  NSL <- sprintf("%s_lila", NS)
  for (k in seq_len(DF_SPLINE)) {
    mod[[NS[k]]]  <- bas[, k]
    mod[[NSL[k]]] <- bas[, k] * lila
  }

  # 3b. quintiles of % age 65+
  qbrk <- quantile(age, seq(0, 1, .2))
  qbrk[1] <- qbrk[1] - 1e-9; qbrk[length(qbrk)] <- qbrk[length(qbrk)] + 1e-9
  age_q <- cut(age, breaks = qbrk, labels = paste0("Q", 1:5))
  stopifnot(!anyNA(age_q))
  QD  <- sprintf("age_q%d", 2:5)                  # Q1 is the reference
  QDL <- sprintf("%s_lila", QD)
  for (j in 2:5) {
    mod[[QD[j - 1]]]  <- as.numeric(age_q == paste0("Q", j))
    mod[[QDL[j - 1]]] <- as.numeric(age_q == paste0("Q", j)) * lila
  }

  # 3c. aging x environment products. 
  AGEENV <- c("age_pm25", "age_o3", "age_tmax", "age_walk")
  mod$age_pm25 <- age * mod$pm25_annual
  mod$age_o3   <- age * mod$o3_8hrmax_4thmax
  mod$age_tmax <- age * mod$tmax_warm
  mod$age_walk <- age * mod$walk_index
  mod$age_lila <- age * lila                     

  BASE <- c("pct_hispanic", "ice_income", "pct_bachelor_plus", "ice_race",
            "ruca_class", "pm25_annual", "o3_8hrmax_4thmax", "tmax_warm",
            "walk_index", "LILATracts_1And10")
  stopifnot(setequal(setdiff(BASE, "ruca_class"),
                     setdiff(c(CORE_PRED_EXC, ENV_PRED),
                             c("pct_age_65_plus", "ruca_class"))))
 
  chk_no_behav <- function(s)
    stopifnot(!any(vapply(BEHAV, grepl, logical(1), x = s, fixed = TRUE)))

  # ---------------------------------------------------------------------------
  # 4. Linear combinations: the LILA contrast at each reporting percentile
  # ---------------------------------------------------------------------------
  pt_c   <- unname(quantile(age, PCTS))          
  pt_raw <- pt_c + age_mean_raw               
  bas_at <- predict(bas, newx = pt_c)            
  stopifnot(nrow(bas_at) == length(PCTS), ncol(bas_at) == DF_SPLINE)

  lab <- sprintf("p%02d", round(PCTS * 100))

  # Spline: contrast(a) = b_LILA + sum_k ns_k(a) * b_{ns_k:LILA}
  lc_spline <- do.call(c, lapply(seq_along(PCTS), function(i) {
    l <- do.call(INLA::inla.make.lincomb,
                 c(setNames(list(1), "LILATracts_1And10"),
                   setNames(as.list(bas_at[i, ]), NSL)))
    names(l) <- paste0("lila_", lab[i]); l
  }))

  # Quintile: contrast in Q1 is b_LILA; in Qj it is b_LILA + b_{Qj:LILA}
  lc_quint <- do.call(c, lapply(1:5, function(j) {
    args <- setNames(list(1), "LILATracts_1And10")
    if (j > 1) args <- c(args, setNames(list(1), QDL[j - 1]))
    l <- do.call(INLA::inla.make.lincomb, args)
    names(l) <- paste0("lila_Q", j); l
  }))

  # Linear: contrast(a) = b_LILA + a * b_{age:LILA}
  lc_linear <- do.call(c, lapply(seq_along(PCTS), function(i) {
    l <- INLA::inla.make.lincomb(LILATracts_1And10 = 1, age_lila = pt_c[i])
    names(l) <- paste0("lila_", lab[i]); l
  }))

  # ---------------------------------------------------------------------------
  # 5. Fit
  # ---------------------------------------------------------------------------
  fit_bym2 <- function(label, rhs, lincomb) {
    chk_no_behav(rhs)
    fstr <- paste("ckm_core_pca ~", rhs)
    fml  <- as.formula(paste0(
      fstr, " + f(idarea, model = 'bym2', graph = '", graph_file, "',",
      " scale.model = TRUE,",
      " hyper = list(phi  = list(prior = 'pc', param = c(0.5, 0.5)),",
      "              prec = list(prior = 'pc.prec', param = c(1, 0.01))))"))
    message(sprintf("\n--- Fitting %s (n = %d) ---\n%s", label, nrow(mod), fstr))
    t0 <- Sys.time()
    f <- INLA::inla(fml, family = "gaussian", data = mod,
                    lincomb = lincomb,
                    control.compute   = list(dic = TRUE, waic = TRUE),
                    control.inla      = list(int.strategy = "eb"),
                    control.predictor = list(compute = FALSE))
    message(sprintf("  done in %.1f min | DIC %.1f | WAIC %.1f",
                    as.numeric(difftime(Sys.time(), t0, units = "mins")),
                    f$dic$dic, f$waic$waic))
    # A lincomb that silently failed to resolve would return no derived summary.
    stopifnot(!is.null(f$summary.lincomb.derived),
              nrow(f$summary.lincomb.derived) == length(lincomb))
    attr(f, "formula_str") <- fstr
    f
  }

  rhs_A <- paste(c("pct_age_65_plus", BASE, AGEENV, "age_lila"), collapse = " + ")
  rhs_B <- paste(c(NS,                BASE, AGEENV, NSL),        collapse = " + ")
  rhs_C <- paste(c(QD,                BASE, AGEENV, QDL),        collapse = " + ")

  fitA <- fit_bym2("A  LINEAR (published Model 1 spec)",   rhs_A, lc_linear)
  saveRDS(fitA$summary.fixed, file.path(dir_proc, "16_partial_A.rds"))
  fitB <- fit_bym2("B  SPLINE (natural cubic, df = 4)",    rhs_B, lc_spline)
  saveRDS(fitB$summary.fixed, file.path(dir_proc, "16_partial_B.rds"))
  fitC <- fit_bym2("C  QUINTILE (5 groups)",               rhs_C, lc_quint)
  saveRDS(fitC$summary.fixed, file.path(dir_proc, "16_partial_C.rds"))

  # ---------------------------------------------------------------------------
  # 6. Validation gate:
  # ---------------------------------------------------------------------------
  pub <- read.csv(file.path(dir_tab, "06_6g_coefficients_bym2_nobehav.csv"),
                  stringsAsFactors = FALSE)
  pcol <- names(pub)[1]
  pmean <- names(pub)[grepl("^mean$|^Mean$|posterior", names(pub))][1]
  stopifnot(!is.na(pmean))
 
  xw <- c("pct_age_65_plus" = "pct_age_65_plus",
          "pct_hispanic" = "pct_hispanic", "ice_income" = "ice_income",
          "pct_bachelor_plus" = "pct_bachelor_plus", "ice_race" = "ice_race",
          "ruca_classMicropolitan" = "ruca_classMicropolitan",
          "ruca_classSmall town" = "ruca_classSmall town",
          "ruca_classRural" = "ruca_classRural",
          "pm25_annual" = "pm25_annual",
          "o3_8hrmax_4thmax" = "o3_8hrmax_4thmax",
          "LILATracts_1And10" = "LILATracts_1And10",
          "tmax_warm" = "tmax_warm", "walk_index" = "walk_index",
          "pct_age_65_plus:pm25_annual" = "age_pm25",
          "pct_age_65_plus:o3_8hrmax_4thmax" = "age_o3",
          "pct_age_65_plus:LILATracts_1And10" = "age_lila",
          "pct_age_65_plus:tmax_warm" = "age_tmax",
          "pct_age_65_plus:walk_index" = "age_walk")
  have <- intersect(pub[[pcol]], names(xw))
  val <- data.frame(
    Published_term = have,
    Published = pub[[pmean]][match(have, pub[[pcol]])],
    Refit     = fitA$summary.fixed[xw[have], "mean"],
    row.names = NULL)
  val$Abs_diff <- val$Refit - val$Published
  message("\n--- Validation: fit A vs published Model 1 ---")
  print(val, digits = 4, row.names = FALSE)
  message(sprintf("max |diff| = %.2e", max(abs(val$Abs_diff))))
  write.csv(val, file.path(dir_tab, "16_spline_lila_validation.csv"),
            row.names = FALSE)
  if (max(abs(val$Abs_diff)) > 1e-3)
    warning("fit A departs from the published Model 1 by more than 1e-3; ",
            "treat every comparison below as unvalidated")

  # ---------------------------------------------------------------------------
  # 7. Contrasts
  # ---------------------------------------------------------------------------
  grab <- function(f, spec, at, at_raw) {
    s <- f$summary.lincomb.derived
    data.frame(Spec = spec, At = at, Age_pct_65plus = at_raw,
               Contrast = s[, "mean"], SD = s[, "sd"],
               Lo95 = s[, "0.025quant"], Hi95 = s[, "0.975quant"],
               Credible = sign(s[, "0.025quant"]) == sign(s[, "0.975quant"]),
               row.names = NULL)
  }
  q_raw <- unname(quantile(age, c(.10, .30, .50, .70, .90))) + age_mean_raw
  contrasts <- rbind(
    grab(fitA, "A_linear",   lab,                pt_raw),
    grab(fitB, "B_spline4",  lab,                pt_raw),
    grab(fitC, "C_quintile", paste0("Q", 1:5),   q_raw))
  message("\n--- Conditional LILA association by % age 65+ ---")
  print(contrasts, digits = 4, row.names = FALSE)
  write.csv(contrasts, file.path(dir_tab, "16_spline_lila_contrasts.csv"),
            row.names = FALSE)

  # ---------------------------------------------------------------------------
  # 8. Model fit comparison 
  # ---------------------------------------------------------------------------
  mf <- data.frame(
    Spec = c("A_linear", "B_spline4", "C_quintile"),
    n_fixed = c(nrow(fitA$summary.fixed), nrow(fitB$summary.fixed),
                nrow(fitC$summary.fixed)),
    DIC  = c(fitA$dic$dic,   fitB$dic$dic,   fitC$dic$dic),
    WAIC = c(fitA$waic$waic, fitB$waic$waic, fitC$waic$waic))
  mf$dDIC  <- mf$DIC  - mf$DIC[1]
  mf$dWAIC <- mf$WAIC - mf$WAIC[1]
  message("\n--- Fit comparison (negative delta favours the flexible spec) ---")
  print(mf, digits = 6, row.names = FALSE)
  write.csv(mf, file.path(dir_tab, "16_spline_lila_modelfit.csv"),
            row.names = FALSE)

  phi <- function(f) tryCatch(
    f$summary.hyperpar["Phi for idarea", "mean"], error = function(e) NA_real_)
  message(sprintf("\nposterior phi: A %.3f | B %.3f | C %.3f",
                  phi(fitA), phi(fitB), phi(fitC)))

  saveRDS(list(
    contrasts = contrasts, modelfit = mf, validation = val,
    summary_fixed = list(A = fitA$summary.fixed, B = fitB$summary.fixed,
                         C = fitC$summary.fixed),
    summary_hyper = list(A = fitA$summary.hyperpar, B = fitB$summary.hyperpar,
                         C = fitC$summary.hyperpar),
    lincomb = list(A = fitA$summary.lincomb.derived,
                   B = fitB$summary.lincomb.derived,
                   C = fitC$summary.lincomb.derived),
    spline_basis = bas, spline_at = bas_at, pct_points = PCTS,
    pct_centered = pt_c, pct_raw = pt_raw, age_mean_raw = age_mean_raw,
    quintile_breaks_raw = unname(qbrk) + age_mean_raw,
    formulas = c(A = attr(fitA, "formula_str"), B = attr(fitB, "formula_str"),
                 C = attr(fitC, "formula_str")),
    n = nrow(mod)),
    file.path(dir_proc, "16_spline_lila_interaction.rds"))

  for (f in sprintf("16_partial_%s.rds", c("A", "B", "C")))
    unlink(file.path(dir_proc, f))
  message(sprintf("\nSaved. Total elapsed %.1f min",
                  as.numeric(difftime(Sys.time(), t_start, units = "mins"))))

}
environment(.stage) <- new.env(parent = globalenv())
.run_stage("16_spline_lila_interaction.R", .stage)

# ---- stage: 16b_spline_lila_decomposition.R ----
.stage <- function() {
  # =============================================================================
  # 16b_spline_lila_decomposition.R
  # INPUTS   as 16, plus data/processed/16_spline_lila_interaction.rds
  # OUTPUTS  data/processed/16b_spline_lila_decomposition.rds
  #          output/tables/16b_decomposition_modelfit.csv
  #          output/tables/16b_decomposition_contrasts.csv
  # =============================================================================

  suppressMessages({
    library(dplyr); library(tidyr); library(sf); library(spdep)
    library(splines); library(INLA)
  })

  root <- getwd()
  if (!dir.exists(file.path(root, "data")) &&
      dir.exists(file.path(root, "..", "data"))) root <- normalizePath(file.path(root, ".."))
  dir_proc <- file.path(root, "data", "processed")
  dir_tab  <- file.path(root, "output", "tables")
  stopifnot(dir.exists(dir_proc))

  prev <- readRDS(file.path(dir_proc, "16_spline_lila_interaction.rds"))

  BEHAV     <- c("ACCESS2", "CHECKUP", "CSMOKING", "LPA")
  CORE_PRED <- c("pct_age_65_plus", "pct_hispanic", "ice_income",
                 "pct_bachelor_plus", "ice_race", "ACCESS2", "CHECKUP",
                 "ruca_class", "CSMOKING", "LPA")
  ENV_PRED  <- c("pm25_annual", "o3_8hrmax_4thmax", "LILATracts_1And10",
                 "tmax_warm", "walk_index")
  CENTER    <- c("pct_age_65_plus", "pm25_annual", "o3_8hrmax_4thmax",
                 "tmax_warm", "walk_index")
  PCTS      <- prev$pct_points

  t_start <- Sys.time()
  message("=== 16b  Decomposing the spline gain ===")

  # ---------------------------------------------------------------------------
  # 1. Sample
  # ---------------------------------------------------------------------------
  an <- readRDS(file.path(dir_proc, "02_ckm_index.rds"))
  if (inherits(an, "sf")) an <- st_drop_geometry(an)
  geo <- readRDS(file.path(dir_proc, "01_tracts_geo.rds"))
  d   <- an |> left_join(st_drop_geometry(geo)[, c("GEOID", "ALAND")], by = "GEOID")
  conus <- d |>
    filter(!substr(GEOID, 1, 2) %in% c("02", "15", "60", "66", "69", "72", "78")) |>
    filter(ALAND > 0, !is.na(ckm_core_pca))
  mod <- conus |>
    mutate(ruca_class = factor(ruca_class,
             levels = c("Metro", "Micropolitan", "Small town", "Rural"))) |>
    drop_na(ckm_core_pca, pct_age_65_plus, ice_race, ice_income, ruca_class,
            pct_hispanic, pct_black, pct_bachelor_plus, median_income) |>
    drop_na(all_of(c("ckm_core_pca", CORE_PRED, ENV_PRED)))
  stopifnot(nrow(mod) == 69530L)
  pw <- readRDS(file.path(dir_proc, "09b_precision_weighted_model2.rds"))
  stopifnot(identical(as.character(mod$GEOID), as.character(pw$weights$GEOID)))
  message("G1/G2  sample and row order match the validated reconstruction")

  age_mean_raw <- mean(mod$pct_age_65_plus, na.rm = TRUE)
  stopifnot(isTRUE(all.equal(age_mean_raw, prev$age_mean_raw)))
  mod$o3_8hrmax_4thmax <- mod$o3_8hrmax_4thmax * 100
  for (v in CENTER) mod[[v]] <- mod[[v]] - mean(mod[[v]], na.rm = TRUE)

  mod_geo <- geo[match(mod$GEOID, geo$GEOID), ]
  stopifnot(identical(mod_geo$GEOID, mod$GEOID))
  coords <- suppressWarnings(
    st_coordinates(st_centroid(mod_geo, of_largest_polygon = TRUE)))
  nb <- knn2nb(knearneigh(coords, k = 8), sym = TRUE)
  nb2INLA_fast <- function(file, nb) {
    crd <- spdep::card(nb)
    writeLines(c(as.character(length(nb)),
      vapply(seq_along(nb), function(i)
        if (crd[i] == 0L) paste(i, 0)
        else paste(c(i, crd[i], nb[[i]]), collapse = " "),
        character(1))), file)
  }
  graph_file <- file.path(dir_proc, "16b_graph.adj")
  nb2INLA_fast(graph_file, nb)
  stopifnot(identical(tools::md5sum(graph_file)[[1]],
                      tools::md5sum(file.path(dir_proc, "09b_pw_m2_graph.adj"))[[1]]))
  message("G3  neighbour graph byte-identical")
  mod$idarea <- seq_len(nrow(mod))

  # ---------------------------------------------------------------------------
  # 2. Design columns
  # ---------------------------------------------------------------------------
  age <- mod$pct_age_65_plus; lila <- mod$LILATracts_1And10
  BASE <- c("pct_hispanic", "ice_income", "pct_bachelor_plus", "ice_race",
            "ruca_class", "pm25_annual", "o3_8hrmax_4thmax", "tmax_warm",
            "walk_index", "LILATracts_1And10")
  AGEENV <- c("age_pm25", "age_o3", "age_tmax", "age_walk")
  mod$age_pm25 <- age * mod$pm25_annual
  mod$age_o3   <- age * mod$o3_8hrmax_4thmax
  mod$age_tmax <- age * mod$tmax_warm
  mod$age_walk <- age * mod$walk_index
  mod$age_lila <- age * lila

  pt_c   <- prev$pct_centered
  pt_raw <- prev$pct_raw
  lab    <- sprintf("p%02d", round(PCTS * 100))
  stopifnot(isTRUE(all.equal(pt_c, unname(quantile(age, PCTS)))))

 
  mk_spline <- function(df) {
    b  <- ns(age, df = df)
    nm <- sprintf("s%d_ns%d", df, seq_len(df))
    for (k in seq_len(df)) {
      mod[[nm[k]]]              <<- b[, k]
      mod[[paste0(nm[k], "_l")]] <<- b[, k] * lila
    }
    list(basis = b, nm = nm, nml = paste0(nm, "_l"),
         at = predict(b, newx = pt_c))
  }
  S3 <- mk_spline(3L); S4 <- mk_spline(4L); S5 <- mk_spline(5L)


  stopifnot(isTRUE(all.equal(unclass(S4$basis)[, ], unclass(prev$spline_basis)[, ],
                             check.attributes = FALSE)))
  message("spline df=4 basis identical to script 16's")

  qbrk <- quantile(age, seq(0, 1, .2))
  qbrk[1] <- qbrk[1] - 1e-9; qbrk[length(qbrk)] <- qbrk[length(qbrk)] + 1e-9
  age_q <- cut(age, breaks = qbrk, labels = paste0("Q", 1:5))
  QD <- sprintf("age_q%d", 2:5); QDL <- sprintf("%s_lila", QD)
  for (j in 2:5) {
    mod[[QD[j - 1]]]  <- as.numeric(age_q == paste0("Q", j))
    mod[[QDL[j - 1]]] <- as.numeric(age_q == paste0("Q", j)) * lila
  }
  q_mid_raw <- as.numeric(tapply(age, age_q, median)) + age_mean_raw

  # ---------------------------------------------------------------------------
  # 3. Lincombs
  # ---------------------------------------------------------------------------
  lc_lin <- do.call(c, lapply(seq_along(PCTS), function(i) {
    l <- INLA::inla.make.lincomb(LILATracts_1And10 = 1, age_lila = pt_c[i])
    names(l) <- paste0("lila_", lab[i]); l }))
  mk_lc_spline <- function(S) do.call(c, lapply(seq_along(PCTS), function(i) {
    l <- do.call(INLA::inla.make.lincomb,
         c(setNames(list(1), "LILATracts_1And10"),
           setNames(as.list(S$at[i, ]), S$nml)))
    names(l) <- paste0("lila_", lab[i]); l }))
  lc_quint <- do.call(c, lapply(1:5, function(j) {
    args <- setNames(list(1), "LILATracts_1And10")
    if (j > 1) args <- c(args, setNames(list(1), QDL[j - 1]))
    l <- do.call(INLA::inla.make.lincomb, args); names(l) <- paste0("lila_Q", j); l }))

  # ---------------------------------------------------------------------------
  # 4. Fit
  # ---------------------------------------------------------------------------
  fit_bym2 <- function(label, rhs, lincomb) {
    stopifnot(!any(vapply(BEHAV, grepl, logical(1), x = rhs, fixed = TRUE)))
    fstr <- paste("ckm_core_pca ~", rhs)
    fml  <- as.formula(paste0(fstr,
      " + f(idarea, model = 'bym2', graph = '", graph_file, "',",
      " scale.model = TRUE,",
      " hyper = list(phi  = list(prior = 'pc', param = c(0.5, 0.5)),",
      "              prec = list(prior = 'pc.prec', param = c(1, 0.01))))"))
    message(sprintf("\n--- %s ---", label))
    t0 <- Sys.time()
    f <- INLA::inla(fml, family = "gaussian", data = mod, lincomb = lincomb,
                    control.compute   = list(dic = TRUE, waic = TRUE),
                    control.inla      = list(int.strategy = "eb"),
                    control.predictor = list(compute = FALSE))
    stopifnot(!is.null(f$summary.lincomb.derived),
              nrow(f$summary.lincomb.derived) == length(lincomb))
    message(sprintf("  %.1f min | %d fixed | DIC %.1f | WAIC %.1f",
                    as.numeric(difftime(Sys.time(), t0, units = "mins")),
                    nrow(f$summary.fixed), f$dic$dic, f$waic$waic))
    f
  }

  rhs_B0 <- paste(c(S4$nm, BASE, AGEENV, "age_lila"), collapse = " + ")
  rhs_B3 <- paste(c(S3$nm, BASE, AGEENV, S3$nml),     collapse = " + ")
  rhs_B5 <- paste(c(S5$nm, BASE, AGEENV, S5$nml),     collapse = " + ")
  rhs_C2 <- paste(c("pct_age_65_plus", QD, BASE, AGEENV, QDL), collapse = " + ")

  fB0 <- fit_bym2("B0  spline df=4 main effect + LINEAR aging x LILA", rhs_B0, lc_lin)
  fB3 <- fit_bym2("B3  spline df=3, basis and interaction", rhs_B3, mk_lc_spline(S3))
  fB5 <- fit_bym2("B5  spline df=5, basis and interaction", rhs_B5, mk_lc_spline(S5))
  fC2 <- fit_bym2("C2  quintiles + continuous aging main effect", rhs_C2, lc_quint)

  # ---------------------------------------------------------------------------
  # 5. Decomposition
  # ---------------------------------------------------------------------------
  mf0 <- prev$modelfit
  gv <- function(spec, col) mf0[[col]][mf0$Spec == spec]
  mf <- data.frame(
    Spec = c("A_linear", "B0_splineMain_linearInt", "B_spline4", "B3_spline3",
             "B5_spline5", "C_quintile_noTrend", "C2_quintile_withTrend"),
    n_fixed = c(gv("A_linear", "n_fixed"), nrow(fB0$summary.fixed),
                gv("B_spline4", "n_fixed"), nrow(fB3$summary.fixed),
                nrow(fB5$summary.fixed), gv("C_quintile", "n_fixed"),
                nrow(fC2$summary.fixed)),
    DIC = c(gv("A_linear", "DIC"), fB0$dic$dic, gv("B_spline4", "DIC"),
            fB3$dic$dic, fB5$dic$dic, gv("C_quintile", "DIC"), fC2$dic$dic),
    WAIC = c(gv("A_linear", "WAIC"), fB0$waic$waic, gv("B_spline4", "WAIC"),
             fB3$waic$waic, fB5$waic$waic, gv("C_quintile", "WAIC"),
             fC2$waic$waic))
  mf$dDIC_vs_linear  <- mf$DIC  - mf$DIC[1]
  mf$dWAIC_vs_linear <- mf$WAIC - mf$WAIC[1]
  message("\n--- Fit comparison, all on the same sample and graph ---")
  print(mf, digits = 7, row.names = FALSE)

  dic_main <- mf$DIC[mf$Spec == "B0_splineMain_linearInt"] - mf$DIC[1]
  dic_int  <- mf$DIC[mf$Spec == "B_spline4"] -
              mf$DIC[mf$Spec == "B0_splineMain_linearInt"]
  tot <- dic_main + dic_int
  message(sprintf(
    "\nDECOMPOSITION of the spline DIC gain (%.1f total):\n  aging MAIN effect  %.1f (%.1f%%)\n  aging x LILA INTERACTION %.1f (%.1f%%)",
    tot, dic_main, 100 * dic_main / tot, dic_int, 100 * dic_int / tot))

  # ---------------------------------------------------------------------------
  # 6. Contrasts
  # ---------------------------------------------------------------------------
  grab <- function(f, spec, at, at_raw) {
    s <- f$summary.lincomb.derived
    data.frame(Spec = spec, At = at, Age_pct_65plus = at_raw,
               Contrast = s[, "mean"], SD = s[, "sd"],
               Lo95 = s[, "0.025quant"], Hi95 = s[, "0.975quant"],
               Credible = sign(s[, "0.025quant"]) == sign(s[, "0.975quant"]),
               row.names = NULL)
  }
  contrasts <- rbind(
    grab(fB0, "B0_splineMain_linearInt", lab, pt_raw),
    grab(fB3, "B3_spline3",              lab, pt_raw),
    grab(fB5, "B5_spline5",              lab, pt_raw),
    grab(fC2, "C2_quintile_withTrend", paste0("Q", 1:5), q_mid_raw))
  message("\n--- Conditional LILA association ---")
  print(contrasts, digits = 4, row.names = FALSE)

  write.csv(mf, file.path(dir_tab, "16b_decomposition_modelfit.csv"), row.names = FALSE)
  write.csv(contrasts, file.path(dir_tab, "16b_decomposition_contrasts.csv"),
            row.names = FALSE)
  saveRDS(list(modelfit = mf, contrasts = contrasts,
               dic_decomp = c(main = dic_main, interaction = dic_int),
               summary_fixed = list(B0 = fB0$summary.fixed, B3 = fB3$summary.fixed,
                                    B5 = fB5$summary.fixed, C2 = fC2$summary.fixed),
               summary_hyper = list(B0 = fB0$summary.hyperpar, B3 = fB3$summary.hyperpar,
                                    B5 = fB5$summary.hyperpar, C2 = fC2$summary.hyperpar),
               q_mid_raw = q_mid_raw, n = nrow(mod)),
          file.path(dir_proc, "16b_spline_lila_decomposition.rds"))
  message(sprintf("\nSaved. Elapsed %.1f min",
                  as.numeric(difftime(Sys.time(), t_start, units = "mins"))))

}
environment(.stage) <- new.env(parent = globalenv())
.run_stage("16b_spline_lila_decomposition.R", .stage)

# ---- stage: 17_lila_signchange.R ----
.stage <- function() {
  # =============================================================================
  # 17_lila_signchange.R
  #

  # OUTPUT   output/tables/17_lila_signchange.csv
  # =============================================================================

  suppressMessages({ library(dplyr); library(tidyr); library(sf) })

  root <- getwd()
  if (!dir.exists(file.path(root, "data")) &&
      dir.exists(file.path(root, "..", "data"))) root <- normalizePath(file.path(root, ".."))
  dir_proc <- file.path(root, "data", "processed")
  dir_tab  <- file.path(root, "output", "tables")
  stopifnot(dir.exists(dir_proc))

  CORE_PRED <- c("pct_age_65_plus", "pct_hispanic", "ice_income",
                 "pct_bachelor_plus", "ice_race", "ACCESS2", "CHECKUP",
                 "ruca_class", "CSMOKING", "LPA")
  ENV_PRED  <- c("pm25_annual", "o3_8hrmax_4thmax", "LILATracts_1And10",
                 "tmax_warm", "walk_index")


  an <- readRDS(file.path(dir_proc, "02_ckm_index.rds"))
  if (inherits(an, "sf")) an <- st_drop_geometry(an)
  geo <- readRDS(file.path(dir_proc, "01_tracts_geo.rds"))
  d   <- an |> left_join(st_drop_geometry(geo)[, c("GEOID", "ALAND")], by = "GEOID")
  mod <- d |>
    filter(!substr(GEOID, 1, 2) %in% c("02", "15", "60", "66", "69", "72", "78")) |>
    filter(ALAND > 0, !is.na(ckm_core_pca)) |>
    mutate(ruca_class = factor(ruca_class,
             levels = c("Metro", "Micropolitan", "Small town", "Rural"))) |>
    drop_na(ckm_core_pca, pct_age_65_plus, ice_race, ice_income, ruca_class,
            pct_hispanic, pct_black, pct_bachelor_plus, median_income) |>
    drop_na(all_of(c("ckm_core_pca", CORE_PRED, ENV_PRED)))
  stopifnot(nrow(mod) == 69530L)                                          # G1
  pw <- readRDS(file.path(dir_proc, "09b_precision_weighted_model2.rds"))
  stopifnot(identical(as.character(mod$GEOID), as.character(pw$weights$GEOID)))  # G2

  a    <- mod$pct_age_65_plus
  abar <- mean(a)


  m1 <- read.csv(file.path(dir_tab, "06_6g_coefficients_bym2_nobehav.csv"),
                 stringsAsFactors = FALSE, check.names = FALSE)
  g1 <- function(term) {
    v <- m1$mean[m1$predictor == term]; stopifnot(length(v) == 1L); v }
  b1_lila <- g1("LILATracts_1And10")
  b1_int  <- g1("pct_age_65_plus:LILATracts_1And10")

  m2 <- read.csv(file.path(dir_tab, "09_table3_bym2.csv"),
                 stringsAsFactors = FALSE, check.names = FALSE)
  g2 <- function(term) {
    v <- m2[[2]][m2[[1]] == term]; stopifnot(length(v) == 1L); v }
  b2_lila <- g2("Low-income low-access tract")
  b2_int  <- g2("% Age ≥65 × LILA")


  stopifnot(abs(b1_lila - 0.0695) < 5e-4, abs(b1_int - 0.0186) < 5e-4,
            abs(b2_lila + 0.0136) < 5e-4, abs(b2_int - 0.0167) < 5e-4)


  stopifnot(b1_int > 0, b2_int > 0)

  star <- function(b_lila, b_int) abar - b_lila / b_int
  a1 <- star(b1_lila, b1_int)
  a2 <- star(b2_lila, b2_int)
  stopifnot(a1 > min(a), a1 < max(a), a2 > min(a), a2 < max(a))  

  out <- data.frame(
    Model = c("Model 1 (primary, behavior-excluded)",
              "Model 2 (behavior-adjusted)"),
    beta_LILA        = c(b1_lila, b2_lila),
    beta_interaction = c(b1_int,  b2_int),
    age_mean_centering = abar,
    sign_change_pct_age65 = c(a1, a2),
    tracts_below_n   = c(sum(a < a1), sum(a < a2)),
    tracts_below_pct = c(100 * mean(a < a1), 100 * mean(a < a2)),
    lila_tracts_below_pct = c(100 * mean(a[mod$LILATracts_1And10 == 1] < a1),
                              100 * mean(a[mod$LILATracts_1And10 == 1] < a2)),
    n = nrow(mod),
    source_file = c("output/tables/06_6g_coefficients_bym2_nobehav.csv",
                    "output/tables/09_table3_bym2.csv"))

  print(out, digits = 6, row.names = FALSE)
  write.csv(out, file.path(dir_tab, "17_lila_signchange.csv"), row.names = FALSE)
  cat("\nsaved: output/tables/17_lila_signchange.csv\n")
  cat(sprintf("\nMANUSCRIPT VALUES\n  Model 1 sign change: %.1f%% aged 65+  (%.1f%% of tracts below)\n  Model 2 sign change: %.1f%% aged 65+  (%.1f%% of tracts below)\n",
              a1, 100 * mean(a < a1), a2, 100 * mean(a < a2)))

}
environment(.stage) <- new.env(parent = globalenv())
.run_stage("17_lila_signchange.R", .stage)

# ---- stage: 19_s11_indicator_split.R ----
.stage <- function() {
  # =============================================================================
  # 19_s11_indicator_split.R

  # OUTPUT  output/tables/19_s11_indicator_split.csv
  # =============================================================================

  suppressMessages({ library(dplyr); library(tidyr); library(sf) })

  root <- getwd()
  if (!dir.exists(file.path(root, "data")) &&
      dir.exists(file.path(root, "..", "data"))) root <- normalizePath(file.path(root, ".."))
  dir_proc <- file.path(root, "data", "processed")
  dir_tab  <- file.path(root, "output", "tables")
  stopifnot(dir.exists(dir_proc))

  CORE_PRED <- c("pct_age_65_plus", "pct_hispanic", "ice_income",
                 "pct_bachelor_plus", "ice_race", "ACCESS2", "CHECKUP",
                 "ruca_class", "CSMOKING", "LPA")
  ENV_PRED  <- c("pm25_annual", "o3_8hrmax_4thmax", "LILATracts_1And10",
                 "tmax_warm", "walk_index")

  SOUTH <- c("01","05","10","11","12","13","21","22","24",
             "28","37","40","45","47","48","51","54")
  stopifnot(length(SOUTH) == 17L)                                          # G0

 
  an <- readRDS(file.path(dir_proc, "02_ckm_index.rds"))
  if (inherits(an, "sf")) an <- st_drop_geometry(an)
  geo <- readRDS(file.path(dir_proc, "01_tracts_geo.rds"))
  d   <- an |> left_join(st_drop_geometry(geo)[, c("GEOID", "ALAND")], by = "GEOID")
  mod <- d |>
    filter(!substr(GEOID, 1, 2) %in% c("02", "15", "60", "66", "69", "72", "78")) |>
    filter(ALAND > 0, !is.na(ckm_core_pca)) |>
    mutate(ruca_class = factor(ruca_class,
             levels = c("Metro", "Micropolitan", "Small town", "Rural"))) |>
    drop_na(ckm_core_pca, pct_age_65_plus, ice_race, ice_income, ruca_class,
            pct_hispanic, pct_black, pct_bachelor_plus, median_income) |>
    drop_na(all_of(c("ckm_core_pca", CORE_PRED, ENV_PRED)))
  stopifnot(nrow(mod) == 69530L)                                           # G1
  pw <- readRDS(file.path(dir_proc, "09b_precision_weighted_model2.rds"))
  stopifnot(identical(as.character(mod$GEOID), as.character(pw$weights$GEOID)))  # G2

  is_south <- substr(mod$GEOID, 1, 2) %in% SOUTH
  stopifnot(sum(is_south) > 0, sum(!is_south) > 0)

 
  m2 <- read.csv(file.path(dir_tab, "09_table3_bym2.csv"),
                 stringsAsFactors = FALSE, check.names = FALSE)
  g2 <- function(term) {
    v <- m2[[2]][m2[[1]] == term]; stopifnot(length(v) == 1L); v }

  IND <- tibble::tribble(
    ~var,        ~label,                                ~term,
    "LPA",       "No leisure-time physical activity",   "No leisure-time physical activity",
    "CHECKUP",   "Routine checkup in past year",        "Routine checkup in past year",
    "CSMOKING",  "Current smoking",                     "Current smoking",
    "ACCESS2",   "Lacking health insurance (ages 18-64)", "Lacking health insurance (ages 18-64)")
  stopifnot(nrow(IND) == 4L, all(IND$var %in% names(mod)))

  IND$beta <- vapply(IND$term, g2, numeric(1))

  # Cross-check 
  stopifnot(abs(IND$beta[IND$var == "LPA"]      - 0.1634) < 5e-4,
            abs(IND$beta[IND$var == "CHECKUP"]  - 0.2430) < 5e-4,
            abs(IND$beta[IND$var == "CSMOKING"] - 0.0929) < 5e-4,
            abs(IND$beta[IND$var == "ACCESS2"]  + 0.0310) < 5e-4)

  # ---- the decomposition ----------------------------------------------------
  mean_s  <- vapply(IND$var, function(v) mean(mod[[v]][is_south]),  numeric(1))
  mean_ns <- vapply(IND$var, function(v) mean(mod[[v]][!is_south]), numeric(1))

  out <- data.frame(
    Indicator            = IND$label,
    beta_model2          = IND$beta,
    prevalence_South     = mean_s,
    prevalence_nonSouth  = mean_ns,
    prevalence_diff_pp   = mean_s - mean_ns,
    contribution_South   = IND$beta * mean_s,
    contribution_nonSouth= IND$beta * mean_ns,
    contribution_diff    = IND$beta * (mean_s - mean_ns),
    row.names = NULL)
  out <- out[order(-out$contribution_diff), ]

  blk_s  <- sum(out$contribution_South)
  blk_ns <- sum(out$contribution_nonSouth)
  blk_d  <- sum(out$contribution_diff)

 
  stopifnot(abs(blk_s  - 23.8843) < 5e-3)
  stopifnot(abs(blk_ns - 22.6034) < 5e-3)
  stopifnot(abs(blk_d  -  1.2809) < 5e-3)

  stopifnot(abs(blk_d - (blk_s - blk_ns)) < 1e-10)

  out <- rbind(out, data.frame(
    Indicator = "Behavioral and healthcare engagement (block total)",
    beta_model2 = NA_real_, prevalence_South = NA_real_,
    prevalence_nonSouth = NA_real_, prevalence_diff_pp = NA_real_,
    contribution_South = blk_s, contribution_nonSouth = blk_ns,
    contribution_diff = blk_d, row.names = NULL))

  print(out, digits = 5, row.names = FALSE)
  write.csv(out, file.path(dir_tab, "19_s11_indicator_split.csv"), row.names = FALSE)
  cat("\nsaved: output/tables/19_s11_indicator_split.csv\n")
  cat(sprintf("\nGATE PASSED  block total reproduced: South %.4f | non-South %.4f | diff %.4f\n",
              blk_s, blk_ns, blk_d))
  cat(sprintf("CHECKUP + ACCESS2 combined contribution: %+.4f\n",
              sum(out$contribution_diff[out$Indicator %in%
                  c("Routine checkup in past year",
                    "Lacking health insurance (ages 18-64)")])))

}
environment(.stage) <- new.env(parent = globalenv())
.run_stage("19_s11_indicator_split.R", .stage)

# ---- stage: 20_regional_variance.R ----
.stage <- function() {
  # =============================================================================
  # 20_regional_variance.R

  # OUTPUT  output/tables/20_regional_variance.csv
  # =============================================================================

  suppressMessages({ library(dplyr); library(tidyr); library(sf) })

  root <- getwd()
  if (!dir.exists(file.path(root, "data")) &&
      dir.exists(file.path(root, "..", "data"))) root <- normalizePath(file.path(root, ".."))
  dir_proc <- file.path(root, "data", "processed")
  dir_tab  <- file.path(root, "output", "tables")
  stopifnot(dir.exists(dir_proc))

  CORE_PRED <- c("pct_age_65_plus", "pct_hispanic", "ice_income",
                 "pct_bachelor_plus", "ice_race", "ACCESS2", "CHECKUP",
                 "ruca_class", "CSMOKING", "LPA")
  ENV_PRED  <- c("pm25_annual", "o3_8hrmax_4thmax", "LILATracts_1And10",
                 "tmax_warm", "walk_index")

  an <- readRDS(file.path(dir_proc, "02_ckm_index.rds"))
  if (inherits(an, "sf")) an <- st_drop_geometry(an)
  geo <- readRDS(file.path(dir_proc, "01_tracts_geo.rds"))
  d   <- an |> left_join(st_drop_geometry(geo)[, c("GEOID", "ALAND")], by = "GEOID")
  mod <- d |>
    filter(!substr(GEOID, 1, 2) %in% c("02", "15", "60", "66", "69", "72", "78")) |>
    filter(ALAND > 0, !is.na(ckm_core_pca)) |>
    mutate(ruca_class = factor(ruca_class,
             levels = c("Metro", "Micropolitan", "Small town", "Rural"))) |>
    drop_na(ckm_core_pca, pct_age_65_plus, ice_race, ice_income, ruca_class,
            pct_hispanic, pct_black, pct_bachelor_plus, median_income) |>
    drop_na(all_of(c("ckm_core_pca", CORE_PRED, ENV_PRED)))
  stopifnot(nrow(mod) == 69530L)                                           # G1
  pw <- readRDS(file.path(dir_proc, "09b_precision_weighted_model2.rds"))
  stopifnot(identical(as.character(mod$GEOID), as.character(pw$weights$GEOID)))  # G2

  state <- factor(substr(mod$GEOID, 1, 2))

  stopifnot(nlevels(state) == 48L)
  stopifnot(!("34" %in% levels(state)))
  stopifnot(all(c("06", "48", "36", "12") %in% levels(state)))  # CA TX NY FL present

  between_share <- function(x) {
    x <- as.numeric(x)
    gm <- mean(x)
    ssb <- sum(tapply(x, state, function(v) length(v) * (mean(v) - gm)^2))
    sst <- sum((x - gm)^2)
    ssb / sst
  }

  VARS <- c(aging      = "pct_age_65_plus",
            pm25       = "pm25_annual",
            ozone      = "o3_8hrmax_4thmax",
            heat       = "tmax_warm",
            walkability= "walk_index",
            lila       = "LILATracts_1And10",
            ice_income = "ice_income",
            ice_race   = "ice_race")

  res <- data.frame(
    variable = names(VARS),
    column   = unname(VARS),
    between_state_share = vapply(unname(VARS), function(v) between_share(mod[[v]]),
                                 numeric(1)),
    row.names = NULL)
  res$within_state_share <- 1 - res$between_state_share
  res <- res[order(-res$between_state_share), ]


  stopifnot(all(res$between_state_share >= 0), all(res$between_state_share <= 1))


  aging <- res$between_state_share[res$variable == "aging"]
  envs  <- res$between_state_share[res$variable %in%
                                     c("pm25", "ozone", "heat")]

  print(res, digits = 4, row.names = FALSE)
  write.csv(res, file.path(dir_tab, "20_regional_variance.csv"), row.names = FALSE)
  cat("\nsaved: output/tables/20_regional_variance.csv\n")
  cat(sprintf("\nAging between-state share: %.3f\n", aging))
  cat(sprintf("Environmental main effects: min %.3f  max %.3f\n",
              min(envs), max(envs)))
  cat(sprintf("\nVERDICT: aging is %s regionally structured than the LEAST\n",
              ifelse(aging > min(envs), "MORE", "LESS")))
  cat(sprintf("         regionally structured environmental exposure (%.3f).\n",
              min(envs)))
  cat(ifelse(aging > min(envs),
    "  -> the caution's environmental-only scope is NOT defensible on scale grounds.\n",
    "  -> the caution's environmental-only scope IS defensible on scale grounds.\n"))

}
environment(.stage) <- new.env(parent = globalenv())
.run_stage("20_regional_variance.R", .stage)

# ---- stage: 22_senior_access_sensitivity.R ----
.stage <- function() {
  # =============================================================================
  # 22_senior_access_sensitivity.R
  #

  suppressMessages({library(dplyr); library(readxl)})

  .root <- getwd()
  if (!dir.exists(file.path(.root, "data")) &&
      dir.exists(file.path(.root, "..", "data")))
    .root <- normalizePath(file.path(.root, ".."))

  dir_tab <- file.path(.root, "output", "tables")
  FARA    <- file.path(.root, "data", "raw", "fara_2019.xlsx")
  stopifnot(file.exists(FARA))

  s   <- ckm_sample()
  mod <- s$mod

  # ---------------------------------------------------------------------------
  # 1. Read 
  # ---------------------------------------------------------------------------
  message("\nreading FARA senior columns...")
  fa <- read_excel(FARA, sheet = "Food Access Research Atlas",
                   col_types = "text", .name_repair = "minimal") |>
    select(CensusTract, Pop2010, TractSeniors, laseniors1, laseniors1share,
           lapop1, LILATracts_1And10)


  null_sr  <- fa$laseniors1 == "NULL"
  null_pop <- fa$lapop1     == "NULL"
  message(sprintf("  laseniors1 'NULL': %d   lapop1 'NULL': %d   identical set: %s",
                  sum(null_sr), sum(null_pop), identical(null_sr, null_pop)))
  stopifnot(sum(null_sr) == 19989L)
  if (!identical(null_sr, null_pop))
    stop("laseniors1 and lapop1 NULL row sets differ; the 21_fara_null_check.py ",
         "finding does not transfer and the recode is not licensed.",
         call. = FALSE)

  num <- function(x) as.numeric(ifelse(x == "NULL", "0", x))

 
  n_ts_null <- sum(fa$TractSeniors == "NULL")
  stopifnot(n_ts_null == 4L)
  fa <- fa |>
    mutate(GEOID       = CensusTract,
           TractSeniors = as.numeric(ifelse(TractSeniors == "NULL", NA,
                                            TractSeniors)),
           laseniors1   = num(laseniors1),
           sr_share_pub = num(laseniors1share) / 100,   
           LILA_raw     = as.numeric(LILATracts_1And10))
  message(sprintf("  TractSeniors 'NULL': %d (denominator unknown, held as NA)",
                  n_ts_null))


  fa$sr_ratio <- ifelse(!is.na(fa$TractSeniors) & fa$TractSeniors > 0,
                        fa$laseniors1 / fa$TractSeniors, NA)
  stopifnot(all(fa$sr_ratio <= 1 + 1e-6, na.rm = TRUE))

  d <- mod |> left_join(fa[, c("GEOID", "TractSeniors", "sr_share_pub",
                               "sr_ratio", "LILA_raw")], by = "GEOID")
  stopifnot(nrow(d) == nrow(mod))

 
  if (!identical(as.numeric(d$LILA_raw), as.numeric(d$LILATracts_1And10)))
    stop("FARA join mismatch: LILA re-read from the workbook differs from the ",
         "LILA in the estimation sample. The join is on the wrong rows.",
         call. = FALSE)
  message("  join OK: LILA re-read from raw is identical to the sample column")

  
  n_zero <- sum(!is.na(d$TractSeniors) & d$TractSeniors == 0)
  n_nats <- sum(is.na(d$TractSeniors))
  message(sprintf("  ratio undefined for %d of %d sample rows (%d with zero ",
                  n_zero + n_nats, nrow(d), n_zero),
          sprintf("seniors, %d with unknown senior count)", n_nats))
  stopifnot(sum(is.na(d$sr_ratio)) == n_zero + n_nats)

  # ---------------------------------------------------------------------------
  # 2. What do the two senior measures actually measure?
  # ---------------------------------------------------------------------------
 
  cr <- function(v) cor(d[[v]], d$pct_age_65_plus, use = "complete.obs")
  n_ok <- sum(!is.na(d$sr_ratio))

  meas <- data.frame(
    measure = c("laseniors1share (FARA published, / tract population)",
                "laseniors1 / TractSeniors (correctly denominated)",
                "LILATracts_1And10 (binary, low-income AND low-access)"),
    variable = c("sr_share_pub", "sr_ratio", "LILATracts_1And10"),
    n_defined = c(sum(!is.na(d$sr_share_pub)), n_ok,
                  sum(!is.na(d$LILATracts_1And10))),
    mean = c(mean(d$sr_share_pub, na.rm = TRUE), mean(d$sr_ratio, na.rm = TRUE),
             mean(d$LILATracts_1And10)),
    sd = c(sd(d$sr_share_pub, na.rm = TRUE), sd(d$sr_ratio, na.rm = TRUE),
           sd(d$LILATracts_1And10)),
    cor_with_pct_age_65_plus = c(cr("sr_share_pub"), cr("sr_ratio"),
                                 cr("LILATracts_1And10")),
    row.names = NULL)

  message("\n--- what each senior access measure is correlated with ---")
  print(meas |> select(variable, n_defined, mean, sd, cor_with_pct_age_65_plus),
        digits = 3, row.names = FALSE)
  write.csv(meas, file.path(dir_tab, "22_senior_access_measures.csv"),
            row.names = FALSE)

  # ---------------------------------------------------------------------------
  # 3. Does an access-only measure reproduce the interaction?
  # ---------------------------------------------------------------------------

  sub  <- d |> filter(!is.na(sr_ratio))
  BASE <- paste(c(M1_PRED, setdiff(ENV_PRED, "LILATracts_1And10"),
                  paste0("pct_age_65_plus:",
                         setdiff(ENV_PRED, "LILATracts_1And10"))),
                collapse = " + ")

  specs <- list(
    A_LILA_published = "LILATracts_1And10 + pct_age_65_plus:LILATracts_1And10",
    B_ratio_only     = "sr_ratio + pct_age_65_plus:sr_ratio",
    C_share_pub_only = "sr_share_pub + pct_age_65_plus:sr_share_pub",
    D_both           = paste("LILATracts_1And10 + sr_ratio +",
                             "pct_age_65_plus:LILATracts_1And10 +",
                             "pct_age_65_plus:sr_ratio"))

  rows <- list()
  for (nm in names(specs)) {
    f <- lm(as.formula(paste("ckm_core_pca ~", BASE, "+", specs[[nm]])), data = sub)
    cf <- summary(f)$coefficients
    keep <- grep("^(pct_age_65_plus$|LILATracts_1And10$|sr_ratio$|sr_share_pub$|pct_age_65_plus:(LILA|sr_))",
                 rownames(cf), value = TRUE)
    rows[[nm]] <- data.frame(spec = nm, n = nobs(f), r2 = summary(f)$r.squared,
                             term = keep, beta = cf[keep, 1], se = cf[keep, 2],
                             p = cf[keep, 4], row.names = NULL)
  }
  
  fA_full <- lm(as.formula(paste("ckm_core_pca ~", BASE, "+", specs$A_LILA_published)),
                data = d)
  cfA <- summary(fA_full)$coefficients
  keepA <- grep("^(pct_age_65_plus$|LILATracts_1And10$|pct_age_65_plus:LILA)",
                rownames(cfA), value = TRUE)
  rows[["A_LILA_full_sample"]] <- data.frame(
    spec = "A_LILA_full_sample", n = nobs(fA_full),
    r2 = summary(fA_full)$r.squared, term = keepA, beta = cfA[keepA, 1],
    se = cfA[keepA, 2], p = cfA[keepA, 4], row.names = NULL)

  coef_tbl <- bind_rows(rows)
  message("\n--- aging x access interaction under four measures (same rows) ---")
  print(coef_tbl, digits = 4, row.names = FALSE)
  write.csv(coef_tbl, file.path(dir_tab, "22_senior_access_coefficients.csv"),
            row.names = FALSE)

  # ---------------------------------------------------------------------------
  # 4. The comparison stated as a testable claim, recorded either way
  # ---------------------------------------------------------------------------
  g <- function(sp, tm) coef_tbl$beta[coef_tbl$spec == sp & coef_tbl$term == tm]
  lila_A  <- g("A_LILA_published", "pct_age_65_plus:LILATracts_1And10")
  ratio_B <- g("B_ratio_only",     "pct_age_65_plus:sr_ratio")
  lila_D  <- g("D_both",           "pct_age_65_plus:LILATracts_1And10")
  ratio_D <- g("D_both",           "pct_age_65_plus:sr_ratio")

  message(sprintf(
    paste0("\naging x LILA alone      %+.5f",
           "\naging x senior ratio alone %+.5f",
           "\njointly:  LILA %+.5f   senior ratio %+.5f"),
    lila_A, ratio_B, lila_D, ratio_D))
  message(sprintf(
    "\npublished share correlates r = %+.3f with %% aged >=65; the correctly",
    meas$cor_with_pct_age_65_plus[1]))
  message(sprintf("denominated ratio correlates r = %+.3f.",
                  meas$cor_with_pct_age_65_plus[2]))
  message("\nwrote output/tables/22_senior_access_{measures,coefficients}.csv")

}
environment(.stage) <- new.env(parent = .ckm_env)
.run_stage("22_senior_access_sensitivity.R", .stage)

# ---- stage: 23_state_fe_heat.R ----
.stage <- function() {
  # =============================================================================
  # 23_state_fe_heat.R
 
  # OUTPUTS  output/tables/23_state_fe_variance.csv
  #          output/tables/23_state_fe_coefficients.csv
  #          data/processed/23_bym2_state_fe.rds
  # =============================================================================
  suppressMessages({library(dplyr); library(tidyr)})

  .root <- getwd()
  if (!dir.exists(file.path(.root, "data")) &&
      dir.exists(file.path(.root, "..", "data")))
    .root <- normalizePath(file.path(.root, ".."))

  OLS_ONLY <- "--ols-only" %in% commandArgs(TRUE)
  dir_tab  <- file.path(.root, "output", "tables")
  dir_proc <- file.path(.root, "data", "processed")

  s   <- ckm_sample()
  mod <- s$mod
  RHS <- paste(c(M1_PRED, ENV_PRED, paste0("pct_age_65_plus:", ENV_PRED)),
               collapse = " + ")

  # ---------------------------------------------------------------------------
  # 1. How much of each predictor lives between states?
  # ---------------------------------------------------------------------------
 
  vars <- c("pct_age_65_plus", "pct_hispanic", "ice_income", "pct_bachelor_plus",
            "ice_race", ENV_PRED)
  vshare <- vapply(vars, function(v) {
    a <- anova(lm(mod[[v]] ~ factor(mod$state)))
    100 * a[1, 2] / sum(a[, 2])
  }, numeric(1))
  var_tbl <- data.frame(predictor = vars, pct_variance_between_state = vshare,
                        row.names = NULL) |> arrange(desc(pct_variance_between_state))
  message("\n--- between-state variance share ---")
  print(var_tbl, digits = 3, row.names = FALSE)

  # ---------------------------------------------------------------------------
  # 2. OLS, with and without state fixed effects
  # ---------------------------------------------------------------------------
  fit_ols   <- lm(as.formula(paste("ckm_core_pca ~", RHS)), data = mod)
  fit_ols_fe <- lm(as.formula(paste("ckm_core_pca ~", RHS, "+ factor(state)")),
                   data = mod)

  grab <- function(f, prefix) {
    cf <- summary(f)$coefficients
    keep <- rownames(cf)[!grepl("^factor\\(state\\)", rownames(cf))]
    setNames(data.frame(term = keep, cf[keep, 1], cf[keep, 2], cf[keep, 4],
                        row.names = NULL),
             c("term", paste0(prefix, c("_beta", "_se", "_p"))))
  }
  ols_tbl <- full_join(grab(fit_ols, "ols"), grab(fit_ols_fe, "ols_fe"),
                       by = "term") |>
    mutate(ols_retained_pct = 100 * ols_fe_beta / ols_beta)

  message(sprintf("\n--- OLS  R2 = %.4f (no FE)  %.4f (state FE) ---",
                  summary(fit_ols)$r.squared, summary(fit_ols_fe)$r.squared))
  print(ols_tbl |> select(term, ols_beta, ols_fe_beta, ols_retained_pct, ols_fe_p),
        digits = 4, row.names = FALSE)

  write.csv(var_tbl, file.path(dir_tab, "23_state_fe_variance.csv"),
            row.names = FALSE)

 
  amb <- ols_tbl |> filter(term %in% c("tmax_warm", "pm25_annual",
                                       "o3_8hrmax_4thmax"))
  age <- ols_tbl |> filter(term == "pct_age_65_plus")
  message(sprintf(
    "\ns4.3 prediction: ambient terms retain %.0f-%.0f%% of their OLS effect; ",
    min(amb$ols_retained_pct), max(amb$ols_retained_pct)))
  message(sprintf("                 %% age >=65 retains %.0f%%.",
                  age$ols_retained_pct))

  if (OLS_ONLY) {
    write.csv(ols_tbl, file.path(dir_tab, "23_state_fe_coefficients.csv"),
              row.names = FALSE)
    message("\n--ols-only: stopping before the INLA stage.")
    return(invisible(NULL))
  }

  # ---------------------------------------------------------------------------
  # 3. BYM2 with state fixed effects -- the primary specification
  # ---------------------------------------------------------------------------
  stopifnot(requireNamespace("INLA", quietly = TRUE))
  message(sprintf("\nFitting BYM2 + state FE (n = %d)...", nrow(mod)))
  t0 <- Sys.time()
  fml <- as.formula(paste0(
    "ckm_core_pca ~ ", RHS, " + factor(state)",
    " + f(idarea, model = 'bym2', graph = '", s$graph_file, "',",
    " scale.model = TRUE,",
    " hyper = list(phi  = list(prior = 'pc', param = c(0.5, 0.5)),",
    "              prec = list(prior = 'pc.prec', param = c(1, 0.01))))"))
  fit <- INLA::inla(fml, data = mod, family = "gaussian",
                    control.predictor = list(compute = TRUE),
                    control.compute   = list(dic = TRUE, waic = TRUE,
                                             return.marginals.predictor = FALSE),
                    control.inla      = list(strategy = "adaptive",
                                             int.strategy = "eb"),
                    verbose = FALSE)
  message(sprintf("  done in %.1f min",
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))

  saveRDS(list(summary_fixed = fit$summary.fixed,
               summary_hyperpar = fit$summary.hyperpar,
               dic = fit$dic$dic, waic = fit$waic$waic,
               n = nrow(mod), GEOID = mod$GEOID,
               formula = deparse(fml)),
          file.path(dir_proc, "23_bym2_state_fe.rds"))

  
  pub <- s$ref$summary_fixed
  rf  <- fit$summary.fixed
  shared <- intersect(rownames(pub), rownames(rf))
  stopifnot(length(shared) >= 15)

  bym_tbl <- data.frame(
    term        = shared,
    bym2_beta   = pub[shared, "mean"],
    bym2_lo     = pub[shared, "0.025quant"],
    bym2_hi     = pub[shared, "0.975quant"],
    bym2fe_beta = rf[shared, "mean"],
    bym2fe_lo   = rf[shared, "0.025quant"],
    bym2fe_hi   = rf[shared, "0.975quant"],
    row.names = NULL) |>
    mutate(bym2_credible   = (bym2_lo > 0)   == (bym2_hi > 0),
           bym2fe_credible = (bym2fe_lo > 0) == (bym2fe_hi > 0),
           sign_flip = sign(bym2_beta) != sign(bym2fe_beta),
           retained_pct = 100 * bym2fe_beta / bym2_beta)

  message("\n--- BYM2: no FE (validated refit) vs + state FE ---")
  print(bym_tbl |> select(term, bym2_beta, bym2fe_beta, retained_pct,
                          bym2fe_credible, sign_flip),
        digits = 4, row.names = FALSE)
  message(sprintf("\nDIC  no FE %.1f -> state FE %.1f", s$ref$dic, fit$dic$dic))
  message(sprintf("WAIC no FE %.1f -> state FE %.1f", s$ref$waic, fit$waic$waic))

  out <- full_join(ols_tbl, bym_tbl, by = "term")
  write.csv(out, file.path(dir_tab, "23_state_fe_coefficients.csv"),
            row.names = FALSE)
  message("\nwrote output/tables/23_state_fe_coefficients.csv")

}
environment(.stage) <- new.env(parent = .ckm_env)
.run_stage("23_state_fe_heat.R", .stage)

# ---- stage: 24_spatial_plus.R ----
.stage <- function() {
  # =============================================================================
  # 24_spatial_plus.R

  # OUTPUTS  output/tables/24_spatial_plus_smooths.csv
  #          output/tables/24_spatial_plus_coefficients.csv
  #          data/processed/24_bym2_spatial_plus.rds
  # =============================================================================
  suppressMessages({library(dplyr); library(sf); library(mgcv)})

  .root <- getwd()
  if (!dir.exists(file.path(.root, "data")) &&
      dir.exists(file.path(.root, "..", "data")))
    .root <- normalizePath(file.path(.root, ".."))

  dir_tab  <- file.path(.root, "output", "tables")
  dir_proc <- file.path(.root, "data", "processed")
  K_SMOOTH <- 200L

  s   <- ckm_sample()
  mod <- s$mod

  # ---------------------------------------------------------------------------
  # 1. Spatial basis and per-covariate residualization
  # ---------------------------------------------------------------------------
  xy <- suppressWarnings(st_coordinates(st_centroid(st_geometry(s$geo))))
  stopifnot(nrow(xy) == nrow(mod), st_crs(s$geo)$epsg == 5070L)
  mod$cx <- xy[, 1]; mod$cy <- xy[, 2]

  CONT <- c(setdiff(M1_PRED, "ruca_class"), ENV_PRED)   
  stopifnot(!"ruca_class" %in% CONT, "LILATracts_1And10" %in% CONT)

  message(sprintf("\nresidualizing %d covariates on a thin-plate basis (k = %d)...",
                  length(CONT), K_SMOOTH))
  sm <- list()
  for (v in CONT) {
    b <- bam(as.formula(paste(v, "~ s(cx, cy, bs = 'tp', k =", K_SMOOTH, ")")),
             data = mod, discrete = TRUE)
    r <- as.numeric(residuals(b))
    sm[[v]] <- data.frame(
      covariate = v,
      dev_expl_by_space = summary(b)$dev.expl,
      sd_original = sd(mod[[v]]),
      sd_residual = sd(r),
      sd_ratio = sd(r) / sd(mod[[v]]))
    mod[[paste0(v, "_sp")]] <- r
  }
  sm_tbl <- bind_rows(sm) |> arrange(desc(dev_expl_by_space))
  message("\n--- how much of each covariate is smooth spatial structure? ---")
  print(sm_tbl, digits = 3, row.names = FALSE)
  write.csv(sm_tbl, file.path(dir_tab, "24_spatial_plus_smooths.csv"),
            row.names = FALSE)

  # ---------------------------------------------------------------------------
  # 2-3. Two BYM2 refits on the residualized covariates
  # ---------------------------------------------------------------------------
  stopifnot(requireNamespace("INLA", quietly = TRUE))

  sp <- function(v) paste0(v, "_sp")
  main_sp <- c(sp(setdiff(M1_PRED, "ruca_class")), "ruca_class", sp(ENV_PRED))

 
  int_res <- paste0(sp("pct_age_65_plus"), ":", sp(ENV_PRED))
  int_org <- paste0("pct_age_65_plus:", ENV_PRED)

  fit_one <- function(rhs, label) {
    fml <- as.formula(paste0(
      "ckm_core_pca ~ ", rhs,
      " + f(idarea, model = 'bym2', graph = '", s$graph_file, "',",
      " scale.model = TRUE,",
      " hyper = list(phi  = list(prior = 'pc', param = c(0.5, 0.5)),",
      "              prec = list(prior = 'pc.prec', param = c(1, 0.01))))"))
    message(sprintf("\nfitting %s (n = %d)...", label, nrow(mod)))
    t0 <- Sys.time()
    f <- INLA::inla(fml, data = mod, family = "gaussian",
                    control.predictor = list(compute = TRUE),
                    control.compute   = list(dic = TRUE, waic = TRUE,
                                             return.marginals.predictor = FALSE),
                    control.inla      = list(strategy = "adaptive",
                                             int.strategy = "eb"),
                    verbose = FALSE)
    message(sprintf("  done in %.1f min",
                    as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    f
  }

  fit_r <- fit_one(paste(c(main_sp, int_res), collapse = " + "),
                   "spatial+ (interactions = products of residuals)")
  fit_o <- fit_one(paste(c(main_sp, int_org), collapse = " + "),
                   "spatial+ (interactions = products of originals)")

  saveRDS(list(smooths = sm_tbl,
               resid_int = list(summary_fixed = fit_r$summary.fixed,
                                dic = fit_r$dic$dic, waic = fit_r$waic$waic),
               orig_int  = list(summary_fixed = fit_o$summary.fixed,
                                dic = fit_o$dic$dic, waic = fit_o$waic$waic),
               n = nrow(mod), k_smooth = K_SMOOTH, GEOID = mod$GEOID),
          file.path(dir_proc, "24_bym2_spatial_plus.rds"))

  # ---------------------------------------------------------------------------
  # 4. Assemble, on both scales
  # ---------------------------------------------------------------------------
 
  unsp <- function(x) gsub("_sp\\b", "", gsub("_sp:", ":", x))

  pull <- function(f, pre) {
    d <- f$summary.fixed
    data.frame(term = unsp(rownames(d)), m = d[, "mean"],
               lo = d[, "0.025quant"], hi = d[, "0.975quant"], row.names = NULL) |>
      setNames(c("term", paste0(pre, c("_beta", "_lo", "_hi"))))
  }

  pub <- s$ref$summary_fixed
  base <- data.frame(term = rownames(pub), bym2_beta = pub[, "mean"],
                     bym2_lo = pub[, "0.025quant"],
                     bym2_hi = pub[, "0.975quant"], row.names = NULL)

  out <- base |> full_join(pull(fit_r, "spres"), by = "term") |>
    full_join(pull(fit_o, "sporg"), by = "term")
  stopifnot(sum(is.na(out$spres_beta)) == 0, sum(is.na(out$bym2_beta)) == 0)

 
  sd_of <- function(term, resid) {
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    if (any(parts == "(Intercept)") || grepl("^ruca_class", term)) return(NA_real_)
    v <- sapply(parts, function(p) {
      if (!p %in% CONT) return(NA_real_)
      if (resid) sd(mod[[sp(p)]]) else sd(mod[[p]])
    })
    if (anyNA(v)) NA_real_ else prod(v)
  }
  out$sd_published <- vapply(out$term, sd_of, numeric(1), resid = FALSE)
  out$sd_spatialplus_resid <- vapply(out$term, sd_of, numeric(1), resid = TRUE)
  out <- out |>
    mutate(bym2_beta_per_sd  = bym2_beta  * sd_published,
           spres_beta_per_sd = spres_beta * sd_spatialplus_resid,
           sporg_beta_per_sd = sporg_beta * sd_published,
           spres_credible = (spres_lo > 0) == (spres_hi > 0),
           sporg_credible = (sporg_lo > 0) == (sporg_hi > 0),
           sign_flip_res  = sign(bym2_beta) != sign(spres_beta),
           retained_pct_per_sd = 100 * spres_beta_per_sd / bym2_beta_per_sd)

  message("\n--- BYM2 published vs spatial+ (per SD of the regressor used) ---")
  print(out |> select(term, bym2_beta_per_sd, spres_beta_per_sd,
                      retained_pct_per_sd, spres_credible, sign_flip_res),
        digits = 3, row.names = FALSE)
  message(sprintf("\nDIC  published %.1f | spatial+ resid-int %.1f | orig-int %.1f",
                  s$ref$dic, fit_r$dic$dic, fit_o$dic$dic))
  message(sprintf("WAIC published %.1f | spatial+ resid-int %.1f | orig-int %.1f",
                  s$ref$waic, fit_r$waic$waic, fit_o$waic$waic))

  write.csv(out, file.path(dir_tab, "24_spatial_plus_coefficients.csv"),
            row.names = FALSE)
  message("\nwrote output/tables/24_spatial_plus_{smooths,coefficients}.csv")

}
environment(.stage) <- new.env(parent = .ckm_env)
.run_stage("24_spatial_plus.R", .stage)

# ---- stage: 25_county_anchor.R ----
.stage <- function() {
  # =============================================================================
  # 25_county_anchor.R
 
  # OUTPUTS  output/tables/25_county_anchor.csv
  #          output/figures/25_county_anchor.png
  # =============================================================================
  suppressMessages({library(dplyr); library(tidyr); library(sf)})

  .root <- getwd()
  if (!dir.exists(file.path(.root, "data")) &&
      dir.exists(file.path(.root, "..", "data")))
    .root <- normalizePath(file.path(.root, ".."))

  dir_raw <- file.path(.root, "data", "raw")
  dir_tab <- file.path(.root, "output", "tables")
  dir_fig <- file.path(.root, "output", "figures")

  # ---------------------------------------------------------------------------
  # 1. Locate an independent county outcome
  # ---------------------------------------------------------------------------
  CAND <- c(wonder = "wonder_county_ckm_mortality.txt",
            cms    = "cms_county_chronic.csv",
            generic = "county_outcome.csv")
  found <- CAND[file.exists(file.path(dir_raw, CAND))]

  if (length(found) == 0) {
    message("\n", strrep("-", 74))
    message("25_county_anchor.R: NO INDEPENDENT COUNTY OUTCOME ON DISK.")
    message(strrep("-", 74))
    message("Checked data/raw/ for:")
    for (f in CAND) message("    ", f)
    message("")
    message("This script deliberately does not substitute a PLACES-derived")
    message("measure. A PLACES county rate would be the same small-area model")
    message("aggregated, which is the very thing being tested -- the test would")
    message("pass by construction and mean nothing.")
    message("")
    message("Download one of the sources named in this file's header, save it")
    message("under data/raw/ with the matching filename, and re-run.")
    message(strrep("-", 74))
    return(invisible(NULL))
  }
  src_kind <- names(found)[1]
  src_file <- file.path(dir_raw, found[1])
  message("county outcome source: ", found[1], "  (", src_kind, ")")

  read_outcome <- function(kind, path) {
    if (kind == "wonder") {
      ln <- readLines(path, warn = FALSE)

      cut <- which(grepl("^\"?---", ln))
      if (length(cut) == 0)
        stop("no '---' notes block: this does not look like a raw WONDER ",
             "export. Re-download and do not re-save it in Excel.",
             call. = FALSE)
      d <- read.delim(text = paste(ln[seq_len(cut[1] - 1L)], collapse = "\n"),
                      stringsAsFactors = FALSE, check.names = TRUE)
      fips_col <- grep("^County.Code$", names(d), value = TRUE)
      rate_col <- grep("Age.Adjusted.Rate|Crude.Rate", names(d), value = TRUE)
      if (!length(fips_col) || !length(rate_col))
        stop("WONDER export lacks County Code and/or a rate column; got: ",
             paste(names(d), collapse = ", "), call. = FALSE)
      out <- data.frame(county_fips = sprintf("%05s", trimws(d[[fips_col]])),
                        rate_raw = trimws(as.character(d[[rate_col[1]]])),
                        stringsAsFactors = FALSE)

      out$suppressed <- !grepl("^[0-9.]+$", out$rate_raw)
      out$rate <- suppressWarnings(as.numeric(out$rate_raw))
      out
    } else {
      d <- read.csv(path, stringsAsFactors = FALSE, colClasses = "character")
      fips_col <- grep("fips|county.code|County.Code", names(d),
                       ignore.case = TRUE, value = TRUE)
      rate_col <- grep("rate|prevalence", names(d), ignore.case = TRUE,
                       value = TRUE)
      if (!length(fips_col) || !length(rate_col))
        stop("CSV needs a FIPS-like and a rate/prevalence-like column; got: ",
             paste(names(d), collapse = ", "), call. = FALSE)
      message("  using columns: fips = ", fips_col[1], ", rate = ", rate_col[1])
      out <- data.frame(
        county_fips = sprintf("%05s", trimws(d[[fips_col[1]]])),
        rate = suppressWarnings(as.numeric(trimws(d[[rate_col[1]]]))),
        stringsAsFactors = FALSE)
      out$suppressed <- is.na(out$rate)
      out
    }
  }

  oc <- read_outcome(src_kind, src_file)
  stopifnot(nrow(oc) > 0)
  if (any(nchar(oc$county_fips) != 5))
    stop("county FIPS are not all 5 characters after padding -- the file was ",
         "probably opened and re-saved in Excel, which strips leading zeros.",
         call. = FALSE)
  message(sprintf("  %d county rows, %d suppressed/unusable (%.1f%%)",
                  nrow(oc), sum(oc$suppressed),
                  100 * mean(oc$suppressed)))

  # ---------------------------------------------------------------------------
  # 2. Population-weighted county aggregation of the estimation sample
  # ---------------------------------------------------------------------------

  s   <- ckm_sample()
  mod <- s$mod
  mod$w <- ifelse(is.na(mod$total_pop) | mod$total_pop == 0, 1, mod$total_pop)

  DEMO <- c("pct_age_65_plus", "pct_hispanic", "ice_income",
            "pct_bachelor_plus", "ice_race")

  cty <- mod |>
    mutate(county_fips = substr(GEOID, 1, 5)) |>
    group_by(county_fips) |>
    summarise(across(all_of(c("ckm_core_pca", DEMO)),
                     ~ stats::weighted.mean(.x, w = w, na.rm = TRUE)),
              pop = sum(w, na.rm = TRUE), n_tracts = dplyr::n(),
              .groups = "drop")
  message(sprintf("counties from the estimation sample: %d", nrow(cty)))

  d <- inner_join(cty, oc[, c("county_fips", "rate", "suppressed")],
                  by = "county_fips") |>
    filter(!suppressed, is.finite(rate)) |>
    drop_na(all_of(c("ckm_core_pca", DEMO, "rate", "pop")))
  message(sprintf("matched, unsuppressed, complete: %d counties (%.1f%% of the ",
                  nrow(d), 100 * nrow(d) / nrow(cty)),
          "estimation sample's counties)")
  if (nrow(d) < 200)
    stop("only ", nrow(d), " counties matched -- check the FIPS format in ",
         basename(src_file), " before trusting anything downstream.",
         call. = FALSE)


  kept <- cty$county_fips %in% d$county_fips
  message(sprintf("  retained counties hold %.1f%% of the sample population",
                  100 * sum(cty$pop[kept]) / sum(cty$pop)))

  # ---------------------------------------------------------------------------
  # 3. The anchor
  # ---------------------------------------------------------------------------
  r_p <- cor(d$ckm_core_pca, d$rate, method = "pearson")
  r_s <- cor(d$ckm_core_pca, d$rate, method = "spearman")
  ct  <- cor.test(d$ckm_core_pca, d$rate, method = "pearson")

  fml_d  <- as.formula(paste("rate ~", paste(DEMO, collapse = " + ")))
  fml_di <- as.formula(paste("rate ~", paste(c(DEMO, "ckm_core_pca"),
                                             collapse = " + ")))
  m_d  <- lm(fml_d,  data = d, weights = pop)
  m_di <- lm(fml_di, data = d, weights = pop)

  r2_d   <- summary(m_d)$r.squared
  r2_di  <- summary(m_di)$r.squared
  incr   <- r2_di - r2_d
  av     <- anova(m_d, m_di)
  p_incr <- av[["Pr(>F)"]][2]
  b_ckm  <- coef(summary(m_di))["ckm_core_pca", ]


  pr <- cor(residuals(lm(fml_d, data = d, weights = pop)),
            residuals(lm(as.formula(paste("ckm_core_pca ~",
                                          paste(DEMO, collapse = " + "))),
                         data = d, weights = pop)))

  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("COUNTY ANCHOR  (n = ", nrow(d), " counties)\n", sep = "")
  cat(strrep("=", 70), "\n")
  cat(sprintf("  raw Pearson  r = %+.3f  [%.3f, %.3f]  p = %.3g\n",
              r_p, ct$conf.int[1], ct$conf.int[2], ct$p.value))
  cat(sprintf("  raw Spearman r = %+.3f\n", r_s))
  cat("\n  -- incremental over demographics (population-weighted) --\n")
  cat(sprintf("  R2  demographics only        %.4f\n", r2_d))
  cat(sprintf("  R2  demographics + CKM index %.4f\n", r2_di))
  cat(sprintf("  incremental R2               %.4f   (F p = %.3g)\n",
              incr, p_incr))
  cat(sprintf("  partial r | demographics     %+.3f\n", pr))
  cat(sprintf("  index beta                   %+.4f (SE %.4f, p = %.3g)\n",
              b_ckm["Estimate"], b_ckm["Std. Error"], b_ckm["Pr(>|t|)"]))
  cat("\n")

  if (is.finite(p_incr) && p_incr < 0.05 && incr >= 0.01) {
    cat("  READING: the index adds signal beyond demographics.\n")
  } else {
    cat("  READING: the index adds little or nothing beyond demographics.\n")
    cat("  This is the adverse result. It does NOT invalidate the spatial\n")
    cat("  models, but it does mean the index cannot be described as a health\n")
    cat("  measure distinct from its demographic inputs, and s4.1 must say so.\n")
  }
  cat(strrep("=", 70), "\n")

  # ---------------------------------------------------------------------------
  # 4. Write out
  # ---------------------------------------------------------------------------
  out <- data.frame(
    quantity = c("n_counties", "pct_sample_pop_retained", "pct_suppressed",
                 "pearson_r", "pearson_lo", "pearson_hi", "pearson_p",
                 "spearman_r", "r2_demographics", "r2_demographics_plus_index",
                 "incremental_r2", "incremental_F_p", "partial_r_given_demo",
                 "index_beta", "index_se", "index_p"),
    value = c(nrow(d), 100 * sum(cty$pop[kept]) / sum(cty$pop),
              100 * mean(oc$suppressed),
              r_p, ct$conf.int[1], ct$conf.int[2], ct$p.value, r_s,
              r2_d, r2_di, incr, p_incr, pr,
              b_ckm["Estimate"], b_ckm["Std. Error"], b_ckm["Pr(>|t|)"]),
    source = basename(src_file), source_kind = src_kind,
    row.names = NULL)
  write.csv(out, file.path(dir_tab, "25_county_anchor.csv"), row.names = FALSE)

  png(file.path(dir_fig, "25_county_anchor.png"), width = 1600, height = 1400,
      res = 200)
  op <- par(mar = c(4.5, 4.5, 3, 1))

  symbols(d$ckm_core_pca, d$rate, circles = sqrt(d$pop), inches = 0.16,
          fg = grDevices::rgb(0, 0, 0, 0.25), bg = grDevices::rgb(0.2, 0.4, 0.7, 0.25),
          xlab = "County CKM index (population-weighted tract mean)",
          ylab = paste0("Independent county outcome (", src_kind, ")"),
          main = sprintf("County anchor: r = %+.3f, incremental R2 = %.3f",
                         r_p, incr))
  abline(lm(rate ~ ckm_core_pca, data = d, weights = pop), col = "firebrick", lwd = 2)
  par(op); invisible(dev.off())

  message("\nwrote output/tables/25_county_anchor.csv")
  message("wrote output/figures/25_county_anchor.png")

}
environment(.stage) <- new.env(parent = .ckm_env)
.run_stage("25_county_anchor.R", .stage)


# ---- stage: 26_lila_decomposition.R ----
.stage <- function() {
  # ===========================================================================
  # 26_lila_decomposition.R -- split the binary LILA flag into its factors
  
  # OUTPUTS  output/tables/26_lila_decomposition_coefficients.csv
  #          output/tables/26_lila_conjunction_check.csv

  # ===========================================================================
  suppressMessages({library(dplyr)})

  .root <- getwd()
  if (!dir.exists(file.path(.root, "data")) &&
      dir.exists(file.path(.root, "..", "data")))
    .root <- normalizePath(file.path(.root, ".."))
  dir_tab <- file.path(.root, "output", "tables")
  DO_SWEEP <- TRUE

  s   <- ckm_sample()
  mod <- s$mod

  NEED <- c("LowIncomeTracts", "LATracts_half", "LATracts1",
            "LATracts10", "LATracts20", "LILATracts_Vehicle")
  miss <- setdiff(NEED, names(mod))
  if (length(miss)) {
    message("  MISSING from the estimation sample: ",
            paste(miss, collapse = ", "))
    message("  delete data/processed/01f_food_tract.rds and re-run section 1f;")
    message("  it rebuilds 02_ckm_index.rds carrying these columns.")
    return(invisible(NULL))
  }


  b01 <- function(x, nm) {
    v  <- suppressWarnings(as.integer(as.character(x)))
    na <- sum(is.na(v))
    if (na)
      stop(sprintf(paste0("%s: %d of %d values are NA or non-numeric. FARA ",
                          "does not cover these tracts uniformly; decide on ",
                          "a recode before fitting rather than letting INLA ",
                          "drop them silently."),
                   nm, na, length(v)))
    bad <- setdiff(unique(v), c(0L, 1L))
    if (length(bad))
      stop(sprintf("%s: expected a 0/1 flag, also saw %s",
                   nm, paste(sort(bad), collapse = ", ")))
    v
  }
  for (v in c(NEED, "LILATracts_1And10")) mod[[v]] <- b01(mod[[v]], v)


  recon <- as.integer(mod$LowIncomeTracts == 1L &
                      (mod$LATracts1 == 1L | mod$LATracts10 == 1L))
  agree <- mean(recon == mod$LILATracts_1And10)
  xt    <- table(reconstructed = recon, published = mod$LILATracts_1And10)
  message(sprintf("\n  LILA = LowIncome & (LA1 | LA10) reproduces %.4f of tracts",
                  agree))
  print(xt)
  write.csv(as.data.frame(xt),
            file.path(dir_tab, "26_lila_conjunction_check.csv"),
            row.names = FALSE)

  message(sprintf("  prevalence: LILA %.3f | LowIncome %.3f | LA1 %.3f | LA10 %.3f",
                  mean(mod$LILATracts_1And10), mean(mod$LowIncomeTracts),
                  mean(mod$LATracts1), mean(mod$LATracts10)))


  OTHER   <- setdiff(ENV_PRED, "LILATracts_1And10")
  bas_mn  <- c(M1_PRED, OTHER)
  bas_int <- paste0("pct_age_65_plus:", OTHER)

  fit_one <- function(food_mn, food_int, label) {
    rhs <- paste(c(bas_mn, food_mn, bas_int, food_int), collapse = " + ")
    fml <- as.formula(paste0(
      "ckm_core_pca ~ ", rhs,
      " + f(idarea, model = 'bym2', graph = '", s$graph_file, "',",
      " scale.model = TRUE,",
      " hyper = list(phi  = list(prior = 'pc', param = c(0.5, 0.5)),",
      "              prec = list(prior = 'pc.prec', param = c(1, 0.01))))"))
    message(sprintf("\n  fitting %s (n = %d)...", label, nrow(mod)))
    t0 <- Sys.time()
    f <- INLA::inla(fml, data = mod, family = "gaussian",
                    control.predictor = list(compute = TRUE),
                    control.compute   = list(dic = TRUE, waic = TRUE,
                                             return.marginals.predictor = FALSE),
                    control.inla      = list(strategy = "adaptive",
                                             int.strategy = "eb"),
                    verbose = FALSE)
    message(sprintf("    done in %.1f min",
                    as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    sf <- f$summary.fixed
    data.frame(spec = label, term = rownames(sf),
               beta = sf[, "mean"], lo95 = sf[, "0.025quant"],
               hi95 = sf[, "0.975quant"],
               dic = f$dic$dic, waic = f$waic$waic, row.names = NULL)
  }

  specs <- list(
    list("LILATracts_1And10", "pct_age_65_plus:LILATracts_1And10",
         "A_published_binary_LILA"),

    list(c("LowIncomeTracts", "LATracts1", "LowIncomeTracts:LATracts1"),
         c("pct_age_65_plus:LowIncomeTracts", "pct_age_65_plus:LATracts1",
           "pct_age_65_plus:LowIncomeTracts:LATracts1"),
         "B_decomposed_income_x_access1"))

  if (DO_SWEEP)
    for (v in c("LATracts_half", "LATracts10", "LATracts20"))
      specs[[length(specs) + 1L]] <- list(
        c("LowIncomeTracts", v, paste0("LowIncomeTracts:", v)),
        c("pct_age_65_plus:LowIncomeTracts", paste0("pct_age_65_plus:", v),
          paste0("pct_age_65_plus:LowIncomeTracts:", v)),
        paste0("C_distance_", v))
  if (DO_SWEEP)
    specs[[length(specs) + 1L]] <- list(
      "LILATracts_Vehicle", "pct_age_65_plus:LILATracts_Vehicle",
      "D_vehicle_access")

  out <- do.call(rbind, lapply(specs,
                               function(p) fit_one(p[[1]], p[[2]], p[[3]])))
  out$credible <- sign(out$lo95) == sign(out$hi95)

  f_out <- file.path(dir_tab, "26_lila_decomposition_coefficients.csv")
  write.csv(out, f_out, row.names = FALSE)
  message("\n  wrote ", f_out)

  cat("\n  food-access terms by specification:\n")
  print(out |>
          filter(grepl("LILA|LowIncome|LATracts", term)) |>
          select(spec, term, beta, lo95, hi95, credible),
        row.names = FALSE)

  cat("\n  fit:\n")
  print(out |> distinct(spec, dic, waic), row.names = FALSE)
  invisible(NULL)
}
environment(.stage) <- new.env(parent = .ckm_env)
.run_stage("26_lila_decomposition.R", .stage)

# ---- stage: 27_heat_interannual.R ----
.stage <- function() {
  # ===========================================================================
  # 27_heat_interannual.R -- does the PRISM averaging window matter?
 
  # OUTPUT   output/tables/27_heat_interannual_stability.csv

  # ===========================================================================
  if (!requireNamespace("terra", quietly = TRUE) ||
      !requireNamespace("sf", quietly = TRUE)) {
    message("  terra/sf not available - skipping.")
    return(invisible(NULL))
  }

  .root <- getwd()
  if (!dir.exists(file.path(.root, "data")) &&
      dir.exists(file.path(.root, "..", "data")))
    .root <- normalizePath(file.path(.root, ".."))
  dir_tab <- file.path(.root, "output", "tables")
  d_prism <- file.path(.root, "data", "raw", "prism")
  f_geo   <- file.path(.root, "data", "processed", "01_tracts_geo.rds")

  if (!file.exists(f_geo)) {
    message("  01_tracts_geo.rds absent - run section 1 first. Skipping.")
    return(invisible(NULL))
  }


  tif_for <- function(yr) {
    out <- character(0)
    for (mo in 5:9) {
      f <- list.files(file.path(d_prism, sprintf("tmax_%d%02d", yr, mo)),
                      pattern = "[.](tif|bil)$", full.names = TRUE,
                      ignore.case = TRUE)
      if (!length(f)) return(NULL)
      out <- c(out, f[1])
    }
    out
  }
  t19 <- tif_for(2019); t20 <- tif_for(2020)
  if (is.null(t19) || is.null(t20)) {
    message("  PRISM May-Sep grids for 2019 and 2020 are not both on disk ",
            "- skipping the window check.")
    return(invisible(NULL))
  }

  geo <- readRDS(f_geo)


  extract_warm <- function(tifs, trv) {
    r  <- terra::app(terra::rast(tifs), fun = mean, na.rm = TRUE)
    v  <- terra::extract(r, trv, fun = mean, na.rm = TRUE)[, 2]
    na_i <- which(is.na(v))
    if (length(na_i)) {
      sub <- trv[na_i, ]
      has <- which(terra::expanse(sub, unit = "km") > 0)
      if (length(has)) {
        ex <- terra::extract(r, terra::centroids(sub[has, ]))
        v[na_i[has]] <- ex[[2]][match(seq_along(has), ex[[1]])]
      }
    }
    v
  }

  trv <- terra::vect(sf::st_transform(geo, terra::crs(terra::rast(t19[1]))))
  w19 <- extract_warm(t19, trv)
  w20 <- extract_warm(t20, trv)
  wbo <- extract_warm(c(t19, t20), trv)

  k <- stats::complete.cases(w19, w20, wbo)
  if (sum(k) < 1000) {
    message("  only ", sum(k), " tracts with all three windows - skipping.")
    return(invisible(NULL))
  }
  a <- w19[k]; b <- w20[k]; p <- wbo[k]
  d <- b - a

  out <- data.frame(
    metric = c("r_2019_2020", "r_2020_pooled", "r_2019_pooled",
               "spearman_2019_2020", "mean_2019_C", "mean_2020_C",
               "mean_pooled_C", "mean_diff_C", "sd_diff_C",
               "p5_diff_C", "p95_diff_C", "max_abs_diff_C",
               "sd_between_tract_pooled_C", "noise_to_signal_ratio",
               "n_tracts"),
    value = c(stats::cor(a, b), stats::cor(b, p), stats::cor(a, p),
              stats::cor(a, b, method = "spearman"),
              mean(a), mean(b), mean(p), mean(d), stats::sd(d),
              stats::quantile(d, .05), stats::quantile(d, .95),
              max(abs(d)), stats::sd(p), stats::sd(d) / stats::sd(p),
              sum(k)))
  f <- file.path(dir_tab, "27_heat_interannual_stability.csv")
  utils::write.csv(out, f, row.names = FALSE)

  cat("\n  PRISM warm-season window check (n = ", sum(k), " tracts)\n", sep = "")
  cat(sprintf("    r(2019, 2020)   = %.4f   <- independent years\n",
              stats::cor(a, b)))
  cat(sprintf("    r(2020, pooled) = %.4f   <- circular: pooled contains 2020\n",
              stats::cor(b, p)))
  cat(sprintf(paste0("    year-to-year difference: SD %.3f C vs between-tract ",
                     "SD %.3f C (%.0f%%)\n"),
              stats::sd(d), stats::sd(p), 100 * stats::sd(d) / stats::sd(p)))
  cat("    -> the averaging window is not immaterial; tmax_warm pools ",
      "2019+2020.\n", sep = "")
  cat("  saved: ", f, "\n", sep = "")
  invisible(NULL)
}
environment(.stage) <- new.env(parent = .ckm_env)
.run_stage("27_heat_interannual.R", .stage)

# ---- stage: 28_extended_bym2.R ----
.stage <- function() {
  # =============================================================================
  # 28_extended_bym2.R — BYM2 (Model 1 spec) on the EXTENDED 7-indicator index
  # -----------------------------------------------------------------------------
  # PURPOSE. Krishnan et al. (2026) model coronary heart disease, stroke and CKD
  # (AHA Stage 3-4 endpoints) with non-spatial regressions. Our primary index is
  # Stage 1-2 by construction. §2.1 notes an extended 7-indicator score (Core +
  # CHD + STROKE) computed at index construction. This stage fits the PRIMARY
  # Model 1 (behaviour-excluded) BYM2 specification on that extended index, so
  # the paper can answer Krishnan's outcome set directly on our spatial
  # machinery: do the fixed effects and the residual Southern geography hold
  # when the Stage 4 endpoints are folded in?
  #
  # DESIGN. Identical to stage 12 (12_bym2_posterior_residual): same estimation-
  # sample cascade, same k = 8 symmetric knn graph rebuilt on this sample's
  # ordering, same BYM2 PC priors (phi ~ pc(0.5, 0.5), prec ~ pc.prec(1, 0.01)),
  # same control.inla (strategy = "adaptive", int.strategy = "eb"). Only the
  # outcome changes: ckm_ext_pca instead of ckm_core_pca.
  #
  # INPUTS.
  #   data/processed/02_ckm_index.rds              analytic + both indices
  #   data/processed/01_tracts_geo.rds             tract geometry (ALAND, coords)
  #   data/processed/06_6g_inla_bym2_nobehav.rds   published Model 1 (core index)
  #   data/processed/12_bym2_refit_model1.rds      Model 1 refit incl. random
  #                                                field + GEOID (stage 12)
  # OUTPUTS.
  #   data/processed/28_bym2_extended.rds
  #   output/tables/28_extended_bym2_coefficients.csv   ext vs core Model 1,
  #                                                     raw and per-outcome-SD
  #   output/tables/28_extended_bym2_summary.csv        Moran / phi / field
  #                                                     correlation / South
  #                                                     contrast / DIC / WAIC
  #   output/logs/28_extended_bym2.log
  #
  # RUNTIME. One INLA BYM2 fit at n = 69,530; expect 30-90 min.
  # =============================================================================
  suppressMessages({
    library(dplyr); library(tidyr); library(sf); library(spdep)
  })
  stopifnot(requireNamespace("INLA", quietly = TRUE))

  root <- getwd()
  if (!dir.exists(file.path(root, "data")) &&
      dir.exists(file.path(root, "..", "data"))) root <- normalizePath(file.path(root, ".."))
  stopifnot(dir.exists(file.path(root, "data", "processed")))
  dir_proc <- file.path(root, "data", "processed")
  dir_tab  <- file.path(root, "output", "tables")
  dir_log  <- file.path(root, "output", "logs")
  dir.create(dir_log, recursive = TRUE, showWarnings = FALSE)

  log_file <- file.path(dir_log, "28_extended_bym2.log")
  log_con  <- file(log_file, open = "wt")
  sink(log_con, split = TRUE); sink(log_con, type = "message")

  CORE_PRED <- c("pct_age_65_plus", "pct_hispanic", "ice_income",
                 "pct_bachelor_plus", "ice_race", "ACCESS2", "CHECKUP",
                 "ruca_class", "CSMOKING", "LPA")
  ENV_PRED  <- c("pm25_annual", "o3_8hrmax_4thmax", "LILATracts_1And10",
                 "tmax_warm", "walk_index")
  CENTER    <- c("pct_age_65_plus", "pm25_annual", "o3_8hrmax_4thmax",
                 "tmax_warm", "walk_index")
  BEHAV     <- c("ACCESS2", "CHECKUP", "CSMOKING", "LPA")
  M1_PRED   <- setdiff(CORE_PRED, BEHAV)

  # Census Bureau South region (17 state FIPS incl. DC)
  SOUTH_FIPS <- c("01","05","10","11","12","13","21","22","24","28",
                  "37","40","45","47","48","51","54")

  # ---------------------------------------------------------------------------
  # 1. Rebuild the estimation sample (identical to stage 12)
  # ---------------------------------------------------------------------------
  an <- readRDS(file.path(dir_proc, "02_ckm_index.rds"))
  if (inherits(an, "sf")) an <- st_drop_geometry(an)
  geo <- readRDS(file.path(dir_proc, "01_tracts_geo.rds"))
  d   <- an |> left_join(st_drop_geometry(geo)[, c("GEOID", "ALAND")], by = "GEOID")

  conus <- d |>
    filter(!substr(GEOID, 1, 2) %in% c("02", "15", "60", "66", "69", "72", "78")) |>
    filter(ALAND > 0, !is.na(ckm_core_pca))
  message(sprintf("CONUS tracts with index: %d", nrow(conus)))

  mod <- conus |>
    mutate(ruca_class = factor(ruca_class,
             levels = c("Metro", "Micropolitan", "Small town", "Rural"))) |>
    drop_na(ckm_core_pca, pct_age_65_plus, ice_race, ice_income, ruca_class,
            pct_hispanic, pct_black, pct_bachelor_plus, median_income) |>
    drop_na(all_of(c("ckm_core_pca", CORE_PRED, ENV_PRED)))

  m1p <- readRDS(file.path(dir_proc, "06_6g_inla_bym2_nobehav.rds"))
  stopifnot(nrow(mod) == m1p$n)                     # same 69,530 sample as Model 1
  stopifnot(!any(is.na(mod$ckm_ext_pca)))           # extended index adds NO attrition
  message(sprintf("Estimation sample: n = %d (identical to published Model 1)",
                  nrow(mod)))

  mod$o3_8hrmax_4thmax <- mod$o3_8hrmax_4thmax * 100     # ppm -> per-10-ppb
  for (v in CENTER) mod[[v]] <- mod[[v]] - mean(mod[[v]], na.rm = TRUE)

  # ---------------------------------------------------------------------------
  # 2. Neighbour graph on this sample's ordering (nb2INLA_fast, as stage 12)
  # ---------------------------------------------------------------------------
  mod_geo <- geo[match(mod$GEOID, geo$GEOID), ]
  stopifnot(identical(mod_geo$GEOID, mod$GEOID))
  coords <- suppressWarnings(
    st_coordinates(st_centroid(mod_geo, of_largest_polygon = TRUE)))
  nb <- knn2nb(knearneigh(coords, k = 8), sym = TRUE)
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  graph_file <- file.path(dir_proc, "28_bym2_graph.adj")
  nb2INLA_fast <- function(file, nb) {
    crd <- spdep::card(nb)
    writeLines(c(as.character(length(nb)),
      vapply(seq_along(nb), function(i)
        if (crd[i] == 0L) paste(i, 0)
        else paste(c(i, crd[i], nb[[i]]), collapse = " "),
        character(1))), file)
  }
  nb2INLA_fast(graph_file, nb)
  mod$idarea <- seq_len(nrow(mod))
  message(sprintf("Neighbour graph: %d nodes, mean %.2f neighbours",
                  length(nb), mean(lengths(nb))))

  y_ext  <- mod$ckm_ext_pca
  y_core <- mod$ckm_core_pca
  message(sprintf("Extended outcome: sd = %.4f | core sd = %.4f | r = %.4f",
                  sd(y_ext), sd(y_core), cor(y_ext, y_core)))

  # ---------------------------------------------------------------------------
  # 3. Fit Model 1 spec on the extended index
  # ---------------------------------------------------------------------------
  rhs <- paste(c(M1_PRED, ENV_PRED, paste0("pct_age_65_plus:", ENV_PRED)),
               collapse = " + ")
  fml <- as.formula(paste0(
    "ckm_ext_pca ~ ", rhs,
    " + f(idarea, model = 'bym2', graph = '", graph_file, "',",
    " scale.model = TRUE,",
    " hyper = list(phi  = list(prior = 'pc', param = c(0.5, 0.5)),",
    "              prec = list(prior = 'pc.prec', param = c(1, 0.01))))"))
  message(sprintf("\nFitting extended-index BYM2, Model 1 spec (n = %d)...",
                  nrow(mod)))
  t0 <- Sys.time()
  fit <- INLA::inla(
    fml, data = mod, family = "gaussian",
    control.predictor = list(compute = TRUE),
    control.compute   = list(dic = TRUE, waic = TRUE,
                             return.marginals.predictor = FALSE),
    control.inla      = list(strategy = "adaptive", int.strategy = "eb"),
    verbose = FALSE)
  message(sprintf("  done in %.1f min",
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))

  # ---------------------------------------------------------------------------
  # 4. Coefficient comparison: extended vs published core Model 1
  # ---------------------------------------------------------------------------
  pf <- m1p$summary_fixed            # published core Model 1 (same spec, same n)
  rf <- fit$summary.fixed
  stopifnot(setequal(rownames(pf), rownames(rf)))
  sh <- rownames(rf)

  sd_ext  <- sd(y_ext)
  sd_core <- sd(y_core)

  comp <- data.frame(
    term          = sh,
    ext_mean      = rf[sh, "mean"],
    ext_lo        = rf[sh, "0.025quant"],
    ext_hi        = rf[sh, "0.975quant"],
    core_mean     = pf[sh, "mean"],
    core_lo       = pf[sh, "0.025quant"],
    core_hi       = pf[sh, "0.975quant"],
    row.names = NULL) |>
    mutate(
      ext_perSD      = ext_mean  / sd_ext,     # per outcome-SD, comparable scale
      core_perSD     = core_mean / sd_core,
      ext_credible   = ifelse(ext_lo  > 0 | ext_hi  < 0, "Yes", "No"),
      core_credible  = ifelse(core_lo > 0 | core_hi < 0, "Yes", "No"),
      sign_agrees    = ifelse(sign(ext_mean) == sign(core_mean), "Yes", "No"))

  nonint <- comp |> filter(term != "(Intercept)")
  message(sprintf("\nSign agreement (excl. intercept): %d / %d terms",
                  sum(nonint$sign_agrees == "Yes"), nrow(nonint)))
  message(sprintf("Per-SD coefficient correlation (excl. intercept): r = %.4f",
                  cor(nonint$ext_perSD, nonint$core_perSD)))
  print(comp |> mutate(across(where(is.numeric), ~ round(.x, 4))),
        row.names = FALSE)

  # ---------------------------------------------------------------------------
  # 5. Residual spatial structure: does the Southern geography hold?
  # ---------------------------------------------------------------------------
  n  <- nrow(mod)
  fv <- fit$summary.fitted.values
  eta <- fv$mean[seq_len(n)]

  X  <- model.matrix(as.formula(paste("~", rhs)), data = mod)
  b  <- fit$summary.fixed
  stopifnot(setequal(colnames(X), rownames(b)))
  Xb <- as.vector(X[, rownames(b), drop = FALSE] %*% b$mean)

  r_post <- y_ext - eta
  r_fix  <- y_ext - Xb
  if (sd(r_post) > sd(r_fix))
    stop("posterior residual SD exceeds fixed-only residual SD", call. = FALSE)

  mi_y    <- moran.test(y_ext,  lw, zero.policy = TRUE)
  mi_fix  <- moran.test(r_fix,  lw, zero.policy = TRUE)
  mi_post <- moran.test(r_post, lw, zero.policy = TRUE)

  # BYM2 total random effect = first n rows of summary.random$idarea
  re_ext <- fit$summary.random$idarea$mean[seq_len(n)]

  # Published Model 1 random field from the stage-12 refit (carries GEOID)
  ref1   <- readRDS(file.path(dir_proc, "12_bym2_refit_model1.rds"))
  stopifnot(!is.null(ref1$summary_random), !is.null(ref1$GEOID))
  re_core_full <- ref1$summary_random$mean[seq_len(length(ref1$GEOID))]
  ix <- match(mod$GEOID, ref1$GEOID)
  stopifnot(!any(is.na(ix)))
  re_core <- re_core_full[ix]

  south <- substr(mod$GEOID, 1, 2) %in% SOUTH_FIPS
  message(sprintf("\nSouth region tracts: %d of %d (%.1f%%)",
                  sum(south), n, 100 * mean(south)))

  field_r        <- cor(re_ext, re_core)
  south_ext      <- mean(re_ext[south])  - mean(re_ext[!south])
  south_core     <- mean(re_core[south]) - mean(re_core[!south])
  south_ext_sd   <- south_ext  / sd_ext     # in outcome-SD units
  south_core_sd  <- south_core / sd_core

  phi_row <- fit$summary.hyperpar[grep("^Phi for idarea$",
                                       rownames(fit$summary.hyperpar)), ]

  message(sprintf("Moran's I: outcome %.4f | fixed-only resid %.4f | posterior resid %.4f",
                  mi_y$estimate[[1]], mi_fix$estimate[[1]], mi_post$estimate[[1]]))
  message(sprintf("phi (spatial share): %.4f [%.4f, %.4f]",
                  phi_row$mean, phi_row$`0.025quant`, phi_row$`0.975quant`))
  message(sprintf("Random-field correlation, extended vs core Model 1: r = %.4f",
                  field_r))
  message(sprintf("South minus non-South mean random effect: extended %+.4f (%.3f SD) | core %+.4f (%.3f SD)",
                  south_ext, south_ext_sd, south_core, south_core_sd))

  summary_df <- data.frame(
    model                 = "bym2_extended7_model1spec",
    n                     = n,
    outcome_sd            = sd_ext,
    core_outcome_sd       = sd_core,
    index_cor_ext_core    = cor(y_ext, y_core),
    moran_outcome         = mi_y$estimate[[1]],
    moran_resid_fixedonly = mi_fix$estimate[[1]],
    moran_resid_posterior = mi_post$estimate[[1]],
    phi                   = phi_row$mean,
    phi_lo                = phi_row$`0.025quant`,
    phi_hi                = phi_row$`0.975quant`,
    field_cor_ext_core    = field_r,
    south_excess_ext      = south_ext,
    south_excess_ext_sd   = south_ext_sd,
    south_excess_core     = south_core,
    south_excess_core_sd  = south_core_sd,
    sign_agreement        = sprintf("%d/%d",
                                    sum(nonint$sign_agrees == "Yes"), nrow(nonint)),
    perSD_coef_cor        = cor(nonint$ext_perSD, nonint$core_perSD),
    dic                   = fit$dic$dic,
    waic                  = fit$waic$waic,
    row.names = NULL)

  # ---------------------------------------------------------------------------
  # 6. Save
  # ---------------------------------------------------------------------------
  write.csv(comp,       file.path(dir_tab, "28_extended_bym2_coefficients.csv"),
            row.names = FALSE)
  write.csv(summary_df, file.path(dir_tab, "28_extended_bym2_summary.csv"),
            row.names = FALSE)
  saveRDS(list(
    summary_fixed    = fit$summary.fixed,
    summary_random   = fit$summary.random$idarea,
    summary_hyperpar = fit$summary.hyperpar,
    dic = fit$dic$dic, waic = fit$waic$waic,
    n = n, GEOID = mod$GEOID,
    formula = paste(deparse(fml), collapse = " ")),
    file.path(dir_proc, "28_bym2_extended.rds"))

  cat("\n--- written ---\n")
  cat("  ", file.path(dir_tab, "28_extended_bym2_coefficients.csv"), "\n")
  cat("  ", file.path(dir_tab, "28_extended_bym2_summary.csv"), "\n")
  cat("  ", file.path(dir_proc, "28_bym2_extended.rds"), "\n")
  sink(type = "message"); sink(); close(log_con)
}
.run_stage("28_extended_bym2.R", .stage)

# ---- stage: 29_recompute_deposit_gaps.R ----
.stage <- function() {
  # 29_recompute_deposit_gaps.R  (added 2026-09-23)
  #
  # Recomputes, from cached PUBLISHED objects only (no model is refit), three
  # quantities the manuscript reports that no deposited table carried:
  #   (1) residual Moran's I of the full-covariate (focal-only) OLS -- the
  #       correct comparator for the ESF and focal+Wx fits (Supplemental Results,
  #       ESF cross-check and Wx paragraphs; Table S4 Panel B). The value printed
  #       there before 2026-09-23 (0.470) belongs to the six-predictor baseline OLS;
  #   (2) South / non-South summaries of the published Model 1 and Model 2 BYM2
  #       random effects ("Post-adjustment Southern excess", Supplemental Results);
  #   (3) state means of those random effects.
  # Parts added 2026-09-27: (4) the central-90% span of % age >= 65; (5) behavioral
  # prevalence by burden quintile; (6) the stage-12 refits' drift against the current
  # published fits (coefficients, DIC, WAIC, BYM2 mixing parameter); (7) the baseline
  # OLS coefficients shown in Table S1.
  #
  # Run from the project root:  Rscript code/29_recompute_deposit_gaps.R
  # Every run re-checks itself: the full OLS R^2 must reproduce 0.8661, and the
  # baseline and ESF residual Moran's I must reproduce 0.4702 and 0.4475;
  # otherwise the script stops before writing anything.
  suppressMessages({ library(spdep); library(sf) })
  dir_proc <- "data/processed"; dir_tbl <- "output/tables"
  rd <- function(f) readRDS(file.path(dir_proc, f))

  # ---- (1) full-covariate OLS on the ESF's own design matrix and coordinates --
  esf  <- rd("06_spmoran_resf.rds"); fit <- esf$fit
  base <- rd("05_ols_baseline.rds")
  X <- fit$other$x; y <- as.numeric(fit$other$y); co <- fit$other$coords
  stopifnot(isTRUE(all.equal(y, as.numeric(model.response(model.frame(base$model))),
                             tolerance = 1e-10)))            # same rows, same order
  ols <- lm.fit(X, y)
  r2_full <- 1 - sum(ols$residuals^2) / sum((y - mean(y))^2)
  nb <- knn2nb(knearneigh(co, k = 8), sym = TRUE)            # as in the main script
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  mi <- function(r) unname(moran.test(r, lw, zero.policy = TRUE,
                                      randomisation = TRUE)$estimate[1])
  m_base <- mi(residuals(base$model)); m_esf <- mi(as.numeric(fit$resid))
  m_full <- mi(ols$residuals)
  stopifnot(round(r2_full, 4) == 0.8661, round(m_base, 4) == 0.4702,
            round(m_esf, 4) == 0.4475)
  write.csv(data.frame(
    model   = c("OLS baseline (six predictors)", "OLS full covariate set (focal-only)",
                "spmoran ESF (full covariate set)"),
    n       = length(y),
    r_squared = c(base$summary$r.squared, r2_full, NA),
    moran_I = c(m_base, m_full, m_esf)),
    file.path(dir_tbl, "29_residual_moran_full_ols.csv"), row.names = FALSE)

  # ---- (2)-(3) regional and state summaries of the published random effects --
  geo <- sf::st_drop_geometry(rd("01_tracts_geo.rds"))
  geo$GEOID <- trimws(as.character(geo$GEOID))
  south <- c("01","05","10","11","12","13","21","22","24","28","37","40","45",
             "47","48","51","54")                             # as in the main script
  re_of <- function(f) {
    o <- rd(f); n <- length(o$GEOID)
    d <- data.frame(GEOID = trimws(as.character(o$GEOID)),
                    re = o$summary_random$mean[seq_len(n)])   # total BYM2 effect
    d <- merge(d, geo[, c("GEOID", "ALAND")], by = "GEOID")
    d[!substr(d$GEOID, 1, 2) %in% c("60","66","69","72","78") & d$ALAND > 0, ]
  }
  fits <- c("Model 1" = "06_6g_inla_bym2_nobehav.rds", "Model 2" = "06_inla_bym2.rds")
  reg <- do.call(rbind, lapply(names(fits), function(m) {
    d <- re_of(fits[[m]]); s <- substr(d$GEOID, 1, 2) %in% south
    data.frame(model = m, n = nrow(d),
               mean_south = mean(d$re[s]), pct_pos_south = 100 * mean(d$re[s] > 0),
               mean_non_south = mean(d$re[!s]), pct_pos_non_south = 100 * mean(d$re[!s] > 0))
  }))
  st <- do.call(rbind, lapply(names(fits), function(m) {
    d <- re_of(fits[[m]]); d$state_fips <- substr(d$GEOID, 1, 2)
    a <- aggregate(re ~ state_fips, d, mean); a$n_tracts <- as.integer(table(d$state_fips)[a$state_fips])
    a$model <- m; a[order(-a$re), c("model", "state_fips", "n_tracts", "re")]
  }))
  names(st)[names(st) == "re"] <- "mean_random_effect"
  # text state names (the verifier's drift self-test rescales every numeric cell,
  # so lookups must key on text, not on numeric-looking FIPS codes)
  fips <- c("01"="Alabama","04"="Arizona","05"="Arkansas","06"="California","08"="Colorado",
    "09"="Connecticut","10"="Delaware","11"="District of Columbia","12"="Florida","13"="Georgia",
    "16"="Idaho","17"="Illinois","18"="Indiana","19"="Iowa","20"="Kansas","21"="Kentucky",
    "22"="Louisiana","23"="Maine","24"="Maryland","25"="Massachusetts","26"="Michigan",
    "27"="Minnesota","28"="Mississippi","29"="Missouri","30"="Montana","31"="Nebraska",
    "32"="Nevada","33"="New Hampshire","34"="New Jersey","35"="New Mexico","36"="New York",
    "37"="North Carolina","38"="North Dakota","39"="Ohio","40"="Oklahoma","41"="Oregon",
    "42"="Pennsylvania","44"="Rhode Island","45"="South Carolina","46"="South Dakota",
    "47"="Tennessee","48"="Texas","49"="Utah","50"="Vermont","51"="Virginia","53"="Washington",
    "54"="West Virginia","55"="Wisconsin","56"="Wyoming")
  st$state <- unname(fips[st$state_fips]); stopifnot(!anyNA(st$state))
  st <- st[, c("model", "state", "state_fips", "n_tracts", "mean_random_effect")]
  write.csv(reg, file.path(dir_tbl, "29_south_random_effect_summary.csv"), row.names = FALSE)
  write.csv(st,  file.path(dir_tbl, "29_state_random_effect_means.csv"),   row.names = FALSE)

  # ---- (4) central-90% span of % age >= 65 on the estimation sample (§3.3 translation)
  ck <- rd("02_ckm_index.rds")
  a  <- ck$pct_age_65_plus[match(base$GEOID, ck$GEOID)]
  stopifnot(!anyNA(a), length(a) == 69530L)
  q <- stats::quantile(a, c(0.05, 0.95), type = 7, names = FALSE)
  write.csv(data.frame(variable = "pct_age_65_plus", n = length(a), p05 = q[1], p95 = q[2], span = q[2] - q[1]),
            file.path(dir_tbl, "29_age_distribution.csv"), row.names = FALSE)

  # ---- (5) behavioral prevalence by burden quintile (added 2026-09-27; §4.4 evidence)
  #          Mean crude prevalence of no leisure-time physical activity (LPA) and current
  #          smoking (CSMOKING) in each fifth of the 69,530-tract estimation sample,
  #          ordered by the CKM burden index.
  beh <- ck[match(base$GEOID, ck$GEOID), c("ckm_core_pca", "LPA", "CSMOKING")]
  stopifnot(!anyNA(beh), nrow(beh) == 69530L)
  rk <- rank(beh$ckm_core_pca, ties.method = "first")
  beh$quintile <- paste0("Q", ceiling(rk / (nrow(beh) / 5)))
  bq <- do.call(rbind, lapply(paste0("Q", 1:5), function(qq) {
    d <- beh[beh$quintile == qq, ]
    data.frame(quintile = qq, n = nrow(d), LPA_mean = mean(d$LPA), CSMOKING_mean = mean(d$CSMOKING))
  }))
  write.csv(bq, file.path(dir_tbl, "29_behavior_by_burden_quintile.csv"), row.names = FALSE)

  # ---- (6) cross-session drift of the stage-12 refits against the CURRENT published fits
  #          (added 2026-09-27; Table S10 note, §S2.4). 12_bym2_refit_validation.csv compares
  #          the refits with the published fits as they stood when stage 12 ran (2026-08-29);
  #          both published models were refit on 2026-08-31, so that file's "published" column
  #          no longer matches Table 3. This recomputes the comparison from the cached objects.
  pairs <- list("Model 1" = c(pub = "06_6g_inla_bym2_nobehav.rds", refit = "12_bym2_refit_model1.rds"),
                "Model 2" = c(pub = "06_inla_bym2.rds",            refit = "12_bym2_refit_model2.rds"))
  dterm <- list(); dsum <- list()
  for (m in names(pairs)) {
    P <- rd(pairs[[m]][["pub"]]); R <- rd(pairs[[m]][["refit"]])
    stopifnot(P$n == 69530L, R$n == 69530L,
              setequal(rownames(P$summary_fixed), rownames(R$summary_fixed)))
    k <- rownames(P$summary_fixed)
    d <- data.frame(model = m, term = k, published = P$summary_fixed[k, "mean"],
                    refit = R$summary_fixed[k, "mean"])
    d$abs_diff <- abs(d$refit - d$published)
    hp <- P$summary_hyperpar["Phi for idarea", ]; hr <- R$summary_hyperpar["Phi for idarea", ]
    dterm[[m]] <- d
    dsum[[m]] <- data.frame(model = m, n = P$n, max_abs_diff_mean = max(d$abs_diff),
      term_at_max = d$term[which.max(d$abs_diff)],
      dic_published = P$dic, dic_refit = R$dic, waic_published = P$waic, waic_refit = R$waic,
      phi_mean_published = hp[["mean"]], phi_sd_published = hp[["sd"]],
      phi_mean_refit = hr[["mean"]], phi_sd_refit = hr[["sd"]])
  }
  dterm <- do.call(rbind, dterm); dsum <- do.call(rbind, dsum)
  # the published side must be Table 3's own sources, the refit side stage 12's own output
  pub_csv <- rbind(data.frame(model = "Model 1", read.csv(file.path(dir_tbl, "06_6g_coefficients_bym2_nobehav.csv"))[, 1:2]),
                   data.frame(model = "Model 2", read.csv(file.path(dir_tbl, "06_coefficients_bym2.csv"))[, 1:2]))
  chk <- merge(dterm, pub_csv, by.x = c("model", "term"), by.y = c("model", "predictor"))
  val <- read.csv(file.path(dir_tbl, "12_bym2_refit_validation.csv"))
  chk <- merge(chk, val[, c("model", "term", "refit")], by = c("model", "term"), suffixes = c("", "_s12"))
  res <- read.csv(file.path(dir_tbl, "12_bym2_posterior_residual.csv"))
  stopifnot(nrow(chk) == nrow(dterm), max(abs(chk$published - chk$mean)) < 1e-12,
            max(abs(chk$refit - chk$refit_s12)) < 1e-10,
            abs(dsum$dic_published[1] - read.csv(file.path(dir_tbl, "06_6g_fit_nobehav.csv"))$dic) < 0.1,
            max(abs(c(dsum$dic_refit, dsum$waic_refit) - c(res$dic, res$waic))) < 1e-6)
  write.csv(dterm, file.path(dir_tbl, "29_refit_drift_terms.csv"),   row.names = FALSE)
  write.csv(dsum,  file.path(dir_tbl, "29_refit_drift_summary.csv"), row.names = FALSE)

  # ---- (7) coefficients of the six-predictor baseline OLS (added 2026-09-27; the
  #          "OLS baseline" column of Table S1), read from the cached fit.
  cf <- summary(base$model)$coefficients
  ob <- data.frame(term = rownames(cf), estimate = cf[, 1], std_error = cf[, 2],
                   p_value = cf[, 4], n = nobs(base$model))
  stopifnot(nrow(ob) == 9L, ob$n[1] == 69530L)
  write.csv(ob, file.path(dir_tbl, "29_ols_baseline_coefficients.csv"), row.names = FALSE)
  print(reg); print(dsum[, c("model", "max_abs_diff_mean", "dic_published", "dic_refit", "waic_published", "waic_refit")])
  cat(sprintf("full-covariate OLS: R2 %.4f, residual Moran's I %.4f\n", r2_full, m_full))
}
.run_stage("29_recompute_deposit_gaps.R", .stage)

# ---- stage: 30_nanda_food_env.R ----
.stage <- function() {
  # 30_nanda_food_env.R  (added 2026-10-06)
  #
  # Food-access data-source sensitivity (Supplemental Results S3.10, Table S14).
  # Replaces the USDA Food Access Research Atlas low-income low-access flag
  # (LILA) with food-store supply from the National Neighborhood Data Archive
  # (NaNDA): 2019 counts of supermarkets, warehouse clubs, meat and fish markets,
  # and fruit and vegetable markets per 1,000 residents (primary) and per square
  # mile (secondary), each -z(log1p(density)) so that higher means fewer stores.
  # Reports agreement with LILA overall and by urban/rural, then refits Model 1
  # BYM2 on the same sample, graph, priors and covariates; only the
  # food-environment term changes:
  #   N1 linear, per 1,000          N2 spline (df = 4), per 1,000
  #   N3 linear, per square mile    N4 linear, LILA and per 1,000 together
  #   N5 spline, LILA and per 1,000 together
  #   N6 spline, per square mile    N7 spline, LILA and per square mile together
  # Contrasts are evaluated at the 10th-95th percentiles of % age 65+, as in
  # Table S13. DIC/WAIC are recorded but are not comparable across these fits:
  # the Gaussian observation precision is weakly identified against the BYM2
  # unstructured effect, and DIC moves with it.
  #
  # INPUT   data/raw/ICPSR_209313-V2.1/nanda_grocery_Tract10_1990-2022_01.dta
  #         (ICPSR 209313 V2.1, https://doi.org/10.3886/ICPSR209313.V2; free
  #         ICPSR account; CC BY-NC 4.0, so it is not redistributed here)
  # OUTPUT  output/tables/30_nanda_{agreement,coefficients,contrasts,modelfit}.csv
  #         data/processed/30_nanda_2019_tract10.rds, 30_nanda_fits.rds (cache)
  # RUNTIME about 2 minutes per BYM2 fit, 7 fits.
  suppressMessages({
    library(haven); library(dplyr); library(sf); library(splines)
  })
  stopifnot(requireNamespace("INLA", quietly = TRUE))
  root     <- ckm_root()
  dir_proc <- file.path(root, "data", "processed")
  dir_tab  <- file.path(root, "output", "tables")
  dir_log  <- file.path(root, "output", "logs")
  dir.create(dir_log, recursive = TRUE, showWarnings = FALSE)
  log_con <- file(file.path(dir_log, "30_nanda_food_env.log"), open = "wt")
  sink(log_con, split = TRUE); sink(log_con, type = "message")
  on.exit({ sink(type = "message"); sink(); close(log_con) }, add = TRUE)

  DF_SPLINE <- 4L
  PCTS      <- c(.10, .25, .50, .75, .90, .95)

  # ---- 1. estimation sample (identical rows and order to the published fits)
  s   <- ckm_sample()
  mod <- s$mod
  graph_file <- s$graph_file
  an  <- readRDS(file.path(dir_proc, "02_ckm_index.rds"))
  if (inherits(an, "sf")) an <- st_drop_geometry(an)
  age_raw <- an$pct_age_65_plus[match(mod$GEOID, an$GEOID)]
  acs_pop <- an$total_pop[match(mod$GEOID, an$GEOID)]
  stopifnot(!anyNA(age_raw), !anyNA(acs_pop), all(acs_pop > 0))
  age_mean_raw <- mean(age_raw)
  stopifnot(abs((age_raw - age_mean_raw) - mod$pct_age_65_plus) < 1e-8)

  # ---- 2. NaNDA 2019 store counts on 2010 tracts
  f_nanda <- file.path(root, "data", "raw", "ICPSR_209313-V2.1",
                       "nanda_grocery_Tract10_1990-2022_01.dta")
  if (!file.exists(f_nanda))
    stop("NaNDA file not found: ", f_nanda,
         " (download ICPSR 209313 V2.1, Tract10 Stata file)")
  na19 <- read_dta(f_nanda, col_select = c(tract_fips10, year, totpop, aland10,
                                           count_supermarkets, count_warehousefood,
                                           count_meatfish, count_fruitveg)) |>
    filter(year == 2019) |>
    mutate(GEOID = as.character(tract_fips10))
  stopifnot(!anyDuplicated(na19$GEOID))
  x <- na19[match(mod$GEOID, na19$GEOID), ]
  stopifnot(!anyNA(x$GEOID))                          # every sample tract matched
  x$stores <- with(x, count_supermarkets + count_warehousefood +
                      count_meatfish + count_fruitveg)
  stopifnot(!anyNA(x$stores))
  # Denominator: NaNDA's own population. Where it is 0 or missing (one tract in
  # this sample), use the ACS population used elsewhere in the paper.
  pop <- ifelse(is.na(x$totpop) | x$totpop <= 0, acs_pop, x$totpop)
  message(sprintf("NaNDA population replaced by ACS for %d tract(s)",
                  sum(is.na(x$totpop) | x$totpop <= 0)))
  x$den_pc   <- 1000 * x$stores / pop
  x$den_area <- x$stores / x$aland10
  stopifnot(!anyNA(x$den_pc), all(is.finite(x$den_area)))

  # Neighbourhood version: the tract plus its graph neighbours (agreement only).
  g   <- readLines(graph_file)
  stopifnot(as.integer(g[1]) == nrow(mod))
  nbl <- lapply(strsplit(trimws(g[-1]), "\\s+"), as.integer)
  stopifnot(length(nbl) == nrow(mod),
            identical(vapply(nbl, `[`, 1L, 1L), seq_len(nrow(mod))),
            all(vapply(nbl, function(r) length(r) == r[2] + 2L, logical(1))))
  nb_sum <- function(v) vapply(nbl, function(r)
    sum(v[c(r[1], r[-(1:2)])]), numeric(1))
  x$stores_nb <- nb_sum(x$stores)
  x$den_nb    <- 1000 * x$stores_nb / nb_sum(pop)

  zneg <- function(v) { l <- log1p(v); -(l - mean(l)) / sd(l) }
  mod$low_pc   <- zneg(x$den_pc)
  mod$low_area <- zneg(x$den_area)
  low_nb       <- zneg(x$den_nb)
  saveRDS(data.frame(GEOID = mod$GEOID, stores = x$stores, pop = pop,
                     aland_sqmi = x$aland10, den_pc = x$den_pc,
                     den_area = x$den_area, stores_nb = x$stores_nb,
                     den_nb = x$den_nb, low_pc = mod$low_pc,
                     low_area = mod$low_area, low_nb = low_nb),
          file.path(dir_proc, "30_nanda_2019_tract10.rds"))
  message(sprintf("tracts with no counted store: %.1f%% (tract), %.1f%% (with neighbours)",
                  100 * mean(x$stores == 0), 100 * mean(x$stores_nb == 0)))

  # ---- 3. agreement with FARA LILA, overall and by urban / rural
  auc <- function(score, y) {      # P(a LILA tract has the lower store supply)
    r <- rank(score); n1 <- sum(y == 1); n0 <- sum(y == 0)
    (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  }
  kappa2 <- function(a, b) {        # Cohen's kappa for two 0/1 vectors
    po <- mean(a == b); pe <- mean(a) * mean(b) + mean(1 - a) * mean(1 - b)
    (po - pe) / (1 - pe)
  }
  lila <- mod$LILATracts_1And10
  grp  <- list(All            = rep(TRUE, nrow(mod)),
               Urban_metro    = mod$ruca_class == "Metro",
               Rural_nonmetro = mod$ruca_class != "Metro",
               Micropolitan   = mod$ruca_class == "Micropolitan",
               Small_town     = mod$ruca_class == "Small town",
               Rural          = mod$ruca_class == "Rural")
  meas <- list(
    per_1000_residents = list(score = mod$low_pc,   den = x$den_pc,   none = x$stores == 0),
    per_square_mile    = list(score = mod$low_area, den = x$den_area, none = x$stores == 0),
    tract_plus_neighbours_per_1000 = list(score = low_nb, den = x$den_nb,
                                          none = x$stores_nb == 0))
  agree <- do.call(rbind, lapply(names(grp), function(gn) {
    k <- grp[[gn]]
    do.call(rbind, lapply(names(meas), function(mn) {
      m <- meas[[mn]]; y <- lila[k]; sc <- m$score[k]; de <- m$den[k]
      nn <- as.integer(m$none[k])
      data.frame(group = gn, measure = mn, n = sum(k), n_lila = sum(y),
                 pct_lila = 100 * mean(y),
                 pct_no_store_lila  = 100 * mean(nn[y == 1]),
                 pct_no_store_other = 100 * mean(nn[y == 0]),
                 median_density_lila  = median(de[y == 1]),
                 median_density_other = median(de[y == 0]),
                 mean_density_lila  = mean(de[y == 1]),
                 mean_density_other = mean(de[y == 0]),
                 auc_low_supply_vs_lila = auc(sc, y),
                 kappa_no_store_vs_lila = kappa2(nn, y))
    }))
  }))
  write.csv(agree, file.path(dir_tab, "30_nanda_agreement.csv"), row.names = FALSE)
  print(agree, digits = 3, row.names = FALSE)

  # ---- 4. models (Table S13 machinery, food-environment term swapped)
  age <- mod$pct_age_65_plus                          # centred, as published
  mod$age_pm25 <- age * mod$pm25_annual
  mod$age_o3   <- age * mod$o3_8hrmax_4thmax
  mod$age_tmax <- age * mod$tmax_warm
  mod$age_walk <- age * mod$walk_index
  mod$age_lila <- age * lila
  mod$age_low_pc   <- age * mod$low_pc
  mod$age_low_area <- age * mod$low_area
  AGEENV <- c("age_pm25", "age_o3", "age_tmax", "age_walk")
  BASE0  <- c("pct_hispanic", "ice_income", "pct_bachelor_plus", "ice_race",
              "ruca_class", "pm25_annual", "o3_8hrmax_4thmax", "tmax_warm",
              "walk_index")
  stopifnot(setequal(c("pct_age_65_plus", BASE0, "LILATracts_1And10"),
                     c(M1_PRED, ENV_PRED)))           # Model 1 minus LILA
  bas <- ns(age, df = DF_SPLINE)
  NS  <- sprintf("age_ns%d", seq_len(DF_SPLINE))
  NSL  <- sprintf("%s_low", NS)                       # x low_pc
  NSA  <- sprintf("%s_area", NS)                      # x low_area
  NSLL <- sprintf("%s_lila", NS)                      # x LILA, as in stage 16
  for (k in seq_len(DF_SPLINE)) {
    mod[[NS[k]]]   <- bas[, k]
    mod[[NSL[k]]]  <- bas[, k] * mod$low_pc
    mod[[NSA[k]]]  <- bas[, k] * mod$low_area
    mod[[NSLL[k]]] <- bas[, k] * lila
  }
  pt_c   <- unname(quantile(age, PCTS))
  pt_raw <- pt_c + age_mean_raw
  bas_at <- predict(bas, newx = pt_c)
  lab    <- sprintf("p%02d", round(PCTS * 100))
  lc_linear <- function(prefix, main, inter) do.call(c, lapply(seq_along(PCTS), function(i) {
    l <- do.call(INLA::inla.make.lincomb, setNames(list(1, pt_c[i]), c(main, inter)))
    names(l) <- paste0(prefix, "_", lab[i]); l }))
  lc_spline <- function(prefix, main, cols) do.call(c, lapply(seq_along(PCTS), function(i) {
    l <- do.call(INLA::inla.make.lincomb,
                 c(setNames(list(1), main), setNames(as.list(bas_at[i, ]), cols)))
    names(l) <- paste0(prefix, "_", lab[i]); l }))

  fit_bym2 <- function(label, rhs, lincomb) {
    fstr <- paste("ckm_core_pca ~", rhs)
    fml  <- as.formula(paste0(
      fstr, " + f(idarea, model = 'bym2', graph = '", graph_file, "',",
      " scale.model = TRUE,",
      " hyper = list(phi  = list(prior = 'pc', param = c(0.5, 0.5)),",
      "              prec = list(prior = 'pc.prec', param = c(1, 0.01))))"))
    message(sprintf("\n--- Fitting %s (n = %d) ---\n%s", label, nrow(mod), fstr))
    t0 <- Sys.time()
    f <- INLA::inla(fml, family = "gaussian", data = mod, lincomb = lincomb,
                    control.compute   = list(dic = TRUE, waic = TRUE),
                    control.inla      = list(int.strategy = "eb"),
                    control.predictor = list(compute = FALSE))
    message(sprintf("  done in %.1f min | DIC %.1f | WAIC %.1f",
                    as.numeric(difftime(Sys.time(), t0, units = "mins")),
                    f$dic$dic, f$waic$waic))
    stopifnot(!is.null(f$summary.lincomb.derived),
              nrow(f$summary.lincomb.derived) == length(lincomb))
    list(label = label, formula = fstr, fixed = f$summary.fixed,
         hyper = f$summary.hyperpar, lincomb = f$summary.lincomb.derived,
         dic = f$dic$dic, waic = f$waic$waic, n = nrow(mod))
  }
  f_fits <- file.path(dir_proc, "30_nanda_fits.rds")
  fits <- if (file.exists(f_fits)) readRDS(f_fits) else list()
  run <- function(key, label, rhs, lc) {
    if (is.null(fits[[key]])) {
      fits[[key]] <<- fit_bym2(label, paste(rhs, collapse = " + "), lc)
      saveRDS(fits, f_fits)                           # checkpoint after each fit
    } else message("cached: ", key)
  }
  run("N1", "N1 linear, stores per 1,000 residents",
      c("pct_age_65_plus", BASE0, "low_pc", AGEENV, "age_low_pc"),
      lc_linear("nanda", "low_pc", "age_low_pc"))
  run("N2", "N2 spline (df = 4), stores per 1,000 residents",
      c(NS, BASE0, "low_pc", AGEENV, NSL), lc_spline("nanda", "low_pc", NSL))
  run("N3", "N3 linear, stores per square mile",
      c("pct_age_65_plus", BASE0, "low_area", AGEENV, "age_low_area"),
      lc_linear("nanda", "low_area", "age_low_area"))
  run("N4", "N4 linear, LILA and stores per 1,000 residents together",
      c("pct_age_65_plus", BASE0, "LILATracts_1And10", "low_pc", AGEENV,
        "age_lila", "age_low_pc"),
      c(lc_linear("nanda", "low_pc", "age_low_pc"),
        lc_linear("lila", "LILATracts_1And10", "age_lila")))
  run("N5", "N5 spline (df = 4), LILA and stores per 1,000 residents together",
      c(NS, BASE0, "LILATracts_1And10", "low_pc", AGEENV, NSLL, NSL),
      c(lc_spline("nanda", "low_pc", NSL),
        lc_spline("lila", "LILATracts_1And10", NSLL)))
  run("N6", "N6 spline (df = 4), stores per square mile",
      c(NS, BASE0, "low_area", AGEENV, NSA), lc_spline("nanda", "low_area", NSA))
  run("N7", "N7 spline (df = 4), LILA and stores per square mile together",
      c(NS, BASE0, "LILATracts_1And10", "low_area", AGEENV, NSLL, NSA),
      c(lc_spline("nanda", "low_area", NSA),
        lc_spline("lila", "LILATracts_1And10", NSLL)))

  # ---- 5. tables
  coefs <- do.call(rbind, lapply(names(fits), function(k) {
    fx <- fits[[k]]$fixed
    data.frame(model = k, term = rownames(fx), mean = fx[, "mean"], sd = fx[, "sd"],
               lo = fx[, "0.025quant"], hi = fx[, "0.975quant"],
               credible = fx[, "0.025quant"] > 0 | fx[, "0.975quant"] < 0,
               row.names = NULL)
  }))
  contr <- do.call(rbind, lapply(names(fits), function(k) {
    lc <- fits[[k]]$lincomb
    nm <- rownames(lc)
    data.frame(model = k, measure = sub("_p[0-9]+$", "", nm),
               at = sub("^.*_", "", nm),
               pct_age_65plus = pt_raw[match(sub("^.*_", "", nm), lab)],
               contrast = lc[, "mean"], sd = lc[, "sd"],
               lo = lc[, "0.025quant"], hi = lc[, "0.975quant"],
               credible = lc[, "0.025quant"] > 0 | lc[, "0.975quant"] < 0,
               row.names = NULL)
  }))
  mfit <- do.call(rbind, lapply(names(fits), function(k)
    data.frame(model = k, label = fits[[k]]$label, n = fits[[k]]$n,
               dic = fits[[k]]$dic, waic = fits[[k]]$waic,
               formula = fits[[k]]$formula)))
  write.csv(coefs, file.path(dir_tab, "30_nanda_coefficients.csv"), row.names = FALSE)
  write.csv(contr, file.path(dir_tab, "30_nanda_contrasts.csv"),    row.names = FALSE)
  write.csv(mfit,  file.path(dir_tab, "30_nanda_modelfit.csv"),     row.names = FALSE)
  print(coefs[grepl("low_|LILA|lila", coefs$term), ], digits = 3, row.names = FALSE)
  print(contr, digits = 3, row.names = FALSE)
  message("\n30_nanda_food_env.R complete")
  invisible(NULL)
}
environment(.stage) <- new.env(parent = .ckm_env)
.run_stage("30_nanda_food_env.R", .stage)

rm(.stage)
cat("\n", strrep("-", 74), "\n", sep = "")
if (!RUN_POST) {
  cat("  RUN_POST is FALSE - section 10 skipped.\n")
} else if (length(.post_fail) == 0) {
  cat("  Section 10 complete - all ", .post_n, " stages ran.\n", sep = "")
} else {
  cat("  Section 10 INCOMPLETE - ", length(.post_fail), " of ", .post_n,
      " stages failed:\n", sep = "")
  for (f in .post_fail) cat("      ", f, "\n", sep = "")
}
cat(strrep("-", 74), "\n")

# ---- Environment provenance -------------------------------------------------

f_si <- file.path(dir_tbl, "sessionInfo.txt")
si_con <- file(f_si, open = "wt")
writeLines(c(
  "# Environment for the published CKM_US run.",
  paste0("# Written automatically at the end of CKM_US_Main.R on ",
         format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "."),
  paste0("# Platform: ", R.version$platform),
  ""), si_con)
capture.output(sessionInfo(), file = si_con)
if (requireNamespace("INLA", quietly = TRUE))
  writeLines(c("", paste0("INLA version: ",
                          as.character(utils::packageVersion("INLA")))), si_con)
close(si_con)
cat("\n  saved: ", f_si, "\n")
