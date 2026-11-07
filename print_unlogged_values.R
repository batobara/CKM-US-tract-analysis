# print_unlogged_values.R
# Prints six manuscript values that the main log does not print.
# Reads saved files only; fits no model. It finds the project folder (the one
# holding data/processed) from the working directory or from its own location,
# so source(), Rscript and RStudio's Source button all work without setwd().

suppressMessages(library(sf))
find_root <- function() {
  walk_up <- function(start, levels = 6L) {
    if (is.null(start) || is.na(start) || !nzchar(start)) return(NA_character_)
    p <- suppressWarnings(normalizePath(start, mustWork = FALSE))
    for (i in seq_len(levels)) {
      if (dir.exists(file.path(p, "data", "processed"))) return(normalizePath(p))
      if (identical(dirname(p), p)) break
      p <- dirname(p)
    }
    NA_character_
  }
  starts <- getwd()
  for (i in rev(seq_len(sys.nframe()))) {          # file given to source()
    of <- tryCatch(get("ofile", envir = sys.frame(i), inherits = FALSE),
                   error = function(e) NULL)
    if (is.character(of) && length(of) == 1L) starts <- c(starts, dirname(of))
  }
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)  # Rscript
  if (length(a))  # Rscript encodes spaces in the path as "~+~"
    starts <- c(starts, dirname(gsub("~+~", " ", sub("^--file=", "", a[[1L]]), fixed = TRUE)))
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE))) {
    ed <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(ed)) starts <- c(starts, dirname(ed))
  }
  for (s0 in starts) { r <- walk_up(s0); if (!is.na(r)) return(r) }
  stop("Could not find a folder containing data/processed.\n",
       "  Run the main pipeline first, then either open this script from the\n",
       "  project folder or run setwd(\"<path to project folder>\") before source().",
       call. = FALSE)
}
root <- find_root()
dp <- file.path(root, "data", "processed")
cat("Project folder:", root, "\n\n")

# 1. Global Moran's I of the CKM index, 69,854 contiguous-U.S. tracts (k = 8)
m <- readRDS(file.path(dp, "03_moran_core.rds"))
cat("1. Global Moran's I =", sprintf("%.4f", m$moran_core$estimate[["Moran I statistic"]]),
    "| n =", length(m$lw_knn_summary), "tracts | p =",
    format.pval(m$moran_core$p.value, eps = 1e-16), "\n")

# 2. Cronbach's alpha, 5-indicator index
ix <- readRDS(file.path(dp, "02_ckm_index.rds"))
if (inherits(ix, "sf")) ix <- st_drop_geometry(ix)
core <- c("DIABETES", "BPHIGH", "OBESITY", "HIGHCHOL", "KIDNEY")
z <- scale(ix[, core]); z <- z[complete.cases(z), ]
cat("2. Cronbach's alpha =", sprintf("%.4f", DescTools::CronbachAlpha(z)),
    "| n =", nrow(z), "tracts\n")

# 3. NHANES analytic file
nh <- readRDS(file.path(dp, "04_nhanes_individual.rds"))
cat("3. NHANES nrow =", nrow(nh), "adults\n")

# 4. Upper boundary knot of the aging spline = max % aged >= 65, estimation sample
ref <- readRDS(file.path(dp, "12_bym2_refit_model1.rds"))
age <- ix$pct_age_65_plus[match(ref$GEOID, ix$GEOID)]
sp  <- readRDS(file.path(dp, "16_spline_lila_interaction.rds"))
cat("4. max(pct_age_65_plus) =", sprintf("%.2f", max(age)), "| n =", length(age),
    "| spline boundary knots =",
    paste(sprintf("%.2f", attr(sp$spline_basis, "Boundary.knots") + sp$age_mean_raw),
          collapse = ", "), "\n")

# 5. Semivariogram of OLS residuals (5,000-tract subsample, 50-km bins)
v <- readRDS(file.path(dp, "06_5d_residual_variogram.rds"))
cat("5. Semivariogram bins:\n")
print(data.frame(km = round(v$dist / 1000), gamma = round(v$gamma, 3), pairs = v$np),
      row.names = FALSE)

# 6. Tract geometries with no PLACES estimate (land tracts, New Jersey excluded)
g <- st_drop_geometry(readRDS(file.path(dp, "01_tracts_geo.rds")))
p <- readRDS(file.path(dp, "01_places_tract.rds"))
g$GEOID <- trimws(as.character(g$GEOID))
nop <- !(g$GEOID %in% p$GEOID) & substr(g$GEOID, 1, 2) != "34" & g$ALAND > 0
cat("6. Suppressed tracts (no PLACES estimate) =", sum(nop), "\n")
