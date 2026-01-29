# ============================================================
# main.R — One-Click Data Cleaning Pipeline Runner
# ============================================================
# Usage: Open RStudio, set working directory to this folder,
#        then run: source("main.R")
#
# Pipeline Flow:
#   00_merge_data.R
#     -> 01_format_conversion       (merged -> converted)
#     -> 02_distribution_check      (visualization only)
#     -> 03_outlier_detection       (converted -> outlier_flagged)
#     -> 04_outlier_to_na           (flagged -> outlier_removed)
#     -> 05_multivariate_outlier    (removed -> multivariate_flagged)
#     -> 06_multivariate_outlier_na (flagged -> final_cleaned)
#     -> 07_data_health_check       (read-only analysis)
#     -> 08_preprocessing           (final_cleaned -> preprocessed)
#     -> 09_categorical_cleaning    (preprocessed -> categorical_cleaned)
#     -> 10_final_quality_check     (read-only validation)
#     -> 11_mice_imputation         (categorical_cleaned -> mice_imputed)
#     -> 12_imputation_validation   (read-only validation)
#     -> 13_before_after_comparison (read-only visualization)
# ============================================================

# --- Configuration ---
SKIP_VISUALIZATION <- FALSE  # Set TRUE to skip plot-generating scripts (faster)

# --- Helper function ---
run_step <- function(script_path, step_name) {
  cat("\n")
  cat(strrep("=", 60), "\n")
  cat("STEP:", step_name, "\n")
  cat("File:", script_path, "\n")
  cat(strrep("=", 60), "\n\n")

  if (!file.exists(script_path)) {
    cat("ERROR: Script not found:", script_path, "\n")
    stop(paste("Missing script:", script_path))
  }

  start_time <- Sys.time()
  tryCatch(
    source(script_path, local = new.env(parent = globalenv())),
    error = function(e) {
      cat("\nERROR in", step_name, ":\n")
      cat(conditionMessage(e), "\n")
      stop(e)
    }
  )
  elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
  cat("\n--- Completed:", step_name, "in", elapsed, "seconds ---\n")
}

# --- Pre-flight checks ---
cat(strrep("=", 60), "\n")
cat("DATA CLEANING PIPELINE\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 60), "\n")

# Check required raw data files exist
required_files <- c(
  "raw_data/HackingData_Part1.csv",
  "raw_data/HackingData_Part2.xlsx",
  "raw_data/HackingData_Part3.txt"
)
for (f in required_files) {
  if (!file.exists(f)) stop(paste("Missing raw data file:", f))
}
cat("All raw data files found.\n")

# Ensure output directories exist
dir.create("raw_data/processed_data", recursive = TRUE, showWarnings = FALSE)
dir.create("docs/reports", recursive = TRUE, showWarnings = FALSE)

pipeline_start <- Sys.time()

# ============================================================
# PHASE 1: Data Loading & Merge
# ============================================================
run_step("scripts/data_clean/00_merge_data.R", "Data Merge (3 parts -> merged)")

# ============================================================
# PHASE 2: Format Conversion (text -> numeric, date parsing)
# ============================================================
run_step("scripts/data_clean/01_format_conversion.R",
         "Format Conversion (text preprocessing + type conversion)")

# ============================================================
# PHASE 3: Distribution Analysis (visualization only)
# ============================================================
if (!SKIP_VISUALIZATION) {
  run_step("scripts/data_clean/02_distribution_check.R",
           "Distribution Check (generates PDFs)")
} else {
  cat("\n--- SKIPPED: Distribution Check (SKIP_VISUALIZATION=TRUE) ---\n")
}

# ============================================================
# PHASE 4: Univariate Outlier Detection & Removal
# ============================================================
run_step("scripts/data_clean/03_outlier_detection.R",
         "Outlier Detection (MAD + Percentile)")

run_step("scripts/data_clean/04_outlier_to_na.R",
         "Outlier -> NA Conversion")

# ============================================================
# PHASE 5: Multivariate Outlier Detection & Removal
# ============================================================
run_step("scripts/data_clean/05_multivariate_outlier_detection.R",
         "Multivariate Outlier Detection (Isolation Forest)")

run_step("scripts/data_clean/06_multivariate_outlier_to_na.R",
         "Multivariate Outlier -> NA Conversion")

# ============================================================
# PHASE 6: Health Check & Preprocessing
# ============================================================
run_step("scripts/data_clean/07_data_health_check.R",
         "Data Health Check (missing data analysis)")

run_step("scripts/data_clean/08_preprocessing_for_imputation.R",
         "Preprocessing (log transform, feature engineering)")

# ============================================================
# PHASE 7: Categorical Cleaning
# ============================================================
run_step("scripts/data_clean/09_categorical_cleaning.R",
         "Categorical Cleaning (Notify, WebServer, Country, etc.)")

# ============================================================
# PHASE 8: Quality Validation
# ============================================================
run_step("scripts/data_clean/10_final_quality_check.R",
         "Final Quality Check")

# ============================================================
# PHASE 9: MICE Imputation
# ============================================================
run_step("scripts/data_clean/11_mice_imputation.R",
         "MICE Imputation (PMM, divide-and-conquer)")

# ============================================================
# PHASE 10: Post-Imputation Validation
# ============================================================
run_step("scripts/data_clean/12_imputation_validation.R",
         "Imputation Validation (KS tests, density checks)")

# ============================================================
# PHASE 11: Before/After Comparison
# ============================================================
if (!SKIP_VISUALIZATION) {
  run_step("scripts/data_clean/13_before_after_comparison.R",
           "Before/After Comparison (generates PDFs)")
} else {
  cat("\n--- SKIPPED: Before/After Comparison (SKIP_VISUALIZATION=TRUE) ---\n")
}

# ============================================================
# PIPELINE COMPLETE
# ============================================================
pipeline_elapsed <- round(difftime(Sys.time(), pipeline_start, units = "mins"), 1)

cat("\n")
cat(strrep("=", 60), "\n")
cat("PIPELINE COMPLETE\n")
cat("Finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Total time:", pipeline_elapsed, "minutes\n")
cat(strrep("=", 60), "\n\n")

cat("Output files:\n")
cat("  Final dataset:  raw_data/processed_data/mice_imputed_data.RData\n")
cat("  Clean dataset:  raw_data/processed_data/final_cleaned_data.RData\n")
cat("  Reports:        docs/reports/\n\n")

cat("To load the final cleaned & imputed data:\n")
cat('  load("raw_data/processed_data/mice_imputed_data.RData")\n')
cat("  # data_final_imputed is ready for analysis\n")
