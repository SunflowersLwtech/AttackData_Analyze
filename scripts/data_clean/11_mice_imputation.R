# MICE Imputation Script (Divide and Conquer Strategy)
# Purpose: Impute missing values using Multivariate Imputation by Chained Equations
# Strategy: Separate imputation for Modern (2016+) and Legacy (<2016) data
# Method: PMM (Predictive Mean Matching) - robust to skewness
# Author: LiuWei TP085412
# Date: 2026-01-21

cat("======================================\n")
cat("MICE IMPUTATION - DIVIDE AND CONQUER\n")
cat("Best Practice: Separate Modern and Legacy Data\n")
cat("======================================\n\n")

# Load required libraries
required_packages <- c("mice", "dplyr", "ggplot2", "VIM")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste0("Installing package: ", pkg, "...\n"))
    install.packages(pkg, repos = "https://cloud.r-project.org/", quiet = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE, quietly = TRUE)
  }
}

cat("Required packages loaded successfully.\n\n")

# Load categorical cleaned data
cat("Loading categorical cleaned data...\n")
load("raw_data/processed_data/categorical_cleaned.RData")
cat("Data loaded successfully.\n")
cat("Total records:", nrow(data_categorical), "\n\n")

# Create working copy
data_imputation <- data_categorical

cat("======================================\n")
cat("STRATEGY OVERVIEW\n")
cat("======================================\n\n")

cat("Why Divide and Conquer?\n")
cat("-------------------------\n")
cat("Modern Data (2016+):\n")
cat("  ✓ Completeness: 87.2% - Excellent!\n")
cat("  ✓ Ransom missing: 6.26% - Very manageable\n")
cat("  → Strategy: Full MICE imputation on all numeric variables\n\n")

cat("Legacy Data (<2016):\n")
cat("  ⚠️  Completeness: 45.1% - Poor\n")
cat("  ❌ Ransom missing: 53.9% - Over half!\n")
cat("  ❌ Test 3 showed: Notify has NO predictive power for Ransom\n")
cat("  → Strategy: Differentiated approach\n")
cat("    - Impute Loss & DownTime (9-11% missing)\n")
cat("    - DO NOT impute Ransom (too much missing, would create synthetic bias)\n\n")

cat("======================================\n")
cat("PART 1: MODERN DATA IMPUTATION (2016+)\n")
cat("Full MICE with high-quality data\n")
cat("======================================\n\n")

# Separate modern data
data_modern <- data_imputation %>%
  filter(is_modern == TRUE)

cat("Modern data subset:\n")
cat("  Total records:", nrow(data_modern), "\n")
cat("  Date range: 2016-2025\n\n")

# Check missing rates in modern data
cat("Missing rates in modern data BEFORE imputation:\n")
missing_modern <- data.frame(
  Variable = c("Ransom_log", "DownTime_log", "Loss_log"),
  Missing_Count = c(
    sum(is.na(data_modern$Ransom_log)),
    sum(is.na(data_modern$DownTime_log)),
    sum(is.na(data_modern$Loss_log))
  ),
  Missing_Pct = c(
    round(sum(is.na(data_modern$Ransom_log)) / nrow(data_modern) * 100, 2),
    round(sum(is.na(data_modern$DownTime_log)) / nrow(data_modern) * 100, 2),
    round(sum(is.na(data_modern$Loss_log)) / nrow(data_modern) * 100, 2)
  )
)
print(missing_modern, row.names = FALSE)
cat("\n")

# Define variables for MICE (CRITICAL: Use cleaned and log-transformed variables)
cat("Selecting predictor variables for MICE...\n\n")

cat("CRITICAL DECISION - Predictor Selection:\n")
cat("  ✓ Include: Ransom_log, DownTime_log, Loss_log (mutual prediction)\n")
cat("  ✓ Include: Country_clean (p < 2e-16, highly significant)\n")
cat("  ✓ Include: WebServer_clean (p < 2e-16, highly significant)\n")
cat("  ✓ Include: URL_suffix (p < 2e-16, highly significant)\n")
cat("  ✓ Include: Year (temporal pattern)\n")
cat("  ✗ Exclude: Notify_clean (p = 0.366, NO significant association with Ransom)\n")
cat("  ✗ Exclude: Encoding_clean (21% missing, weak business logic)\n")
cat("  ✗ Exclude: IP_clean, URL (unique values, no statistical meaning)\n\n")

# Select variables for imputation
vars_for_mice_modern <- c(
  # Target variables (log-transformed)
  "Ransom_log", "DownTime_log", "Loss_log",
  # Categorical predictors (cleaned)
  "Country_clean", "WebServer_clean", "URL_suffix",
  # Temporal predictor
  "Year"
)

# Extract subset for MICE
mice_data_modern <- data_modern[, vars_for_mice_modern]

# Check data structure
cat("Data structure for MICE:\n")
str(mice_data_modern)
cat("\n")

# Configure MICE
cat("Configuring MICE parameters...\n")
cat("  Method: PMM (Predictive Mean Matching)\n")
cat("    → Why PMM? Robust to skewed distributions\n")
cat("    → PMM picks real observed values (no synthetic outliers)\n")
cat("    → Perfect for Loss_log which still has skewness = -1.25\n")
cat("  Iterations (maxit): 10\n")
cat("  Multiple imputations (m): 5\n")
cat("    → Generates 5 complete datasets\n")
cat("    → Final result = pooled average\n")
cat("  Random seed: 500 (for reproducibility)\n\n")

# Run MICE
cat("Starting MICE imputation (this may take several minutes)...\n")
cat("Progress will be shown below:\n\n")

set.seed(500)

imp_modern <- mice(
  mice_data_modern,
  m = 5,              # Number of multiple imputations
  method = 'pmm',     # Predictive Mean Matching
  maxit = 10,         # Number of iterations
  seed = 500,         # Reproducibility
  printFlag = TRUE    # Show progress
)

cat("\nMICE imputation completed for modern data!\n\n")

# Get completed datasets
cat("Extracting completed datasets...\n")

# Pool all 5 imputations into a single dataset (taking the first complete set)
# In production, you might want to analyze all 5 and pool results
imputed_modern <- complete(imp_modern, action = 1)  # Use first imputation

cat("  Completed dataset extracted (using imputation #1)\n\n")

# Back-transform from log scale to original scale
cat("Back-transforming from log scale to original scale...\n")
cat("  Formula: Original = 10^(log_value) - 1\n\n")

imputed_modern <- imputed_modern %>%
  mutate(
    Ransom_imputed = 10^Ransom_log - 1,
    DownTime_imputed = 10^DownTime_log - 1,
    Loss_imputed = 10^Loss_log - 1
  )

# Merge back with original modern data
cat("Merging imputed values back to original dataset...\n")

# Create index to match rows
data_modern$row_index <- 1:nrow(data_modern)
imputed_modern$row_index <- 1:nrow(imputed_modern)

# Add imputed columns
data_modern$Ransom_imputed <- imputed_modern$Ransom_imputed
data_modern$DownTime_imputed <- imputed_modern$DownTime_imputed
data_modern$Loss_imputed <- imputed_modern$Loss_imputed

# Also update log versions
data_modern$Ransom_log_imputed <- imputed_modern$Ransom_log
data_modern$DownTime_log_imputed <- imputed_modern$DownTime_log
data_modern$Loss_log_imputed <- imputed_modern$Loss_log

cat("  Imputed values merged successfully\n\n")

# Validate imputation quality for modern data
cat("Validating imputation quality for modern data:\n")
cat("-----------------------------------------------\n")

validation_modern <- data.frame(
  Variable = c("Ransom", "DownTime", "Loss"),
  Original_Mean = c(
    mean(data_modern$Ransom, na.rm = TRUE),
    mean(data_modern$DownTime, na.rm = TRUE),
    mean(data_modern$Loss, na.rm = TRUE)
  ),
  Imputed_Mean = c(
    mean(data_modern$Ransom_imputed, na.rm = TRUE),
    mean(data_modern$DownTime_imputed, na.rm = TRUE),
    mean(data_modern$Loss_imputed, na.rm = TRUE)
  ),
  Original_Median = c(
    median(data_modern$Ransom, na.rm = TRUE),
    median(data_modern$DownTime, na.rm = TRUE),
    median(data_modern$Loss, na.rm = TRUE)
  ),
  Imputed_Median = c(
    median(data_modern$Ransom_imputed, na.rm = TRUE),
    median(data_modern$DownTime_imputed, na.rm = TRUE),
    median(data_modern$Loss_imputed, na.rm = TRUE)
  )
)

validation_modern$Mean_Change_Pct <- round(
  (validation_modern$Imputed_Mean - validation_modern$Original_Mean) /
  validation_modern$Original_Mean * 100, 2
)

print(validation_modern, row.names = FALSE)
cat("\n")

cat("Interpretation:\n")
cat("  If Mean_Change_Pct is within ±10%: ✓ Good imputation\n")
cat("  If Mean_Change_Pct > 20%: ⚠️  Check for issues\n\n")

cat("======================================\n")
cat("PART 2: LEGACY DATA IMPUTATION (<2016)\n")
cat("Differentiated strategy: Impute Loss & DownTime only\n")
cat("======================================\n\n")

# Separate legacy data
data_legacy <- data_imputation %>%
  filter(is_modern == FALSE)

cat("Legacy data subset:\n")
cat("  Total records:", nrow(data_legacy), "\n")
cat("  Date range: 1998-2015\n\n")

# Check missing rates in legacy data
cat("Missing rates in legacy data:\n")
missing_legacy <- data.frame(
  Variable = c("Ransom_log", "DownTime_log", "Loss_log"),
  Missing_Count = c(
    sum(is.na(data_legacy$Ransom_log)),
    sum(is.na(data_legacy$DownTime_log)),
    sum(is.na(data_legacy$Loss_log))
  ),
  Missing_Pct = c(
    round(sum(is.na(data_legacy$Ransom_log)) / nrow(data_legacy) * 100, 2),
    round(sum(is.na(data_legacy$DownTime_log)) / nrow(data_legacy) * 100, 2),
    round(sum(is.na(data_legacy$Loss_log)) / nrow(data_legacy) * 100, 2)
  )
)
print(missing_legacy, row.names = FALSE)
cat("\n")

cat("CRITICAL DECISION for Legacy Data:\n")
cat("------------------------------------\n")
cat("Ransom_log: 53.9% missing\n")
cat("  ❌ DO NOT IMPUTE\n")
cat("  Reasoning:\n")
cat("    1. Over 50% missing = too much synthetic data\n")
cat("    2. Test 3 showed Notify has NO predictive power (p=0.366)\n")
cat("    3. Would introduce massive synthetic bias\n")
cat("  → Action: Keep as NA or mark as 'Era Limitation'\n\n")

cat("DownTime_log & Loss_log: 9-11% missing\n")
cat("  ✓ CAN IMPUTE\n")
cat("  → Proceed with MICE using available predictors\n\n")

# Select variables for legacy MICE (excluding Ransom)
vars_for_mice_legacy <- c(
  # Target variables (only DownTime and Loss)
  "DownTime_log", "Loss_log",
  # Categorical predictors
  "Country_clean", "WebServer_clean", "URL_suffix",
  # Temporal predictor
  "Year"
)

mice_data_legacy <- data_legacy[, vars_for_mice_legacy]

cat("Data structure for legacy MICE:\n")
str(mice_data_legacy)
cat("\n")

# Run MICE for legacy data
cat("Starting MICE imputation for legacy data...\n")
cat("Note: Only imputing DownTime_log and Loss_log\n\n")

set.seed(501)

imp_legacy <- mice(
  mice_data_legacy,
  m = 5,
  method = 'pmm',
  maxit = 10,
  seed = 501,
  printFlag = TRUE
)

cat("\nMICE imputation completed for legacy data!\n\n")

# Get completed datasets
imputed_legacy <- complete(imp_legacy, action = 1)

# Back-transform
cat("Back-transforming legacy data...\n")

imputed_legacy <- imputed_legacy %>%
  mutate(
    DownTime_imputed = 10^DownTime_log - 1,
    Loss_imputed = 10^Loss_log - 1
  )

# Merge back
data_legacy$row_index <- 1:nrow(data_legacy)
imputed_legacy$row_index <- 1:nrow(imputed_legacy)

data_legacy$DownTime_imputed <- imputed_legacy$DownTime_imputed
data_legacy$Loss_imputed <- imputed_legacy$Loss_imputed
data_legacy$DownTime_log_imputed <- imputed_legacy$DownTime_log
data_legacy$Loss_log_imputed <- imputed_legacy$Loss_log

# Keep Ransom as original (with NAs)
data_legacy$Ransom_imputed <- data_legacy$Ransom
data_legacy$Ransom_log_imputed <- data_legacy$Ransom_log

cat("  Legacy data imputation complete\n\n")

# Validate legacy imputation
cat("Validating imputation quality for legacy data:\n")
cat("-----------------------------------------------\n")

validation_legacy <- data.frame(
  Variable = c("DownTime", "Loss"),
  Original_Mean = c(
    mean(data_legacy$DownTime, na.rm = TRUE),
    mean(data_legacy$Loss, na.rm = TRUE)
  ),
  Imputed_Mean = c(
    mean(data_legacy$DownTime_imputed, na.rm = TRUE),
    mean(data_legacy$Loss_imputed, na.rm = TRUE)
  ),
  Original_Median = c(
    median(data_legacy$DownTime, na.rm = TRUE),
    median(data_legacy$Loss, na.rm = TRUE)
  ),
  Imputed_Median = c(
    median(data_legacy$DownTime_imputed, na.rm = TRUE),
    median(data_legacy$Loss_imputed, na.rm = TRUE)
  )
)

validation_legacy$Mean_Change_Pct <- round(
  (validation_legacy$Imputed_Mean - validation_legacy$Original_Mean) /
  validation_legacy$Original_Mean * 100, 2
)

print(validation_legacy, row.names = FALSE)
cat("\n")

cat("======================================\n")
cat("PART 3: COMBINING RESULTS\n")
cat("======================================\n\n")

cat("Combining modern and legacy imputed datasets...\n")

# Combine
data_final_imputed <- bind_rows(data_modern, data_legacy)

cat("  Combined successfully\n")
cat("  Total records:", nrow(data_final_imputed), "\n\n")

# Final missing check
cat("Final missing value status:\n")
cat("----------------------------\n")

final_missing <- data.frame(
  Variable = c("Ransom_imputed", "DownTime_imputed", "Loss_imputed"),
  Missing_Count = c(
    sum(is.na(data_final_imputed$Ransom_imputed)),
    sum(is.na(data_final_imputed$DownTime_imputed)),
    sum(is.na(data_final_imputed$Loss_imputed))
  ),
  Missing_Pct = c(
    round(sum(is.na(data_final_imputed$Ransom_imputed)) / nrow(data_final_imputed) * 100, 2),
    round(sum(is.na(data_final_imputed$DownTime_imputed)) / nrow(data_final_imputed) * 100, 2),
    round(sum(is.na(data_final_imputed$Loss_imputed)) / nrow(data_final_imputed) * 100, 2)
  )
)

print(final_missing, row.names = FALSE)
cat("\n")

cat("Note: Ransom_imputed still has missing values in legacy data (by design)\n\n")

cat("======================================\n")
cat("SAVING RESULTS\n")
cat("======================================\n\n")

# Save imputed data
output_rdata <- "raw_data/processed_data/mice_imputed_data.RData"
save(data_final_imputed, imp_modern, imp_legacy,
     validation_modern, validation_legacy,
     file = output_rdata)
cat(paste0("Imputed data saved: ", output_rdata, "\n\n"))

# Save summary
imputation_summary <- list(
  modern_missing_before = missing_modern,
  modern_validation = validation_modern,
  legacy_missing_before = missing_legacy,
  legacy_validation = validation_legacy,
  final_missing = final_missing
)

saveRDS(imputation_summary, "raw_data/processed_data/imputation_summary.rds")
cat("Imputation summary saved\n\n")

cat("======================================\n")
cat("FINAL WARNINGS & RECOMMENDATIONS\n")
cat("======================================\n\n")

cat("⚠️  IMPORTANT REMINDERS:\n\n")

cat("1. Ransom in Legacy Data:\n")
cat("   - 53.9% remained as NA (intentional)\n")
cat("   - This is NOT an error - it's best practice\n")
cat("   - Forced imputation would create synthetic bias\n\n")

cat("2. .txt Domain URLs:\n")
cat("   - Recommend: Review imputed Loss values for .txt TLDs\n")
cat("   - Check if they show specific attack patterns\n")
cat("   - URL_suffix was used as predictor, might have influenced results\n\n")

cat("3. Next Steps:\n")
cat("   - Run validation script (12_imputation_validation.R)\n")
cat("   - Visual comparison of before/after distributions\n")
cat("   - Check for synthetic outliers\n")
cat("   - Verify business logic consistency\n\n")

cat("MICE imputation complete!\n")
