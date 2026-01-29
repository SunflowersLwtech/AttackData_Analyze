# Outlier Detection Script
# Purpose: Detect and flag outliers in two phases
#   Phase 1: Logical cleaning (negative values)
#   Phase 2: Extreme value detection (MAD + Percentile)
# Author: LiuWei TP085412
# Date: 2026-01-21

cat("======================================\n")
cat("OUTLIER DETECTION - TWO PHASE APPROACH\n")
cat("======================================\n\n")

# Load converted data
cat("Loading converted data...\n")
load("raw_data/processed_data/converted_hacking_data.RData")
cat("Data loaded successfully.\n")
cat("Total records:", nrow(data_converted), "\n\n")

# Create working copy
data_flagged <- data_converted

# Focus on numeric columns
numeric_cols <- c("Ransom", "DownTime", "Loss")

# Initialize tracking data frame
outlier_summary <- data.frame(
  Variable = character(),
  Total_Records = integer(),
  Original_NA = integer(),
  Phase1_Logical = integer(),
  Phase2_MAD = integer(),
  Phase2_Percentile = integer(),
  Total_Flagged = integer(),
  Remaining_Valid = integer(),
  stringsAsFactors = FALSE
)

cat("======================================\n")
cat("PHASE 1: LOGICAL CLEANING\n")
cat("Target: Invalid values (negative, zero, etc.)\n")
cat("======================================\n\n")

cat("Phase 1 focuses on physically impossible or logically invalid values:\n")
cat("  - Negative values (Loss < 0, DownTime < 0)\n")
cat("  - Other domain-specific invalid values\n\n")

# Initialize flag columns for Phase 1
data_flagged$outlier_ransom_logical <- FALSE
data_flagged$outlier_downtime_logical <- FALSE
data_flagged$outlier_loss_logical <- FALSE

# Phase 1: Ransom
cat(">>> Processing: Ransom <<<\n")
original_na_ransom <- sum(is.na(data_flagged$Ransom))
cat("  Original NA count:", original_na_ransom, "\n")

# Check for negative values
negative_ransom <- sum(data_flagged$Ransom < 0, na.rm = TRUE)
cat("  Negative values:", negative_ransom, "\n")

# Check for zero values (ransom = 0 might be valid, just report)
zero_ransom <- sum(data_flagged$Ransom == 0, na.rm = TRUE)
cat("  Zero values:", zero_ransom, "(not flagged, might be valid)\n")

# Flag negative values
data_flagged$outlier_ransom_logical[!is.na(data_flagged$Ransom) & data_flagged$Ransom < 0] <- TRUE
cat("  Phase 1 flagged:", sum(data_flagged$outlier_ransom_logical), "\n\n")

# Phase 1: DownTime
cat(">>> Processing: DownTime <<<\n")
original_na_downtime <- sum(is.na(data_flagged$DownTime))
cat("  Original NA count:", original_na_downtime, "\n")

# Check for negative values
negative_downtime <- sum(data_flagged$DownTime < 0, na.rm = TRUE)
cat("  Negative values:", negative_downtime, "\n")

# Flag negative values (downtime cannot be negative)
data_flagged$outlier_downtime_logical[!is.na(data_flagged$DownTime) & data_flagged$DownTime < 0] <- TRUE
cat("  Phase 1 flagged:", sum(data_flagged$outlier_downtime_logical), "\n\n")

# Phase 1: Loss
cat(">>> Processing: Loss <<<\n")
original_na_loss <- sum(is.na(data_flagged$Loss))
cat("  Original NA count:", original_na_loss, "\n")

# Check for negative values
negative_loss <- sum(data_flagged$Loss < 0, na.rm = TRUE)
cat("  Negative values:", negative_loss, "\n")

# Flag negative values (financial loss cannot be negative)
data_flagged$outlier_loss_logical[!is.na(data_flagged$Loss) & data_flagged$Loss < 0] <- TRUE
cat("  Phase 1 flagged:", sum(data_flagged$outlier_loss_logical), "\n\n")

cat("Phase 1 Summary:\n")
cat("  Total logical outliers flagged:",
    sum(data_flagged$outlier_ransom_logical) +
    sum(data_flagged$outlier_downtime_logical) +
    sum(data_flagged$outlier_loss_logical), "\n\n")

cat("======================================\n")
cat("PHASE 2: EXTREME VALUE DETECTION\n")
cat("Methods: Modified Z-Score (MAD) + Percentile\n")
cat("======================================\n\n")

cat("Phase 2 uses statistical methods to detect extreme outliers:\n")
cat("  - Modified Z-Score (MAD-based): Robust to outliers\n")
cat("  - Percentile method: Flag values beyond 99.9th percentile\n\n")

# Initialize flag columns for Phase 2
data_flagged$outlier_ransom_mad <- FALSE
data_flagged$outlier_downtime_mad <- FALSE
data_flagged$outlier_loss_mad <- FALSE

data_flagged$outlier_ransom_percentile <- FALSE
data_flagged$outlier_downtime_percentile <- FALSE
data_flagged$outlier_loss_percentile <- FALSE

# Function: Modified Z-Score using MAD (Median Absolute Deviation)
# More robust than standard Z-Score for skewed distributions
calculate_modified_zscore <- function(x, threshold = 10) {
  # Remove NA values
  x_clean <- x[!is.na(x)]

  if (length(x_clean) == 0) {
    return(rep(FALSE, length(x)))
  }

  # Calculate median
  med <- median(x_clean)

  # Calculate MAD (Median Absolute Deviation)
  mad <- median(abs(x_clean - med))

  # Avoid division by zero
  if (mad == 0) {
    # If MAD is 0, use mean absolute deviation as fallback
    mad <- mean(abs(x_clean - med))
    if (mad == 0) {
      # If still 0, no variation in data
      return(rep(FALSE, length(x)))
    }
  }

  # Modified Z-Score = 0.6745 * (x - median) / MAD
  # 0.6745 is the 75th percentile of the standard normal distribution
  modified_z <- 0.6745 * abs(x - med) / mad

  # Flag values exceeding threshold
  is_outlier <- !is.na(x) & (modified_z > threshold)

  return(is_outlier)
}

# Function: Percentile-based outlier detection
detect_percentile_outlier <- function(x, lower_percentile = 0.001, upper_percentile = 0.999) {
  # Remove NA values for calculation
  x_clean <- x[!is.na(x)]

  if (length(x_clean) == 0) {
    return(rep(FALSE, length(x)))
  }

  # Calculate percentile thresholds
  lower_bound <- quantile(x_clean, lower_percentile, na.rm = TRUE)
  upper_bound <- quantile(x_clean, upper_percentile, na.rm = TRUE)

  # Flag values outside bounds
  is_outlier <- !is.na(x) & (x < lower_bound | x > upper_bound)

  return(is_outlier)
}

# Phase 2: Ransom
cat(">>> Processing: Ransom <<<\n")

# Exclude Phase 1 outliers from Phase 2 analysis
ransom_for_phase2 <- data_flagged$Ransom
ransom_for_phase2[data_flagged$outlier_ransom_logical] <- NA

# MAD method (threshold = 10 for very conservative detection)
cat("  Applying Modified Z-Score (MAD) with threshold = 10...\n")
data_flagged$outlier_ransom_mad <- calculate_modified_zscore(ransom_for_phase2, threshold = 10)
cat("    Flagged:", sum(data_flagged$outlier_ransom_mad), "extreme values\n")

# Percentile method (99.9th percentile)
cat("  Applying Percentile method (0.1% - 99.9%)...\n")
data_flagged$outlier_ransom_percentile <- detect_percentile_outlier(ransom_for_phase2,
                                                                     lower_percentile = 0.001,
                                                                     upper_percentile = 0.999)
cat("    Flagged:", sum(data_flagged$outlier_ransom_percentile), "extreme values\n")

# Show some statistics
ransom_clean <- ransom_for_phase2[!is.na(ransom_for_phase2)]
if (length(ransom_clean) > 0) {
  cat("  Value range after Phase 1:", min(ransom_clean), "to", max(ransom_clean), "\n")
  cat("  99.9th percentile:", quantile(ransom_clean, 0.999, na.rm = TRUE), "\n")
}
cat("\n")

# Phase 2: DownTime
cat(">>> Processing: DownTime <<<\n")

# Exclude Phase 1 outliers
downtime_for_phase2 <- data_flagged$DownTime
downtime_for_phase2[data_flagged$outlier_downtime_logical] <- NA

# MAD method
cat("  Applying Modified Z-Score (MAD) with threshold = 10...\n")
data_flagged$outlier_downtime_mad <- calculate_modified_zscore(downtime_for_phase2, threshold = 10)
cat("    Flagged:", sum(data_flagged$outlier_downtime_mad), "extreme values\n")

# Percentile method
cat("  Applying Percentile method (0.1% - 99.9%)...\n")
data_flagged$outlier_downtime_percentile <- detect_percentile_outlier(downtime_for_phase2,
                                                                       lower_percentile = 0.001,
                                                                       upper_percentile = 0.999)
cat("    Flagged:", sum(data_flagged$outlier_downtime_percentile), "extreme values\n")

# Show statistics
downtime_clean <- downtime_for_phase2[!is.na(downtime_for_phase2)]
if (length(downtime_clean) > 0) {
  cat("  Value range after Phase 1:", min(downtime_clean), "to", max(downtime_clean), "\n")
  cat("  99.9th percentile:", quantile(downtime_clean, 0.999, na.rm = TRUE), "\n")
}
cat("\n")

# Phase 2: Loss
cat(">>> Processing: Loss <<<\n")

# Exclude Phase 1 outliers
loss_for_phase2 <- data_flagged$Loss
loss_for_phase2[data_flagged$outlier_loss_logical] <- NA

# MAD method
cat("  Applying Modified Z-Score (MAD) with threshold = 10...\n")
data_flagged$outlier_loss_mad <- calculate_modified_zscore(loss_for_phase2, threshold = 10)
cat("    Flagged:", sum(data_flagged$outlier_loss_mad), "extreme values\n")

# Percentile method
cat("  Applying Percentile method (0.1% - 99.9%)...\n")
data_flagged$outlier_loss_percentile <- detect_percentile_outlier(loss_for_phase2,
                                                                   lower_percentile = 0.001,
                                                                   upper_percentile = 0.999)
cat("    Flagged:", sum(data_flagged$outlier_loss_percentile), "extreme values\n")

# Show statistics
loss_clean <- loss_for_phase2[!is.na(loss_for_phase2)]
if (length(loss_clean) > 0) {
  cat("  Value range after Phase 1:", min(loss_clean), "to", max(loss_clean), "\n")
  cat("  99.9th percentile:", quantile(loss_clean, 0.999, na.rm = TRUE), "\n")
}
cat("\n")

cat("======================================\n")
cat("COMBINED OUTLIER FLAGS\n")
cat("======================================\n\n")

cat("Creating combined flags (any method flags = outlier)...\n\n")

# Combine all flags for each variable
data_flagged$is_outlier_ransom <- data_flagged$outlier_ransom_logical |
                                   data_flagged$outlier_ransom_mad |
                                   data_flagged$outlier_ransom_percentile

data_flagged$is_outlier_downtime <- data_flagged$outlier_downtime_logical |
                                     data_flagged$outlier_downtime_mad |
                                     data_flagged$outlier_downtime_percentile

data_flagged$is_outlier_loss <- data_flagged$outlier_loss_logical |
                                 data_flagged$outlier_loss_mad |
                                 data_flagged$outlier_loss_percentile

# Build summary table
for (col in numeric_cols) {
  col_lower <- tolower(col)

  total_records <- nrow(data_flagged)
  original_na <- sum(is.na(data_converted[[col]]))
  phase1 <- sum(data_flagged[[paste0("outlier_", col_lower, "_logical")]])
  phase2_mad <- sum(data_flagged[[paste0("outlier_", col_lower, "_mad")]])
  phase2_pct <- sum(data_flagged[[paste0("outlier_", col_lower, "_percentile")]])
  total_flagged <- sum(data_flagged[[paste0("is_outlier_", col_lower)]])

  # Valid data = Total - Original NA - Newly flagged outliers
  remaining_valid <- total_records - original_na - total_flagged

  outlier_summary <- rbind(outlier_summary, data.frame(
    Variable = col,
    Total_Records = total_records,
    Original_NA = original_na,
    Phase1_Logical = phase1,
    Phase2_MAD = phase2_mad,
    Phase2_Percentile = phase2_pct,
    Total_Flagged = total_flagged,
    Remaining_Valid = remaining_valid
  ))
}

cat("Outlier Detection Summary:\n")
cat("---------------------------\n")
print(outlier_summary, row.names = FALSE)
cat("\n")

cat("======================================\n")
cat("SAVING RESULTS\n")
cat("======================================\n\n")

# Save flagged data
output_rdata <- "raw_data/processed_data/outlier_flagged_data.RData"
save(data_flagged, outlier_summary, file = output_rdata)
cat("Flagged data saved to:", output_rdata, "\n\n")

# Save summary as CSV
output_summary <- "docs/reports/outlier_detection_summary.csv"
write.csv(outlier_summary, output_summary, row.names = FALSE)
cat("Summary saved to:", output_summary, "\n\n")

cat("======================================\n")
cat("NEXT STEPS\n")
cat("======================================\n\n")

cat("1. Review the outlier detection summary above\n")
cat("2. Run visualization script to inspect flagged values (optional)\n")
cat("3. Run 04_outlier_to_na.R to convert flagged outliers to NA\n\n")

cat("Column Guide:\n")
cat("  - outlier_XXX_logical: Phase 1 flags (negative values, etc.)\n")
cat("  - outlier_XXX_mad: Phase 2 MAD method flags\n")
cat("  - outlier_XXX_percentile: Phase 2 percentile method flags\n")
cat("  - is_outlier_XXX: Combined flag (TRUE if ANY method flagged it)\n\n")

cat("Detection complete!\n")
