# Multivariate Outlier Removal Script (Phase 3)
# Purpose: Convert flagged multivariate outliers to NA
# Author: LiuWei TP085412
# Date: 2026-01-21

cat("======================================\n")
cat("PHASE 3: MULTIVARIATE OUTLIER REMOVAL\n")
cat("Convert Isolation Forest Anomalies to NA\n")
cat("======================================\n\n")

# Load flagged data from Phase 3
cat("Loading multivariate outlier flagged data...\n")
load("raw_data/processed_data/multivariate_outlier_flagged_data.RData")
cat("Data loaded successfully.\n")
cat("Total records:", nrow(data_multivariate), "\n\n")

# Create clean copy
data_final <- data_multivariate

# Numeric columns
numeric_cols <- c("Ransom", "DownTime", "Loss")

cat("======================================\n")
cat("BEFORE REMOVAL - DATA SUMMARY\n")
cat("======================================\n\n")

# Count current state
before_summary <- data.frame(
  Variable = character(),
  Total_Records = integer(),
  Valid_Values = integer(),
  NA_Count = integer(),
  NA_Percentage = numeric(),
  Flagged_Outliers = integer(),
  Outlier_Percentage = numeric(),
  stringsAsFactors = FALSE
)

for (col in numeric_cols) {
  valid_data <- data_final[[col]][!is.na(data_final[[col]])]
  flagged_count <- sum(data_final$outlier_iforest & !is.na(data_final[[col]]), na.rm = TRUE)

  before_summary <- rbind(before_summary, data.frame(
    Variable = col,
    Total_Records = nrow(data_final),
    Valid_Values = length(valid_data),
    NA_Count = sum(is.na(data_final[[col]])),
    NA_Percentage = round(sum(is.na(data_final[[col]])) / nrow(data_final) * 100, 2),
    Flagged_Outliers = flagged_count,
    Outlier_Percentage = round(flagged_count / length(valid_data) * 100, 2)
  ))
}

cat("Data Summary BEFORE Multivariate Outlier Removal:\n")
cat("--------------------------------------------------\n")
print(before_summary, row.names = FALSE)
cat("\n")

cat("Isolation Forest anomaly flags:\n")
cat("  Total flagged as multivariate outliers:", sum(data_final$outlier_iforest), "\n")
cat("  Percentage of complete cases:",
    round(sum(data_final$outlier_iforest) / sum(complete.cases(data_final[, numeric_cols])) * 100, 2), "%\n\n")

cat("======================================\n")
cat("REVIEW TOP ANOMALIES\n")
cat("======================================\n\n")

cat("Before removing, let's review the most anomalous records:\n\n")

if (sum(data_final$outlier_iforest) > 0) {
  # Get top 20 anomalies by score
  anomaly_subset <- data_final[data_final$outlier_iforest, ]
  top_20_idx <- order(anomaly_subset$iforest_score, decreasing = TRUE)[1:min(20, nrow(anomaly_subset))]

  top_20_anomalies <- anomaly_subset[top_20_idx, c("Ransom", "DownTime", "Loss", "iforest_score", "Country")]

  cat("Top 20 Most Anomalous Records:\n")
  cat("-------------------------------\n")
  print(head(top_20_anomalies, 20), row.names = FALSE)
  cat("\n")

  # Analyze patterns
  cat("Pattern Analysis of Flagged Anomalies:\n")
  cat("---------------------------------------\n")

  # Calculate medians from non-anomalous records
  normal_data <- data_final[!data_final$outlier_iforest & complete.cases(data_final[, numeric_cols]), ]

  if (nrow(normal_data) > 0) {
    median_ransom <- median(normal_data$Ransom, na.rm = TRUE)
    median_downtime <- median(normal_data$DownTime, na.rm = TRUE)
    median_loss <- median(normal_data$Loss, na.rm = TRUE)

    # Pattern 1: High Loss, Low DownTime (suspicious!)
    pattern1 <- !is.na(anomaly_subset$Loss) & !is.na(anomaly_subset$DownTime) &
                anomaly_subset$Loss > median_loss & anomaly_subset$DownTime < median_downtime
    cat("  Pattern 1 - High Loss + Low DownTime:", sum(pattern1), "records\n")
    cat("    Example: Short outage causing huge damage\n")

    # Pattern 2: Low Loss, High DownTime (also suspicious!)
    pattern2 <- !is.na(anomaly_subset$Loss) & !is.na(anomaly_subset$DownTime) &
                anomaly_subset$Loss < median_loss & anomaly_subset$DownTime > median_downtime
    cat("  Pattern 2 - Low Loss + High DownTime:", sum(pattern2), "records\n")
    cat("    Example: Long downtime causing minimal damage\n")

    # Pattern 3: High Ransom, Low Loss (ransom > loss doesn't make sense)
    pattern3 <- !is.na(anomaly_subset$Ransom) & !is.na(anomaly_subset$Loss) &
                anomaly_subset$Ransom > median_ransom & anomaly_subset$Loss < median_loss
    cat("  Pattern 3 - High Ransom + Low Loss:", sum(pattern3), "records\n")
    cat("    Example: Ransom demand exceeds actual damage\n")

    # Pattern 4: Other combinations
    pattern4 <- !(pattern1 | pattern2 | pattern3)
    cat("  Pattern 4 - Other unusual combinations:", sum(pattern4), "records\n\n")
  }
}

cat("======================================\n")
cat("DECISION POINT\n")
cat("======================================\n\n")

cat("⚠️  IMPORTANT CONSIDERATION:\n\n")
cat("Multivariate outliers represent 'weird combinations' that may be:\n")
cat("  A) Data entry errors (should be removed)\n")
cat("  B) Genuine rare events (should be kept)\n")
cat("  C) Important anomalies for fraud detection (should be kept!)\n\n")

cat("For this analysis, we will:\n")
cat("  - Convert flagged outliers to NA\n")
cat("  - Preserve the flags for later inspection\n")
cat("  - Allow you to reverse this decision if needed\n\n")

cat("Proceeding with conversion...\n\n")

cat("======================================\n")
cat("CONVERTING OUTLIERS TO NA\n")
cat("======================================\n\n")

# Track conversions
conversion_details <- data.frame(
  Variable = character(),
  Values_Converted = integer(),
  Records_Affected = integer(),
  stringsAsFactors = FALSE
)

# For each numeric column, convert flagged values to NA
for (col in numeric_cols) {
  cat(paste0(">>> Processing: ", col, " <<<\n"))

  # Count how many non-NA values will be converted
  to_convert <- sum(data_final$outlier_iforest & !is.na(data_final[[col]]))

  cat("  Values to convert to NA:", to_convert, "\n")

  # Convert to NA
  data_final[[col]][data_final$outlier_iforest] <- NA

  cat("  Conversion complete.\n\n")

  conversion_details <- rbind(conversion_details, data.frame(
    Variable = col,
    Values_Converted = to_convert,
    Records_Affected = sum(data_final$outlier_iforest)
  ))
}

cat("Conversion Details:\n")
cat("--------------------\n")
print(conversion_details, row.names = FALSE)
cat("\n")

cat("Note: Records_Affected is the same for all variables because\n")
cat("      Isolation Forest flags entire RECORDS, not individual values.\n\n")

cat("======================================\n")
cat("AFTER REMOVAL - DATA SUMMARY\n")
cat("======================================\n\n")

# New state summary
after_summary <- data.frame(
  Variable = character(),
  Total_Records = integer(),
  Valid_Values = integer(),
  NA_Count = integer(),
  NA_Percentage = numeric(),
  Min = numeric(),
  Max = numeric(),
  Mean = numeric(),
  Median = numeric(),
  stringsAsFactors = FALSE
)

for (col in numeric_cols) {
  valid_data <- data_final[[col]][!is.na(data_final[[col]])]

  after_summary <- rbind(after_summary, data.frame(
    Variable = col,
    Total_Records = nrow(data_final),
    Valid_Values = length(valid_data),
    NA_Count = sum(is.na(data_final[[col]])),
    NA_Percentage = round(sum(is.na(data_final[[col]])) / nrow(data_final) * 100, 2),
    Min = ifelse(length(valid_data) > 0, min(valid_data), NA),
    Max = ifelse(length(valid_data) > 0, max(valid_data), NA),
    Mean = ifelse(length(valid_data) > 0, mean(valid_data), NA),
    Median = ifelse(length(valid_data) > 0, median(valid_data), NA)
  ))
}

cat("Data Summary AFTER Multivariate Outlier Removal:\n")
cat("-------------------------------------------------\n")
print(after_summary, row.names = FALSE)
cat("\n")

cat("======================================\n")
cat("BEFORE vs AFTER COMPARISON\n")
cat("======================================\n\n")

comparison <- data.frame(
  Variable = before_summary$Variable,
  Valid_Before = before_summary$Valid_Values,
  Valid_After = after_summary$Valid_Values,
  Values_Removed = before_summary$Valid_Values - after_summary$Valid_Values,
  Removal_Rate = round((before_summary$Valid_Values - after_summary$Valid_Values) /
                       before_summary$Valid_Values * 100, 2),
  NA_Before = before_summary$NA_Count,
  NA_After = after_summary$NA_Count,
  NA_Increase = after_summary$NA_Count - before_summary$NA_Count,
  Mean_Before = c(
    mean(data_multivariate$Ransom[!data_multivariate$outlier_iforest], na.rm = TRUE),
    mean(data_multivariate$DownTime[!data_multivariate$outlier_iforest], na.rm = TRUE),
    mean(data_multivariate$Loss[!data_multivariate$outlier_iforest], na.rm = TRUE)
  ),
  Mean_After = after_summary$Mean,
  Mean_Change_Pct = round((after_summary$Mean - c(
    mean(data_multivariate$Ransom[!data_multivariate$outlier_iforest], na.rm = TRUE),
    mean(data_multivariate$DownTime[!data_multivariate$outlier_iforest], na.rm = TRUE),
    mean(data_multivariate$Loss[!data_multivariate$outlier_iforest], na.rm = TRUE)
  )) / c(
    mean(data_multivariate$Ransom[!data_multivariate$outlier_iforest], na.rm = TRUE),
    mean(data_multivariate$DownTime[!data_multivariate$outlier_iforest], na.rm = TRUE),
    mean(data_multivariate$Loss[!data_multivariate$outlier_iforest], na.rm = TRUE)
  ) * 100, 2)
)

cat("Detailed Comparison:\n")
cat("--------------------\n")
print(comparison, row.names = FALSE)
cat("\n")

cat("Key Observations:\n")
for (i in 1:nrow(comparison)) {
  cat(sprintf("\n%s:\n", comparison$Variable[i]))
  cat(sprintf("  Removed %d values (%.2f%% of valid data)\n",
              comparison$Values_Removed[i],
              comparison$Removal_Rate[i]))
  cat(sprintf("  NA count increased: %d → %d (+%d)\n",
              comparison$NA_Before[i],
              comparison$NA_After[i],
              comparison$NA_Increase[i]))

  if (!is.na(comparison$Mean_Before[i]) && !is.na(comparison$Mean_After[i])) {
    cat(sprintf("  Mean change: %.2f → %.2f (%.2f%% change)\n",
                comparison$Mean_Before[i],
                comparison$Mean_After[i],
                comparison$Mean_Change_Pct[i]))
  }
}
cat("\n")

cat("======================================\n")
cat("CUMULATIVE CLEANING IMPACT\n")
cat("======================================\n\n")

cat("Summary of all cleaning phases:\n")
cat("--------------------------------\n")
cat("Original data → Phase 1 (Logical) → Phase 2 (Univariate) → Phase 3 (Multivariate)\n\n")

# Calculate cumulative impact (approximate, based on current NA counts)
cat("Estimated cumulative NA counts:\n")
for (col in numeric_cols) {
  cat(sprintf("  %s: %.2f%% NA\n", col, after_summary$NA_Percentage[after_summary$Variable == col]))
}
cat("\n")

cat("======================================\n")
cat("SAVING CLEANED DATA\n")
cat("======================================\n\n")

# Save full version with flags
output_rdata_full <- "raw_data/processed_data/final_cleaned_data_full.RData"
save(data_final, conversion_details, before_summary, after_summary, comparison,
     file = output_rdata_full)
cat("Full cleaned data (with all flags) saved to:", output_rdata_full, "\n\n")

# Create minimal version (original columns only, no flags)
data_final_minimal <- data_final[, c("Date", "Notify", "URL", "IP", "Country",
                                      "WebServer", "Encoding", "Ransom", "DownTime", "Loss")]

output_rdata_minimal <- "raw_data/processed_data/final_cleaned_data.RData"
save(data_final_minimal, file = output_rdata_minimal)
cat("Minimal cleaned data (no flags) saved to:", output_rdata_minimal, "\n\n")

# Save comparison report
output_comparison <- "docs/reports/multivariate_outlier_removal_report.csv"
write.csv(comparison, output_comparison, row.names = FALSE)
cat("Comparison report saved to:", output_comparison, "\n\n")

cat("======================================\n")
cat("FINAL DATA STRUCTURE\n")
cat("======================================\n\n")

cat("Cleaned data structure:\n")
str(data_final_minimal)
cat("\n")

cat("======================================\n")
cat("NEXT STEPS\n")
cat("======================================\n\n")

cat("All three phases of outlier detection are now complete!\n\n")

cat("Phase Summary:\n")
cat("  Phase 1: Logical cleaning (negative values)\n")
cat("  Phase 2: Univariate extremes (MAD + Percentile)\n")
cat("  Phase 3: Multivariate anomalies (Isolation Forest)\n\n")

cat("Recommended next steps:\n")
cat("  1. Review the docs/reports/multivariate_outlier_analysis.pdf visualization\n")
cat("  2. Verify that removed anomalies were indeed errors\n")
cat("  3. Proceed with missing value imputation\n")
cat("  4. Use 'final_cleaned_data.RData' for analysis\n\n")

cat("Files created:\n")
cat("  - raw_data/processed_data/final_cleaned_data_full.RData (with all tracking flags)\n")
cat("  - raw_data/processed_data/final_cleaned_data.RData (clean version, ready for imputation)\n")
cat("  - docs/reports/multivariate_outlier_removal_report.csv (statistical report)\n\n")

cat("⚠️  Remember: The flags are preserved in the _full version if you need to\n")
cat("   review or reverse any decisions!\n\n")

cat("Phase 3 outlier removal complete!\n")
