# Outlier Removal Script
# Purpose: Convert flagged outliers to NA
# Author: LiuWei TP085412
# Date: 2026-01-21

cat("======================================\n")
cat("OUTLIER REMOVAL - CONVERT TO NA\n")
cat("======================================\n\n")

# Load flagged data
cat("Loading flagged data...\n")
load("raw_data/processed_data/outlier_flagged_data.RData")
cat("Data loaded successfully.\n")
cat("Total records:", nrow(data_flagged), "\n\n")

# Create a clean copy
data_clean <- data_flagged

# Numeric columns to clean
numeric_cols <- c("Ransom", "DownTime", "Loss")

cat("======================================\n")
cat("BEFORE REMOVAL - DATA SUMMARY\n")
cat("======================================\n\n")

# Show current state
before_summary <- data.frame(
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
  valid_data <- data_clean[[col]][!is.na(data_clean[[col]])]

  before_summary <- rbind(before_summary, data.frame(
    Variable = col,
    Total_Records = nrow(data_clean),
    Valid_Values = length(valid_data),
    NA_Count = sum(is.na(data_clean[[col]])),
    NA_Percentage = round(sum(is.na(data_clean[[col]])) / nrow(data_clean) * 100, 2),
    Min = ifelse(length(valid_data) > 0, min(valid_data), NA),
    Max = ifelse(length(valid_data) > 0, max(valid_data), NA),
    Mean = ifelse(length(valid_data) > 0, mean(valid_data), NA),
    Median = ifelse(length(valid_data) > 0, median(valid_data), NA)
  ))
}

cat("Data Summary BEFORE Outlier Removal:\n")
cat("-------------------------------------\n")
print(before_summary, row.names = FALSE)
cat("\n")

cat("======================================\n")
cat("CONVERTING OUTLIERS TO NA\n")
cat("======================================\n\n")

# Convert flagged outliers to NA
conversion_log <- data.frame(
  Variable = character(),
  Values_Converted = integer(),
  Conversion_Details = character(),
  stringsAsFactors = FALSE
)

# Ransom
cat(">>> Processing: Ransom <<<\n")
ransom_to_convert <- sum(data_clean$is_outlier_ransom, na.rm = TRUE)
cat("  Values to convert to NA:", ransom_to_convert, "\n")

# Show breakdown
logical_count <- sum(data_clean$outlier_ransom_logical, na.rm = TRUE)
mad_count <- sum(data_clean$outlier_ransom_mad, na.rm = TRUE)
pct_count <- sum(data_clean$outlier_ransom_percentile, na.rm = TRUE)

cat(sprintf("    Phase 1 (Logical): %d\n", logical_count))
cat(sprintf("    Phase 2 (MAD): %d\n", mad_count))
cat(sprintf("    Phase 2 (Percentile): %d\n", pct_count))

# Convert to NA
data_clean$Ransom[data_clean$is_outlier_ransom] <- NA
cat("  Conversion complete.\n\n")

conversion_log <- rbind(conversion_log, data.frame(
  Variable = "Ransom",
  Values_Converted = ransom_to_convert,
  Conversion_Details = sprintf("Logical:%d, MAD:%d, Percentile:%d", logical_count, mad_count, pct_count)
))

# DownTime
cat(">>> Processing: DownTime <<<\n")
downtime_to_convert <- sum(data_clean$is_outlier_downtime, na.rm = TRUE)
cat("  Values to convert to NA:", downtime_to_convert, "\n")

# Show breakdown
logical_count <- sum(data_clean$outlier_downtime_logical, na.rm = TRUE)
mad_count <- sum(data_clean$outlier_downtime_mad, na.rm = TRUE)
pct_count <- sum(data_clean$outlier_downtime_percentile, na.rm = TRUE)

cat(sprintf("    Phase 1 (Logical): %d\n", logical_count))
cat(sprintf("    Phase 2 (MAD): %d\n", mad_count))
cat(sprintf("    Phase 2 (Percentile): %d\n", pct_count))

# Convert to NA
data_clean$DownTime[data_clean$is_outlier_downtime] <- NA
cat("  Conversion complete.\n\n")

conversion_log <- rbind(conversion_log, data.frame(
  Variable = "DownTime",
  Values_Converted = downtime_to_convert,
  Conversion_Details = sprintf("Logical:%d, MAD:%d, Percentile:%d", logical_count, mad_count, pct_count)
))

# Loss
cat(">>> Processing: Loss <<<\n")
loss_to_convert <- sum(data_clean$is_outlier_loss, na.rm = TRUE)
cat("  Values to convert to NA:", loss_to_convert, "\n")

# Show breakdown
logical_count <- sum(data_clean$outlier_loss_logical, na.rm = TRUE)
mad_count <- sum(data_clean$outlier_loss_mad, na.rm = TRUE)
pct_count <- sum(data_clean$outlier_loss_percentile, na.rm = TRUE)

cat(sprintf("    Phase 1 (Logical): %d\n", logical_count))
cat(sprintf("    Phase 2 (MAD): %d\n", mad_count))
cat(sprintf("    Phase 2 (Percentile): %d\n", pct_count))

# Convert to NA
data_clean$Loss[data_clean$is_outlier_loss] <- NA
cat("  Conversion complete.\n\n")

conversion_log <- rbind(conversion_log, data.frame(
  Variable = "Loss",
  Values_Converted = loss_to_convert,
  Conversion_Details = sprintf("Logical:%d, MAD:%d, Percentile:%d", logical_count, mad_count, pct_count)
))

cat("Conversion Log:\n")
cat("----------------\n")
print(conversion_log, row.names = FALSE)
cat("\n")

cat("======================================\n")
cat("AFTER REMOVAL - DATA SUMMARY\n")
cat("======================================\n\n")

# Show new state
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
  valid_data <- data_clean[[col]][!is.na(data_clean[[col]])]

  after_summary <- rbind(after_summary, data.frame(
    Variable = col,
    Total_Records = nrow(data_clean),
    Valid_Values = length(valid_data),
    NA_Count = sum(is.na(data_clean[[col]])),
    NA_Percentage = round(sum(is.na(data_clean[[col]])) / nrow(data_clean) * 100, 2),
    Min = ifelse(length(valid_data) > 0, min(valid_data), NA),
    Max = ifelse(length(valid_data) > 0, max(valid_data), NA),
    Mean = ifelse(length(valid_data) > 0, mean(valid_data), NA),
    Median = ifelse(length(valid_data) > 0, median(valid_data), NA)
  ))
}

cat("Data Summary AFTER Outlier Removal:\n")
cat("------------------------------------\n")
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
  Removal_Rate = round((before_summary$Valid_Values - after_summary$Valid_Values) / before_summary$Valid_Values * 100, 2),
  Min_Before = before_summary$Min,
  Min_After = after_summary$Min,
  Max_Before = before_summary$Max,
  Max_After = after_summary$Max,
  Mean_Before = before_summary$Mean,
  Mean_After = after_summary$Mean,
  Mean_Change_Pct = round((after_summary$Mean - before_summary$Mean) / before_summary$Mean * 100, 2)
)

cat("Comparison Table:\n")
cat("-----------------\n")
print(comparison, row.names = FALSE)
cat("\n")

cat("Key Observations:\n")
for (i in 1:nrow(comparison)) {
  cat(sprintf("\n%s:\n", comparison$Variable[i]))
  cat(sprintf("  Removed %d values (%.2f%% of valid data)\n",
              comparison$Values_Removed[i],
              comparison$Removal_Rate[i]))

  if (!is.na(comparison$Max_Before[i]) && !is.na(comparison$Max_After[i])) {
    cat(sprintf("  Max value changed: %.2f → %.2f\n",
                comparison$Max_Before[i],
                comparison$Max_After[i]))
  }

  if (!is.na(comparison$Mean_Before[i]) && !is.na(comparison$Mean_After[i])) {
    cat(sprintf("  Mean changed: %.2f → %.2f (%.2f%% change)\n",
                comparison$Mean_Before[i],
                comparison$Mean_After[i],
                comparison$Mean_Change_Pct[i]))
  }
}
cat("\n")

cat("======================================\n")
cat("CLEANING UP FLAG COLUMNS (OPTIONAL)\n")
cat("======================================\n\n")

cat("The flagged data contains detailed flag columns for traceability.\n")
cat("You can choose to:\n")
cat("  A. Keep all flag columns (recommended for academic/audit purposes)\n")
cat("  B. Keep only combined flags (is_outlier_XXX)\n")
cat("  C. Remove all flag columns (minimal dataset)\n\n")

cat("Current approach: Keeping all flag columns for full traceability.\n")
cat("To remove flags later, you can select only the original columns.\n\n")

cat("======================================\n")
cat("SAVING CLEANED DATA\n")
cat("======================================\n\n")

# Save cleaned data with all flags (for traceability)
output_rdata_full <- "raw_data/processed_data/outlier_removed_data_full.RData"
save(data_clean, conversion_log, before_summary, after_summary, comparison,
     file = output_rdata_full)
cat("Full cleaned data (with flags) saved to:", output_rdata_full, "\n\n")

# Create minimal version (original columns + cleaned data only)
data_clean_minimal <- data_clean[, c("Date", "Notify", "URL", "IP", "Country",
                                      "WebServer", "Encoding", "Ransom", "DownTime", "Loss")]

output_rdata_minimal <- "raw_data/processed_data/outlier_removed_data.RData"
save(data_clean_minimal, file = output_rdata_minimal)
cat("Minimal cleaned data (no flags) saved to:", output_rdata_minimal, "\n\n")

# Save comparison report as CSV
output_comparison <- "docs/reports/outlier_removal_report.csv"
write.csv(comparison, output_comparison, row.names = FALSE)
cat("Comparison report saved to:", output_comparison, "\n\n")

cat("======================================\n")
cat("DATA STRUCTURE\n")
cat("======================================\n\n")

cat("Minimal cleaned data structure:\n")
str(data_clean_minimal)
cat("\n")

cat("======================================\n")
cat("NEXT STEPS\n")
cat("======================================\n\n")

cat("1. Review the before/after comparison above\n")
cat("2. Check if the removal rates are reasonable\n")
cat("3. Proceed with missing value imputation\n")
cat("4. Use 'outlier_removed_data.RData' for further analysis\n\n")

cat("Files created:\n")
cat("  - raw_data/processed_data/outlier_removed_data_full.RData (with all tracking flags)\n")
cat("  - raw_data/processed_data/outlier_removed_data.RData (clean version, ready for analysis)\n")
cat("  - docs/reports/outlier_removal_report.csv (statistical comparison)\n\n")

cat("Outlier removal complete!\n")
