# Multivariate Outlier Detection Script (Phase 3)
# Purpose: Detect "weird combinations" using Isolation Forest
# Method: Isolation Forest - detects structural anomalies in multivariate data
# Author: LiuWei TP085412
# Date: 2026-01-21

cat("======================================\n")
cat("PHASE 3: MULTIVARIATE OUTLIER DETECTION\n")
cat("Target: Weird Combinations (Structural Anomalies)\n")
cat("======================================\n\n")

# Load required library
if (!require("isotree")) {
  cat("Installing required package: isotree...\n")
  install.packages("isotree", repos = "https://cloud.r-project.org/")
  library(isotree)
} else {
  library(isotree)
}

# Load data from Phase 2
cat("Loading cleaned data from Phase 2...\n")
load("raw_data/processed_data/outlier_removed_data.RData")
cat("Data loaded successfully.\n")
cat("Total records:", nrow(data_clean_minimal), "\n\n")

# Create working copy
data_multivariate <- data_clean_minimal

# Numeric columns for multivariate analysis
numeric_cols <- c("Ransom", "DownTime", "Loss")

cat("======================================\n")
cat("PHASE 3 OVERVIEW\n")
cat("======================================\n\n")

cat("What is Isolation Forest?\n")
cat("---------------------------\n")
cat("Isolation Forest is an unsupervised machine learning algorithm that:\n")
cat("  - Detects anomalies by isolating observations\n")
cat("  - Works well with high-dimensional data\n")
cat("  - Doesn't assume any distribution (non-parametric)\n")
cat("  - Identifies 'weird combinations' that univariate methods miss\n\n")

cat("Examples of weird combinations:\n")
cat("  Record A: DownTime = 1 hour, Loss = $10,000,000\n")
cat("            → Very short downtime but huge loss (suspicious!)\n")
cat("  Record B: DownTime = 500 hours, Loss = $100\n")
cat("            → Long downtime but tiny loss (weird!)\n\n")

cat("These records have normal individual values but abnormal relationships.\n\n")

cat("======================================\n")
cat("DATA PREPARATION\n")
cat("======================================\n\n")

# Count complete cases (no NA in any numeric column)
complete_cases <- complete.cases(data_multivariate[, numeric_cols])
n_complete <- sum(complete_cases)
n_incomplete <- sum(!complete_cases)

cat("Complete cases analysis:\n")
cat("  Records with ALL numeric values:", n_complete, "\n")
cat("  Records with ANY missing values:", n_incomplete, "\n")
cat("  Percentage complete:", round(n_complete / nrow(data_multivariate) * 100, 2), "%\n\n")

if (n_complete < 100) {
  cat("ERROR: Not enough complete cases for Isolation Forest.\n")
  cat("Minimum required: 100 complete records.\n")
  cat("Please run missing value imputation first.\n")
  stop("Insufficient data for multivariate analysis.")
}

# Extract complete cases for analysis
data_for_iforest <- data_multivariate[complete_cases, numeric_cols]

cat("Data prepared for Isolation Forest:\n")
cat("  Samples:", nrow(data_for_iforest), "\n")
cat("  Features:", ncol(data_for_iforest), "\n\n")

# Show summary statistics of the data going into Isolation Forest
cat("Summary statistics of complete cases:\n")
cat("--------------------------------------\n")
print(summary(data_for_iforest))
cat("\n")

cat("======================================\n")
cat("ISOLATION FOREST TRAINING\n")
cat("======================================\n\n")

cat("Training Isolation Forest model...\n")
cat("Parameters:\n")
cat("  - Number of trees: 100\n")
cat("  - Sample size: 256 (or all if less)\n")
cat("  - Contamination rate: 0.05 (5% expected anomalies)\n")
cat("  - Random seed: 42 (for reproducibility)\n\n")

# Set random seed for reproducibility
set.seed(42)

# Train Isolation Forest
# ntrees: number of isolation trees (more = more stable, slower)
# sample_size: number of samples to use for each tree
# ndim: number of dimensions to use for each split (default 1)
cat("Building isolation forest (this may take a moment)...\n")

iforest_model <- isolation.forest(
  data_for_iforest,
  ntrees = 100,
  sample_size = min(256, nrow(data_for_iforest)),
  ndim = 1,
  seed = 42,
  nthreads = 1
)

cat("Model training complete!\n\n")

cat("======================================\n")
cat("ANOMALY SCORING\n")
cat("======================================\n\n")

cat("Computing anomaly scores for all complete cases...\n")
cat("Anomaly score interpretation:\n")
cat("  - Score > 0.5: Likely anomaly (easier to isolate)\n")
cat("  - Score ~ 0.5: Normal observation\n")
cat("  - Score < 0.5: Very normal observation\n\n")

# Predict anomaly scores
# Higher score = more anomalous
anomaly_scores <- predict(iforest_model, data_for_iforest, type = "score")

cat("Anomaly score statistics:\n")
cat("  Min:   ", round(min(anomaly_scores), 4), "\n")
cat("  Q1:    ", round(quantile(anomaly_scores, 0.25), 4), "\n")
cat("  Median:", round(median(anomaly_scores), 4), "\n")
cat("  Mean:  ", round(mean(anomaly_scores), 4), "\n")
cat("  Q3:    ", round(quantile(anomaly_scores, 0.75), 4), "\n")
cat("  Max:   ", round(max(anomaly_scores), 4), "\n\n")

# Determine threshold for anomaly detection
# Method 1: Fixed threshold (e.g., 0.6)
threshold_fixed <- 0.6

# Method 2: Percentile-based (e.g., top 5%)
threshold_percentile <- quantile(anomaly_scores, 0.95)

cat("Anomaly detection thresholds:\n")
cat("  Method 1 (Fixed):      ", round(threshold_fixed, 4), "\n")
cat("  Method 2 (95th %ile):  ", round(threshold_percentile, 4), "\n\n")

# Use the more conservative (higher) threshold
threshold <- max(threshold_fixed, threshold_percentile)
cat("Selected threshold:      ", round(threshold, 4), "\n\n")

# Flag anomalies
is_anomaly_iforest <- anomaly_scores > threshold
n_anomalies <- sum(is_anomaly_iforest)

cat("Anomalies detected:\n")
cat("  Count:", n_anomalies, "\n")
cat("  Percentage:", round(n_anomalies / length(anomaly_scores) * 100, 2), "%\n\n")

cat("======================================\n")
cat("FLAGGING RESULTS\n")
cat("======================================\n\n")

# Add anomaly flags to the full dataset
# Initialize all as FALSE
data_multivariate$outlier_iforest <- FALSE
data_multivariate$iforest_score <- NA

# Set flags for complete cases
data_multivariate$outlier_iforest[complete_cases] <- is_anomaly_iforest
data_multivariate$iforest_score[complete_cases] <- anomaly_scores

cat("Flagging summary:\n")
cat("  Total records:", nrow(data_multivariate), "\n")
cat("  Complete cases analyzed:", n_complete, "\n")
cat("  Flagged as multivariate outliers:", n_anomalies, "\n")
cat("  Records with missing values (not analyzed):", n_incomplete, "\n\n")

cat("======================================\n")
cat("ANOMALY ANALYSIS\n")
cat("======================================\n\n")

if (n_anomalies > 0) {
  cat("Analyzing detected anomalies...\n\n")

  # Get anomalous records
  anomaly_records <- data_for_iforest[is_anomaly_iforest, ]

  cat("Top 10 most anomalous records:\n")
  cat("-------------------------------\n")

  # Sort by anomaly score (descending)
  top_anomalies_idx <- order(anomaly_scores, decreasing = TRUE)[1:min(10, n_anomalies)]
  top_anomalies <- data_for_iforest[top_anomalies_idx, ]
  top_scores <- anomaly_scores[top_anomalies_idx]

  # Display with scores
  top_anomalies_display <- cbind(
    Rank = 1:nrow(top_anomalies),
    top_anomalies,
    AnomalyScore = round(top_scores, 4)
  )
  print(top_anomalies_display, row.names = FALSE)
  cat("\n")

  # Analyze characteristics of anomalies vs normal
  cat("Comparison: Anomalies vs Normal Records\n")
  cat("----------------------------------------\n")

  normal_records <- data_for_iforest[!is_anomaly_iforest, ]

  comparison_table <- data.frame(
    Variable = numeric_cols,
    Anomaly_Mean = sapply(anomaly_records, mean),
    Normal_Mean = sapply(normal_records, mean),
    Anomaly_Median = sapply(anomaly_records, median),
    Normal_Median = sapply(normal_records, median),
    Anomaly_SD = sapply(anomaly_records, sd),
    Normal_SD = sapply(normal_records, sd)
  )

  print(comparison_table, row.names = FALSE)
  cat("\n")

  # Identify common patterns in anomalies
  cat("Common patterns in anomalies:\n")
  cat("------------------------------\n")

  # Pattern 1: High Loss, Low DownTime
  pattern1 <- anomaly_records$Loss > median(normal_records$Loss) &
               anomaly_records$DownTime < median(normal_records$DownTime)
  cat("  High Loss + Low DownTime:", sum(pattern1), "records\n")

  # Pattern 2: Low Loss, High DownTime
  pattern2 <- anomaly_records$Loss < median(normal_records$Loss) &
               anomaly_records$DownTime > median(normal_records$DownTime)
  cat("  Low Loss + High DownTime:", sum(pattern2), "records\n")

  # Pattern 3: High Ransom, Low Loss
  pattern3 <- anomaly_records$Ransom > median(normal_records$Ransom, na.rm = TRUE) &
               anomaly_records$Loss < median(normal_records$Loss)
  cat("  High Ransom + Low Loss:", sum(pattern3), "records\n")

  # Pattern 4: Other combinations
  pattern4 <- !(pattern1 | pattern2 | pattern3)
  cat("  Other unusual combinations:", sum(pattern4), "records\n\n")

} else {
  cat("No anomalies detected with current threshold.\n")
  cat("Consider lowering the threshold if you expect more anomalies.\n\n")
}

cat("======================================\n")
cat("VISUALIZATION PREPARATION\n")
cat("======================================\n\n")

cat("Creating visualization for anomaly detection...\n")

# Create PDF with visualizations
pdf("docs/reports/multivariate_outlier_analysis.pdf", width = 14, height = 10)

# Layout: 2x2 plots
par(mfrow = c(2, 2))

# Plot 1: Anomaly Score Distribution
hist(anomaly_scores,
     breaks = 50,
     main = "Anomaly Score Distribution",
     xlab = "Anomaly Score",
     ylab = "Frequency",
     col = "skyblue",
     border = "white")
abline(v = threshold, col = "red", lwd = 2, lty = 2)
legend("topright",
       legend = c(paste("Threshold =", round(threshold, 3)),
                  paste("Anomalies =", n_anomalies)),
       col = c("red", "black"),
       lty = c(2, 0),
       lwd = 2)

# Plot 2: Loss vs DownTime (colored by anomaly status)
plot(data_for_iforest$DownTime,
     data_for_iforest$Loss,
     col = ifelse(is_anomaly_iforest, "red", "blue"),
     pch = ifelse(is_anomaly_iforest, 17, 16),
     cex = ifelse(is_anomaly_iforest, 1.2, 0.5),
     main = "Loss vs DownTime\n(Red = Multivariate Outliers)",
     xlab = "DownTime (hours)",
     ylab = "Loss ($)",
     log = "xy")
grid()
legend("bottomright",
       legend = c("Normal", "Anomaly"),
       col = c("blue", "red"),
       pch = c(16, 17),
       cex = 0.8)

# Plot 3: Ransom vs Loss (colored by anomaly status)
plot(data_for_iforest$Ransom,
     data_for_iforest$Loss,
     col = ifelse(is_anomaly_iforest, "red", "blue"),
     pch = ifelse(is_anomaly_iforest, 17, 16),
     cex = ifelse(is_anomaly_iforest, 1.2, 0.5),
     main = "Ransom vs Loss\n(Red = Multivariate Outliers)",
     xlab = "Ransom ($)",
     ylab = "Loss ($)",
     log = "xy")
grid()
legend("bottomright",
       legend = c("Normal", "Anomaly"),
       col = c("blue", "red"),
       pch = c(16, 17),
       cex = 0.8)

# Plot 4: Ransom vs DownTime (colored by anomaly status)
plot(data_for_iforest$DownTime,
     data_for_iforest$Ransom,
     col = ifelse(is_anomaly_iforest, "red", "blue"),
     pch = ifelse(is_anomaly_iforest, 17, 16),
     cex = ifelse(is_anomaly_iforest, 1.2, 0.5),
     main = "DownTime vs Ransom\n(Red = Multivariate Outliers)",
     xlab = "DownTime (hours)",
     ylab = "Ransom ($)",
     log = "xy")
grid()
legend("bottomright",
       legend = c("Normal", "Anomaly"),
       col = c("blue", "red"),
       pch = c(16, 17),
       cex = 0.8)

dev.off()

cat("Visualization saved to: docs/reports/multivariate_outlier_analysis.pdf\n\n")

cat("======================================\n")
cat("SAVING RESULTS\n")
cat("======================================\n\n")

# Save flagged data with Isolation Forest results
output_rdata <- "raw_data/processed_data/multivariate_outlier_flagged_data.RData"
save(data_multivariate, iforest_model, threshold, anomaly_scores,
     file = output_rdata)
cat("Flagged data saved to:", output_rdata, "\n\n")

# Create summary report
phase3_summary <- data.frame(
  Metric = c("Total Records",
             "Complete Cases",
             "Incomplete Cases",
             "Anomalies Detected",
             "Anomaly Percentage",
             "Threshold Used"),
  Value = c(nrow(data_multivariate),
            n_complete,
            n_incomplete,
            n_anomalies,
            round(n_anomalies / n_complete * 100, 2),
            round(threshold, 4))
)

# Save summary
output_summary <- "docs/reports/multivariate_outlier_summary.csv"
write.csv(phase3_summary, output_summary, row.names = FALSE)
cat("Summary saved to:", output_summary, "\n\n")

cat("======================================\n")
cat("PHASE 3 SUMMARY\n")
cat("======================================\n\n")

print(phase3_summary, row.names = FALSE)
cat("\n")

cat("======================================\n")
cat("NEXT STEPS\n")
cat("======================================\n\n")

cat("1. Review the visualization PDF to understand anomaly patterns\n")
cat("2. Examine the top anomalous records to verify they are indeed weird\n")
cat("3. Run 06_multivariate_outlier_to_na.R to convert these to NA (if appropriate)\n")
cat("4. Or keep them if these 'weird combinations' are valid edge cases\n\n")

cat("Important Notes:\n")
cat("  - Isolation Forest found STRUCTURAL anomalies (weird relationships)\n")
cat("  - These may be data entry errors OR genuine rare events\n")
cat("  - Human judgment is crucial: not all anomalies should be removed!\n")
cat("  - Consider domain knowledge before removing these records\n\n")

cat("Column Added:\n")
cat("  - outlier_iforest: TRUE if flagged by Isolation Forest\n")
cat("  - iforest_score: Anomaly score (higher = more anomalous)\n\n")

cat("Phase 3 complete!\n")
