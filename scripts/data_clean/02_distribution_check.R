# Distribution Check Script
# Purpose: Quick screening to determine if data follows normal or skewed distribution
# Author: LiuWei TP085412
# Date: 2026-01-21

cat("======================================\n")
cat("DISTRIBUTION SCREENING ANALYSIS\n")
cat("======================================\n\n")

# Load converted data
cat("Loading converted data...\n")
load("raw_data/processed_data/converted_hacking_data.RData")
cat("Data loaded successfully.\n")
cat("Total records:", nrow(data_converted), "\n\n")

# Focus on numeric columns: Ransom, DownTime, Loss
numeric_cols <- c("Ransom", "DownTime", "Loss")

cat("======================================\n")
cat("STEP 1: DESCRIPTIVE STATISTICS\n")
cat("The First Meeting with Your Data\n")
cat("======================================\n\n")

cat("Key Signal to Watch:\n")
cat("  - If Mean >> Median: Red flag for skewed distribution\n")
cat("  - If Max is orders of magnitude larger than 3rd Quartile: Extreme outliers present\n\n")

# Display summary statistics
cat("Summary Statistics for Numeric Variables:\n")
cat("------------------------------------------\n\n")

for (col in numeric_cols) {
  cat(paste0(">>> ", col, " <<<\n"))
  print(summary(data_converted[[col]]))

  # Calculate additional metrics
  non_na_data <- data_converted[[col]][!is.na(data_converted[[col]])]

  if (length(non_na_data) > 0) {
    mean_val <- mean(non_na_data, na.rm = TRUE)
    median_val <- median(non_na_data, na.rm = TRUE)
    sd_val <- sd(non_na_data, na.rm = TRUE)

    cat("Standard Deviation:", format(sd_val, scientific = TRUE, digits = 3), "\n")
    cat("Mean/Median Ratio:", format(mean_val / median_val, digits = 3), "\n")

    # Interpretation
    if (mean_val > median_val * 1.5) {
      cat(">>> ALERT: Mean >> Median - Strong RIGHT SKEW detected! <<<\n")
    } else if (mean_val < median_val * 0.67) {
      cat(">>> ALERT: Mean << Median - LEFT SKEW detected! <<<\n")
    } else {
      cat(">>> OK: Mean ≈ Median - Relatively symmetric <<<\n")
    }
  }
  cat("\n")
}

cat("\n======================================\n")
cat("STEP 2: VISUALIZATION CHECK\n")
cat("The Histogram 'Wall-Hitting' Test\n")
cat("======================================\n\n")

cat("Creating distribution plots...\n")
cat("Output directory: docs/reports/\n\n")

# Create PDF with all diagnostic plots
pdf("docs/reports/distribution_diagnostics.pdf", width = 12, height = 8)

for (col in numeric_cols) {
  # Remove NA values for plotting
  plot_data <- data_converted[[col]][!is.na(data_converted[[col]])]

  if (length(plot_data) == 0) {
    cat(paste0("Skipping ", col, " - no valid data\n"))
    next
  }

  cat(paste0("Plotting ", col, "...\n"))

  # Create a 2x2 layout for each variable
  par(mfrow = c(2, 2))

  # 1. Standard Histogram (will likely "break")
  hist(plot_data,
       main = paste0(col, ": Raw Distribution\n(Expect: One tall bar on left, empty space on right)"),
       xlab = col,
       col = "steelblue",
       border = "white",
       breaks = 30)

  # Add vertical lines for mean and median
  abline(v = mean(plot_data), col = "red", lwd = 2, lty = 2)
  abline(v = median(plot_data), col = "blue", lwd = 2, lty = 2)
  legend("topright",
         legend = c("Mean", "Median"),
         col = c("red", "blue"),
         lty = 2,
         lwd = 2,
         cex = 0.8)

  # 2. Log-transformed Histogram (to see the real distribution)
  # Only if data has positive values
  if (min(plot_data) > 0) {
    hist(log10(plot_data),
         main = paste0(col, ": Log10-Transformed Distribution\n(Better view of actual pattern)"),
         xlab = paste0("log10(", col, ")"),
         col = "coral",
         border = "white",
         breaks = 30)
  } else {
    plot.new()
    text(0.5, 0.5, "Cannot log-transform:\nData contains negative values", cex = 1.5)
  }

  # 3. Boxplot (to see outliers)
  boxplot(plot_data,
          main = paste0(col, ": Boxplot\n(Check for extreme outliers)"),
          ylab = col,
          col = "lightgreen",
          border = "darkgreen",
          outcol = "red",
          outpch = 16)

  # Calculate outlier statistics
  q1 <- quantile(plot_data, 0.25)
  q3 <- quantile(plot_data, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  outliers <- sum(plot_data < lower_bound | plot_data > upper_bound)

  text(1, max(plot_data) * 0.9,
       paste0("Outliers: ", outliers, " (", round(outliers/length(plot_data)*100, 2), "%)"),
       col = "red", cex = 0.9)

  # 4. Q-Q Plot (diagnostic for normality)
  qqnorm(plot_data,
         main = paste0(col, ": Q-Q Plot\n(Points on line = Normal, Curve = Skewed)"),
         pch = 16,
         col = rgb(0, 0, 1, 0.3))
  qqline(plot_data, col = "red", lwd = 2)

  # Add interpretation text
  text(min(qqnorm(plot_data, plot.it = FALSE)$x),
       max(qqnorm(plot_data, plot.it = FALSE)$y) * 0.9,
       "If points deviate from red line:\n→ NOT normally distributed",
       adj = 0, cex = 0.8, col = "darkred")
}

dev.off()

cat("\n>>> PDF saved: docs/reports/distribution_diagnostics.pdf <<<\n\n")

cat("======================================\n")
cat("STEP 3: NORMALITY DIAGNOSTIC TESTS\n")
cat("Statistical Evidence of Distribution Type\n")
cat("======================================\n\n")

cat("Q-Q Plot Interpretation Guide:\n")
cat("  - Points follow red line: Normal distribution\n")
cat("  - Points form upward curve: Right-skewed (common in financial data)\n")
cat("  - Points form downward curve: Left-skewed\n")
cat("  - Points deviate at extremes: Heavy-tailed distribution\n\n")

cat("Visual inspection results saved to PDF.\n")
cat("Please review the plots to determine:\n")
cat("  1. Is the distribution skewed? (Check histogram and Q-Q plot)\n")
cat("  2. Are there extreme outliers? (Check boxplot)\n")
cat("  3. Should we use log transformation? (Compare raw vs log histogram)\n\n")

cat("======================================\n")
cat("STEP 4: SKEWNESS ANALYSIS\n")
cat("======================================\n\n")

cat("Computing skewness metrics...\n\n")

# Create summary table
skew_summary <- data.frame(
  Variable = character(),
  N_Valid = integer(),
  Mean = numeric(),
  Median = numeric(),
  Mean_Median_Ratio = numeric(),
  SD = numeric(),
  Min = numeric(),
  Max = numeric(),
  Range_Span = numeric(),
  Distribution_Type = character(),
  stringsAsFactors = FALSE
)

for (col in numeric_cols) {
  plot_data <- data_converted[[col]][!is.na(data_converted[[col]])]

  if (length(plot_data) == 0) next

  mean_val <- mean(plot_data)
  median_val <- median(plot_data)
  ratio <- mean_val / median_val

  # Determine distribution type
  if (ratio > 1.5) {
    dist_type <- "Right-Skewed (SEVERE)"
  } else if (ratio > 1.2) {
    dist_type <- "Right-Skewed (MODERATE)"
  } else if (ratio < 0.67) {
    dist_type <- "Left-Skewed (SEVERE)"
  } else if (ratio < 0.83) {
    dist_type <- "Left-Skewed (MODERATE)"
  } else {
    dist_type <- "Approximately Symmetric"
  }

  skew_summary <- rbind(skew_summary, data.frame(
    Variable = col,
    N_Valid = length(plot_data),
    Mean = mean_val,
    Median = median_val,
    Mean_Median_Ratio = ratio,
    SD = sd(plot_data),
    Min = min(plot_data),
    Max = max(plot_data),
    Range_Span = max(plot_data) / (min(plot_data) + 1), # Add 1 to avoid division by 0
    Distribution_Type = dist_type
  ))
}

cat("Skewness Summary Table:\n")
cat("------------------------\n")
print(skew_summary, row.names = FALSE)

cat("\n======================================\n")
cat("FINAL CONCLUSION\n")
cat("======================================\n\n")

cat("Based on the analysis above:\n\n")

for (i in 1:nrow(skew_summary)) {
  cat(paste0(skew_summary$Variable[i], ":\n"))
  cat(paste0("  Distribution Type: ", skew_summary$Distribution_Type[i], "\n"))

  if (grepl("SEVERE|MODERATE", skew_summary$Distribution_Type[i])) {
    cat("  ❌ CANNOT use Z-Score for outlier detection (assumes normal distribution)\n")
    cat("  ❌ CANNOT use Mahalanobis distance (assumes multivariate normal)\n")
    cat("  ✅ RECOMMEND: IQR method, Percentile-based methods, or Robust methods\n")
    cat("  ✅ CONSIDER: Log transformation before analysis\n")
  } else {
    cat("  ✅ MAY use parametric methods (Z-Score, etc.)\n")
    cat("  ⚠️  Still verify with Q-Q plot in the PDF\n")
  }
  cat("\n")
}

cat("======================================\n")
cat("NEXT STEPS\n")
cat("======================================\n\n")

cat("1. Review the PDF file: docs/reports/distribution_diagnostics.pdf\n")
cat("2. Choose appropriate outlier detection method based on distribution type\n")
cat("3. Consider data transformation if severe skewness detected\n")
cat("4. Proceed with outlier detection using suitable algorithms\n\n")

cat("Analysis complete!\n")
