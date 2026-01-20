# Data Health Check Script
# Purpose: Visualize missing data patterns and post-cleaning distribution quality
# Methods: naniar, VIM packages for missingness analysis
# Author: LiuWei TP085412
# Date: 2026-01-21

cat("======================================\n")
cat("DATA HEALTH CHECK\n")
cat("Missing Patterns & Distribution Quality\n")
cat("======================================\n\n")

# Install and load required packages
required_packages <- c("naniar", "VIM", "moments", "ggplot2", "gridExtra")

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

# Load cleaned data
cat("Loading final cleaned data...\n")
load("raw_data/processed_data/final_cleaned_data.RData")
cat("Data loaded successfully.\n")
cat("Total records:", nrow(data_final_minimal), "\n\n")

# Create working copy
data_health <- data_final_minimal

# Define column groups
numeric_cols <- c("Ransom", "DownTime", "Loss")
categorical_cols <- c("Country", "WebServer", "Encoding")
all_analysis_cols <- c(numeric_cols, categorical_cols, "Date")

cat("======================================\n")
cat("PART 1: MISSING DATA DIAGNOSIS\n")
cat("Objective: Understand NA patterns\n")
cat("======================================\n\n")

cat("Missing Data Overview:\n")
cat("-----------------------\n")

# Calculate missing statistics
missing_stats <- data.frame(
  Variable = names(data_health),
  Missing_Count = colSums(is.na(data_health)),
  Missing_Percentage = round(colSums(is.na(data_health)) / nrow(data_health) * 100, 2),
  Present_Count = colSums(!is.na(data_health)),
  Present_Percentage = round(colSums(!is.na(data_health)) / nrow(data_health) * 100, 2)
)

missing_stats <- missing_stats[order(missing_stats$Missing_Percentage, decreasing = TRUE), ]

print(missing_stats, row.names = FALSE)
cat("\n")

# Complete case analysis
complete_cases_count <- sum(complete.cases(data_health[, numeric_cols]))
complete_cases_pct <- round(complete_cases_count / nrow(data_health) * 100, 2)

cat("Complete Cases Analysis:\n")
cat("  Records with ALL numeric values complete:", complete_cases_count,
    paste0("(", complete_cases_pct, "%)"), "\n")
cat("  Records with ANY missing numeric value:",
    nrow(data_health) - complete_cases_count,
    paste0("(", 100 - complete_cases_pct, "%)"), "\n\n")

cat("======================================\n")
cat("PART 2: MISSINGNESS MECHANISM TEST\n")
cat("Testing: MCAR, MAR, or MNAR?\n")
cat("======================================\n\n")

cat("Definitions:\n")
cat("  MCAR (Missing Completely At Random): Missing values are randomly distributed\n")
cat("        → Safe to use simple imputation methods\n")
cat("  MAR  (Missing At Random): Missing depends on observed variables\n")
cat("        → Need advanced methods like MICE (Multiple Imputation)\n")
cat("  MNAR (Missing Not At Random): Missing depends on the missing value itself\n")
cat("        → Very difficult to impute correctly\n\n")

# Test 1: Missing pattern analysis by categorical variables
cat("Test 1: Is missingness related to categorical variables?\n")
cat("----------------------------------------------------------\n")

# Country
if (!all(is.na(data_health$Country))) {
  cat("\nMissing Rate by Country (Top 10):\n")

  country_missing <- aggregate(
    cbind(Ransom_NA = is.na(data_health$Ransom),
          DownTime_NA = is.na(data_health$DownTime),
          Loss_NA = is.na(data_health$Loss)) ~ Country,
    data = data_health,
    FUN = function(x) round(mean(x) * 100, 2)
  )

  # Calculate average missingness
  country_missing$Avg_Missing <- rowMeans(country_missing[, c("Ransom_NA", "DownTime_NA", "Loss_NA")])
  country_missing <- country_missing[order(country_missing$Avg_Missing, decreasing = TRUE), ]

  print(head(country_missing, 10), row.names = FALSE)

  # Statistical test: Chi-square test for independence
  # Test if missingness is independent of Country
  cat("\n  Statistical Test: Is missingness independent of Country?\n")

  # Create binary missing indicator for any numeric variable
  data_health$any_numeric_missing <- apply(data_health[, numeric_cols], 1, function(x) any(is.na(x)))

  # Chi-square test (sample top countries to avoid sparse cells)
  top_countries <- names(sort(table(data_health$Country), decreasing = TRUE))[1:10]
  test_data <- data_health[data_health$Country %in% top_countries, ]

  if (nrow(test_data) > 0) {
    contingency_table <- table(test_data$Country, test_data$any_numeric_missing)
    chi_test <- chisq.test(contingency_table)

    cat("  Chi-square test p-value:", format.pval(chi_test$p.value, digits = 3), "\n")

    if (chi_test$p.value < 0.05) {
      cat("  → Result: Missingness IS related to Country (p < 0.05)\n")
      cat("  → Implication: Data is likely MAR or MNAR, not MCAR\n")
    } else {
      cat("  → Result: Missingness is NOT significantly related to Country (p >= 0.05)\n")
      cat("  → Implication: Supports MCAR hypothesis\n")
    }
  }
}

# WebServer
cat("\n\nMissing Rate by WebServer (Top 10):\n")
webserver_missing <- aggregate(
  cbind(Ransom_NA = is.na(data_health$Ransom),
        DownTime_NA = is.na(data_health$DownTime),
        Loss_NA = is.na(data_health$Loss)) ~ WebServer,
  data = data_health,
  FUN = function(x) round(mean(x) * 100, 2)
)

webserver_missing$Avg_Missing <- rowMeans(webserver_missing[, c("Ransom_NA", "DownTime_NA", "Loss_NA")])
webserver_missing <- webserver_missing[order(webserver_missing$Avg_Missing, decreasing = TRUE), ]

print(head(webserver_missing, 10), row.names = FALSE)

# Test 2: Temporal patterns
cat("\n\nTest 2: Is missingness related to time?\n")
cat("------------------------------------------\n")

if (!all(is.na(data_health$Date))) {
  # Extract year from Date
  data_health$Year <- as.numeric(format(data_health$Date, "%Y"))

  # Only analyze years with valid dates
  valid_year_data <- data_health[!is.na(data_health$Year), ]

  if (nrow(valid_year_data) > 0) {
    year_missing <- aggregate(
      cbind(Ransom_NA = is.na(valid_year_data$Ransom),
            DownTime_NA = is.na(valid_year_data$DownTime),
            Loss_NA = is.na(valid_year_data$Loss)) ~ Year,
      data = valid_year_data,
      FUN = function(x) round(mean(x) * 100, 2)
    )

    year_missing$Avg_Missing <- rowMeans(year_missing[, c("Ransom_NA", "DownTime_NA", "Loss_NA")])
    year_missing <- year_missing[order(year_missing$Year), ]

    cat("\nMissing Rate by Year:\n")
    print(year_missing, row.names = FALSE)

    # Check for trend
    if (nrow(year_missing) > 2) {
      cor_year_missing <- cor(year_missing$Year, year_missing$Avg_Missing, use = "complete.obs")
      cat("\n  Correlation between Year and Missing Rate:", round(cor_year_missing, 3), "\n")

      if (abs(cor_year_missing) > 0.3) {
        cat("  → Result: Moderate to strong temporal trend detected\n")
        cat("  → Implication: Missingness may be MAR (related to time period)\n")
      } else {
        cat("  → Result: Weak temporal correlation\n")
        cat("  → Implication: Time is not a major factor in missingness\n")
      }
    }
  } else {
    cat("  Insufficient valid dates for temporal analysis.\n")
  }
} else {
  cat("  No valid dates available for temporal analysis.\n")
}

cat("\n\n======================================\n")
cat("PART 3: MISSING PATTERN VISUALIZATION\n")
cat("======================================\n\n")

cat("Creating comprehensive missing data visualizations...\n\n")

# Create PDF for missing data analysis
pdf("docs/reports/missing_data_analysis.pdf", width = 16, height = 12)

# Page 1: Missing data overview using naniar
cat("  Generating Page 1: Missing data overview...\n")

par(mfrow = c(2, 2))

# Plot 1: Missing data summary
miss_var_summary_data <- miss_var_summary(data_health)
if (nrow(miss_var_summary_data) > 0) {
  barplot(as.numeric(miss_var_summary_data$pct_miss),
          names.arg = miss_var_summary_data$variable,
          las = 2,
          main = "Missing Data Percentage by Variable",
          ylab = "Percentage Missing (%)",
          col = "coral",
          cex.names = 0.7)
  abline(h = 50, col = "red", lty = 2, lwd = 2)
  text(1, 55, "50% threshold", col = "red", cex = 0.8)
}

# Plot 2: Missing data patterns (using VIM package)
cat("  Generating Page 2: Missing patterns matrix...\n")
# Aggregate plot from VIM - shows combinations of missing patterns
aggr_plot <- aggr(data_health[, all_analysis_cols],
                   col = c('navyblue', 'red'),
                   numbers = TRUE,
                   sortVars = TRUE,
                   labels = names(data_health[, all_analysis_cols]),
                   cex.axis = 0.7,
                   gap = 3,
                   ylab = c("Histogram of missing data", "Pattern"),
                   plot = TRUE)

# Page 2: Missingness by categorical variables
cat("  Generating Page 3: Missingness by categories...\n")

par(mfrow = c(2, 2))

# Country
if (nrow(country_missing) > 0) {
  top_10_countries <- head(country_missing, 10)
  barplot(t(as.matrix(top_10_countries[, c("Ransom_NA", "DownTime_NA", "Loss_NA")])),
          beside = TRUE,
          names.arg = top_10_countries$Country,
          las = 2,
          main = "Missing Rate by Country (Top 10)",
          ylab = "Missing Percentage (%)",
          col = c("skyblue", "orange", "pink"),
          legend.text = c("Ransom", "DownTime", "Loss"),
          args.legend = list(x = "topright", cex = 0.7),
          cex.names = 0.6)
}

# WebServer
if (nrow(webserver_missing) > 0) {
  top_10_webserver <- head(webserver_missing, 10)
  barplot(t(as.matrix(top_10_webserver[, c("Ransom_NA", "DownTime_NA", "Loss_NA")])),
          beside = TRUE,
          names.arg = top_10_webserver$WebServer,
          las = 2,
          main = "Missing Rate by WebServer (Top 10)",
          ylab = "Missing Percentage (%)",
          col = c("skyblue", "orange", "pink"),
          legend.text = c("Ransom", "DownTime", "Loss"),
          args.legend = list(x = "topright", cex = 0.7),
          cex.names = 0.6)
}

# Temporal trend
if (exists("year_missing") && nrow(year_missing) > 0) {
  plot(year_missing$Year, year_missing$Ransom_NA,
       type = "b", col = "skyblue", lwd = 2, pch = 16,
       main = "Missing Data Trend Over Time",
       xlab = "Year",
       ylab = "Missing Percentage (%)",
       ylim = c(0, max(c(year_missing$Ransom_NA, year_missing$DownTime_NA, year_missing$Loss_NA), na.rm = TRUE)))
  lines(year_missing$Year, year_missing$DownTime_NA, type = "b", col = "orange", lwd = 2, pch = 16)
  lines(year_missing$Year, year_missing$Loss_NA, type = "b", col = "pink", lwd = 2, pch = 16)
  legend("topright", legend = c("Ransom", "DownTime", "Loss"),
         col = c("skyblue", "orange", "pink"), lwd = 2, pch = 16, cex = 0.8)
  grid()
}

dev.off()

cat("\nMissing data analysis PDF saved: docs/reports/missing_data_analysis.pdf\n\n")

cat("======================================\n")
cat("PART 4: POST-CLEANING DISTRIBUTION\n")
cat("Objective: Assess data quality after outlier removal\n")
cat("======================================\n\n")

cat("Distribution Quality Metrics:\n")
cat("------------------------------\n\n")

# Calculate skewness and kurtosis for numeric variables
dist_quality <- data.frame(
  Variable = character(),
  N_Valid = integer(),
  Mean = numeric(),
  Median = numeric(),
  SD = numeric(),
  Skewness = numeric(),
  Kurtosis = numeric(),
  Normality_Assessment = character(),
  stringsAsFactors = FALSE
)

for (col in numeric_cols) {
  valid_data <- data_health[[col]][!is.na(data_health[[col]])]

  if (length(valid_data) > 3) {
    skew <- skewness(valid_data)
    kurt <- kurtosis(valid_data)

    # Assess normality
    if (abs(skew) < 0.5 && abs(kurt - 3) < 1) {
      assessment <- "Approximately Normal"
    } else if (abs(skew) < 1 && abs(kurt - 3) < 3) {
      assessment <- "Moderately Skewed"
    } else if (skew > 1) {
      assessment <- "Right-Skewed (Severe)"
    } else if (skew < -1) {
      assessment <- "Left-Skewed (Severe)"
    } else {
      assessment <- "Heavy-Tailed"
    }

    dist_quality <- rbind(dist_quality, data.frame(
      Variable = col,
      N_Valid = length(valid_data),
      Mean = mean(valid_data),
      Median = median(valid_data),
      SD = sd(valid_data),
      Skewness = skew,
      Kurtosis = kurt,
      Normality_Assessment = assessment
    ))
  }
}

print(dist_quality, row.names = FALSE)
cat("\n")

cat("Interpretation Guide:\n")
cat("  Skewness:\n")
cat("    -0.5 to 0.5: Approximately symmetric\n")
cat("    0.5 to 1 or -1 to -0.5: Moderately skewed\n")
cat("    > 1 or < -1: Highly skewed\n\n")
cat("  Kurtosis (excess kurtosis, normal = 3):\n")
cat("    2 to 4: Normal range\n")
cat("    > 4: Heavy tails (leptokurtic)\n")
cat("    < 2: Light tails (platykurtic)\n\n")

# Recommendations based on distribution
cat("======================================\n")
cat("IMPUTATION RECOMMENDATIONS\n")
cat("======================================\n\n")

cat("Based on distribution analysis:\n\n")

for (i in 1:nrow(dist_quality)) {
  var <- dist_quality$Variable[i]
  skew <- dist_quality$Skewness[i]
  assessment <- dist_quality$Normality_Assessment[i]

  cat(paste0(var, " (", assessment, "):\n"))

  if (abs(skew) < 0.5) {
    cat("  ✅ Suitable for: Mean imputation, KNN, MICE\n")
    cat("  📊 Distribution: Nearly normal, most methods will work well\n")
  } else if (abs(skew) < 1) {
    cat("  ⚠️  Suitable for: Median imputation, KNN, MICE\n")
    cat("  📊 Recommendation: Avoid mean imputation, prefer robust methods\n")
  } else {
    cat("  ❌ NOT suitable for: Mean imputation\n")
    cat("  ✅ Suitable for: Median imputation, KNN with transformed data\n")
    cat("  📊 Strong recommendation: Apply log transformation before imputation\n")
    cat("  💡 Consider: Log transform → Impute → Back transform\n")
  }

  cat("\n")
}

cat("======================================\n")
cat("PART 5: DISTRIBUTION VISUALIZATION\n")
cat("======================================\n\n")

cat("Creating post-cleaning distribution visualizations...\n\n")

# Create PDF for distribution analysis
pdf("docs/reports/post_cleaning_distribution.pdf", width = 14, height = 10)

for (col in numeric_cols) {
  valid_data <- data_health[[col]][!is.na(data_health[[col]])]

  if (length(valid_data) == 0) next

  cat(paste0("  Plotting distributions for ", col, "...\n"))

  # 2x3 layout for each variable
  par(mfrow = c(2, 3))

  # Plot 1: Histogram (original scale)
  hist(valid_data,
       breaks = 50,
       main = paste0(col, ": Histogram (Original Scale)"),
       xlab = col,
       col = "lightblue",
       border = "white")
  abline(v = mean(valid_data), col = "red", lwd = 2, lty = 2)
  abline(v = median(valid_data), col = "blue", lwd = 2, lty = 2)
  legend("topright",
         legend = c("Mean", "Median"),
         col = c("red", "blue"),
         lty = 2, lwd = 2, cex = 0.8)

  # Plot 2: Histogram (log scale) - only if all positive
  if (min(valid_data) > 0) {
    hist(log10(valid_data),
         breaks = 50,
         main = paste0(col, ": Histogram (Log10 Scale)"),
         xlab = paste0("log10(", col, ")"),
         col = "lightgreen",
         border = "white")
    abline(v = log10(mean(valid_data)), col = "red", lwd = 2, lty = 2)
    abline(v = log10(median(valid_data)), col = "blue", lwd = 2, lty = 2)
  } else {
    plot.new()
    text(0.5, 0.5, "Cannot log transform:\nData contains zero or negative values", cex = 1.2)
  }

  # Plot 3: Density plot
  plot(density(valid_data),
       main = paste0(col, ": Density Plot"),
       xlab = col,
       lwd = 2,
       col = "darkblue")
  polygon(density(valid_data), col = rgb(0, 0, 1, 0.3), border = NA)
  abline(v = mean(valid_data), col = "red", lwd = 2, lty = 2)
  abline(v = median(valid_data), col = "blue", lwd = 2, lty = 2)

  # Plot 4: Q-Q plot
  qqnorm(valid_data,
         main = paste0(col, ": Q-Q Plot"),
         pch = 16,
         col = rgb(0, 0, 1, 0.3))
  qqline(valid_data, col = "red", lwd = 2)

  # Add skewness info
  skew_val <- dist_quality$Skewness[dist_quality$Variable == col]
  kurt_val <- dist_quality$Kurtosis[dist_quality$Variable == col]
  text(min(qqnorm(valid_data, plot.it = FALSE)$x),
       max(qqnorm(valid_data, plot.it = FALSE)$y) * 0.9,
       paste0("Skewness: ", round(skew_val, 2), "\nKurtosis: ", round(kurt_val, 2)),
       adj = 0, cex = 0.9)

  # Plot 5: Boxplot
  boxplot(valid_data,
          main = paste0(col, ": Boxplot"),
          ylab = col,
          col = "lightyellow",
          border = "darkred")

  # Plot 6: ECDF (Empirical Cumulative Distribution Function)
  plot(ecdf(valid_data),
       main = paste0(col, ": Empirical CDF"),
       xlab = col,
       ylab = "Cumulative Probability",
       lwd = 2,
       col = "darkgreen")
  abline(h = c(0.25, 0.5, 0.75), col = "gray", lty = 2)
  abline(v = quantile(valid_data, c(0.25, 0.5, 0.75)), col = "red", lty = 2)
}

dev.off()

cat("\nPost-cleaning distribution PDF saved: docs/reports/post_cleaning_distribution.pdf\n\n")

cat("======================================\n")
cat("SUMMARY & FINAL RECOMMENDATIONS\n")
cat("======================================\n\n")

# Missingness mechanism assessment
cat("Missingness Mechanism Assessment:\n")
cat("-----------------------------------\n")

# Determine likely mechanism based on tests
if (exists("chi_test")) {
  if (chi_test$p.value < 0.05) {
    cat("✅ Evidence suggests: MAR (Missing At Random)\n")
    cat("   Rationale: Missingness is significantly related to observed variables (Country)\n")
    cat("   Recommendation: Use MICE (Multiple Imputation by Chained Equations)\n")
    cat("   Alternative: KNN imputation with categorical variables as predictors\n\n")
  } else {
    cat("✅ Evidence suggests: MCAR (Missing Completely At Random)\n")
    cat("   Rationale: No significant relationship between missingness and observed variables\n")
    cat("   Recommendation: Simple methods (mean, median, KNN) are acceptable\n")
    cat("   Note: Still prefer KNN or MICE for better accuracy\n\n")
  }
} else {
  cat("⚠️  Insufficient evidence to determine mechanism definitively\n")
  cat("   Recommendation: Assume MAR to be conservative\n")
  cat("   Use: MICE or KNN imputation\n\n")
}

# Distribution-based recommendations
cat("Distribution-Based Recommendations:\n")
cat("------------------------------------\n")

for (i in 1:nrow(dist_quality)) {
  var <- dist_quality$Variable[i]
  skew <- dist_quality$Skewness[i]
  miss_pct <- missing_stats$Missing_Percentage[missing_stats$Variable == var]

  cat(paste0("\n", var, ":\n"))
  cat(paste0("  Missing: ", miss_pct, "%\n"))
  cat(paste0("  Skewness: ", round(skew, 2), "\n"))

  if (miss_pct > 30) {
    cat("  ⚠️  WARNING: High missingness (>30%)\n")
    cat("  → Consider multiple imputation (m=5 to 10 iterations)\n")
    cat("  → Validate imputation quality carefully\n")
  }

  if (abs(skew) > 1) {
    cat("  📊 Recommendation: Transform before imputation\n")
    cat("  → Workflow: Log transform → Impute → Inverse transform\n")
  } else {
    cat("  ✅ Distribution acceptable for direct imputation\n")
  }
}

cat("\n\n======================================\n")
cat("NEXT STEPS\n")
cat("======================================\n\n")

cat("1. Review generated PDFs:\n")
cat("   - missing_data_analysis.pdf: Understand missing patterns\n")
cat("   - post_cleaning_distribution.pdf: Assess distribution quality\n\n")

cat("2. Choose imputation strategy based on:\n")
cat("   - Missingness mechanism (MCAR/MAR/MNAR)\n")
cat("   - Distribution shape (normal/skewed)\n")
cat("   - Missing percentage\n\n")

cat("3. Recommended imputation approach:\n")

# Overall recommendation
high_missing <- any(missing_stats$Missing_Percentage[missing_stats$Variable %in% numeric_cols] > 30)
high_skew <- any(abs(dist_quality$Skewness) > 1)

if (high_missing && high_skew) {
  cat("   → MICE with log-transformed predictors (most robust)\n")
  cat("   → Alternative: KNN with k=5, log-transformed features\n")
} else if (high_missing) {
  cat("   → MICE (handles high missingness well)\n")
  cat("   → Alternative: Multiple imputation with chained equations\n")
} else if (high_skew) {
  cat("   → KNN with log transformation\n")
  cat("   → Alternative: Median imputation (simple baseline)\n")
} else {
  cat("   → KNN imputation (k=5 to 10)\n")
  cat("   → Alternative: MICE for more sophisticated approach\n")
}

cat("\n4. Proceed to imputation script (to be created next)\n\n")

cat("Health check complete!\n")
