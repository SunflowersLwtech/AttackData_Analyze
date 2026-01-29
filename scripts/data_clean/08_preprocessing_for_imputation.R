# Data Preprocessing for Imputation Script
# Purpose: Prepare data for missing value imputation
# Steps: 1) Variable categorization, 2) Feature engineering, 3) Log transformation
# Author: LiuWei TP085412
# Date: 2026-01-21

cat("======================================\n")
cat("DATA PREPROCESSING FOR IMPUTATION\n")
cat("Prepare data structure before filling missing values\n")
cat("======================================\n\n")

# Load required libraries
if (!require("dplyr")) {
  install.packages("dplyr", repos = "https://cloud.r-project.org/")
  library(dplyr)
} else {
  library(dplyr)
}

cat("Required packages loaded.\n\n")

# Load final cleaned data
cat("Loading final cleaned data...\n")
load("raw_data/processed_data/final_cleaned_data.RData")
cat("Data loaded successfully.\n")
cat("Total records:", nrow(data_final_minimal), "\n\n")

# Create working copy
data_prep <- data_final_minimal

cat("======================================\n")
cat("STEP 1: VARIABLE CATEGORIZATION\n")
cat("Objective: Define data types explicitly\n")
cat("======================================\n\n")

cat("Why this is critical:\n")
cat("  - R needs to know which columns are categorical vs numerical\n")
cat("  - Imputation algorithms treat these types differently\n")
cat("  - Categorical: Use mode (most frequent value)\n")
cat("  - Numerical: Use median, mean, or modeling-based methods\n\n")

# Check current data types
cat("Current data types BEFORE categorization:\n")
cat("------------------------------------------\n")
str(data_prep)
cat("\n")

# Step 1.1: Convert categorical variables to factors
cat("Step 1.1: Converting categorical variables to factors...\n")

categorical_vars <- c("Notify", "Country", "WebServer", "Encoding")

for (var in categorical_vars) {
  cat(paste0("  Converting ", var, " to factor...\n"))

  # Show original type and unique count
  original_type <- class(data_prep[[var]])
  unique_count <- length(unique(data_prep[[var]]))

  cat(paste0("    Original type: ", original_type, "\n"))
  cat(paste0("    Unique values: ", unique_count, "\n"))

  # Convert to factor
  data_prep[[var]] <- as.factor(data_prep[[var]])

  cat(paste0("    ✓ Converted to factor\n\n"))
}

# Step 1.2: Ensure Date is proper Date type
cat("Step 1.2: Ensuring Date column is proper Date type...\n")
original_date_type <- class(data_prep$Date)
cat(paste0("  Original type: ", original_date_type, "\n"))

if (!inherits(data_prep$Date, "Date")) {
  cat("  Converting to Date type...\n")
  data_prep$Date <- as.Date(data_prep$Date)
} else {
  cat("  ✓ Already Date type\n")
}

# Show date range
valid_dates <- data_prep$Date[!is.na(data_prep$Date)]
if (length(valid_dates) > 0) {
  cat(paste0("  Date range: ", min(valid_dates), " to ", max(valid_dates), "\n"))
}
cat("\n")

# Step 1.3: Ensure numeric variables are properly typed
cat("Step 1.3: Ensuring numeric variables are proper numeric type...\n")

numeric_vars <- c("Ransom", "DownTime", "Loss")

for (var in numeric_vars) {
  cat(paste0("  Checking ", var, "...\n"))

  original_type <- class(data_prep[[var]])
  cat(paste0("    Original type: ", original_type, "\n"))

  if (!is.numeric(data_prep[[var]])) {
    cat("    Converting to numeric...\n")
    data_prep[[var]] <- as.numeric(data_prep[[var]])
    cat("    ✓ Converted to numeric\n")
  } else {
    cat("    ✓ Already numeric\n")
  }
  cat("\n")
}

# URL and IP - keep as character for now (may not be useful for imputation)
cat("Step 1.4: Keeping URL and IP as character strings...\n")
cat("  (These are identifiers, not useful for imputation predictors)\n\n")

# Summary after categorization
cat("Data types AFTER categorization:\n")
cat("---------------------------------\n")
str(data_prep)
cat("\n")

cat("======================================\n")
cat("STEP 2: FEATURE ENGINEERING\n")
cat("Objective: Create useful derived variables\n")
cat("======================================\n\n")

cat("Why feature engineering before imputation:\n")
cat("  - Time-based patterns: Data quality improved dramatically after 2016\n")
cat("  - Missing patterns: Early years have NO Ransom data (systematic)\n")
cat("  - Domain knowledge: Create binary flags and temporal features\n\n")

# Feature 2.1: Extract Year from Date
cat("Feature 2.1: Extracting Year from Date...\n")

data_prep$Year <- as.integer(format(data_prep$Date, "%Y"))

year_stats <- table(data_prep$Year, useNA = "ifany")
cat("\n  Year distribution (top 10):\n")
print(head(sort(year_stats, decreasing = TRUE), 10))

# Count records with valid year
valid_years <- sum(!is.na(data_prep$Year))
cat(paste0("\n  Records with valid year: ", valid_years, " (",
           round(valid_years / nrow(data_prep) * 100, 2), "%)\n\n"))

# Feature 2.2: Create modern data flag (Year >= 2016)
cat("Feature 2.2: Creating 'modern data' indicator (Year >= 2016)...\n")
cat("  Rationale: Health check revealed dramatic quality improvement after 2016\n")

data_prep$is_modern <- ifelse(!is.na(data_prep$Year) & data_prep$Year >= 2016, TRUE, FALSE)

modern_count <- sum(data_prep$is_modern, na.rm = TRUE)
legacy_count <- sum(!data_prep$is_modern, na.rm = TRUE)

cat(paste0("  Modern records (2016+): ", modern_count, " (",
           round(modern_count / nrow(data_prep) * 100, 2), "%)\n"))
cat(paste0("  Legacy records (<2016): ", legacy_count, " (",
           round(legacy_count / nrow(data_prep) * 100, 2), "%)\n\n"))

# Check missing rates by era
cat("  Missing rate comparison: Modern vs Legacy\n")
cat("  ------------------------------------------\n")

modern_data <- data_prep[data_prep$is_modern, ]
legacy_data <- data_prep[!data_prep$is_modern, ]

for (var in numeric_vars) {
  modern_na_rate <- round(sum(is.na(modern_data[[var]])) / nrow(modern_data) * 100, 2)
  legacy_na_rate <- round(sum(is.na(legacy_data[[var]])) / nrow(legacy_data) * 100, 2)

  cat(sprintf("  %s: Modern=%s%%, Legacy=%s%%\n", var, modern_na_rate, legacy_na_rate))
}
cat("\n")

# Feature 2.3: Create ransom demand indicator
cat("Feature 2.3: Creating 'has_ransom_demand' binary indicator...\n")
cat("  Rationale: Whether ransom was demanded is meaningful information\n")

data_prep$has_ransom_demand <- !is.na(data_prep$Ransom)

ransom_demand_count <- sum(data_prep$has_ransom_demand)
cat(paste0("  Records with ransom demand: ", ransom_demand_count, " (",
           round(ransom_demand_count / nrow(data_prep) * 100, 2), "%)\n"))
cat(paste0("  Records without ransom demand: ", nrow(data_prep) - ransom_demand_count, " (",
           round((nrow(data_prep) - ransom_demand_count) / nrow(data_prep) * 100, 2), "%)\n\n"))

# Feature 2.4: Create severity categories (optional - for analysis)
cat("Feature 2.4: Creating loss severity categories...\n")
cat("  Rationale: Categorical severity levels may help with pattern recognition\n")

# Calculate quartiles for Loss (on valid data only)
loss_quartiles <- quantile(data_prep$Loss, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

data_prep$loss_severity <- cut(data_prep$Loss,
                                breaks = loss_quartiles,
                                labels = c("Low", "Medium", "High", "Critical"),
                                include.lowest = TRUE)

cat("  Loss severity levels (based on quartiles):\n")
print(table(data_prep$loss_severity, useNA = "ifany"))
cat("\n")

# Feature 2.5: Create downtime categories
cat("Feature 2.5: Creating downtime categories...\n")

# Calculate quartiles for DownTime
downtime_quartiles <- quantile(data_prep$DownTime, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

data_prep$downtime_category <- cut(data_prep$DownTime,
                                    breaks = downtime_quartiles,
                                    labels = c("Brief", "Short", "Extended", "Prolonged"),
                                    include.lowest = TRUE)

cat("  Downtime categories (based on quartiles):\n")
print(table(data_prep$downtime_category, useNA = "ifany"))
cat("\n")

cat("======================================\n")
cat("STEP 3: LOG TRANSFORMATION\n")
cat("Objective: Normalize skewed distributions\n")
cat("======================================\n\n")

cat("Why log transformation is CRITICAL:\n")
cat("  - Health check revealed severe right skewness (1.35 to 2.13)\n")
cat("  - Skewed data causes imputation algorithms to fail\n")
cat("  - Log transform makes distribution more symmetric\n")
cat("  - KNN and MICE work much better on normalized data\n\n")

cat("Transformation formula: log10(x + 1)\n")
cat("  - Adding 1 handles zero values (log(0) is undefined)\n")
cat("  - Base 10 for easier interpretation\n\n")

# Create log-transformed versions of numeric variables
log_transformed_vars <- c()

for (var in numeric_vars) {
  log_var_name <- paste0(var, "_log")

  cat(paste0("Transforming ", var, " → ", log_var_name, "...\n"))

  # Get valid (non-NA, non-negative) data
  valid_data <- data_prep[[var]][!is.na(data_prep[[var]])]

  if (length(valid_data) > 0) {
    # Check for negative values
    negative_count <- sum(valid_data < 0)

    if (negative_count > 0) {
      cat(paste0("  ⚠️  WARNING: ", negative_count, " negative values detected\n"))
      cat("  Strategy: Shift all values to make minimum = 0\n")

      min_val <- min(valid_data)
      shift_amount <- abs(min_val) + 1

      cat(paste0("  Shift amount: ", shift_amount, "\n"))

      # Apply: log10(x - min + 1)
      data_prep[[log_var_name]] <- log10(data_prep[[var]] - min_val + 1)

    } else {
      # Standard log transformation: log10(x + 1)
      data_prep[[log_var_name]] <- log10(data_prep[[var]] + 1)
    }

    # Calculate skewness before and after
    if (require("moments", quietly = TRUE)) {
      skew_before <- skewness(valid_data)

      log_valid_data <- data_prep[[log_var_name]][!is.na(data_prep[[log_var_name]])]
      skew_after <- skewness(log_valid_data)

      cat(sprintf("  Skewness: %.2f → %.2f (improvement: %.2f)\n",
                  skew_before, skew_after, abs(skew_before) - abs(skew_after)))
    }

    log_transformed_vars <- c(log_transformed_vars, log_var_name)
    cat(paste0("  ✓ ", log_var_name, " created\n\n"))

  } else {
    cat("  ⚠️  No valid data to transform\n\n")
  }
}

cat("======================================\n")
cat("TRANSFORMATION QUALITY CHECK\n")
cat("======================================\n\n")

cat("Comparing distributions before and after log transformation:\n")
cat("--------------------------------------------------------------\n\n")

transformation_summary <- data.frame(
  Variable = character(),
  N_Valid = integer(),
  Original_Skewness = numeric(),
  Log_Skewness = numeric(),
  Improvement = numeric(),
  Quality = character(),
  stringsAsFactors = FALSE
)

for (i in 1:length(numeric_vars)) {
  var <- numeric_vars[i]
  log_var <- log_transformed_vars[i]

  valid_original <- data_prep[[var]][!is.na(data_prep[[var]])]
  valid_log <- data_prep[[log_var]][!is.na(data_prep[[log_var]])]

  if (length(valid_original) > 0 && length(valid_log) > 0) {
    skew_orig <- skewness(valid_original)
    skew_log <- skewness(valid_log)
    improvement <- abs(skew_orig) - abs(skew_log)

    # Quality assessment
    if (abs(skew_log) < 0.5) {
      quality <- "Excellent (Nearly symmetric)"
    } else if (abs(skew_log) < 1) {
      quality <- "Good (Moderate skew)"
    } else {
      quality <- "Fair (Still skewed)"
    }

    transformation_summary <- rbind(transformation_summary, data.frame(
      Variable = var,
      N_Valid = length(valid_original),
      Original_Skewness = round(skew_orig, 2),
      Log_Skewness = round(skew_log, 2),
      Improvement = round(improvement, 2),
      Quality = quality
    ))
  }
}

print(transformation_summary, row.names = FALSE)
cat("\n")

cat("Interpretation:\n")
cat("  Positive improvement: Log transformation reduced skewness\n")
cat("  Target: Absolute skewness < 0.5 (nearly symmetric)\n\n")

cat("======================================\n")
cat("DATA STRUCTURE SUMMARY\n")
cat("======================================\n\n")

cat("Final preprocessed data structure:\n")
cat("------------------------------------\n")
str(data_prep)
cat("\n")

cat("New variables created:\n")
cat("  Time-based features:\n")
cat("    - Year (integer): Extracted from Date\n")
cat("    - is_modern (logical): TRUE if Year >= 2016\n")
cat("  Domain features:\n")
cat("    - has_ransom_demand (logical): TRUE if Ransom is not NA\n")
cat("    - loss_severity (factor): Low/Medium/High/Critical\n")
cat("    - downtime_category (factor): Brief/Short/Extended/Prolonged\n")
cat("  Transformed features:\n")
for (log_var in log_transformed_vars) {
  cat(paste0("    - ", log_var, " (numeric): Log10 transformed\n"))
}
cat("\n")

cat("======================================\n")
cat("SAVING PREPROCESSED DATA\n")
cat("======================================\n\n")

# Save preprocessed data
output_rdata <- "raw_data/processed_data/preprocessed_for_imputation.RData"
save(data_prep, transformation_summary, file = output_rdata)
cat(paste0("Preprocessed data saved: ", output_rdata, "\n\n"))

# Create a summary report
preprocessing_report <- list(
  total_records = nrow(data_prep),
  modern_records = sum(data_prep$is_modern, na.rm = TRUE),
  legacy_records = sum(!data_prep$is_modern, na.rm = TRUE),
  ransom_demand_records = sum(data_prep$has_ransom_demand),
  categorical_vars = categorical_vars,
  numeric_vars = numeric_vars,
  log_vars = log_transformed_vars,
  transformation_quality = transformation_summary
)

# Save report as RDS
saveRDS(preprocessing_report, "raw_data/processed_data/preprocessing_report.rds")
cat("Preprocessing report saved: raw_data/processed_data/preprocessing_report.rds\n\n")

cat("======================================\n")
cat("RECOMMENDATIONS FOR IMPUTATION\n")
cat("======================================\n\n")

cat("Based on preprocessing results:\n\n")

cat("1. Use LOG-TRANSFORMED variables for imputation:\n")
for (i in 1:length(numeric_vars)) {
  cat(sprintf("   - Use '%s' instead of '%s'\n", log_transformed_vars[i], numeric_vars[i]))
}
cat("\n")

cat("2. Use these predictors in imputation models:\n")
cat("   Categorical predictors:\n")
cat("     - Country, WebServer, Encoding\n")
cat("   Temporal predictors:\n")
cat("     - Year, is_modern\n")
cat("   Binary indicators:\n")
cat("     - has_ransom_demand\n")
cat("   Other numeric predictors:\n")
cat("     - Use log-transformed versions of other numeric variables\n\n")

cat("3. Consider separate imputation strategies:\n")
cat("   Strategy A: Impute modern data (2016+) separately\n")
cat("     - Higher quality, lower missing rate\n")
cat("     - More reliable predictions\n")
cat("   Strategy B: Impute legacy data (<2016) separately or exclude\n")
cat("     - Very high missing rate (especially Ransom = 100%)\n")
cat("     - May not be worth imputing\n\n")

cat("4. After imputation, remember to:\n")
cat("   - Back-transform log variables: 10^x - 1\n")
cat("   - Validate imputed values are within reasonable ranges\n")
cat("   - Compare distributions before and after imputation\n\n")

cat("======================================\n")
cat("NEXT STEPS\n")
cat("======================================\n\n")

cat("Preprocessing complete! Ready for imputation.\n\n")

cat("Next script should implement:\n")
cat("  1. Load preprocessed_for_imputation.RData\n")
cat("  2. Choose imputation method (MICE or KNN)\n")
cat("  3. Use log-transformed variables as targets\n")
cat("  4. Use engineered features as predictors\n")
cat("  5. Back-transform after imputation\n")
cat("  6. Validate imputation quality\n\n")

cat("Preprocessing complete!\n")
