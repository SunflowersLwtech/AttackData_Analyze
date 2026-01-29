# Data Format Conversion Script
# Purpose: Convert data types to appropriate formats for analysis
# Updated: 2026-01-29 — Fixed as.numeric() text destruction bug (Audit Bug #1)

cat("=== DATA FORMAT CONVERSION ===\n\n")

# Load required libraries
if (!require("readr", quietly = TRUE)) {
  install.packages("readr", repos = "https://cloud.r-project.org/", quiet = TRUE)
}
library(readr)

if (!require("dplyr", quietly = TRUE)) {
  install.packages("dplyr", repos = "https://cloud.r-project.org/", quiet = TRUE)
}
library(dplyr)

# Load the merged data (using relative path)
cat("Loading merged data...\n")
load("raw_data/processed_data/merged_hacking_data.RData")

cat("Original data dimensions:", nrow(merged_data), "rows x", ncol(merged_data), "columns\n\n")

# Create a copy for conversion
data_converted <- merged_data

# ============================================
# 1. DATE CONVERSION
# ============================================
cat("1. Converting Date column...\n")

# Date column has multiple formats, try to parse them
# Sample formats: "2/1/1998", "17/1/2024", "30/9/2022"

# Function to try multiple date formats
convert_date <- function(date_string) {
  # Try different date formats
  formats <- c("%d/%m/%Y", "%m/%d/%Y", "%Y-%m-%d", "%d-%m-%Y")

  for (fmt in formats) {
    result <- as.Date(date_string, format = fmt)
    if (!is.na(result)) {
      return(result)
    }
  }
  # If all formats fail, return NA
  return(as.Date(NA))
}

# Apply conversion
cat("   Converting dates (this may take a moment)...\n")
data_converted$Date <- sapply(data_converted$Date, convert_date)
data_converted$Date <- as.Date(data_converted$Date, origin = "1970-01-01")

# Check conversion results
successful_dates <- sum(!is.na(data_converted$Date))
failed_dates <- sum(is.na(data_converted$Date))
cat("   Successfully converted:", successful_dates, "dates\n")
cat("   Failed conversions (NA):", failed_dates, "dates\n")

if (successful_dates > 0) {
  cat("   Date range:", min(data_converted$Date, na.rm = TRUE), "to",
      max(data_converted$Date, na.rm = TRUE), "\n")
}
cat("\n")

# ============================================
# 2. NUMERIC CONVERSIONS
# ============================================
cat("2. Converting numeric columns...\n")

# Convert Ransom column (with text preprocessing for Part3 values)
cat("   Converting Ransom column...\n")
ransom_original_na <- sum(is.na(data_converted$Ransom))

# Step 1: Preprocess text-encoded values BEFORE numeric conversion
ransom_raw <- trimws(as.character(data_converted$Ransom))
cat("      Preprocessing text values...\n")

# Count text patterns for logging
n_k_suffix <- sum(grepl("[0-9]k$", ransom_raw, ignore.case = TRUE), na.rm = TRUE)
n_ten_thousand <- sum(grepl("^ten thousand$", ransom_raw, ignore.case = TRUE), na.rm = TRUE)
n_unknown <- sum(grepl("^(unknown|N/A|null)$", ransom_raw, ignore.case = TRUE), na.rm = TRUE)
n_dollar_na <- sum(grepl("^\\$\\s+NA$", ransom_raw), na.rm = TRUE)
cat("      Found text patterns: k-suffix=", n_k_suffix,
    ", ten_thousand=", n_ten_thousand,
    ", unknown/NA/null=", n_unknown,
    ", $NA=", n_dollar_na, "\n")

# Step 2: Apply case_when text-to-numeric preprocessing
data_converted$Ransom <- case_when(
  is.na(ransom_raw) | ransom_raw == "" ~ NA_character_,
  grepl("^(unknown|N/A|null)$", ransom_raw, ignore.case = TRUE) ~ NA_character_,
  grepl("^\\$\\s+NA$", ransom_raw) ~ NA_character_,
  grepl("^ten thousand$", ransom_raw, ignore.case = TRUE) ~ "10000",
  grepl("[0-9]+\\.?[0-9]*k$", ransom_raw, ignore.case = TRUE) ~
    as.character(parse_number(ransom_raw) * 1000),
  TRUE ~ ransom_raw
)

# Step 3: Use parse_number() for final conversion (handles $12,345 and $ X.XX)
data_converted$Ransom <- suppressWarnings(parse_number(data_converted$Ransom))

ransom_conversion_na <- sum(is.na(data_converted$Ransom)) - ransom_original_na
cat("      Original NA count:", ransom_original_na, "\n")
cat("      NA from conversion:", ransom_conversion_na, "\n")
cat("      Total NA:", sum(is.na(data_converted$Ransom)), "\n")
if (sum(!is.na(data_converted$Ransom)) > 0) {
  cat("      Range:", min(data_converted$Ransom, na.rm = TRUE), "to",
      max(data_converted$Ransom, na.rm = TRUE), "\n")
}

# Convert DownTime column (with text preprocessing for Part3 values)
cat("   Converting DownTime column...\n")
downtime_original_na <- sum(is.na(data_converted$DownTime))

# Preprocess text values
downtime_raw <- trimws(as.character(data_converted$DownTime))

n_two <- sum(grepl("^two$", downtime_raw, ignore.case = TRUE), na.rm = TRUE)
n_na_dt <- sum(grepl("^(n/a|N/A|unknown|null)$", downtime_raw, ignore.case = TRUE), na.rm = TRUE)
cat("      Found text patterns: two=", n_two, ", n/a/unknown/null=", n_na_dt, "\n")

data_converted$DownTime <- case_when(
  is.na(downtime_raw) | downtime_raw == "" ~ NA_character_,
  grepl("^(n/a|N/A|unknown|null)$", downtime_raw, ignore.case = TRUE) ~ NA_character_,
  grepl("^two$", downtime_raw, ignore.case = TRUE) ~ "2",
  TRUE ~ downtime_raw
)

data_converted$DownTime <- suppressWarnings(parse_number(data_converted$DownTime))

downtime_conversion_na <- sum(is.na(data_converted$DownTime)) - downtime_original_na
cat("      Original NA count:", downtime_original_na, "\n")
cat("      NA from conversion:", downtime_conversion_na, "\n")
cat("      Total NA:", sum(is.na(data_converted$DownTime)), "\n")
if (sum(!is.na(data_converted$DownTime)) > 0) {
  cat("      Range:", min(data_converted$DownTime, na.rm = TRUE), "to",
      max(data_converted$DownTime, na.rm = TRUE), "\n")
}

# Convert Loss column (with text preprocessing for Part3 values)
cat("   Converting Loss column...\n")
loss_original_na <- sum(is.na(data_converted$Loss))

# Preprocess text values
loss_raw <- trimws(as.character(data_converted$Loss))

n_one_million <- sum(grepl("^one million$", loss_raw, ignore.case = TRUE), na.rm = TRUE)
n_usd_na <- sum(grepl("^USD\\s+NA$", loss_raw), na.rm = TRUE)
n_usd_values <- sum(grepl("^USD\\s+[0-9]", loss_raw), na.rm = TRUE)
n_null_loss <- sum(grepl("^(null|unknown|N/A)$", loss_raw, ignore.case = TRUE), na.rm = TRUE)
cat("      Found text patterns: one_million=", n_one_million,
    ", USD_NA=", n_usd_na,
    ", USD_values=", n_usd_values,
    ", null/unknown=", n_null_loss, "\n")

data_converted$Loss <- case_when(
  is.na(loss_raw) | loss_raw == "" ~ NA_character_,
  grepl("^(null|unknown|N/A)$", loss_raw, ignore.case = TRUE) ~ NA_character_,
  grepl("^USD\\s+NA$", loss_raw) ~ NA_character_,
  grepl("^one million$", loss_raw, ignore.case = TRUE) ~ "1000000",
  # Keep USD-prefixed values — parse_number() will extract the number
  TRUE ~ loss_raw
)

data_converted$Loss <- suppressWarnings(parse_number(data_converted$Loss))

loss_conversion_na <- sum(is.na(data_converted$Loss)) - loss_original_na
cat("      Original NA count:", loss_original_na, "\n")
cat("      NA from conversion:", loss_conversion_na, "\n")
cat("      Total NA:", sum(is.na(data_converted$Loss)), "\n")
if (sum(!is.na(data_converted$Loss)) > 0) {
  cat("      Range:", min(data_converted$Loss, na.rm = TRUE), "to",
      max(data_converted$Loss, na.rm = TRUE), "\n")
}
cat("\n")

# ============================================
# 3. DATA STRUCTURE SUMMARY
# ============================================
cat("3. Data structure after conversion:\n")
str(data_converted)
cat("\n")

# ============================================
# 4. SAVE CONVERTED DATA
# ============================================
cat("4. Saving converted data...\n")

# Save to processed_data folder (using relative path)
output_csv <- "raw_data/processed_data/converted_hacking_data.csv"
output_rdata <- "raw_data/processed_data/converted_hacking_data.RData"

write.csv(data_converted, output_csv, row.names = FALSE)
cat("   Saved CSV:", output_csv, "\n")

save(data_converted, file = output_rdata)
cat("   Saved RData:", output_rdata, "\n\n")

# ============================================
# 5. CONVERSION SUMMARY REPORT
# ============================================
cat("=== CONVERSION SUMMARY ===\n")
cat("Total records:", nrow(data_converted), "\n\n")

cat("Column Types:\n")
cat("  Date:      ", class(data_converted$Date), "\n")
cat("  Notify:    ", class(data_converted$Notify), "\n")
cat("  URL:       ", class(data_converted$URL), "\n")
cat("  IP:        ", class(data_converted$IP), "\n")
cat("  Country:   ", class(data_converted$Country), "\n")
cat("  WebServer: ", class(data_converted$WebServer), "\n")
cat("  Encoding:  ", class(data_converted$Encoding), "\n")
cat("  Ransom:    ", class(data_converted$Ransom), "\n")
cat("  DownTime:  ", class(data_converted$DownTime), "\n")
cat("  Loss:      ", class(data_converted$Loss), "\n\n")

cat("Missing Values Summary:\n")
missing_summary <- data.frame(
  Column = names(data_converted),
  Missing_Count = colSums(is.na(data_converted)),
  Missing_Percentage = round(colSums(is.na(data_converted)) / nrow(data_converted) * 100, 2)
)
print(missing_summary)

cat("\n=== CONVERSION COMPLETE ===\n")
cat("Next steps:\n")
cat("1. Outlier detection and marking as NA\n")
cat("2. Missing value imputation using algorithms\n")
cat("3. Data standardization\n")
