# Data Format Conversion Script
# Purpose: Convert data types to appropriate formats for analysis

cat("=== DATA FORMAT CONVERSION ===\n\n")

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

# Convert Ransom column
cat("   Converting Ransom column...\n")
ransom_original_na <- sum(is.na(data_converted$Ransom))
data_converted$Ransom <- as.numeric(data_converted$Ransom)
ransom_conversion_na <- sum(is.na(data_converted$Ransom)) - ransom_original_na
cat("      Original NA count:", ransom_original_na, "\n")
cat("      NA from conversion:", ransom_conversion_na, "\n")
cat("      Total NA:", sum(is.na(data_converted$Ransom)), "\n")
if (sum(!is.na(data_converted$Ransom)) > 0) {
  cat("      Range:", min(data_converted$Ransom, na.rm = TRUE), "to",
      max(data_converted$Ransom, na.rm = TRUE), "\n")
}

# Convert DownTime column
cat("   Converting DownTime column...\n")
downtime_original_na <- sum(is.na(data_converted$DownTime))
data_converted$DownTime <- as.numeric(data_converted$DownTime)
downtime_conversion_na <- sum(is.na(data_converted$DownTime)) - downtime_original_na
cat("      Original NA count:", downtime_original_na, "\n")
cat("      NA from conversion:", downtime_conversion_na, "\n")
cat("      Total NA:", sum(is.na(data_converted$DownTime)), "\n")
if (sum(!is.na(data_converted$DownTime)) > 0) {
  cat("      Range:", min(data_converted$DownTime, na.rm = TRUE), "to",
      max(data_converted$DownTime, na.rm = TRUE), "\n")
}

# Convert Loss column
cat("   Converting Loss column...\n")
loss_original_na <- sum(is.na(data_converted$Loss))
data_converted$Loss <- as.numeric(data_converted$Loss)
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
