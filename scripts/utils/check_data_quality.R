# Check data quality and identify issues

# Load the merged data
load("raw_data/processed_data/merged_hacking_data.RData")

cat("=== DATA QUALITY CHECK ===\n\n")

# 1. Check data structure
cat("1. DATA STRUCTURE:\n")
cat("   Dimensions:", nrow(merged_data), "rows x", ncol(merged_data), "columns\n")
cat("   Column names:", paste(names(merged_data), collapse = ", "), "\n\n")

# 2. Check data types
cat("2. DATA TYPES:\n")
str(merged_data)
cat("\n")

# 3. Check missing values
cat("3. MISSING VALUES:\n")
missing_summary <- data.frame(
  Column = names(merged_data),
  Missing_Count = colSums(is.na(merged_data)),
  Missing_Percentage = round(colSums(is.na(merged_data)) / nrow(merged_data) * 100, 2)
)
print(missing_summary)
cat("\n")

# 4. Check Date column format
cat("4. DATE COLUMN ISSUES:\n")
cat("   Sample dates (first 10):\n")
print(head(merged_data$Date, 10))
cat("   Data type:", class(merged_data$Date), "\n")
cat("   Unique date formats:", length(unique(merged_data$Date)), "\n\n")

# 5. Check numeric columns
cat("5. NUMERIC COLUMNS CHECK:\n")
numeric_cols <- c("Ransom", "DownTime", "Loss")
for (col in numeric_cols) {
  cat("   ", col, ":\n")
  cat("      Type:", class(merged_data[[col]]), "\n")
  cat("      Sample values:", paste(head(merged_data[[col]], 5), collapse = ", "), "\n")
  if (is.numeric(merged_data[[col]])) {
    cat("      Range:", min(merged_data[[col]], na.rm = TRUE), "to",
        max(merged_data[[col]], na.rm = TRUE), "\n")
  }
}
cat("\n")

# 6. Check categorical columns
cat("6. CATEGORICAL COLUMNS:\n")
cat("   Countries:", length(unique(merged_data$Country)), "unique values\n")
cat("   Top 10 countries:\n")
print(head(sort(table(merged_data$Country), decreasing = TRUE), 10))
cat("\n   WebServers:", length(unique(merged_data$WebServer)), "unique values\n")
cat("   Top 10 web servers:\n")
print(head(sort(table(merged_data$WebServer), decreasing = TRUE), 10))
cat("\n")

# 7. Check for duplicates
cat("7. DUPLICATE ROWS:\n")
cat("   Total duplicates:", sum(duplicated(merged_data)), "\n\n")

# 8. Check for special values
cat("8. SPECIAL VALUES CHECK:\n")
cat("   Empty strings in Country:", sum(merged_data$Country == "", na.rm = TRUE), "\n")
cat("   'UNKNOWN' in Country:", sum(merged_data$Country == "UNKNOWN", na.rm = TRUE), "\n")
cat("   'NULL' strings in Encoding:", sum(merged_data$Encoding == "NULL", na.rm = TRUE), "\n\n")

cat("=== RECOMMENDATIONS ===\n")
cat("Based on the analysis, you should consider:\n")
cat("1. Convert Date column to proper Date format\n")
cat("2. Check if numeric columns are correctly formatted\n")
cat("3. Handle missing values (NA) in Ransom and Loss\n")
cat("4. Standardize Country names (UNKNOWN, empty strings, etc.)\n")
cat("5. Handle 'NULL' strings in Encoding column\n")
cat("6. Remove duplicates if necessary\n")
