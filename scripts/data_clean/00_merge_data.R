# Merge three extracted data files into one dataset

# Install and load required packages
if (!require("readxl")) {
  install.packages("readxl")
}
library(readxl)

if (!require("dplyr")) {
  install.packages("dplyr")
}
library(dplyr)

# Set paths using relative paths
raw_data_dir <- "raw_data"
output_dir <- file.path(raw_data_dir, "processed_data")

# Create output directory
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# File paths
part1_file <- file.path(raw_data_dir, "HackingData_Part1.csv")
part2_file <- file.path(raw_data_dir, "HackingData_Part2.xlsx")
part3_file <- file.path(raw_data_dir, "HackingData_Part3.txt")

cat("Reading data files...\n\n")

# Read Part 1 (CSV format)
cat("1. Reading HackingData_Part1.csv...\n")
part1 <- read.csv(part1_file, stringsAsFactors = FALSE)
cat("   - Rows:", nrow(part1), "| Columns:", ncol(part1), "\n")

# Read Part 2 (Excel format)
cat("2. Reading HackingData_Part2.xlsx...\n")
part2 <- read_excel(part2_file)
# Convert to data frame and ensure same data types
part2 <- as.data.frame(part2, stringsAsFactors = FALSE)
cat("   - Rows:", nrow(part2), "| Columns:", ncol(part2), "\n")

# Read Part 3 (Tab-delimited text file)
cat("3. Reading HackingData_Part3.txt...\n")
part3 <- read.delim(part3_file, sep = "\t", stringsAsFactors = FALSE)
cat("   - Rows:", nrow(part3), "| Columns:", ncol(part3), "\n\n")

# Check column names match
cat("Checking column consistency...\n")
cat("Part1 columns:", paste(names(part1), collapse = ", "), "\n")
cat("Part2 columns:", paste(names(part2), collapse = ", "), "\n")
cat("Part3 columns:", paste(names(part3), collapse = ", "), "\n\n")

# Align ALL column types to character before merging
# Part1/Part2 have numeric Ransom/DownTime/Loss, Part3 has character (text values)
# 01_format_conversion.R handles the actual type conversion after merge
cat("Aligning column types before merge...\n")
part1 <- mutate(part1, across(everything(), as.character))
part2 <- mutate(part2, across(everything(), as.character))
part3 <- mutate(part3, across(everything(), as.character))

# Merge all three datasets using bind_rows (safer type handling than rbind)
cat("Merging datasets...\n")
merged_data <- bind_rows(part1, part2, part3)

cat("Merge complete!\n")
cat("Total rows:", nrow(merged_data), "| Total columns:", ncol(merged_data), "\n\n")

# Display summary
cat("Data summary:\n")
cat("Date range:", min(merged_data$Date, na.rm = TRUE), "to",
    max(merged_data$Date, na.rm = TRUE), "\n")
cat("Countries:", length(unique(merged_data$Country)), "\n")
cat("Missing values per column:\n")
print(colSums(is.na(merged_data)))

# Save merged dataset
output_file <- file.path(output_dir, "merged_hacking_data.csv")
cat("\nSaving merged data to:", output_file, "\n")
write.csv(merged_data, output_file, row.names = FALSE)

# Also save as RData for faster loading in R
rdata_file <- file.path(output_dir, "merged_hacking_data.RData")
cat("Saving as RData to:", rdata_file, "\n")
save(merged_data, file = rdata_file)

cat("\n=== Merge Complete ===\n")
cat("Output files:\n")
cat("  - CSV:", output_file, "\n")
cat("  - RData:", rdata_file, "\n")
