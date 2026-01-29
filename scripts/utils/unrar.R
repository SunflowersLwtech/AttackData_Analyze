# Extract RAR files with user-specified paths

if (!require("archive")) {
  install.packages("archive")
}
library(archive)

# Set paths using relative paths
rar_file <- "AssignmentDatasets.rar"  # RAR file in current directory
output_dir <- "raw_data"              # Output to raw_data folder

# Create output directory
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Extract file
cat("Extracting", basename(rar_file), "...\n")
archive_extract(rar_file, dir = output_dir)

# Display results
cat("\nExtraction complete!\n")
cat("Output directory:", output_dir, "\n\n")
cat("Extracted files:\n")
print(list.files(output_dir, recursive = TRUE))
