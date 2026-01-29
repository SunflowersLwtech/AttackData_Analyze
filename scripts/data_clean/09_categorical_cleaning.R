# Categorical Variable Cleaning Script
# Purpose: Clean and standardize high-cardinality categorical variables
# Methods: Version stripping, standardization, frequency lumping
# Author: LiuWei TP085412
# Date: 2026-01-21

cat("======================================\n")
cat("CATEGORICAL VARIABLE CLEANING\n")
cat("Best Practices for High-Cardinality Categories\n")
cat("======================================\n\n")

# Load required libraries
required_packages <- c("dplyr", "stringr", "forcats")

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

# Load preprocessed data
cat("Loading preprocessed data...\n")
load("raw_data/processed_data/preprocessed_for_imputation.RData")
cat("Data loaded successfully.\n")
cat("Total records:", nrow(data_prep), "\n\n")

# Create working copy
data_categorical <- data_prep

cat("======================================\n")
cat("COLUMN NAMING CONVENTION\n")
cat("======================================\n\n")

cat("Naming strategy to preserve data lineage:\n")
cat("  - Original columns: Keep unchanged (e.g., WebServer, Country)\n")
cat("  - Cleaned columns: Add '_clean' suffix (e.g., WebServer_clean)\n")
cat("  - Transformed columns: Add transformation type (e.g., Loss_log)\n")
cat("  - Flag columns: Use 'is_' or '_flag' prefix (e.g., is_outlier)\n\n")

cat("Benefits:\n")
cat("  ✓ Preserves original data for verification\n")
cat("  ✓ Clear data lineage (can trace transformations)\n")
cat("  ✓ Easy rollback if cleaning goes wrong\n\n")

cat("======================================\n")
cat("CATEGORY 1: WEBSERVER CLEANING\n")
cat("Problem: 177 categories with version numbers\n")
cat("======================================\n\n")

cat("Original WebServer statistics:\n")
cat("  Unique values:", length(unique(data_categorical$WebServer)), "\n")
cat("  Top 10 most common:\n")
print(head(sort(table(data_categorical$WebServer), decreasing = TRUE), 10))
cat("\n")

cat("Strategy:\n")
cat("  1. Strip version numbers (keep only software name)\n")
cat("  2. Standardize naming (trim whitespace, consistent case)\n")
cat("  3. Lump low-frequency servers into 'Other'\n")
cat("  4. Keep top 15 most common server types\n\n")

cat("Step 1: Extracting server name (before '/' or space)...\n")

# Extract server name using regex
# Pattern: ^[^/ ]+ matches everything before first '/' or space
data_categorical$WebServer_clean <- str_extract(
  as.character(data_categorical$WebServer),
  "^[^/ ]+"
)

# Trim whitespace
data_categorical$WebServer_clean <- trimws(data_categorical$WebServer_clean)

# Standardize case-inconsistent duplicates and merge synonyms
cat("  Standardizing server names...\n")
ws_val <- data_categorical$WebServer_clean
ws_lower <- tolower(ws_val)

# nginx vs Nginx -> Nginx
n_nginx_merge <- sum(ws_val == "nginx", na.rm = TRUE)
ws_val[ws_val == "nginx"] <- "Nginx"
cat("    nginx -> Nginx:", n_nginx_merge, "rows\n")

# IIS vs Microsoft-IIS -> Microsoft-IIS
n_iis_merge <- sum(ws_val == "IIS", na.rm = TRUE)
ws_val[ws_val == "IIS"] <- "Microsoft-IIS"
cat("    IIS -> Microsoft-IIS:", n_iis_merge, "rows\n")

# XSS injection values -> NA (malicious payloads, not real server names)
n_xss <- sum(grepl("script|alert|<|>", ws_val, ignore.case = TRUE), na.rm = TRUE)
ws_val[grepl("script|alert|<|>", ws_val, ignore.case = TRUE)] <- NA
cat("    XSS injection -> NA:", n_xss, "rows\n")

data_categorical$WebServer_clean <- ws_val

# Set "Unknown", blank, "-", and NA-like values to NA (Bug #3 fix: ~51,284 rows)
n_unknown_ws <- sum(tolower(data_categorical$WebServer_clean) == "unknown", na.rm = TRUE)
n_blank_ws <- sum(data_categorical$WebServer_clean == "" | is.na(data_categorical$WebServer_clean),
                  na.rm = TRUE)
n_dash_ws <- sum(data_categorical$WebServer_clean == "-", na.rm = TRUE)
cat("  Setting semantic-NA values:\n")
cat("    'Unknown' ->NA:", n_unknown_ws, "rows\n")
cat("    blank/NA  ->NA:", n_blank_ws, "rows\n")
cat("    '-'       ->NA:", n_dash_ws, "rows\n")
data_categorical$WebServer_clean[
  tolower(data_categorical$WebServer_clean) == "unknown" |
  data_categorical$WebServer_clean == "" |
  data_categorical$WebServer_clean == "-" |
  is.na(data_categorical$WebServer_clean)
] <- NA

cat("  Extracted server names\n")
cat("  New unique values:", length(unique(data_categorical$WebServer_clean)), "\n\n")

cat("Step 2: Lumping low-frequency servers...\n")

# Convert to factor and lump to top 5 major servers
# After merging IIS->Microsoft-IIS and nginx->Nginx, top 5 = Apache, Microsoft-IIS, Nginx, LiteSpeed, OpenResty
# All others (cloudflare-nginx 101, Zeus 75, lighttpd 61, etc.) -> "Other"
data_categorical$WebServer_clean <- fct_lump_n(
  as.factor(data_categorical$WebServer_clean),
  n = 5,
  other_level = "Other"
)

cat("  Final categories:", length(levels(data_categorical$WebServer_clean)), "\n")
cat("  Category distribution:\n")
print(sort(table(data_categorical$WebServer_clean), decreasing = TRUE))
cat("\n")

cat("======================================\n")
cat("CATEGORY 2: COUNTRY CLEANING\n")
cat("Problem: 360 countries with inconsistent naming\n")
cat("======================================\n\n")

cat("Original Country statistics:\n")
cat("  Unique values:", length(unique(data_categorical$Country)), "\n")
cat("  Top 10 most common:\n")
print(head(sort(table(data_categorical$Country), decreasing = TRUE), 10))
cat("\n")

cat("Strategy:\n")
cat("  1. Standardize case (all uppercase)\n")
cat("  2. Trim whitespace\n")
cat("  3. Map common synonyms (USA, US, UNITED STATES → USA)\n")
cat("  4. Convert 'UNKNOWN' to NA\n")
cat("  5. Keep top 30 countries, lump rest to 'Other'\n\n")

cat("Step 1: Standardizing country names...\n")

# Convert to uppercase and trim
data_categorical$Country_clean <- toupper(trimws(as.character(data_categorical$Country)))

cat("Step 2: Creating country name mapping...\n")

# Define country mappings (note: cannot map empty string directly in recode)
country_mappings <- c(
  "UNITED STATES" = "USA",
  "UNITED STATE" = "USA",
  "US" = "USA",
  "AMERICA" = "USA",
  "UNITED STATES OF AMERICA" = "USA",
  "UNITED KINGDOM" = "UK",
  "GB" = "UK",
  "GREAT BRITAIN" = "UK",
  "ENGLAND" = "UK",
  "BRASIL" = "BRAZIL",
  "BR" = "BRAZIL",
  "DEUTSCHLAND" = "GERMANY",
  "DE" = "GERMANY",
  "RUSSIAN FEDERATION" = "RUSSIA",
  "RU" = "RUSSIA",
  "PEOPLES REPUBLIC OF CHINA" = "CHINA",
  "CN" = "CHINA",
  "PRC" = "CHINA",
  "REPUBLIC OF KOREA" = "SOUTH KOREA",
  "KR" = "SOUTH KOREA",
  "UNKNOWN" = NA_character_
)

# Handle empty strings separately
data_categorical$Country_clean[data_categorical$Country_clean == ""] <- NA_character_

# Apply mappings
data_categorical$Country_clean <- recode(
  data_categorical$Country_clean,
  !!!country_mappings
)

cat("  Applied", length(country_mappings), "country name mappings\n")
cat("  Unique values after mapping:", length(unique(data_categorical$Country_clean)), "\n\n")

cat("Step 3: Lumping to top 30 countries...\n")

# Convert to factor and lump
data_categorical$Country_clean <- fct_lump_n(
  as.factor(data_categorical$Country_clean),
  n = 30,
  other_level = "Other"
)

cat("  Final categories:", length(levels(data_categorical$Country_clean)), "\n")
cat("  Top 15 countries:\n")
print(head(sort(table(data_categorical$Country_clean), decreasing = TRUE), 15))
cat("\n")

cat("======================================\n")
cat("CATEGORY 3: NOTIFY (HACKER GROUPS) CLEANING\n")
cat("Problem: 9,906 organizations, mostly singletons\n")
cat("======================================\n\n")

cat("Original Notify statistics:\n")
cat("  Unique values:", length(unique(data_categorical$Notify)), "\n")
cat("  Top 10 most active:\n")
print(head(sort(table(data_categorical$Notify), decreasing = TRUE), 10))
cat("\n")

cat("Strategy:\n")
cat("  1. Standardize (lowercase, trim)\n")
cat("  2. Keep top 50 most active groups\n")
cat("  3. Lump remaining ~9,850 into 'Other_Groups'\n\n")

cat("Step 1: Standardizing organization names...\n")

# Convert to character and handle encoding issues
notify_char <- as.character(data_categorical$Notify)

# Convert to UTF-8 to handle encoding issues
notify_char <- iconv(notify_char, to = "UTF-8", sub = "")

# Convert to lowercase (handle NA)
notify_char <- tolower(notify_char)

# Replace empty strings with NA
notify_char[notify_char == "" | is.na(notify_char)] <- NA

# Set "anonymous" and "unknown" to NA — these represent unknown reporter identity,
# not real hacker groups (Bug #2 fix: 121,922 rows affected)
n_anonymous <- sum(notify_char == "anonymous", na.rm = TRUE)
n_unknown_notify <- sum(notify_char == "unknown", na.rm = TRUE)
cat("  Setting semantic-NA values:\n")
cat("    'anonymous' ->NA:", n_anonymous, "rows\n")
cat("    'unknown'   ->NA:", n_unknown_notify, "rows\n")
notify_char[notify_char %in% c("anonymous", "unknown")] <- NA

data_categorical$Notify_clean <- notify_char

cat("Step 2: Lumping to top 50 groups...\n")

# Keep top 50 most active hacker groups
data_categorical$Notify_clean <- fct_lump_n(
  as.factor(data_categorical$Notify_clean),
  n = 50,
  other_level = "Other_Groups"
)

cat("  Final categories:", length(levels(data_categorical$Notify_clean)), "\n")
cat("  Top 20 hacker groups:\n")
print(head(sort(table(data_categorical$Notify_clean), decreasing = TRUE), 20))
cat("\n")

cat("======================================\n")
cat("CATEGORY 4: URL DOMAIN SUFFIX EXTRACTION\n")
cat("Problem: URLs are almost unique, not useful as-is\n")
cat("======================================\n\n")

cat("Strategy:\n")
cat("  1. Extract top-level domain (TLD) using regex\n")
cat("  2. Business insight: .gov/.mil vs .com have different attack patterns\n")
cat("  3. Keep top 10 most common TLDs\n\n")

cat("Step 1: Extracting TLD from URLs...\n")

# Step 1a: Extract domain from URL (between :// and first /)
# Then extract TLD from domain (avoids matching file extensions like .htm/.php/.txt)
url_lower <- tolower(data_categorical$URL)
url_domain <- str_extract(url_lower, "(?<=://)[^/]+")

# Step 1b: Extract TLD from domain part only
# Pattern: .co.uk, .com.br, .ac.jp (ccSLD) or .com, .gov, .net (gTLD)
data_categorical$URL_suffix <- str_extract(
  url_domain,
  "\\.[a-z]{2,3}(\\.[a-z]{2})?$"
)

cat("  Unique TLDs extracted:", length(unique(data_categorical$URL_suffix)), "\n")
cat("  Top 15 most common:\n")
print(head(sort(table(data_categorical$URL_suffix), decreasing = TRUE), 15))
cat("\n")

cat("Step 2: Lumping to top 10 TLDs...\n")

data_categorical$URL_suffix <- fct_lump_n(
  as.factor(data_categorical$URL_suffix),
  n = 10,
  other_level = "Other_TLD"
)

cat("  Final TLD categories:", length(levels(data_categorical$URL_suffix)), "\n")
print(table(data_categorical$URL_suffix))
cat("\n")

cat("======================================\n")
cat("CATEGORY 5: IP ADDRESS VALIDATION\n")
cat("Problem: Contains invalid IPs like 'not_an_ip', 'gggg'\n")
cat("======================================\n\n")

cat("Strategy:\n")
cat("  1. Use regex patterns to validate IP addresses\n")
cat("  2. Support both IPv4 and IPv6\n")
cat("  3. Set invalid IPs to NA\n\n")

cat("Step 1: Validating IP addresses...\n")

# Helper function to validate IPv4
is_valid_ipv4 <- function(ip) {
  # IPv4 pattern: 0-255.0-255.0-255.0-255
  pattern <- "^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$"
  grepl(pattern, ip)
}

# Helper function to validate IPv6 (simplified)
is_valid_ipv6 <- function(ip) {
  # IPv6 pattern (simplified - matches basic IPv6 format)
  pattern <- "^([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$|^::1$|^([0-9a-fA-F]{1,4}:){1,7}:$"
  grepl(pattern, ip)
}

# Check if IP column exists and has data
if ("IP" %in% names(data_categorical)) {

  # Get non-empty IPs
  non_empty_ips <- data_categorical$IP[data_categorical$IP != "" & !is.na(data_categorical$IP)]

  cat("  Total non-empty IP entries:", length(non_empty_ips), "\n")

  if (length(non_empty_ips) > 0) {
    # Validate IPv4 and IPv6
    is_valid_v4 <- sapply(data_categorical$IP, function(x) {
      if (is.na(x) || x == "") return(FALSE)
      is_valid_ipv4(x)
    })

    is_valid_v6 <- sapply(data_categorical$IP, function(x) {
      if (is.na(x) || x == "") return(FALSE)
      is_valid_ipv6(x)
    })

    is_valid <- is_valid_v4 | is_valid_v6

    # Count valid IPs
    valid_count <- sum(is_valid, na.rm = TRUE)
    invalid_count <- length(non_empty_ips) - valid_count

    cat("  Valid IPv4 addresses:", sum(is_valid_v4), "\n")
    cat("  Valid IPv6 addresses:", sum(is_valid_v6), "\n")
    cat("  Total valid IPs:", valid_count, "\n")
    cat("  Invalid IP strings:", invalid_count, "\n")

    # Create cleaned IP column
    data_categorical$IP_clean <- ifelse(
      is_valid & data_categorical$IP != "",
      data_categorical$IP,
      NA_character_
    )

    # Show examples of invalid IPs (first 10)
    if (invalid_count > 0) {
      invalid_ips <- unique(data_categorical$IP[!is_valid & data_categorical$IP != ""])
      cat("\n  Examples of invalid IPs removed:\n")
      print(head(invalid_ips, 10))
    }

  } else {
    cat("  All IP entries are empty\n")
    data_categorical$IP_clean <- NA_character_
  }

} else {
  cat("  IP column not found\n")
}
cat("\n")

cat("======================================\n")
cat("CATEGORY 6: ENCODING STANDARDIZATION\n")
cat("Problem: utf-8, UTF-8, utf8 inconsistent\n")
cat("======================================\n\n")

cat("Original Encoding statistics:\n")
cat("  Unique values:", length(unique(data_categorical$Encoding)), "\n")
cat("  Top 10 most common:\n")
print(head(sort(table(data_categorical$Encoding), decreasing = TRUE), 10))
cat("\n")

cat("Strategy:\n")
cat("  1. Convert to uppercase\n")
cat("  2. Standardize common variations\n")
cat("  3. Keep top 10 encodings\n\n")

cat("Step 1: Standardizing encoding names...\n")

# Convert to uppercase and trim
data_categorical$Encoding_clean <- toupper(trimws(as.character(data_categorical$Encoding)))

# Standardize common variations (cannot map empty string directly)
encoding_mappings <- c(
  "UTF8" = "UTF-8",
  "UTF_8" = "UTF-8",
  "ISO88591" = "ISO-8859-1",
  "ISO_8859_1" = "ISO-8859-1",
  "WINDOWS1252" = "WINDOWS-1252",
  "ASCII" = "US-ASCII",
  "NULL" = NA_character_,
  "N" = NA_character_,
  "LITESPEED" = NA_character_
)

# Handle empty strings separately
data_categorical$Encoding_clean[data_categorical$Encoding_clean == ""] <- NA_character_

data_categorical$Encoding_clean <- recode(
  data_categorical$Encoding_clean,
  !!!encoding_mappings
)

cat("  Applied encoding standardization\n")
cat("  Unique values after mapping:", length(unique(data_categorical$Encoding_clean)), "\n\n")

cat("Step 2: Lumping to top 10 encodings...\n")

data_categorical$Encoding_clean <- fct_lump_n(
  as.factor(data_categorical$Encoding_clean),
  n = 10,
  other_level = "Other_Encoding"
)

cat("  Final categories:", length(levels(data_categorical$Encoding_clean)), "\n")
print(table(data_categorical$Encoding_clean))
cat("\n")

cat("======================================\n")
cat("CLEANING SUMMARY\n")
cat("======================================\n\n")

# Create summary table
cleaning_summary <- data.frame(
  Variable = c("WebServer", "Country", "Notify", "URL", "IP", "Encoding"),
  Original_Categories = c(
    length(unique(data_prep$WebServer)),
    length(unique(data_prep$Country)),
    length(unique(data_prep$Notify)),
    "N/A (URLs are unique)",
    "N/A (IPs are unique)",
    length(unique(data_prep$Encoding))
  ),
  Cleaned_Categories = c(
    length(levels(data_categorical$WebServer_clean)),
    length(levels(data_categorical$Country_clean)),
    length(levels(data_categorical$Notify_clean)),
    length(levels(data_categorical$URL_suffix)),
    "N/A (validation only)",
    length(levels(data_categorical$Encoding_clean))
  ),
  Reduction_Strategy = c(
    "Version stripping + Top 15",
    "Standardization + Top 30",
    "Standardization + Top 50",
    "TLD extraction + Top 10",
    "IPv4/IPv6 validation",
    "Standardization + Top 10"
  ),
  Cleaned_Column = c(
    "WebServer_clean",
    "Country_clean",
    "Notify_clean",
    "URL_suffix",
    "IP_clean",
    "Encoding_clean"
  )
)

print(cleaning_summary, row.names = FALSE)
cat("\n")

cat("======================================\n")
cat("DATA STRUCTURE AFTER CLEANING\n")
cat("======================================\n\n")

cat("New cleaned columns added:\n")
cat("  - WebServer_clean: ", class(data_categorical$WebServer_clean), "\n")
cat("  - Country_clean: ", class(data_categorical$Country_clean), "\n")
cat("  - Notify_clean: ", class(data_categorical$Notify_clean), "\n")
cat("  - URL_suffix: ", class(data_categorical$URL_suffix), "\n")
cat("  - IP_clean: ", class(data_categorical$IP_clean), "\n")
cat("  - Encoding_clean: ", class(data_categorical$Encoding_clean), "\n\n")

cat("Original columns preserved:\n")
cat("  - WebServer, Country, Notify, URL, IP, Encoding\n")
cat("  (Can be used for verification or rollback)\n\n")

cat("Total columns now:", ncol(data_categorical), "\n\n")

cat("======================================\n")
cat("QUALITY CHECKS\n")
cat("======================================\n\n")

cat("Checking for unexpected NA increases:\n")
cat("---------------------------------------\n")

# Compare NA counts
na_comparison <- data.frame(
  Variable = c("WebServer", "Country", "Notify", "Encoding"),
  Original_NA = c(
    sum(is.na(data_prep$WebServer) | data_prep$WebServer == ""),
    sum(is.na(data_prep$Country) | data_prep$Country == ""),
    sum(is.na(data_prep$Notify) | data_prep$Notify == ""),
    sum(is.na(data_prep$Encoding) | data_prep$Encoding == "")
  ),
  Cleaned_NA = c(
    sum(is.na(data_categorical$WebServer_clean)),
    sum(is.na(data_categorical$Country_clean)),
    sum(is.na(data_categorical$Notify_clean)),
    sum(is.na(data_categorical$Encoding_clean))
  )
)

na_comparison$NA_Change <- na_comparison$Cleaned_NA - na_comparison$Original_NA

print(na_comparison, row.names = FALSE)
cat("\n")

if (any(na_comparison$NA_Change < 0)) {
  cat("✓ Good: Some variables have fewer NAs after standardization\n\n")
}

cat("======================================\n")
cat("SAVING CLEANED DATA\n")
cat("======================================\n\n")

# Save cleaned data
output_rdata <- "raw_data/processed_data/categorical_cleaned.RData"
save(data_categorical, cleaning_summary, na_comparison, file = output_rdata)
cat(paste0("Cleaned data saved: ", output_rdata, "\n\n"))

# Save summary as CSV
write.csv(cleaning_summary, "docs/reports/categorical_cleaning_summary.csv", row.names = FALSE)
cat("Summary saved: docs/reports/categorical_cleaning_summary.csv\n\n")

cat("======================================\n")
cat("RECOMMENDATIONS FOR MODELING\n")
cat("======================================\n\n")

cat("Use these CLEANED columns in imputation/modeling:\n")
cat("  ✓ WebServer_clean (16 levels instead of 177)\n")
cat("  ✓ Country_clean (31 levels instead of 360)\n")
cat("  ✓ Notify_clean (51 levels instead of 9,906)\n")
cat("  ✓ URL_suffix (11 levels, new feature)\n")
cat("  ✓ IP_clean (validated IPs only)\n")
cat("  ✓ Encoding_clean (11 levels instead of 50)\n\n")

cat("DO NOT use original high-cardinality columns:\n")
cat("  ✗ WebServer (177 levels - too many)\n")
cat("  ✗ Country (360 levels - too many)\n")
cat("  ✗ Notify (9,906 levels - will cause overfitting)\n")
cat("  ✗ URL (unique values - not useful)\n\n")

cat("Benefits of cleaned categories:\n")
cat("  1. Faster model training (fewer dummy variables)\n")
cat("  2. Better generalization (less overfitting)\n")
cat("  3. More interpretable models\n")
cat("  4. Handles rare categories gracefully ('Other' groups)\n\n")

cat("======================================\n")
cat("NEXT STEPS\n")
cat("======================================\n\n")

cat("Data is now ready for missing value imputation!\n\n")

cat("Recommended workflow:\n")
cat("  1. Use categorical_cleaned.RData for imputation\n")
cat("  2. Include cleaned categorical variables as predictors:\n")
cat("     - Country_clean, WebServer_clean, Encoding_clean, URL_suffix\n")
cat("  3. Use log-transformed numeric variables as targets:\n")
cat("     - Ransom_log, DownTime_log, Loss_log\n")
cat("  4. After imputation, back-transform and validate\n\n")

cat("Categorical cleaning complete!\n")
