# ============================================================
# generate_report_plots.R
# Assignment Report Visualization: Data Preparation Plots
# ============================================================
# Generates all plots needed for Chapter 1 (Data Preparation)
# of the analysis report, including:
#   1. Raw data overview
#   2. Data quality issues detected
#   3. Before/After cleaning comparisons
#   4. Before/After imputation comparisons
#   5. Final dataset summary
#
# Output: PDF files in docs/reports/
# ============================================================

# --- Libraries ---
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)
library(forcats)

# --- Setup ---
output_dir <- "docs/reports"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Color palette
pal <- c("#2166AC", "#D6604D", "#4DAF4A", "#FF7F00", "#984EA3",
         "#E7298A", "#66C2A5", "#FC8D62", "#8DA0CB", "#A6D854")
pal_before_after <- c("Before" = "#D6604D", "After" = "#2166AC")

cat("Loading data from all pipeline stages...\n")
load("raw_data/processed_data/merged_hacking_data.RData")       # merged_data (raw)
load("raw_data/processed_data/final_cleaned_data.RData")        # data_final_minimal (outlier-removed)
load("raw_data/processed_data/mice_imputed_data.RData")         # data_final_imputed (final)

raw <- merged_data
cleaned <- data_final_minimal
final <- data_final_imputed

# ============================================================
# SECTION 1: Raw Data Overview
# ============================================================
cat("\n=== Section 1: Raw Data Overview ===\n")

pdf(file.path(output_dir, "01_raw_data_overview.pdf"), width = 12, height = 8)

# --- 1.1 Missing values heatmap ---
na_raw <- data.frame(
  Column = names(raw),
  Present = sapply(raw, function(x) sum(!is.na(x) & x != "")),
  Missing = sapply(raw, function(x) sum(is.na(x) | x == ""))
)
na_raw$Column <- factor(na_raw$Column, levels = rev(names(raw)))
na_long <- pivot_longer(na_raw, cols = c("Present", "Missing"),
                        names_to = "Status", values_to = "Count")
na_long$Status <- factor(na_long$Status, levels = c("Present", "Missing"))

p1 <- ggplot(na_long, aes(x = Column, y = Count, fill = Status)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_fill_manual(values = c("Present" = "#2166AC", "Missing" = "#D6604D")) +
  scale_y_continuous(labels = comma) +
  labs(title = "Raw Data: Missing Values by Column",
       subtitle = paste("Total rows:", comma(nrow(raw))),
       x = NULL, y = "Row Count") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
print(p1)

# --- 1.2 Raw numeric distributions (character -> numeric) ---
raw_ransom <- suppressWarnings(as.numeric(raw$Ransom))
raw_downtime <- suppressWarnings(as.numeric(raw$DownTime))
raw_loss <- suppressWarnings(as.numeric(raw$Loss))

num_raw <- data.frame(
  Ransom = raw_ransom,
  DownTime = raw_downtime,
  Loss = raw_loss
)

num_long <- pivot_longer(num_raw, cols = everything(),
                         names_to = "Variable", values_to = "Value")
num_long <- num_long[!is.na(num_long$Value), ]

p2 <- ggplot(num_long, aes(x = Value)) +
  geom_histogram(bins = 50, fill = "#2166AC", alpha = 0.8) +
  facet_wrap(~Variable, scales = "free", ncol = 3) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(title = "Raw Data: Numeric Variable Distributions",
       subtitle = "Before outlier detection and cleaning",
       x = "Value", y = "Frequency") +
  theme_minimal(base_size = 13)
print(p2)

# --- 1.3 Data source composition (3 files) ---
n1 <- 201097  # Part1 rows (from merge step)
n2 <- 191867  # Part2 rows
n3 <- 199801  # Part3 rows
src_df <- data.frame(
  Source = c("Part1 (CSV)", "Part2 (Excel)", "Part3 (TXT)"),
  Rows = c(n1, n2, n3)
)
src_df$Label <- paste0(comma(src_df$Rows), "\n(",
                       round(100 * src_df$Rows / sum(src_df$Rows), 1), "%)")

p3 <- ggplot(src_df, aes(x = "", y = Rows, fill = Source)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(values = pal[1:3]) +
  labs(title = "Data Sources: Row Contribution",
       subtitle = paste("Total:", comma(sum(src_df$Rows)), "rows")) +
  theme_void(base_size = 13) +
  theme(legend.position = "right")
print(p3)

# --- 1.4 Date distribution (timeline) ---
raw_dates <- as.Date(raw$Date, format = "%d/%m/%Y")
# If already Date type, use directly
if (inherits(raw$Date, "Date")) raw_dates <- raw$Date
date_df <- data.frame(Date = raw_dates[!is.na(raw_dates)])
date_df$Year <- as.integer(format(date_df$Date, "%Y"))

p4 <- ggplot(date_df, aes(x = Year)) +
  geom_bar(fill = "#2166AC", alpha = 0.8) +
  scale_y_continuous(labels = comma) +
  labs(title = "Incidents by Year",
       subtitle = "Distribution of cyber-attack events over time",
       x = "Year", y = "Number of Incidents") +
  theme_minimal(base_size = 13)
print(p4)

dev.off()
cat("  Saved: 01_raw_data_overview.pdf\n")

# ============================================================
# SECTION 2: Data Quality Issues Detected
# ============================================================
cat("\n=== Section 2: Data Quality Issues ===\n")

pdf(file.path(output_dir, "02_data_quality_issues.pdf"), width = 12, height = 8)

# --- 2.1 Duplicate detection ---
# Show duplicate count from raw data
raw_char <- mutate(raw, across(everything(), as.character))
n_dup <- sum(duplicated(raw_char))

dup_df <- data.frame(
  Status = c("Unique", "Duplicate"),
  Count = c(nrow(raw) - n_dup, n_dup)
)
dup_df$Label <- paste0(comma(dup_df$Count), "\n(",
                       round(100 * dup_df$Count / nrow(raw), 1), "%)")

p5 <- ggplot(dup_df, aes(x = Status, y = Count, fill = Status)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = Label), vjust = -0.3, size = 4.5) +
  scale_fill_manual(values = c("Unique" = "#2166AC", "Duplicate" = "#D6604D")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Duplicate Row Detection",
       subtitle = "Exact row-level duplicates in raw merged data",
       x = NULL, y = "Row Count") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p5)

# --- 2.2 Outlier detection: boxplots of numeric variables ---
# Using cleaned (post-format-conversion) data with original + outlier-flagged
num_cleaned <- data.frame(
  Ransom = cleaned$Ransom,
  DownTime = cleaned$DownTime,
  Loss = cleaned$Loss
)
num_cl_long <- pivot_longer(num_cleaned, cols = everything(),
                            names_to = "Variable", values_to = "Value")
num_cl_long <- num_cl_long[!is.na(num_cl_long$Value), ]

p6 <- ggplot(num_cl_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(outlier.colour = "#D6604D", outlier.size = 0.5, outlier.alpha = 0.3) +
  facet_wrap(~Variable, scales = "free", ncol = 3) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = pal[1:3]) +
  labs(title = "Numeric Variables: Boxplot After Format Conversion",
       subtitle = "Red dots indicate potential outliers (after IQR-based detection)",
       x = NULL, y = "Value") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p6)

# --- 2.3 Missing value comparison: raw vs after outlier removal ---
na_compare <- data.frame(
  Column = rep(c("Ransom", "DownTime", "Loss"), 2),
  Stage = rep(c("Raw (Merged)", "After Outlier Removal"), each = 3),
  NA_Count = c(
    sum(is.na(raw_ransom)), sum(is.na(raw_downtime)), sum(is.na(raw_loss)),
    sum(is.na(cleaned$Ransom)), sum(is.na(cleaned$DownTime)), sum(is.na(cleaned$Loss))
  )
)
na_compare$NA_Pct <- round(100 * na_compare$NA_Count / nrow(raw), 1)
na_compare$Stage <- factor(na_compare$Stage,
                           levels = c("Raw (Merged)", "After Outlier Removal"))

p7 <- ggplot(na_compare, aes(x = Column, y = NA_Pct, fill = Stage)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_text(aes(label = paste0(NA_Pct, "%")),
            position = position_dodge(width = 0.6), vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c("Raw (Merged)" = "#D6604D",
                               "After Outlier Removal" = "#FF7F00")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Missing Values: Raw vs After Outlier Removal",
       subtitle = "Outlier removal converts extreme values to NA, increasing missing rate",
       x = NULL, y = "Missing %") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
print(p7)

# --- 2.4 WebServer raw diversity (before cleaning) ---
ws_raw <- raw$WebServer
ws_raw[ws_raw == "" | is.na(ws_raw)] <- "(empty)"
ws_top <- sort(table(ws_raw), decreasing = TRUE)
ws_df <- data.frame(
  Server = names(ws_top)[1:15],
  Count = as.integer(ws_top[1:15])
)
ws_df$Server <- factor(ws_df$Server, levels = rev(ws_df$Server))

p8 <- ggplot(ws_df, aes(x = Server, y = Count)) +
  geom_bar(stat = "identity", fill = "#2166AC", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Raw WebServer: Top 15 Values (Before Cleaning)",
       subtitle = "High cardinality with inconsistent naming",
       x = NULL, y = "Count") +
  theme_minimal(base_size = 13)
print(p8)

# --- 2.5 Country raw issues ---
co_raw <- toupper(trimws(as.character(raw$Country)))
co_raw[co_raw == "" | is.na(co_raw)] <- "(empty)"
# Show problematic pairs
problem_countries <- c("UNITED STATES", "UNITED STATE", "AMERICA", "US", "USA",
                       "UNITED KINGDOM", "UK", "GB", "ENGLAND",
                       "(empty)", "UNKNOWN")
co_prob <- co_raw[co_raw %in% problem_countries]
co_prob_tbl <- sort(table(co_prob), decreasing = TRUE)
co_prob_df <- data.frame(Value = names(co_prob_tbl), Count = as.integer(co_prob_tbl))
co_prob_df$Value <- factor(co_prob_df$Value, levels = rev(co_prob_df$Value))

p9 <- ggplot(co_prob_df, aes(x = Value, y = Count)) +
  geom_bar(stat = "identity", fill = "#D6604D", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Country: Inconsistent Naming Examples",
       subtitle = "Multiple representations of same country need standardization",
       x = NULL, y = "Count") +
  theme_minimal(base_size = 13)
print(p9)

dev.off()
cat("  Saved: 02_data_quality_issues.pdf\n")

# ============================================================
# SECTION 3: Before/After Cleaning Comparisons
# ============================================================
cat("\n=== Section 3: Before/After Cleaning ===\n")

pdf(file.path(output_dir, "03_before_after_cleaning.pdf"), width = 12, height = 8)

# --- 3.1 WebServer: Raw vs Cleaned ---
# Raw
ws_raw_all <- raw$WebServer
ws_raw_all[ws_raw_all == "" | is.na(ws_raw_all)] <- NA
ws_raw_tbl <- sort(table(ws_raw_all, useNA = "no"), decreasing = TRUE)
n_raw_ws <- length(ws_raw_tbl)

# Cleaned
ws_clean_tbl <- sort(table(final$WebServer_clean, useNA = "no"), decreasing = TRUE)
ws_clean_df <- data.frame(Category = names(ws_clean_tbl),
                          Count = as.integer(ws_clean_tbl))
ws_clean_df$Category <- factor(ws_clean_df$Category,
                               levels = rev(ws_clean_df$Category))

p10a <- ggplot(ws_clean_df, aes(x = Category, y = Count)) +
  geom_bar(stat = "identity", fill = "#2166AC", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = paste0("WebServer After Cleaning (", nrow(ws_clean_df), " categories)"),
       subtitle = paste0("Reduced from ", n_raw_ws, " raw values; ",
                         comma(sum(is.na(final$WebServer_clean))), " NA"),
       x = NULL, y = "Count") +
  theme_minimal(base_size = 13)
print(p10a)

# --- 3.2 Country: Raw vs Cleaned ---
co_clean_tbl <- sort(table(final$Country_clean, useNA = "no"), decreasing = TRUE)
co_clean_df <- data.frame(Category = names(co_clean_tbl),
                          Count = as.integer(co_clean_tbl))
co_clean_df$Category <- factor(co_clean_df$Category,
                               levels = rev(co_clean_df$Category))

co_raw2 <- toupper(trimws(as.character(raw$Country)))
co_raw2[co_raw2 == "" | co_raw2 == "UNKNOWN"] <- NA
n_raw_co <- length(unique(co_raw2[!is.na(co_raw2)]))

p10b <- ggplot(co_clean_df, aes(x = Category, y = Count)) +
  geom_bar(stat = "identity", fill = "#4DAF4A", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = paste0("Country After Cleaning (", nrow(co_clean_df), " categories)"),
       subtitle = paste0("Standardized from ~", n_raw_co, " raw variations; ",
                         comma(sum(is.na(final$Country_clean))), " NA"),
       x = NULL, y = "Count") +
  theme_minimal(base_size = 13)
print(p10b)

# --- 3.3 Encoding: Raw vs Cleaned ---
enc_clean_tbl <- sort(table(final$Encoding_clean, useNA = "no"), decreasing = TRUE)
enc_clean_df <- data.frame(Category = names(enc_clean_tbl),
                           Count = as.integer(enc_clean_tbl))
enc_clean_df$Category <- factor(enc_clean_df$Category,
                                levels = rev(enc_clean_df$Category))

enc_raw <- toupper(trimws(as.character(raw$Encoding)))
enc_raw[enc_raw == "" | enc_raw == "NULL" | enc_raw == "N"] <- NA
n_raw_enc <- length(unique(enc_raw[!is.na(enc_raw)]))

p10c <- ggplot(enc_clean_df, aes(x = Category, y = Count)) +
  geom_bar(stat = "identity", fill = "#FF7F00", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = paste0("Encoding After Cleaning (", nrow(enc_clean_df), " categories)"),
       subtitle = paste0("Standardized from ~", n_raw_enc, " raw variations; ",
                         comma(sum(is.na(final$Encoding_clean))), " NA"),
       x = NULL, y = "Count") +
  theme_minimal(base_size = 13)
print(p10c)

# --- 3.4 Notify: Raw vs Cleaned ---
no_clean_tbl <- sort(table(final$Notify_clean, useNA = "no"), decreasing = TRUE)
no_clean_top <- head(no_clean_tbl, 15)
no_clean_df <- data.frame(Category = names(no_clean_top),
                          Count = as.integer(no_clean_top))
no_clean_df$Category <- factor(no_clean_df$Category,
                               levels = rev(no_clean_df$Category))

n_raw_notify <- length(unique(raw$Notify[raw$Notify != "" & !is.na(raw$Notify)]))

p10d <- ggplot(no_clean_df, aes(x = Category, y = Count)) +
  geom_bar(stat = "identity", fill = "#984EA3", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Notify After Cleaning (Top 15 of 51 categories)",
       subtitle = paste0("Reduced from ", comma(n_raw_notify),
                         " raw values; Anonymous/Unknown -> NA (",
                         comma(sum(is.na(final$Notify_clean))), ")"),
       x = NULL, y = "Count") +
  theme_minimal(base_size = 13)
print(p10d)

# --- 3.5 URL_suffix extraction ---
url_tbl <- sort(table(final$URL_suffix, useNA = "no"), decreasing = TRUE)
url_df <- data.frame(TLD = names(url_tbl), Count = as.integer(url_tbl))
url_df$TLD <- factor(url_df$TLD, levels = rev(url_df$TLD))

p10e <- ggplot(url_df, aes(x = TLD, y = Count)) +
  geom_bar(stat = "identity", fill = "#E7298A", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = paste0("URL Suffix (TLD) Distribution (", nrow(url_df), " categories)"),
       subtitle = paste0("Extracted from domain; NA: ",
                         comma(sum(is.na(final$URL_suffix)))),
       x = NULL, y = "Count") +
  theme_minimal(base_size = 13)
print(p10e)

# --- 3.6 Summary: Cardinality reduction ---
card_df <- data.frame(
  Variable = c("WebServer", "Country", "Encoding", "Notify", "URL (-> suffix)"),
  Before = c(n_raw_ws, n_raw_co, n_raw_enc, n_raw_notify, length(unique(raw$URL))),
  After = c(length(ws_clean_tbl), length(co_clean_tbl), length(enc_clean_tbl),
            length(no_clean_tbl), length(url_tbl))
)
card_long <- pivot_longer(card_df, cols = c("Before", "After"),
                          names_to = "Stage", values_to = "UniqueValues")
card_long$Stage <- factor(card_long$Stage, levels = c("Before", "After"))
card_long$Variable <- factor(card_long$Variable, levels = card_df$Variable)

p11 <- ggplot(card_long, aes(x = Variable, y = UniqueValues, fill = Stage)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_text(aes(label = comma(UniqueValues)),
            position = position_dodge(width = 0.6), vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = pal_before_after) +
  scale_y_log10(labels = comma) +
  labs(title = "Categorical Variable Cardinality: Before vs After Cleaning",
       subtitle = "Y-axis on log scale; cleaning reduces high-cardinality to manageable levels",
       x = NULL, y = "Unique Values (log scale)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
print(p11)

dev.off()
cat("  Saved: 03_before_after_cleaning.pdf\n")

# ============================================================
# SECTION 4: Before/After Imputation
# ============================================================
cat("\n=== Section 4: Before/After Imputation ===\n")

pdf(file.path(output_dir, "04_before_after_imputation.pdf"), width = 12, height = 8)

# --- 4.1 Missing values: Before vs After imputation ---
na_impute <- data.frame(
  Variable = rep(c("Ransom\n(modern only)", "DownTime", "Loss"), 2),
  Stage = rep(c("Before Imputation", "After Imputation"), each = 3),
  NA_Count = c(
    sum(is.na(final$Ransom[final$is_modern])),
    sum(is.na(final$DownTime)),
    sum(is.na(final$Loss)),
    sum(is.na(final$Ransom_imputed[final$is_modern])),
    sum(is.na(final$DownTime_imputed)),
    sum(is.na(final$Loss_imputed))
  )
)
na_impute$Stage <- factor(na_impute$Stage,
                          levels = c("Before Imputation", "After Imputation"))

p12 <- ggplot(na_impute, aes(x = Variable, y = NA_Count, fill = Stage)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_text(aes(label = comma(NA_Count)),
            position = position_dodge(width = 0.6), vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c("Before Imputation" = "#D6604D",
                               "After Imputation" = "#2166AC")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Missing Values: Before vs After MICE Imputation",
       subtitle = "PMM (Predictive Mean Matching) used for imputation",
       x = NULL, y = "Missing Count") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
print(p12)

# --- 4.2 Distribution comparison: Ransom original vs imputed ---
ransom_comp <- data.frame(
  Value = c(final$Ransom[!is.na(final$Ransom)],
            final$Ransom_imputed[final$is_modern & is.na(final$Ransom)]),
  Source = c(rep("Original", sum(!is.na(final$Ransom))),
             rep("Imputed", sum(final$is_modern & is.na(final$Ransom))))
)

p13a <- ggplot(ransom_comp, aes(x = Value, fill = Source)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Original" = "#D6604D", "Imputed" = "#2166AC")) +
  labs(title = "Ransom: Original vs Imputed Distribution",
       subtitle = "Density comparison to verify imputation quality",
       x = "Ransom (thousands USD)", y = "Density") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
print(p13a)

# --- 4.3 Distribution comparison: DownTime ---
dt_comp <- data.frame(
  Value = c(final$DownTime[!is.na(final$DownTime)],
            final$DownTime_imputed[is.na(final$DownTime)]),
  Source = c(rep("Original", sum(!is.na(final$DownTime))),
             rep("Imputed", sum(is.na(final$DownTime))))
)

p13b <- ggplot(dt_comp, aes(x = Value, fill = Source)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Original" = "#D6604D", "Imputed" = "#2166AC")) +
  labs(title = "DownTime: Original vs Imputed Distribution",
       subtitle = "Density comparison to verify imputation quality",
       x = "DownTime (days)", y = "Density") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
print(p13b)

# --- 4.4 Distribution comparison: Loss ---
loss_comp <- data.frame(
  Value = c(final$Loss[!is.na(final$Loss)],
            final$Loss_imputed[is.na(final$Loss)]),
  Source = c(rep("Original", sum(!is.na(final$Loss))),
             rep("Imputed", sum(is.na(final$Loss))))
)

p13c <- ggplot(loss_comp, aes(x = Value, fill = Source)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Original" = "#D6604D", "Imputed" = "#2166AC")) +
  labs(title = "Loss: Original vs Imputed Distribution",
       subtitle = "Density comparison to verify imputation quality",
       x = "Estimated Loss", y = "Density") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
print(p13c)

# --- 4.5 Log-transformed distributions ---
log_long <- rbind(
  data.frame(Variable = "Ransom_log",
             Value = final$Ransom_log[!is.na(final$Ransom_log)]),
  data.frame(Variable = "DownTime_log",
             Value = final$DownTime_log[!is.na(final$DownTime_log)]),
  data.frame(Variable = "Loss_log",
             Value = final$Loss_log[!is.na(final$Loss_log)])
)

p14 <- ggplot(log_long, aes(x = Value, fill = Variable)) +
  geom_histogram(bins = 40, alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~Variable, scales = "free", ncol = 3) +
  scale_fill_manual(values = pal[1:3]) +
  labs(title = "Log-Transformed Distributions: log10(x + 1)",
       subtitle = "Used for MICE imputation to improve normality",
       x = "log10(value + 1)", y = "Frequency") +
  theme_minimal(base_size = 13)
print(p14)

# --- 4.6 QQ plots: imputed values (sampled for PDF size) ---
set.seed(42)
n_qq <- 5000  # sample for readable QQ plots

par(mfrow = c(1, 3))

ransom_qq <- sample(final$Ransom_log_imputed[final$is_modern & !is.na(final$Ransom_log_imputed)], n_qq)
qqnorm(ransom_qq,
       main = "QQ Plot: Ransom_log_imputed\n(Modern era, n=5000 sample)",
       col = adjustcolor("#2166AC", 0.4), pch = 16, cex = 0.5)
qqline(ransom_qq, col = "#D6604D", lwd = 2)

dt_qq <- sample(final$DownTime_log_imputed, n_qq)
qqnorm(dt_qq,
       main = "QQ Plot: DownTime_log_imputed\n(n=5000 sample)",
       col = adjustcolor("#4DAF4A", 0.4), pch = 16, cex = 0.5)
qqline(dt_qq, col = "#D6604D", lwd = 2)

loss_qq <- sample(final$Loss_log_imputed, n_qq)
qqnorm(loss_qq,
       main = "QQ Plot: Loss_log_imputed\n(n=5000 sample)",
       col = adjustcolor("#FF7F00", 0.4), pch = 16, cex = 0.5)
qqline(loss_qq, col = "#D6604D", lwd = 2)

par(mfrow = c(1, 1))

dev.off()
cat("  Saved: 04_before_after_imputation.pdf\n")

# ============================================================
# SECTION 5: Final Dataset Summary
# ============================================================
cat("\n=== Section 5: Final Dataset Summary ===\n")

pdf(file.path(output_dir, "05_final_dataset_summary.pdf"), width = 12, height = 8)

# --- 5.1 Final NA overview (all 31 columns) ---
na_final <- data.frame(
  Column = names(final),
  NA_Count = sapply(final, function(x) sum(is.na(x))),
  stringsAsFactors = FALSE
)
na_final$NA_Pct <- round(100 * na_final$NA_Count / nrow(final), 1)
na_final <- na_final[order(-na_final$NA_Pct), ]
na_final$Column <- factor(na_final$Column, levels = rev(na_final$Column))

# Color: green if 0%, yellow if <20%, red if >=20%
na_final$Color <- ifelse(na_final$NA_Pct == 0, "Complete",
                         ifelse(na_final$NA_Pct < 20, "Partial", "High Missing"))

p15 <- ggplot(na_final, aes(x = Column, y = NA_Pct, fill = Color)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("Complete" = "#4DAF4A",
                               "Partial" = "#FF7F00",
                               "High Missing" = "#D6604D"),
                    name = "Status") +
  labs(title = "Final Dataset: Missing Value Percentage by Column",
       subtitle = paste(nrow(final), "rows x", ncol(final), "columns"),
       x = NULL, y = "Missing %") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")
print(p15)

# --- 5.2 Imputed numeric distributions (final) ---
imp_long <- rbind(
  data.frame(Variable = "Ransom",
             Value = final$Ransom_imputed[final$is_modern & !is.na(final$Ransom_imputed)]),
  data.frame(Variable = "DownTime",
             Value = final$DownTime_imputed),
  data.frame(Variable = "Loss",
             Value = final$Loss_imputed)
)

p16 <- ggplot(imp_long, aes(x = Value, fill = Variable)) +
  geom_histogram(bins = 50, alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~Variable, scales = "free", ncol = 3) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = pal[1:3]) +
  labs(title = "Final Imputed Numeric Distributions",
       subtitle = "Complete data after MICE PMM imputation",
       x = "Value", y = "Frequency") +
  theme_minimal(base_size = 13)
print(p16)

# --- 5.3 Correlation matrix of imputed numerics ---
modern_idx <- final$is_modern & !is.na(final$Ransom_imputed)
cor_data <- data.frame(
  Ransom = final$Ransom_imputed[modern_idx],
  DownTime = final$DownTime_imputed[modern_idx],
  Loss = final$Loss_imputed[modern_idx]
)
cor_mat <- cor(cor_data, use = "complete.obs")

cor_long <- as.data.frame(as.table(cor_mat))
names(cor_long) <- c("Var1", "Var2", "Correlation")

p17 <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 3)), size = 5) +
  scale_fill_gradient2(low = "#D6604D", mid = "white", high = "#2166AC",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Correlation Matrix: Imputed Numeric Variables (Modern Era)",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 0))
print(p17)

# --- 5.4 Pipeline flow summary ---
pipeline_df <- data.frame(
  Step = c("0. Merge 3 Files", "1. Format Conversion",
           "2. Distribution Check", "3-4. Univariate Outliers",
           "5-6. Multivariate Outliers", "7. Health Check",
           "8. Preprocessing", "9. Categorical Cleaning",
           "10. Quality Check", "11. MICE Imputation",
           "12. Validation", "13. Comparison"),
  Rows = rep(nrow(final), 12),
  Cols = c(10, 10, 10, 10, 10, 10, 18, 24, 24, 31, 31, 31),
  Phase = c("Merge", "Clean", "Viz", "Clean", "Clean", "Diag",
            "Prep", "Clean", "Diag", "Impute", "Diag", "Diag")
)
pipeline_df$Step <- factor(pipeline_df$Step, levels = rev(pipeline_df$Step))

p18 <- ggplot(pipeline_df, aes(x = Step, y = Cols, fill = Phase)) +
  geom_bar(stat = "identity", width = 0.6) +
  coord_flip() +
  scale_fill_manual(values = c("Merge" = "#2166AC", "Clean" = "#D6604D",
                               "Viz" = "#4DAF4A", "Diag" = "#FF7F00",
                               "Prep" = "#984EA3", "Impute" = "#E7298A")) +
  labs(title = "Pipeline Overview: Column Count at Each Stage",
       subtitle = paste0("14 scripts; ", comma(nrow(final)), " rows maintained throughout"),
       x = NULL, y = "Number of Columns") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
print(p18)

# --- 5.5 Era comparison: Modern vs Legacy ---
era_df <- data.frame(
  Era = c("Legacy (<2016)", "Modern (2016+)"),
  Count = c(sum(!final$is_modern), sum(final$is_modern))
)
era_df$Pct <- round(100 * era_df$Count / nrow(final), 1)
era_df$Label <- paste0(comma(era_df$Count), "\n(", era_df$Pct, "%)")

p19 <- ggplot(era_df, aes(x = Era, y = Count, fill = Era)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = Label), vjust = -0.3, size = 4.5) +
  scale_fill_manual(values = c("Legacy (<2016)" = "#D6604D",
                               "Modern (2016+)" = "#2166AC")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Data Split: Legacy vs Modern Era",
       subtitle = "Modern (2016+): full imputation | Legacy: DownTime + Loss only",
       x = NULL, y = "Row Count") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p19)

dev.off()
cat("  Saved: 05_final_dataset_summary.pdf\n")

# ============================================================
# DONE
# ============================================================
cat("\n============================================================\n")
cat("All report plots generated successfully!\n")
cat("Output directory:", normalizePath(output_dir), "\n")
cat("Files:\n")
cat("  1. 01_raw_data_overview.pdf\n")
cat("  2. 02_data_quality_issues.pdf\n")
cat("  3. 03_before_after_cleaning.pdf\n")
cat("  4. 04_before_after_imputation.pdf\n")
cat("  5. 05_final_dataset_summary.pdf\n")
cat("============================================================\n")
