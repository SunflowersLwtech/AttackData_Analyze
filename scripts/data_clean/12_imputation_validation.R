# Post-Imputation Validation Script
# Purpose: Visual validation and spot checks of MICE imputation quality
# Author: LiuWei TP085412
# Date: 2026-01-21

cat("======================================\n")
cat("POST-IMPUTATION VALIDATION\n")
cat("Visual Comparison & Quality Spot Checks\n")
cat("======================================\n\n")

# Load required libraries
required_packages <- c("ggplot2", "gridExtra", "dplyr", "moments")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste0("Installing package: ", pkg, "...\n"))
    install.packages(pkg, repos = "https://cloud.r-project.org/", quiet = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE, quietly = TRUE)
  }
}

cat("Required packages loaded.\n\n")

# Load imputed data
cat("Loading imputed data...\n")
load("raw_data/processed_data/mice_imputed_data.RData")
data_imputed <- data_final_imputed  # Use the correct object name
cat("Data loaded successfully.\n")
cat("Total records:", nrow(data_imputed), "\n\n")

# Load pre-imputation data for comparison
cat("Loading pre-imputation data for comparison...\n")
load("raw_data/processed_data/preprocessed_for_imputation.RData")
data_before <- data_prep
cat("Pre-imputation data loaded.\n\n")

cat("======================================\n")
cat("PART 1: DISTRIBUTION VISUAL COMPARISON\n")
cat("Objective: Verify imputed values match observed distribution\n")
cat("======================================\n\n")

cat("Theory:\n")
cat("  Ideal scenario: Observed and imputed density curves should OVERLAP\n")
cat("  Red flag: Imputed values create strange spikes at distribution tails\n")
cat("  This indicates: PMM is picking inappropriate donor values\n\n")

# Create imputation status flags
data_imputed$Ransom_was_imputed <- is.na(data_before$Ransom) & !is.na(data_imputed$Ransom_imputed)
data_imputed$DownTime_was_imputed <- is.na(data_before$DownTime) & !is.na(data_imputed$DownTime_imputed)
data_imputed$Loss_was_imputed <- is.na(data_before$Loss) & !is.na(data_imputed$Loss_imputed)

cat("Imputation flags created:\n")
cat("  Ransom_was_imputed: Cases where Ransom was NA and now imputed\n")
cat("  DownTime_was_imputed: Cases where DownTime was NA and now imputed\n")
cat("  Loss_was_imputed: Cases where Loss was NA and now imputed\n\n")

# Count imputed values
cat("Imputation counts:\n")
cat("  Ransom imputed:", sum(data_imputed$Ransom_was_imputed, na.rm = TRUE), "\n")
cat("  DownTime imputed:", sum(data_imputed$DownTime_was_imputed, na.rm = TRUE), "\n")
cat("  Loss imputed:", sum(data_imputed$Loss_was_imputed, na.rm = TRUE), "\n\n")

cat("Creating density comparison plots...\n\n")

# Prepare plot output
pdf("docs/reports/imputation_validation_plots.pdf", width = 14, height = 10)

# ============================================
# PLOT 1: Ransom Distribution Comparison
# ============================================
cat("Plot 1: Ransom_log distribution (Observed vs Imputed)\n")

# Filter modern data only (since legacy Ransom wasn't imputed)
ransom_data <- data_imputed %>%
  filter(is_modern == TRUE) %>%
  select(Ransom_log, Ransom_was_imputed) %>%
  filter(!is.na(Ransom_log))

cat("  Modern data records:", nrow(ransom_data), "\n")
cat("  Observed values:", sum(!ransom_data$Ransom_was_imputed), "\n")
cat("  Imputed values:", sum(ransom_data$Ransom_was_imputed), "\n\n")

p1 <- ggplot(ransom_data, aes(x = Ransom_log, fill = Ransom_was_imputed)) +
  geom_density(alpha = 0.6, adjust = 1.5) +
  scale_fill_manual(
    values = c("TRUE" = "red", "FALSE" = "blue"),
    labels = c("TRUE" = "Imputed Values", "FALSE" = "Observed Values"),
    name = "Value Type"
  ) +
  labs(
    title = "Ransom_log: Distribution Comparison (Modern Data Only)",
    subtitle = paste0("Observed: ", sum(!ransom_data$Ransom_was_imputed),
                     " | Imputed: ", sum(ransom_data$Ransom_was_imputed)),
    x = "Ransom (log10 scale)",
    y = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom") +
  annotate("text", x = Inf, y = Inf,
           label = "✓ Good: Curves overlap\n✗ Bad: Imputed spikes at tails",
           hjust = 1.1, vjust = 1.5, size = 3.5, color = "darkgreen")

# Add statistical comparison
observed_stats <- ransom_data %>%
  filter(!Ransom_was_imputed) %>%
  summarise(
    mean = mean(Ransom_log, na.rm = TRUE),
    median = median(Ransom_log, na.rm = TRUE),
    sd = sd(Ransom_log, na.rm = TRUE),
    skewness = skewness(Ransom_log)
  )

imputed_stats <- ransom_data %>%
  filter(Ransom_was_imputed) %>%
  summarise(
    mean = mean(Ransom_log, na.rm = TRUE),
    median = median(Ransom_log, na.rm = TRUE),
    sd = sd(Ransom_log, na.rm = TRUE),
    skewness = skewness(Ransom_log)
  )

cat("  Ransom_log statistical comparison:\n")
cat("    Observed: mean=", round(observed_stats$mean, 3),
    ", median=", round(observed_stats$median, 3),
    ", sd=", round(observed_stats$sd, 3),
    ", skewness=", round(observed_stats$skewness, 3), "\n")
cat("    Imputed:  mean=", round(imputed_stats$mean, 3),
    ", median=", round(imputed_stats$median, 3),
    ", sd=", round(imputed_stats$sd, 3),
    ", skewness=", round(imputed_stats$skewness, 3), "\n")

# Calculate KS test (distribution similarity) - only if we have both observed and imputed
if (sum(!ransom_data$Ransom_was_imputed) > 0 && sum(ransom_data$Ransom_was_imputed) > 0) {
  ks_test_ransom <- ks.test(
    ransom_data$Ransom_log[ransom_data$Ransom_was_imputed],
    ransom_data$Ransom_log[!ransom_data$Ransom_was_imputed]
  )
  cat("    KS Test p-value:", round(ks_test_ransom$p.value, 4), "\n")
  cat("    Interpretation: p >", "0.05 means distributions are SIMILAR (good!)\n\n")
} else {
  ks_test_ransom <- list(p.value = NA)
  cat("    KS Test: SKIPPED (all values were imputed in modern data)\n")
  cat("    Note: Cannot compare distributions when no observed values exist\n\n")
}

# ============================================
# PLOT 2: DownTime Distribution Comparison
# ============================================
cat("Plot 2: DownTime_log distribution (Observed vs Imputed)\n")

downtime_data <- data_imputed %>%
  select(DownTime_log, DownTime_was_imputed) %>%
  filter(!is.na(DownTime_log))

cat("  Total records:", nrow(downtime_data), "\n")
cat("  Observed values:", sum(!downtime_data$DownTime_was_imputed), "\n")
cat("  Imputed values:", sum(downtime_data$DownTime_was_imputed), "\n\n")

p2 <- ggplot(downtime_data, aes(x = DownTime_log, fill = DownTime_was_imputed)) +
  geom_density(alpha = 0.6, adjust = 1.5) +
  scale_fill_manual(
    values = c("TRUE" = "red", "FALSE" = "blue"),
    labels = c("TRUE" = "Imputed Values", "FALSE" = "Observed Values"),
    name = "Value Type"
  ) +
  labs(
    title = "DownTime_log: Distribution Comparison",
    subtitle = paste0("Observed: ", sum(!downtime_data$DownTime_was_imputed),
                     " | Imputed: ", sum(downtime_data$DownTime_was_imputed)),
    x = "DownTime (log10 scale)",
    y = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom") +
  annotate("text", x = Inf, y = Inf,
           label = "✓ Good: Curves overlap\n✗ Bad: Imputed spikes at tails",
           hjust = 1.1, vjust = 1.5, size = 3.5, color = "darkgreen")

observed_stats_dt <- downtime_data %>%
  filter(!DownTime_was_imputed) %>%
  summarise(
    mean = mean(DownTime_log, na.rm = TRUE),
    median = median(DownTime_log, na.rm = TRUE),
    sd = sd(DownTime_log, na.rm = TRUE),
    skewness = skewness(DownTime_log)
  )

imputed_stats_dt <- downtime_data %>%
  filter(DownTime_was_imputed) %>%
  summarise(
    mean = mean(DownTime_log, na.rm = TRUE),
    median = median(DownTime_log, na.rm = TRUE),
    sd = sd(DownTime_log, na.rm = TRUE),
    skewness = skewness(DownTime_log)
  )

cat("  DownTime_log statistical comparison:\n")
cat("    Observed: mean=", round(observed_stats_dt$mean, 3),
    ", median=", round(observed_stats_dt$median, 3),
    ", sd=", round(observed_stats_dt$sd, 3),
    ", skewness=", round(observed_stats_dt$skewness, 3), "\n")
cat("    Imputed:  mean=", round(imputed_stats_dt$mean, 3),
    ", median=", round(imputed_stats_dt$median, 3),
    ", sd=", round(imputed_stats_dt$sd, 3),
    ", skewness=", round(imputed_stats_dt$skewness, 3), "\n")

if (sum(!downtime_data$DownTime_was_imputed) > 0 && sum(downtime_data$DownTime_was_imputed) > 0) {
  ks_test_downtime <- ks.test(
    downtime_data$DownTime_log[downtime_data$DownTime_was_imputed],
    downtime_data$DownTime_log[!downtime_data$DownTime_was_imputed]
  )
  cat("    KS Test p-value:", round(ks_test_downtime$p.value, 4), "\n")
  cat("    Interpretation: p >", "0.05 means distributions are SIMILAR (good!)\n\n")
} else {
  ks_test_downtime <- list(p.value = NA)
  cat("    KS Test: SKIPPED (insufficient data for comparison)\n\n")
}

# ============================================
# PLOT 3: Loss Distribution Comparison
# ============================================
cat("Plot 3: Loss_log distribution (Observed vs Imputed)\n")

loss_data <- data_imputed %>%
  select(Loss_log, Loss_was_imputed) %>%
  filter(!is.na(Loss_log))

cat("  Total records:", nrow(loss_data), "\n")
cat("  Observed values:", sum(!loss_data$Loss_was_imputed), "\n")
cat("  Imputed values:", sum(loss_data$Loss_was_imputed), "\n\n")

p3 <- ggplot(loss_data, aes(x = Loss_log, fill = Loss_was_imputed)) +
  geom_density(alpha = 0.6, adjust = 1.5) +
  scale_fill_manual(
    values = c("TRUE" = "red", "FALSE" = "blue"),
    labels = c("TRUE" = "Imputed Values", "FALSE" = "Observed Values"),
    name = "Value Type"
  ) +
  labs(
    title = "Loss_log: Distribution Comparison (CRITICAL CHECK)",
    subtitle = paste0("Observed: ", sum(!loss_data$Loss_was_imputed),
                     " | Imputed: ", sum(loss_data$Loss_was_imputed)),
    x = "Loss (log10 scale)",
    y = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom") +
  annotate("text", x = Inf, y = Inf,
           label = "✓ Good: Curves overlap\n✗ Bad: Imputed spikes at tails",
           hjust = 1.1, vjust = 1.5, size = 3.5, color = "darkgreen")

observed_stats_loss <- loss_data %>%
  filter(!Loss_was_imputed) %>%
  summarise(
    mean = mean(Loss_log, na.rm = TRUE),
    median = median(Loss_log, na.rm = TRUE),
    sd = sd(Loss_log, na.rm = TRUE),
    skewness = skewness(Loss_log)
  )

imputed_stats_loss <- loss_data %>%
  filter(Loss_was_imputed) %>%
  summarise(
    mean = mean(Loss_log, na.rm = TRUE),
    median = median(Loss_log, na.rm = TRUE),
    sd = sd(Loss_log, na.rm = TRUE),
    skewness = skewness(Loss_log)
  )

cat("  Loss_log statistical comparison:\n")
cat("    Observed: mean=", round(observed_stats_loss$mean, 3),
    ", median=", round(observed_stats_loss$median, 3),
    ", sd=", round(observed_stats_loss$sd, 3),
    ", skewness=", round(observed_stats_loss$skewness, 3), "\n")
cat("    Imputed:  mean=", round(imputed_stats_loss$mean, 3),
    ", median=", round(imputed_stats_loss$median, 3),
    ", sd=", round(imputed_stats_loss$sd, 3),
    ", skewness=", round(imputed_stats_loss$skewness, 3), "\n")

if (sum(!loss_data$Loss_was_imputed) > 0 && sum(loss_data$Loss_was_imputed) > 0) {
  ks_test_loss <- ks.test(
    loss_data$Loss_log[loss_data$Loss_was_imputed],
    loss_data$Loss_log[!loss_data$Loss_was_imputed]
  )
  cat("    KS Test p-value:", round(ks_test_loss$p.value, 4), "\n")
  cat("    Interpretation: p >", "0.05 means distributions are SIMILAR (good!)\n\n")
} else {
  ks_test_loss <- list(p.value = NA)
  cat("    KS Test: SKIPPED (insufficient data for comparison)\n\n")
}

# ============================================
# PLOT 4: Combined Comparison (Small Multiples)
# ============================================
cat("Plot 4: Combined distribution comparison\n\n")

# Arrange all three plots
grid.arrange(p1, p2, p3, ncol = 2)

dev.off()

cat("Distribution comparison plots saved: docs/reports/imputation_validation_plots.pdf\n\n")

cat("======================================\n")
cat("DISTRIBUTION COMPARISON SUMMARY\n")
cat("======================================\n\n")

# Create summary table
distribution_comparison <- data.frame(
  Variable = c("Ransom_log", "DownTime_log", "Loss_log"),
  Observed_Mean = c(observed_stats$mean, observed_stats_dt$mean, observed_stats_loss$mean),
  Imputed_Mean = c(imputed_stats$mean, imputed_stats_dt$mean, imputed_stats_loss$mean),
  Mean_Diff = c(
    imputed_stats$mean - observed_stats$mean,
    imputed_stats_dt$mean - observed_stats_dt$mean,
    imputed_stats_loss$mean - observed_stats_loss$mean
  ),
  KS_Test_pvalue = c(ks_test_ransom$p.value, ks_test_downtime$p.value, ks_test_loss$p.value),
  Quality_Assessment = c(
    ifelse(is.na(ks_test_ransom$p.value), "N/A (All imputed)",
           ifelse(ks_test_ransom$p.value > 0.05, "✓ PASS (Similar)", "⚠ REVIEW (Different)")),
    ifelse(is.na(ks_test_downtime$p.value), "N/A (Insufficient data)",
           ifelse(ks_test_downtime$p.value > 0.05, "✓ PASS (Similar)", "⚠ REVIEW (Different)")),
    ifelse(is.na(ks_test_loss$p.value), "N/A (Insufficient data)",
           ifelse(ks_test_loss$p.value > 0.05, "✓ PASS (Similar)", "⚠ REVIEW (Different)"))
  )
)

print(distribution_comparison, row.names = FALSE)
cat("\n")

cat("Interpretation:\n")
cat("  KS Test (Kolmogorov-Smirnov): Tests if two distributions are similar\n")
cat("  - p-value > 0.05: Distributions are NOT significantly different (GOOD)\n")
cat("  - p-value < 0.05: Distributions are significantly different (REVIEW NEEDED)\n\n")

# Overall quality assessment
pass_count <- sum(distribution_comparison$KS_Test_pvalue > 0.05, na.rm = TRUE)
valid_tests <- sum(!is.na(distribution_comparison$KS_Test_pvalue))
cat("Overall Distribution Quality:", pass_count, "/", valid_tests, "variables passed similarity test\n\n")

cat("======================================\n")
cat("PART 2: SPOT CHECKS - EXTREME CASES\n")
cat("Objective: Manual review of imputed records\n")
cat("======================================\n\n")

cat("Theory:\n")
cat("  Random sampling of imputed records to check logical consistency\n")
cat("  Example: Country=USA, WebServer=Nginx, Loss=$10 → Does this make sense?\n")
cat("  This catches cases where model fails in specific categorical combinations\n\n")

# ============================================
# Spot Check 1: Sample 10 Ransom Imputations (Modern Data)
# ============================================
cat("Spot Check 1: Ransom Imputations (Modern Data)\n")
cat("----------------------------------------------\n\n")

ransom_imputed_records <- data_imputed %>%
  filter(Ransom_was_imputed == TRUE, is_modern == TRUE) %>%
  select(Date, Country_clean, WebServer_clean, URL_suffix,
         Ransom_imputed, DownTime_imputed, Loss_imputed, Year)

if (nrow(ransom_imputed_records) > 0) {
  # Sample 10 random records
  set.seed(42)
  sample_size <- min(10, nrow(ransom_imputed_records))
  ransom_sample <- ransom_imputed_records[sample(nrow(ransom_imputed_records), sample_size), ]

  cat("Randomly sampled", sample_size, "records where Ransom was imputed:\n\n")

  for (i in 1:nrow(ransom_sample)) {
    cat(paste0("Record ", i, ":\n"))
    cat("  Date:", as.character(ransom_sample$Date[i]), "\n")
    cat("  Country:", ransom_sample$Country_clean[i], "\n")
    cat("  WebServer:", ransom_sample$WebServer_clean[i], "\n")
    cat("  URL TLD:", ransom_sample$URL_suffix[i], "\n")
    cat("  → Imputed Ransom: $", round(ransom_sample$Ransom_imputed[i], 2), "\n")
    cat("  → DownTime:", round(ransom_sample$DownTime_imputed[i], 1), "hours\n")
    cat("  → Loss: $", round(ransom_sample$Loss_imputed[i], 2), "\n")

    # Calculate ransom/loss ratio (should be <= 1 ideally)
    ratio <- ransom_sample$Ransom_imputed[i] / ransom_sample$Loss_imputed[i]
    cat("  → Ransom/Loss Ratio:", round(ratio, 3), "\n")

    if (ratio > 1) {
      cat("  ⚠️  WARNING: Ransom exceeds Loss (unusual!)\n")
    } else if (ratio > 0.5) {
      cat("  ⚠️  NOTE: Ransom is >50% of Loss (high but possible)\n")
    } else {
      cat("  ✓ OK: Ransom/Loss ratio reasonable\n")
    }
    cat("\n")
  }
} else {
  cat("No Ransom imputations found in modern data.\n\n")
}

# ============================================
# Spot Check 2: Sample 10 Loss Imputations
# ============================================
cat("Spot Check 2: Loss Imputations\n")
cat("-------------------------------\n\n")

loss_imputed_records <- data_imputed %>%
  filter(Loss_was_imputed == TRUE) %>%
  select(Date, Country_clean, WebServer_clean, URL_suffix,
         Ransom_imputed, DownTime_imputed, Loss_imputed, Year, is_modern)

if (nrow(loss_imputed_records) > 0) {
  set.seed(43)
  sample_size <- min(10, nrow(loss_imputed_records))
  loss_sample <- loss_imputed_records[sample(nrow(loss_imputed_records), sample_size), ]

  cat("Randomly sampled", sample_size, "records where Loss was imputed:\n\n")

  for (i in 1:nrow(loss_sample)) {
    cat(paste0("Record ", i, ":\n"))
    cat("  Date:", as.character(loss_sample$Date[i]), "(",
        ifelse(loss_sample$is_modern[i], "Modern", "Legacy"), "data)\n")
    cat("  Country:", loss_sample$Country_clean[i], "\n")
    cat("  WebServer:", loss_sample$WebServer_clean[i], "\n")
    cat("  → DownTime:", round(loss_sample$DownTime_imputed[i], 1), "hours\n")
    cat("  → Imputed Loss: $", round(loss_sample$Loss_imputed[i], 2), "\n")

    # Sanity check: Loss vs DownTime
    # Heuristic: Loss should increase with downtime (roughly)
    # Very rough estimate: $1000/hour is reasonable baseline
    expected_min_loss <- loss_sample$DownTime_imputed[i] * 100
    expected_max_loss <- loss_sample$DownTime_imputed[i] * 10000

    if (loss_sample$Loss_imputed[i] < expected_min_loss) {
      cat("  ⚠️  NOTE: Loss seems LOW for", round(loss_sample$DownTime_imputed[i], 1),
          "hours downtime\n")
    } else if (loss_sample$Loss_imputed[i] > expected_max_loss) {
      cat("  ⚠️  NOTE: Loss seems HIGH for", round(loss_sample$DownTime_imputed[i], 1),
          "hours downtime\n")
    } else {
      cat("  ✓ OK: Loss consistent with downtime\n")
    }
    cat("\n")
  }
} else {
  cat("No Loss imputations found.\n\n")
}

# ============================================
# Spot Check 3: Check Category-Specific Patterns
# ============================================
cat("Spot Check 3: Category-Specific Pattern Check\n")
cat("----------------------------------------------\n\n")

cat("Checking if imputed values follow category-specific patterns...\n\n")

# Check: USA vs Other countries - Loss comparison
cat("Pattern 1: USA vs Non-USA Loss Comparison\n")

usa_observed <- data_imputed %>%
  filter(Country_clean == "USA", !Loss_was_imputed) %>%
  summarise(
    mean_loss = mean(Loss_imputed, na.rm = TRUE),
    median_loss = median(Loss_imputed, na.rm = TRUE)
  )

usa_imputed <- data_imputed %>%
  filter(Country_clean == "USA", Loss_was_imputed) %>%
  summarise(
    mean_loss = mean(Loss_imputed, na.rm = TRUE),
    median_loss = median(Loss_imputed, na.rm = TRUE)
  )

cat("  USA - Observed Loss: mean=$", round(usa_observed$mean_loss, 2),
    ", median=$", round(usa_observed$median_loss, 2), "\n")
cat("  USA - Imputed Loss:  mean=$", round(usa_imputed$mean_loss, 2),
    ", median=$", round(usa_imputed$median_loss, 2), "\n")

diff_pct <- abs(usa_imputed$mean_loss - usa_observed$mean_loss) / usa_observed$mean_loss * 100
cat("  Difference:", round(diff_pct, 2), "%\n")

if (diff_pct < 10) {
  cat("  ✓ PASS: Imputed values consistent with observed USA patterns\n\n")
} else {
  cat("  ⚠️  REVIEW: Imputed values differ >10% from observed patterns\n\n")
}

# Check: Nginx vs Apache - DownTime comparison
cat("Pattern 2: Nginx vs Apache DownTime Comparison\n")

nginx_observed <- data_imputed %>%
  filter(WebServer_clean == "nginx", !DownTime_was_imputed) %>%
  summarise(
    mean_dt = mean(DownTime_imputed, na.rm = TRUE),
    median_dt = median(DownTime_imputed, na.rm = TRUE)
  )

nginx_imputed <- data_imputed %>%
  filter(WebServer_clean == "nginx", DownTime_was_imputed) %>%
  summarise(
    mean_dt = mean(DownTime_imputed, na.rm = TRUE),
    median_dt = median(DownTime_imputed, na.rm = TRUE)
  )

if (nrow(nginx_imputed) > 0 && !is.na(nginx_imputed$mean_dt)) {
  cat("  Nginx - Observed DownTime: mean=", round(nginx_observed$mean_dt, 2),
      "hrs, median=", round(nginx_observed$median_dt, 2), "hrs\n")
  cat("  Nginx - Imputed DownTime:  mean=", round(nginx_imputed$mean_dt, 2),
      "hrs, median=", round(nginx_imputed$median_dt, 2), "hrs\n")

  diff_pct_dt <- abs(nginx_imputed$mean_dt - nginx_observed$mean_dt) / nginx_observed$mean_dt * 100
  cat("  Difference:", round(diff_pct_dt, 2), "%\n")

  if (diff_pct_dt < 10) {
    cat("  ✓ PASS: Imputed values consistent with observed Nginx patterns\n\n")
  } else {
    cat("  ⚠️  REVIEW: Imputed values differ >10% from observed patterns\n\n")
  }
} else {
  cat("  (No Nginx imputations to check)\n\n")
}

cat("======================================\n")
cat("PART 3: EXTREME VALUE CHECK\n")
cat("Objective: Ensure no synthetic outliers created\n")
cat("======================================\n\n")

cat("Checking if imputed values fall within plausible ranges...\n\n")

# Check imputed values against observed min/max
variables_to_check <- c("Ransom_imputed", "DownTime_imputed", "Loss_imputed")
was_imputed_flags <- c("Ransom_was_imputed", "DownTime_was_imputed", "Loss_was_imputed")

extreme_check_summary <- data.frame(
  Variable = character(),
  Observed_Min = numeric(),
  Observed_Max = numeric(),
  Imputed_Min = numeric(),
  Imputed_Max = numeric(),
  Out_of_Range_Count = integer(),
  Assessment = character(),
  stringsAsFactors = FALSE
)

for (i in 1:length(variables_to_check)) {
  var <- variables_to_check[i]
  flag <- was_imputed_flags[i]

  # Get observed range
  observed_vals <- data_imputed[[var]][!data_imputed[[flag]] & !is.na(data_imputed[[var]])]
  obs_min <- min(observed_vals, na.rm = TRUE)
  obs_max <- max(observed_vals, na.rm = TRUE)

  # Get imputed range
  imputed_vals <- data_imputed[[var]][data_imputed[[flag]] & !is.na(data_imputed[[var]])]

  if (length(imputed_vals) > 0) {
    imp_min <- min(imputed_vals, na.rm = TRUE)
    imp_max <- max(imputed_vals, na.rm = TRUE)

    # Count out-of-range values
    out_of_range <- sum(imputed_vals < obs_min | imputed_vals > obs_max, na.rm = TRUE)

    # Assessment
    if (out_of_range == 0) {
      assessment <- "✓✓ EXCELLENT: All imputed values within observed range"
    } else if (out_of_range / length(imputed_vals) < 0.01) {
      assessment <- "✓ GOOD: <1% imputed values out of range (acceptable)"
    } else {
      assessment <- "⚠️  REVIEW: >1% imputed values out of observed range"
    }

    extreme_check_summary <- rbind(extreme_check_summary, data.frame(
      Variable = var,
      Observed_Min = round(obs_min, 2),
      Observed_Max = round(obs_max, 2),
      Imputed_Min = round(imp_min, 2),
      Imputed_Max = round(imp_max, 2),
      Out_of_Range_Count = out_of_range,
      Assessment = assessment
    ))
  }
}

cat("Extreme Value Check Results:\n")
cat("-----------------------------\n")
print(extreme_check_summary, row.names = FALSE)
cat("\n")

cat("Interpretation:\n")
cat("  PMM (Predictive Mean Matching) should ONLY pick observed donor values\n")
cat("  Therefore, imputed values MUST be within observed min/max range\n")
cat("  If out-of-range values exist, this indicates a data processing error\n\n")

cat("======================================\n")
cat("FINAL VALIDATION SUMMARY\n")
cat("======================================\n\n")

# Overall pass/fail assessment
validation_results <- list(
  distribution_quality = pass_count,
  extreme_values_pass = sum(extreme_check_summary$Out_of_Range_Count == 0),
  total_checks = 3 + nrow(extreme_check_summary)
)

overall_pass <- validation_results$distribution_quality + validation_results$extreme_values_pass
overall_total <- validation_results$total_checks

cat("Validation Results:\n")
cat("  Distribution Similarity Tests:", validation_results$distribution_quality, "/ 3 PASSED\n")
cat("  Extreme Value Checks:", validation_results$extreme_values_pass, "/",
    nrow(extreme_check_summary), "PASSED\n")
cat("  Overall Score:", overall_pass, "/", overall_total, "\n\n")

if (overall_pass == overall_total) {
  cat("✓✓✓ VALIDATION PASSED: Imputation quality is EXCELLENT\n")
  cat("     All imputed values match observed distributions\n")
  cat("     No synthetic outliers detected\n")
  cat("     Safe to proceed with analysis\n\n")
} else if (overall_pass >= overall_total * 0.8) {
  cat("✓ VALIDATION PASSED: Imputation quality is GOOD\n")
  cat("   Minor discrepancies detected but acceptable\n")
  cat("   Review flagged items before proceeding\n\n")
} else {
  cat("⚠️  VALIDATION WARNING: Review required\n")
  cat("   Significant discrepancies detected\n")
  cat("   Recommend reviewing imputation strategy\n\n")
}

# Save validation report
validation_report <- list(
  distribution_comparison = distribution_comparison,
  extreme_check = extreme_check_summary,
  overall_score = paste0(overall_pass, "/", overall_total),
  validation_date = Sys.time()
)

saveRDS(validation_report, "raw_data/processed_data/imputation_validation_report.rds")
cat("Validation report saved: raw_data/processed_data/imputation_validation_report.rds\n\n")

cat("======================================\n")
cat("FILES CREATED\n")
cat("======================================\n\n")

cat("Output files:\n")
cat("  1. docs/reports/imputation_validation_plots.pdf - Visual distribution comparisons\n")
cat("  2. raw_data/processed_data/imputation_validation_report.rds - Statistical validation summary\n\n")

cat("Validation complete!\n")
