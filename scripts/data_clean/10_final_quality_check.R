# Final Quality Check Script ("Last Mile" Verification)
# Purpose: Comprehensive pre-imputation validation
# Checks: 1) Information content, 2) Multivariate associations, 3) Final missing patterns
# Author: LiuWei TP085412
# Date: 2026-01-21

cat("======================================\n")
cat("FINAL QUALITY CHECK\n")
cat("Last Mile Verification Before Imputation\n")
cat("======================================\n\n")

# Load required libraries
required_packages <- c("dplyr", "tidyr", "ggplot2", "VIM", "naniar", "gridExtra")

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

# Load categorical cleaned data
cat("Loading categorical cleaned data...\n")
load("raw_data/processed_data/categorical_cleaned.RData")
cat("Data loaded successfully.\n")
cat("Total records:", nrow(data_categorical), "\n\n")

# Create working copy
data_final_check <- data_categorical

cat("======================================\n")
cat("CHECK 1: INFORMATION CONTENT ANALYSIS\n")
cat("Objective: Ensure dimension reduction didn't lose too much information\n")
cat("======================================\n\n")

cat("Critical Question:\n")
cat("  Did 'Other' categories capture too much data?\n")
cat("  Healthy threshold: 'Other' should be < 50% of total\n\n")

# Function to calculate 'Other' percentage
calculate_other_percentage <- function(column, column_name) {
  if (!is.factor(column)) {
    cat(paste0("Warning: ", column_name, " is not a factor\n"))
    return(NULL)
  }

  freq_table <- table(column, useNA = "ifany")
  total <- sum(freq_table)

  # Find 'Other' categories
  other_patterns <- c("Other", "Other_Groups", "Other_TLD", "Other_Encoding")
  other_idx <- names(freq_table) %in% other_patterns
  other_count <- sum(freq_table[other_idx])

  other_pct <- (other_count / total) * 100

  return(list(
    column_name = column_name,
    total_records = total,
    other_count = other_count,
    other_percentage = other_pct,
    top_category = names(sort(freq_table, decreasing = TRUE))[1],
    top_category_count = max(freq_table),
    top_category_pct = (max(freq_table) / total) * 100,
    n_categories = length(levels(column))
  ))
}

# Analyze cleaned categorical columns
cleaned_cols <- c("WebServer_clean", "Country_clean", "Notify_clean",
                  "URL_suffix", "Encoding_clean")

info_content_summary <- data.frame()

cat("Information Content by Variable:\n")
cat("----------------------------------\n\n")

for (col in cleaned_cols) {
  if (col %in% names(data_final_check)) {
    result <- calculate_other_percentage(data_final_check[[col]], col)

    if (!is.null(result)) {
      cat(paste0(">>> ", result$column_name, " <<<\n"))
      cat(paste0("  Total categories: ", result$n_categories, "\n"))
      cat(paste0("  Top category: ", result$top_category,
                 " (", round(result$top_category_pct, 2), "%)\n"))
      cat(paste0("  'Other' count: ", result$other_count,
                 " (", round(result$other_percentage, 2), "%)\n"))

      # Health assessment
      if (result$other_percentage > 60) {
        cat("  ❌ WARNING: 'Other' > 60% - Too much information loss!\n")
        cat("  → Recommendation: Increase number of kept categories\n")
      } else if (result$other_percentage > 40) {
        cat("  ⚠️  CAUTION: 'Other' 40-60% - Borderline acceptable\n")
        cat("  → Consider: Review if this variable is useful for imputation\n")
      } else if (result$other_percentage > 20) {
        cat("  ✓ OK: 'Other' 20-40% - Acceptable compression\n")
      } else {
        cat("  ✓✓ EXCELLENT: 'Other' < 20% - Good information retention\n")
      }
      cat("\n")

      # Add to summary
      info_content_summary <- rbind(info_content_summary, data.frame(
        Variable = result$column_name,
        N_Categories = result$n_categories,
        Other_Percentage = round(result$other_percentage, 2),
        Top_Category_Pct = round(result$top_category_pct, 2),
        Health_Status = ifelse(result$other_percentage > 60, "Critical",
                        ifelse(result$other_percentage > 40, "Warning",
                        ifelse(result$other_percentage > 20, "OK", "Excellent")))
      ))
    }
  }
}

cat("Summary Table:\n")
print(info_content_summary, row.names = FALSE)
cat("\n")

cat("======================================\n")
cat("CHECK 2: MULTIVARIATE ASSOCIATION ANALYSIS\n")
cat("Objective: Verify categorical predictors have relationships with targets\n")
cat("======================================\n\n")

cat("Critical Question:\n")
cat("  Do WebServer, Country, etc. actually correlate with Loss/Ransom?\n")
cat("  If boxplots all look the same → Variable is useless for imputation\n\n")

# Create PDF for association analysis
cat("Creating association visualization...\n")
pdf("docs/reports/final_association_check.pdf", width = 16, height = 12)

# Prepare data for plotting - use only complete cases for each analysis
cat("  Preparing data for association tests...\n")

# Test 1: Loss_log vs WebServer_clean
cat("  Test 1: Loss_log ~ WebServer_clean\n")

test1_data <- data_final_check %>%
  filter(!is.na(Loss_log) & !is.na(WebServer_clean)) %>%
  group_by(WebServer_clean) %>%
  filter(n() >= 30) %>%  # Only keep categories with sufficient data
  ungroup()

if (nrow(test1_data) > 0) {
  p1 <- ggplot(test1_data, aes(x = reorder(WebServer_clean, Loss_log, median),
                                y = Loss_log)) +
    geom_boxplot(fill = "steelblue", alpha = 0.7) +
    coord_flip() +
    theme_minimal() +
    labs(title = "Loss (log scale) by WebServer",
         subtitle = "Do different servers show different loss patterns?",
         x = "WebServer", y = "Loss (log10)")

  # Calculate ANOVA to test if groups are different
  anova_test1 <- aov(Loss_log ~ WebServer_clean, data = test1_data)
  p_value1 <- summary(anova_test1)[[1]]["WebServer_clean", "Pr(>F)"]

  cat(paste0("    ANOVA p-value: ", format.pval(p_value1, digits = 3), "\n"))

  if (p_value1 < 0.001) {
    cat("    ✓✓ EXCELLENT: Highly significant association\n")
  } else if (p_value1 < 0.05) {
    cat("    ✓ OK: Significant association\n")
  } else {
    cat("    ❌ WARNING: No significant association\n")
  }
}

# Test 2: Loss_log vs Country_clean
cat("  Test 2: Loss_log ~ Country_clean\n")

test2_data <- data_final_check %>%
  filter(!is.na(Loss_log) & !is.na(Country_clean)) %>%
  group_by(Country_clean) %>%
  filter(n() >= 30) %>%
  ungroup()

if (nrow(test2_data) > 0) {
  # Plot top 15 countries only for readability
  top15_countries <- test2_data %>%
    group_by(Country_clean) %>%
    summarise(median_loss = median(Loss_log)) %>%
    arrange(desc(median_loss)) %>%
    head(15) %>%
    pull(Country_clean)

  p2_data <- test2_data %>% filter(Country_clean %in% top15_countries)

  p2 <- ggplot(p2_data, aes(x = reorder(Country_clean, Loss_log, median),
                             y = Loss_log)) +
    geom_boxplot(fill = "coral", alpha = 0.7) +
    coord_flip() +
    theme_minimal() +
    labs(title = "Loss (log scale) by Country (Top 15)",
         subtitle = "Do different countries show different loss patterns?",
         x = "Country", y = "Loss (log10)")

  anova_test2 <- aov(Loss_log ~ Country_clean, data = test2_data)
  p_value2 <- summary(anova_test2)[[1]]["Country_clean", "Pr(>F)"]

  cat(paste0("    ANOVA p-value: ", format.pval(p_value2, digits = 3), "\n"))

  if (p_value2 < 0.001) {
    cat("    ✓✓ EXCELLENT: Highly significant association\n")
  } else if (p_value2 < 0.05) {
    cat("    ✓ OK: Significant association\n")
  } else {
    cat("    ❌ WARNING: No significant association\n")
  }
}

# Test 3: Ransom_log vs Notify_clean
cat("  Test 3: Ransom_log ~ Notify_clean (Top 15 groups)\n")

test3_data <- data_final_check %>%
  filter(!is.na(Ransom_log) & !is.na(Notify_clean)) %>%
  group_by(Notify_clean) %>%
  filter(n() >= 30) %>%
  ungroup()

if (nrow(test3_data) > 0) {
  top15_groups <- test3_data %>%
    group_by(Notify_clean) %>%
    summarise(median_ransom = median(Ransom_log)) %>%
    arrange(desc(median_ransom)) %>%
    head(15) %>%
    pull(Notify_clean)

  p3_data <- test3_data %>% filter(Notify_clean %in% top15_groups)

  p3 <- ggplot(p3_data, aes(x = reorder(Notify_clean, Ransom_log, median),
                             y = Ransom_log)) +
    geom_boxplot(fill = "lightgreen", alpha = 0.7) +
    coord_flip() +
    theme_minimal() +
    labs(title = "Ransom (log scale) by Hacker Group (Top 15)",
         subtitle = "Do different groups demand different ransom amounts?",
         x = "Hacker Group", y = "Ransom (log10)")

  anova_test3 <- aov(Ransom_log ~ Notify_clean, data = test3_data)
  p_value3 <- summary(anova_test3)[[1]]["Notify_clean", "Pr(>F)"]

  cat(paste0("    ANOVA p-value: ", format.pval(p_value3, digits = 3), "\n"))

  if (p_value3 < 0.001) {
    cat("    ✓✓ EXCELLENT: Highly significant association\n")
  } else if (p_value3 < 0.05) {
    cat("    ✓ OK: Significant association\n")
  } else {
    cat("    ❌ WARNING: No significant association\n")
  }
}

# Test 4: DownTime_log vs URL_suffix
cat("  Test 4: DownTime_log ~ URL_suffix\n")

test4_data <- data_final_check %>%
  filter(!is.na(DownTime_log) & !is.na(URL_suffix)) %>%
  group_by(URL_suffix) %>%
  filter(n() >= 30) %>%
  ungroup()

if (nrow(test4_data) > 0) {
  p4 <- ggplot(test4_data, aes(x = reorder(URL_suffix, DownTime_log, median),
                                y = DownTime_log)) +
    geom_boxplot(fill = "gold", alpha = 0.7) +
    coord_flip() +
    theme_minimal() +
    labs(title = "DownTime (log scale) by TLD",
         subtitle = "Do .gov/.com/etc show different downtime patterns?",
         x = "Top Level Domain", y = "DownTime (log10)")

  anova_test4 <- aov(DownTime_log ~ URL_suffix, data = test4_data)
  p_value4 <- summary(anova_test4)[[1]]["URL_suffix", "Pr(>F)"]

  cat(paste0("    ANOVA p-value: ", format.pval(p_value4, digits = 3), "\n"))

  if (p_value4 < 0.001) {
    cat("    ✓✓ EXCELLENT: Highly significant association\n")
  } else if (p_value4 < 0.05) {
    cat("    ✓ OK: Significant association\n")
  } else {
    cat("    ❌ WARNING: No significant association\n")
  }
}

# Print all plots to PDF
if (exists("p1")) print(p1)
if (exists("p2")) print(p2)
if (exists("p3")) print(p3)
if (exists("p4")) print(p4)

dev.off()

cat("\nAssociation analysis plots saved to:\n")
cat("  docs/reports/final_association_check.pdf\n\n")

cat("======================================\n")
cat("CHECK 3: FINAL MISSING PATTERN ANALYSIS\n")
cat("Objective: Understand complete missing structure after all cleaning\n")
cat("======================================\n\n")

cat("Critical Questions:\n")
cat("  1. When Ransom_log is missing, is Loss_log also usually missing?\n")
cat("  2. In modern data (2016+), are missingness patterns more favorable?\n\n")

# Select key variables for missing pattern analysis
key_vars <- c("Ransom_log", "DownTime_log", "Loss_log",
              "Country_clean", "WebServer_clean", "Encoding_clean",
              "Year", "is_modern")

# Overall missing pattern
cat("Overall Missing Data Summary:\n")
cat("------------------------------\n")

missing_summary <- data_final_check %>%
  summarise(across(all_of(key_vars), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  mutate(Missing_Pct = round(Missing_Count / nrow(data_final_check) * 100, 2)) %>%
  arrange(desc(Missing_Pct))

print(missing_summary, n = Inf)
cat("\n")

# Missing pattern co-occurrence
cat("Missing Pattern Co-occurrence:\n")
cat("-------------------------------\n")

cooccurrence <- data.frame(
  Pattern = character(),
  Count = integer(),
  Percentage = numeric()
)

# Pattern 1: All numeric variables missing
all_missing <- sum(is.na(data_final_check$Ransom_log) &
                   is.na(data_final_check$DownTime_log) &
                   is.na(data_final_check$Loss_log))

cooccurrence <- rbind(cooccurrence, data.frame(
  Pattern = "All 3 numeric variables NA",
  Count = all_missing,
  Percentage = round(all_missing / nrow(data_final_check) * 100, 2)
))

# Pattern 2: Ransom missing but others present
ransom_only <- sum(is.na(data_final_check$Ransom_log) &
                   !is.na(data_final_check$DownTime_log) &
                   !is.na(data_final_check$Loss_log))

cooccurrence <- rbind(cooccurrence, data.frame(
  Pattern = "Only Ransom_log NA",
  Count = ransom_only,
  Percentage = round(ransom_only / nrow(data_final_check) * 100, 2)
))

# Pattern 3: All complete
all_complete <- sum(!is.na(data_final_check$Ransom_log) &
                    !is.na(data_final_check$DownTime_log) &
                    !is.na(data_final_check$Loss_log))

cooccurrence <- rbind(cooccurrence, data.frame(
  Pattern = "All 3 numeric variables present",
  Count = all_complete,
  Percentage = round(all_complete / nrow(data_final_check) * 100, 2)
))

print(cooccurrence, row.names = FALSE)
cat("\n")

# Modern vs Legacy comparison
cat("Missing Rate: Modern (2016+) vs Legacy (<2016):\n")
cat("------------------------------------------------\n")

modern_legacy_comparison <- data_final_check %>%
  group_by(is_modern) %>%
  summarise(
    N = n(),
    Ransom_NA_pct = round(sum(is.na(Ransom_log)) / n() * 100, 2),
    DownTime_NA_pct = round(sum(is.na(DownTime_log)) / n() * 100, 2),
    Loss_NA_pct = round(sum(is.na(Loss_log)) / n() * 100, 2),
    Complete_cases_pct = round(sum(!is.na(Ransom_log) &
                                    !is.na(DownTime_log) &
                                    !is.na(Loss_log)) / n() * 100, 2)
  )

print(modern_legacy_comparison)
cat("\n")

# Create final missing pattern visualization
cat("Creating final missing pattern visualization...\n")

pdf("docs/reports/final_missing_patterns.pdf", width = 14, height = 10)

# Page 1: Missing pattern matrix
aggr_plot <- aggr(data_final_check[, key_vars],
                   col = c('navyblue', 'red'),
                   numbers = TRUE,
                   sortVars = TRUE,
                   labels = key_vars,
                   cex.axis = 0.7,
                   gap = 3,
                   ylab = c("Missing Data Pattern", "Combination Pattern"))

# Page 2: Missing by era
missing_by_era <- data_final_check %>%
  group_by(is_modern) %>%
  summarise(
    Ransom_NA = sum(is.na(Ransom_log)) / n() * 100,
    DownTime_NA = sum(is.na(DownTime_log)) / n() * 100,
    Loss_NA = sum(is.na(Loss_log)) / n() * 100
  ) %>%
  pivot_longer(cols = c(Ransom_NA, DownTime_NA, Loss_NA),
               names_to = "Variable",
               values_to = "Missing_Pct") %>%
  mutate(Era = ifelse(is_modern, "Modern (2016+)", "Legacy (<2016)"))

p_era <- ggplot(missing_by_era, aes(x = Variable, y = Missing_Pct, fill = Era)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Missing Rate Comparison: Modern vs Legacy Data",
       x = "Variable", y = "Missing Percentage (%)") +
  scale_fill_manual(values = c("Modern (2016+)" = "steelblue",
                                "Legacy (<2016)" = "coral"))

print(p_era)

dev.off()

cat("Missing pattern visualizations saved to:\n")
cat("  docs/reports/final_missing_patterns.pdf\n\n")

cat("======================================\n")
cat("FINAL RECOMMENDATIONS\n")
cat("======================================\n\n")

cat("Based on the three checks above:\n\n")

cat("1. INFORMATION CONTENT:\n")
critical_vars <- info_content_summary %>% filter(Health_Status == "Critical")
if (nrow(critical_vars) > 0) {
  cat("   ❌ CRITICAL ISSUES found in:\n")
  for (v in critical_vars$Variable) {
    cat(paste0("      - ", v, "\n"))
  }
  cat("   → Action: Consider increasing number of kept categories\n\n")
} else {
  cat("   ✓ All categorical variables have acceptable information retention\n\n")
}

cat("2. MULTIVARIATE ASSOCIATIONS:\n")
cat("   Review ANOVA p-values above.\n")
cat("   → If p < 0.001: Variable is highly useful for imputation\n")
cat("   → If p > 0.05: Consider excluding from imputation model\n\n")

cat("3. MISSING PATTERNS:\n")
modern_complete_rate <- modern_legacy_comparison %>%
  filter(is_modern == TRUE) %>%
  pull(Complete_cases_pct)

if (!is.na(modern_complete_rate) && modern_complete_rate > 85) {
  cat("   ✓✓ Modern data has excellent completeness (>85%)\n")
  cat("   → Recommendation: Impute modern and legacy data SEPARATELY\n\n")
} else if (!is.na(modern_complete_rate) && modern_complete_rate > 70) {
  cat("   ✓ Modern data has good completeness (70-85%)\n")
  cat("   → Recommendation: Impute modern data first, evaluate legacy\n\n")
} else {
  cat("   ⚠️  Modern data still has significant missingness\n")
  cat("   → Recommendation: Use advanced methods (MICE with m=10)\n\n")
}

cat("======================================\n")
cat("READY FOR IMPUTATION?\n")
cat("======================================\n\n")

# Calculate readiness score
readiness_score <- 0

# Check 1: Information content (max 40 points)
if (nrow(critical_vars) == 0) {
  readiness_score <- readiness_score + 40
} else {
  readiness_score <- readiness_score + max(0, 40 - nrow(critical_vars) * 10)
}

# Check 2: Modern data quality (max 40 points)
if (!is.na(modern_complete_rate)) {
  if (modern_complete_rate > 85) {
    readiness_score <- readiness_score + 40
  } else if (modern_complete_rate > 70) {
    readiness_score <- readiness_score + 30
  } else {
    readiness_score <- readiness_score + 20
  }
}

# Check 3: Have association tests (max 20 points)
if (exists("p_value1") || exists("p_value2")) {
  readiness_score <- readiness_score + 20
}

cat(paste0("Readiness Score: ", readiness_score, "/100\n\n"))

if (readiness_score >= 80) {
  cat("✓✓ EXCELLENT: Data is ready for imputation!\n")
  cat("   Proceed with confidence.\n\n")
} else if (readiness_score >= 60) {
  cat("✓ GOOD: Data is ready for imputation with minor caveats.\n")
  cat("   Review warnings above and proceed.\n\n")
} else {
  cat("⚠️  CAUTION: Data has quality issues.\n")
  cat("   Address critical warnings before imputation.\n\n")
}

cat("Next step: Run imputation script (10_missing_value_imputation.R)\n\n")

cat("Final quality check complete!\n")
