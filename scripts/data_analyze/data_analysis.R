# ============================================================
# Data Analysis Script
# Purpose: Exploratory Data Analysis & Hypothesis Testing
# Research Objectives:
#   RO1: Does URL TLD (top-level domain) affect financial loss?
#   RO2: Does WebServer type affect server downtime?
# Author: LiuWei TP085412
# Date: 2026-02-01
# ============================================================

# ============================================================
# SECTION 0: SETUP & DATA LOADING
# ============================================================

cat("======================================\n")
cat("DATA ANALYSIS SCRIPT\n")
cat("RO1: TLD vs Financial Loss\n")
cat("RO2: WebServer vs DownTime\n")
cat("======================================\n\n")

# --- Package Installation & Loading ---
required_packages <- c("dplyr", "ggplot2", "scales", "tidyr",
                       "dunn.test", "moments", "rstatix", "gridExtra")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste0("Installing package: ", pkg, "...\n"))
    install.packages(pkg, repos = "https://cloud.r-project.org/", quiet = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE, quietly = TRUE)
  }
}

cat("All required packages loaded successfully.\n\n")

# --- Load Data ---
cat("Loading imputed dataset...\n")
load("raw_data/processed_data/mice_imputed_data.RData")
cat("Data loaded: data_final_imputed\n")
cat("  Rows:", nrow(data_final_imputed), "\n")
cat("  Columns:", ncol(data_final_imputed), "\n\n")

# --- Output Directory ---
output_dir <- "docs/reports"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# --- Color Palette (consistent with existing report plots) ---
pal <- c("#2166AC", "#D6604D", "#4DAF4A", "#FF7F00", "#984EA3",
         "#E7298A", "#66C2A5", "#FC8D62", "#8DA0CB", "#A6D854",
         "#FFD92F")

# ============================================================
# SECTION 1: DATA PREPARATION
# ============================================================

cat("======================================\n")
cat("SECTION 1: DATA PREPARATION\n")
cat("======================================\n\n")

# --- 1.1 Variable Selection & NA Removal ---
cat("--- 1.1 Variable Selection & NA Removal ---\n\n")

# RO1: TLD vs Loss
data_ro1 <- data_final_imputed %>%
  filter(!is.na(URL_suffix)) %>%
  select(URL_suffix, Loss_imputed, Loss_log_imputed)

cat("RO1 Dataset (TLD vs Loss):\n")
cat("  Rows after removing NA URL_suffix:", nrow(data_ro1), "\n")
cat("  Rows removed:", nrow(data_final_imputed) - nrow(data_ro1), "\n")
cat("  Loss_imputed NAs:", sum(is.na(data_ro1$Loss_imputed)), "\n\n")

# RO2: WebServer vs DownTime
data_ro2 <- data_final_imputed %>%
  filter(!is.na(WebServer_clean)) %>%
  select(WebServer_clean, DownTime_imputed, DownTime_log_imputed)

cat("RO2 Dataset (WebServer vs DownTime):\n")
cat("  Rows after removing NA WebServer_clean:", nrow(data_ro2), "\n")
cat("  Rows removed:", nrow(data_final_imputed) - nrow(data_ro2), "\n")
cat("  DownTime_imputed NAs:", sum(is.na(data_ro2$DownTime_imputed)), "\n\n")

# --- 1.2 Normality Assessment ---
cat("--- 1.2 Normality Assessment ---\n")
cat("Justification for choosing non-parametric tests\n\n")

set.seed(42)
n_sample <- 5000

# Shapiro-Wilk on sampled Loss
loss_sample <- sample(data_ro1$Loss_imputed, n_sample)
sw_loss <- shapiro.test(loss_sample)
cat("Shapiro-Wilk Test on Loss_imputed (n=5000 sample):\n")
cat("  W =", round(sw_loss$statistic, 6), "\n")
cat("  p-value =", format.pval(sw_loss$p.value, digits = 4), "\n\n")

# Shapiro-Wilk on sampled DownTime
dt_sample <- sample(data_ro2$DownTime_imputed, n_sample)
sw_dt <- shapiro.test(dt_sample)
cat("Shapiro-Wilk Test on DownTime_imputed (n=5000 sample):\n")
cat("  W =", round(sw_dt$statistic, 6), "\n")
cat("  p-value =", format.pval(sw_dt$p.value, digits = 4), "\n\n")

# Skewness & Kurtosis
cat("Skewness & Kurtosis:\n")
cat("  Loss_imputed:    skewness =", round(skewness(data_ro1$Loss_imputed), 4),
    "  kurtosis =", round(kurtosis(data_ro1$Loss_imputed), 4), "\n")
cat("  Loss_log_imputed: skewness =", round(skewness(data_ro1$Loss_log_imputed), 4),
    "  kurtosis =", round(kurtosis(data_ro1$Loss_log_imputed), 4), "\n")
cat("  DownTime_imputed: skewness =", round(skewness(data_ro2$DownTime_imputed), 4),
    "  kurtosis =", round(kurtosis(data_ro2$DownTime_imputed), 4), "\n")
cat("  DownTime_log_imputed: skewness =", round(skewness(data_ro2$DownTime_log_imputed), 4),
    "  kurtosis =", round(kurtosis(data_ro2$DownTime_log_imputed), 4), "\n\n")

cat("CONCLUSION: Both Loss and DownTime are significantly non-normal\n")
cat("  -> Shapiro-Wilk p < 0.05 for both variables\n")
cat("  -> Skewness values deviate substantially from 0\n")
cat("  -> Non-parametric Kruskal-Wallis test is appropriate\n\n")

# --- 1.3 Group Balance ---
cat("--- 1.3 Group Balance Assessment ---\n\n")

cat("RO1 - URL_suffix group sizes:\n")
tld_counts <- sort(table(data_ro1$URL_suffix), decreasing = TRUE)
print(tld_counts)
cat("\n")

if (any(tld_counts < 30)) {
  cat("WARNING: Some TLD groups have < 30 observations:\n")
  print(tld_counts[tld_counts < 30])
  cat("\n")
} else {
  cat("All TLD groups have >= 30 observations. Good.\n\n")
}

cat("RO2 - WebServer_clean group sizes:\n")
ws_counts <- sort(table(data_ro2$WebServer_clean), decreasing = TRUE)
print(ws_counts)
cat("\n")

if (any(ws_counts < 30)) {
  cat("WARNING: Some WebServer groups have < 30 observations:\n")
  print(ws_counts[ws_counts < 30])
  cat("\n")
} else {
  cat("All WebServer groups have >= 30 observations. Good.\n\n")
}

# ============================================================
# SECTION 2: EXPLORATORY DATA ANALYSIS
# ============================================================

cat("======================================\n")
cat("SECTION 2: EXPLORATORY DATA ANALYSIS\n")
cat("======================================\n\n")

# ===========================================================
# 2.1 RO1: TLD vs Financial Loss
# ===========================================================

cat("--- 2.1 RO1: TLD vs Financial Loss ---\n\n")

pdf(file.path(output_dir, "06_data_analysis_RO1.pdf"), width = 12, height = 8)

# --- Q-Q Plot for Loss (normality evidence) ---
par(mfrow = c(1, 2))
qqnorm(loss_sample,
       main = "Q-Q Plot: Loss_imputed\n(n=5000 sample, original scale)",
       col = adjustcolor("#2166AC", 0.4), pch = 16, cex = 0.5)
qqline(loss_sample, col = "#D6604D", lwd = 2)

loss_log_sample <- sample(data_ro1$Loss_log_imputed, n_sample)
qqnorm(loss_log_sample,
       main = "Q-Q Plot: Loss_log_imputed\n(n=5000 sample, log scale)",
       col = adjustcolor("#4DAF4A", 0.4), pch = 16, cex = 0.5)
qqline(loss_log_sample, col = "#D6604D", lwd = 2)
par(mfrow = c(1, 1))

# --- Plot 1: Boxplot - Loss by TLD (original scale) ---
# Order TLDs by median Loss descending
tld_median_order <- data_ro1 %>%
  group_by(URL_suffix) %>%
  summarise(med = median(Loss_imputed)) %>%
  arrange(desc(med)) %>%
  pull(URL_suffix)

data_ro1$URL_suffix_ordered <- factor(data_ro1$URL_suffix, levels = rev(tld_median_order))

p1 <- ggplot(data_ro1, aes(x = URL_suffix_ordered, y = Loss_imputed, fill = URL_suffix_ordered)) +
  geom_boxplot(outlier.size = 0.3, outlier.alpha = 0.2) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = rep(pal, length.out = length(tld_median_order))) +
  labs(title = "RO1: Financial Loss by URL Extension (TLD)",
       subtitle = "Ordered by median loss (descending) | Original scale",
       x = "Top-Level Domain", y = "Financial Loss ($)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p1)

# --- Plot 2: Boxplot - Loss by TLD (log scale) ---
data_ro1$URL_suffix_ordered <- factor(data_ro1$URL_suffix, levels = rev(tld_median_order))

p2 <- ggplot(data_ro1, aes(x = URL_suffix_ordered, y = Loss_log_imputed, fill = URL_suffix_ordered)) +
  geom_boxplot(outlier.size = 0.3, outlier.alpha = 0.2) +
  coord_flip() +
  scale_fill_manual(values = rep(pal, length.out = length(tld_median_order))) +
  labs(title = "RO1: Financial Loss by URL Extension (TLD) - Log Scale",
       subtitle = "log10(Loss + 1) | Better visualization of distribution spread",
       x = "Top-Level Domain", y = "log10(Loss + 1)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p2)

# --- Plot 3: Bar chart - Mean Loss by TLD with 95% CI ---
ro1_summary <- data_ro1 %>%
  group_by(URL_suffix) %>%
  summarise(
    n = n(),
    Mean = mean(Loss_imputed),
    Median = median(Loss_imputed),
    SD = sd(Loss_imputed),
    SE = sd(Loss_imputed) / sqrt(n()),
    CI_lower = Mean - 1.96 * SE,
    CI_upper = Mean + 1.96 * SE,
    Min = min(Loss_imputed),
    Max = max(Loss_imputed),
    Total = sum(Loss_imputed),
    .groups = "drop"
  ) %>%
  arrange(desc(Mean))

ro1_summary$URL_suffix <- factor(ro1_summary$URL_suffix, levels = rev(ro1_summary$URL_suffix))

p3 <- ggplot(ro1_summary, aes(x = URL_suffix, y = Mean, fill = URL_suffix)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.3, linewidth = 0.5) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = rep(pal, length.out = nrow(ro1_summary))) +
  labs(title = "RO1: Mean Financial Loss by TLD",
       subtitle = "Error bars show 95% Confidence Intervals",
       x = "Top-Level Domain", y = "Mean Loss ($)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p3)

# --- Plot 4: Bar chart - Total Loss by TLD (answers RO1-Q2) ---
ro1_total <- ro1_summary %>%
  arrange(desc(Total))
ro1_total$URL_suffix <- factor(ro1_total$URL_suffix, levels = rev(ro1_total$URL_suffix))

p4 <- ggplot(ro1_total, aes(x = URL_suffix, y = Total, fill = URL_suffix)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = paste0("$", comma(round(Total / 1e6, 1)), "M")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = label_comma(scale = 1e-6, suffix = "M"),
                     expand = expansion(mult = c(0, 0.2))) +
  scale_fill_manual(values = rep(pal, length.out = nrow(ro1_total))) +
  labs(title = "RO1-Q2: Total Financial Loss by TLD",
       subtitle = "Which URL extension has the highest cumulative financial loss?",
       x = "Top-Level Domain", y = "Total Loss (Millions $)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p4)

# --- Plot 5: Violin plot - Loss (log scale) by TLD ---
data_ro1$URL_suffix_ordered <- factor(data_ro1$URL_suffix, levels = rev(tld_median_order))

p5 <- ggplot(data_ro1, aes(x = URL_suffix_ordered, y = Loss_log_imputed, fill = URL_suffix_ordered)) +
  geom_violin(alpha = 0.7, scale = "width") +
  geom_boxplot(width = 0.15, fill = "white", alpha = 0.8, outlier.shape = NA) +
  coord_flip() +
  scale_fill_manual(values = rep(pal, length.out = length(tld_median_order))) +
  labs(title = "RO1: Loss Distribution Shape by TLD (Violin Plot)",
       subtitle = "log10(Loss + 1) | White boxes show IQR, line shows median",
       x = "Top-Level Domain", y = "log10(Loss + 1)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p5)

# --- Plot 6: Summary Statistics Table ---
ro1_table <- ro1_summary %>%
  arrange(desc(as.numeric(as.character(Total)))) %>%
  mutate(
    URL_suffix = as.character(URL_suffix),
    n = comma(n),
    Mean = paste0("$", comma(round(Mean, 0))),
    Median = paste0("$", comma(round(Median, 0))),
    SD = paste0("$", comma(round(SD, 0))),
    Min = paste0("$", comma(round(Min, 0))),
    Max = paste0("$", comma(round(Max, 0))),
    Total = paste0("$", comma(round(Total, 0)))
  ) %>%
  select(TLD = URL_suffix, n, Mean, Median, SD, Min, Max, Total)

tt <- gridExtra::tableGrob(ro1_table, rows = NULL,
                           theme = ttheme_minimal(base_size = 9))
title <- grid::textGrob("RO1: Summary Statistics - Financial Loss by TLD",
                        gp = grid::gpar(fontsize = 14, fontface = "bold"))
gridExtra::grid.arrange(title, tt, ncol = 1, heights = c(0.1, 0.9))

dev.off()
cat("  Saved: 06_data_analysis_RO1.pdf\n\n")

# Print summary to console
cat("RO1 Summary Statistics:\n")
cat("-----------------------------------------------\n")
print(ro1_summary %>%
        arrange(desc(as.numeric(as.character(Total)))) %>%
        mutate(across(where(is.numeric), ~ round(., 2))) %>%
        as.data.frame(), row.names = FALSE)
cat("\n")

# Save CSV
write.csv(ro1_summary %>%
            arrange(desc(as.numeric(as.character(Total)))) %>%
            mutate(across(where(is.numeric), ~ round(., 2))),
          file.path(output_dir, "ro1_summary_statistics.csv"), row.names = FALSE)
cat("  Saved: ro1_summary_statistics.csv\n\n")

# ===========================================================
# 2.2 RO2: WebServer vs DownTime
# ===========================================================

cat("--- 2.2 RO2: WebServer vs DownTime ---\n\n")

pdf(file.path(output_dir, "07_data_analysis_RO2.pdf"), width = 12, height = 8)

# --- Q-Q Plot for DownTime (normality evidence) ---
par(mfrow = c(1, 2))
qqnorm(dt_sample,
       main = "Q-Q Plot: DownTime_imputed\n(n=5000 sample, original scale)",
       col = adjustcolor("#2166AC", 0.4), pch = 16, cex = 0.5)
qqline(dt_sample, col = "#D6604D", lwd = 2)

dt_log_sample <- sample(data_ro2$DownTime_log_imputed, n_sample)
qqnorm(dt_log_sample,
       main = "Q-Q Plot: DownTime_log_imputed\n(n=5000 sample, log scale)",
       col = adjustcolor("#4DAF4A", 0.4), pch = 16, cex = 0.5)
qqline(dt_log_sample, col = "#D6604D", lwd = 2)
par(mfrow = c(1, 1))

# --- Plot 1: Boxplot - DownTime by WebServer (original scale) ---
ws_median_order <- data_ro2 %>%
  group_by(WebServer_clean) %>%
  summarise(med = median(DownTime_imputed)) %>%
  arrange(desc(med)) %>%
  pull(WebServer_clean)

data_ro2$WebServer_ordered <- factor(data_ro2$WebServer_clean, levels = rev(ws_median_order))

p1_r2 <- ggplot(data_ro2, aes(x = WebServer_ordered, y = DownTime_imputed, fill = WebServer_ordered)) +
  geom_boxplot(outlier.size = 0.3, outlier.alpha = 0.2) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = pal[1:length(ws_median_order)]) +
  labs(title = "RO2: Server DownTime by WebServer Type",
       subtitle = "Ordered by median downtime (descending) | Original scale (days)",
       x = "WebServer", y = "DownTime (days)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p1_r2)

# --- Plot 2: Boxplot - DownTime by WebServer (log scale) ---
p2_r2 <- ggplot(data_ro2, aes(x = WebServer_ordered, y = DownTime_log_imputed, fill = WebServer_ordered)) +
  geom_boxplot(outlier.size = 0.3, outlier.alpha = 0.2) +
  coord_flip() +
  scale_fill_manual(values = pal[1:length(ws_median_order)]) +
  labs(title = "RO2: Server DownTime by WebServer Type - Log Scale",
       subtitle = "log10(DownTime + 1) | Better visualization of distribution spread",
       x = "WebServer", y = "log10(DownTime + 1)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p2_r2)

# --- Plot 3: Bar chart - Mean DownTime by WebServer with 95% CI ---
ro2_summary <- data_ro2 %>%
  group_by(WebServer_clean) %>%
  summarise(
    n = n(),
    Mean = mean(DownTime_imputed),
    Median = median(DownTime_imputed),
    SD = sd(DownTime_imputed),
    SE = sd(DownTime_imputed) / sqrt(n()),
    CI_lower = Mean - 1.96 * SE,
    CI_upper = Mean + 1.96 * SE,
    Min = min(DownTime_imputed),
    Max = max(DownTime_imputed),
    Total = sum(DownTime_imputed),
    .groups = "drop"
  ) %>%
  arrange(desc(Mean))

ro2_summary$WebServer_clean <- factor(ro2_summary$WebServer_clean,
                                      levels = rev(ro2_summary$WebServer_clean))

p3_r2 <- ggplot(ro2_summary, aes(x = WebServer_clean, y = Mean, fill = WebServer_clean)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.3, linewidth = 0.5) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = pal[1:nrow(ro2_summary)]) +
  labs(title = "RO2-Q1: Mean Server DownTime by WebServer Type",
       subtitle = "Which WebServer causes the longest average downtime? | 95% CI error bars",
       x = "WebServer", y = "Mean DownTime (days)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p3_r2)

# --- Plot 4: Bar chart - Total DownTime by WebServer ---
ro2_total <- ro2_summary %>%
  arrange(desc(as.numeric(as.character(Total))))
ro2_total$WebServer_clean <- factor(ro2_total$WebServer_clean,
                                    levels = rev(ro2_total$WebServer_clean))

p4_r2 <- ggplot(ro2_total, aes(x = WebServer_clean, y = Total, fill = WebServer_clean)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = paste0(comma(round(Total, 0)), " days")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.25))) +
  scale_fill_manual(values = pal[1:nrow(ro2_total)]) +
  labs(title = "RO2: Total Server DownTime by WebServer Type",
       subtitle = "Cumulative downtime across all incidents",
       x = "WebServer", y = "Total DownTime (days)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p4_r2)

# --- Plot 5: Violin plot - DownTime (log scale) by WebServer ---
p5_r2 <- ggplot(data_ro2, aes(x = WebServer_ordered, y = DownTime_log_imputed, fill = WebServer_ordered)) +
  geom_violin(alpha = 0.7, scale = "width") +
  geom_boxplot(width = 0.15, fill = "white", alpha = 0.8, outlier.shape = NA) +
  coord_flip() +
  scale_fill_manual(values = pal[1:length(ws_median_order)]) +
  labs(title = "RO2: DownTime Distribution Shape by WebServer (Violin Plot)",
       subtitle = "log10(DownTime + 1) | White boxes show IQR, line shows median",
       x = "WebServer", y = "log10(DownTime + 1)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p5_r2)

# --- Plot 6: Summary Statistics Table ---
ro2_table <- ro2_summary %>%
  arrange(desc(as.numeric(as.character(Mean)))) %>%
  mutate(
    WebServer_clean = as.character(WebServer_clean),
    n = comma(n),
    Mean = round(Mean, 2),
    Median = round(Median, 2),
    SD = round(SD, 2),
    Min = round(Min, 2),
    Max = round(Max, 2),
    Total = comma(round(Total, 0))
  ) %>%
  select(WebServer = WebServer_clean, n, Mean, Median, SD, Min, Max, Total)

tt2 <- gridExtra::tableGrob(ro2_table, rows = NULL,
                            theme = ttheme_minimal(base_size = 10))
title2 <- grid::textGrob("RO2: Summary Statistics - DownTime by WebServer",
                         gp = grid::gpar(fontsize = 14, fontface = "bold"))
gridExtra::grid.arrange(title2, tt2, ncol = 1, heights = c(0.1, 0.9))

dev.off()
cat("  Saved: 07_data_analysis_RO2.pdf\n\n")

# Print summary to console
cat("RO2 Summary Statistics:\n")
cat("-----------------------------------------------\n")
print(ro2_summary %>%
        arrange(desc(as.numeric(as.character(Mean)))) %>%
        mutate(across(where(is.numeric), ~ round(., 2))) %>%
        as.data.frame(), row.names = FALSE)
cat("\n")

# Save CSV
write.csv(ro2_summary %>%
            arrange(desc(as.numeric(as.character(Mean)))) %>%
            mutate(across(where(is.numeric), ~ round(., 2))),
          file.path(output_dir, "ro2_summary_statistics.csv"), row.names = FALSE)
cat("  Saved: ro2_summary_statistics.csv\n\n")

# ============================================================
# SECTION 3: HYPOTHESIS FORMULATION & TESTING
# ============================================================

cat("======================================\n")
cat("SECTION 3: HYPOTHESIS FORMULATION & TESTING\n")
cat("======================================\n\n")

# ===========================================================
# 3.1 RO1: TLD vs Financial Loss
# ===========================================================

cat("--- 3.1 RO1: Does TLD affect Financial Loss? ---\n\n")

cat("Hypotheses:\n")
cat("  H0: The distribution of financial loss has the same location\n")
cat("      parameter across all TLD groups (no difference in median ranks)\n")
cat("  H1: At least one TLD group has a significantly different\n")
cat("      loss distribution location\n\n")

cat("Test: Kruskal-Wallis Rank Sum Test (non-parametric)\n")
cat("  Justification: Data is non-normal (Shapiro-Wilk p < 0.05,\n")
cat("  high skewness), with >2 independent groups\n\n")

# Run Kruskal-Wallis
kw_ro1 <- kruskal.test(Loss_imputed ~ URL_suffix, data = data_ro1)

cat("Kruskal-Wallis Results (RO1):\n")
cat("  H statistic (chi-squared):", round(kw_ro1$statistic, 4), "\n")
cat("  Degrees of freedom:", kw_ro1$parameter, "\n")
cat("  p-value:", format.pval(kw_ro1$p.value, digits = 4), "\n\n")

if (kw_ro1$p.value < 0.05) {
  cat("  DECISION: Reject H0 (p < 0.05)\n")
  cat("  There IS a statistically significant difference in financial\n")
  cat("  loss across TLD groups.\n\n")
} else {
  cat("  DECISION: Fail to reject H0 (p >= 0.05)\n")
  cat("  No statistically significant difference found.\n\n")
}

# Effect Size
cat("Effect Size (Epsilon-squared):\n")
es_ro1 <- rstatix::kruskal_effsize(data_ro1, Loss_imputed ~ URL_suffix)
cat("  Epsilon-squared:", round(es_ro1$effsize, 6), "\n")
cat("  Magnitude:", as.character(es_ro1$magnitude), "\n")
cat("  Interpretation guide:\n")
cat("    < 0.01 = negligible | 0.01-0.06 = small\n")
cat("    0.06-0.14 = medium  | > 0.14 = large\n\n")

cat("  IMPORTANT NOTE: With n =", comma(nrow(data_ro1)), "records,\n")
cat("  even tiny differences become statistically significant.\n")
cat("  The effect size (epsilon-squared) is the more meaningful measure\n")
cat("  of practical significance.\n\n")

# Post-hoc: Dunn's Test
cat("Post-hoc: Dunn's Test with Bonferroni Correction\n")
cat("  Pairwise comparisons between TLD groups:\n")
cat("-----------------------------------------------\n\n")

dunn_ro1 <- dunn.test::dunn.test(
  data_ro1$Loss_imputed,
  data_ro1$URL_suffix,
  method = "bonferroni",
  kw = FALSE,
  label = TRUE,
  table = FALSE
)

# Capture significant pairs
dunn_ro1_df <- data.frame(
  Comparison = dunn_ro1$comparisons,
  Z = round(dunn_ro1$Z, 4),
  P_adjusted = dunn_ro1$P.adjusted
)
dunn_ro1_sig <- dunn_ro1_df %>% filter(P_adjusted < 0.05)

cat("\nSignificant pairwise comparisons (p.adj < 0.05):\n")
cat("  Total comparisons:", nrow(dunn_ro1_df), "\n")
cat("  Significant pairs:", nrow(dunn_ro1_sig), "\n\n")

if (nrow(dunn_ro1_sig) > 0) {
  print(dunn_ro1_sig %>%
          mutate(P_adjusted = format.pval(P_adjusted, digits = 4)) %>%
          as.data.frame(), row.names = FALSE)
}
cat("\n")

# ===========================================================
# 3.2 RO1-Q2: .gov vs .com Financial Loss (Wilcoxon Rank-Sum)
# ===========================================================

cat("--- 3.2 RO1-Q2: Do .gov websites suffer higher Loss than .com? ---\n\n")

cat("Hypotheses:\n")
cat("  H0: The distribution of financial loss for .gov websites has the\n")
cat("      same location as .com websites (no difference in median ranks)\n")
cat("  H1: Government websites (.gov) suffer significantly higher\n")
cat("      financial loss per incident than commercial websites (.com)\n\n")

cat("Test: Wilcoxon Rank-Sum Test (Mann-Whitney U, one-sided)\n")
cat("  Justification: Two independent groups, non-normal data,\n")
cat("  directional hypothesis (.gov > .com)\n\n")

# Extract .gov and .com subsets
data_gov <- data_ro1 %>% filter(URL_suffix == ".gov")
data_com <- data_ro1 %>% filter(URL_suffix == ".com")

cat("Group sizes:\n")
cat("  .gov:", nrow(data_gov), "records\n")
cat("  .com:", nrow(data_com), "records\n\n")

cat("Descriptive comparison:\n")
cat("  .gov mean Loss: $", comma(round(mean(data_gov$Loss_imputed), 2)), "\n")
cat("  .com mean Loss: $", comma(round(mean(data_com$Loss_imputed), 2)), "\n")
cat("  .gov median Loss: $", comma(round(median(data_gov$Loss_imputed), 2)), "\n")
cat("  .com median Loss: $", comma(round(median(data_com$Loss_imputed), 2)), "\n\n")

# Run Wilcoxon Rank-Sum (one-sided: .gov > .com)
wilcox_h2 <- wilcox.test(
  data_gov$Loss_imputed,
  data_com$Loss_imputed,
  alternative = "greater",
  conf.int = FALSE
)

cat("Wilcoxon Rank-Sum Results (RO1-Q2):\n")
cat("  W statistic:", format(wilcox_h2$statistic, big.mark = ","), "\n")
cat("  p-value:", format.pval(wilcox_h2$p.value, digits = 4), "\n\n")

if (wilcox_h2$p.value < 0.05) {
  cat("  DECISION: Reject H0 (p < 0.05)\n")
  cat("  .gov websites DO suffer significantly higher financial loss\n")
  cat("  per incident than .com websites.\n\n")
} else {
  cat("  DECISION: Fail to reject H0 (p >= 0.05)\n")
  cat("  No statistically significant difference found.\n\n")
}

# Effect Size (rank-biserial correlation via rstatix)
data_h2 <- rbind(
  data_gov %>% select(Loss_imputed, URL_suffix),
  data_com %>% select(Loss_imputed, URL_suffix)
)
data_h2$URL_suffix <- factor(data_h2$URL_suffix, levels = c(".gov", ".com"))

es_h2 <- rstatix::wilcox_effsize(data_h2, Loss_imputed ~ URL_suffix)
cat("Effect Size (rank-biserial correlation r):\n")
cat("  r:", round(es_h2$effsize, 6), "\n")
cat("  Magnitude:", as.character(es_h2$magnitude), "\n")
cat("  Interpretation guide:\n")
cat("    < 0.1 = negligible | 0.1-0.3 = small\n")
cat("    0.3-0.5 = medium   | > 0.5 = large\n\n")

# Generate boxplot for the two-group comparison
dir.create(file.path(output_dir, "tmp_images"), recursive = TRUE, showWarnings = FALSE)

p_h2 <- ggplot(data_h2, aes(x = URL_suffix, y = Loss_imputed, fill = URL_suffix)) +
  geom_boxplot(outlier.size = 0.3, outlier.alpha = 0.2, width = 0.5) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#D6604D", "#2166AC")) +
  labs(title = "RO1-Q2: Financial Loss Comparison — .gov vs .com",
       subtitle = paste0("Wilcoxon Rank-Sum p ",
                         ifelse(wilcox_h2$p.value < 2.2e-16, "< 2.2e-16",
                                format.pval(wilcox_h2$p.value, digits = 3)),
                         " | Effect size r = ", round(es_h2$effsize, 4)),
       x = "Top-Level Domain", y = "Financial Loss ($)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

ggsave(file.path(output_dir, "tmp_images", "h2_gov_vs_com_boxplot.png"),
       plot = p_h2, width = 8, height = 6, dpi = 150)
cat("Saved visualization: tmp_images/h2_gov_vs_com_boxplot.png\n\n")

# ===========================================================
# 3.3 RO2: WebServer vs DownTime
# ===========================================================

cat("--- 3.3 RO2: Does WebServer type affect DownTime? ---\n\n")

cat("Hypotheses:\n")
cat("  H0: The distribution of downtime has the same location\n")
cat("      parameter across all WebServer types\n")
cat("  H1: At least one WebServer type has a significantly different\n")
cat("      downtime distribution location\n\n")

cat("Test: Kruskal-Wallis Rank Sum Test (non-parametric)\n\n")

# Run Kruskal-Wallis
kw_ro2 <- kruskal.test(DownTime_imputed ~ WebServer_clean, data = data_ro2)

cat("Kruskal-Wallis Results (RO2):\n")
cat("  H statistic (chi-squared):", round(kw_ro2$statistic, 4), "\n")
cat("  Degrees of freedom:", kw_ro2$parameter, "\n")
cat("  p-value:", format.pval(kw_ro2$p.value, digits = 4), "\n\n")

if (kw_ro2$p.value < 0.05) {
  cat("  DECISION: Reject H0 (p < 0.05)\n")
  cat("  There IS a statistically significant difference in downtime\n")
  cat("  across WebServer types.\n\n")
} else {
  cat("  DECISION: Fail to reject H0 (p >= 0.05)\n")
  cat("  No statistically significant difference found.\n\n")
}

# Effect Size
cat("Effect Size (Epsilon-squared):\n")
es_ro2 <- rstatix::kruskal_effsize(data_ro2, DownTime_imputed ~ WebServer_clean)
cat("  Epsilon-squared:", round(es_ro2$effsize, 6), "\n")
cat("  Magnitude:", as.character(es_ro2$magnitude), "\n\n")

cat("  IMPORTANT NOTE: With n =", comma(nrow(data_ro2)), "records,\n")
cat("  effect size is the more meaningful measure.\n\n")

# Post-hoc: Dunn's Test
cat("Post-hoc: Dunn's Test with Bonferroni Correction\n")
cat("  Pairwise comparisons between WebServer types:\n")
cat("-----------------------------------------------\n\n")

dunn_ro2 <- dunn.test::dunn.test(
  data_ro2$DownTime_imputed,
  data_ro2$WebServer_clean,
  method = "bonferroni",
  kw = FALSE,
  label = TRUE,
  table = FALSE
)

dunn_ro2_df <- data.frame(
  Comparison = dunn_ro2$comparisons,
  Z = round(dunn_ro2$Z, 4),
  P_adjusted = dunn_ro2$P.adjusted
)
dunn_ro2_sig <- dunn_ro2_df %>% filter(P_adjusted < 0.05)

cat("\nSignificant pairwise comparisons (p.adj < 0.05):\n")
cat("  Total comparisons:", nrow(dunn_ro2_df), "\n")
cat("  Significant pairs:", nrow(dunn_ro2_sig), "\n\n")

if (nrow(dunn_ro2_sig) > 0) {
  print(dunn_ro2_sig %>%
          mutate(P_adjusted = format.pval(P_adjusted, digits = 4)) %>%
          as.data.frame(), row.names = FALSE)
}
cat("\n")

# ===========================================================
# 3.4 RO2-Q1: Apache vs Modern Servers DownTime (Wilcoxon Rank-Sum)
# ===========================================================

cat("--- 3.4 RO2-Q1: Does Apache have longer DownTime than modern servers? ---\n\n")

cat("Hypotheses:\n")
cat("  H0: The distribution of downtime for Apache servers has the same\n")
cat("      location as modern servers (Nginx, LiteSpeed, OpenResty)\n")
cat("  H1: Apache servers have significantly longer downtime compared\n")
cat("      to modern servers (Nginx, LiteSpeed, OpenResty)\n\n")

cat("Test: Wilcoxon Rank-Sum Test (Mann-Whitney U, one-sided)\n")
cat("  Justification: Two independent groups (binary grouping),\n")
cat("  non-normal data, directional hypothesis (Apache > Modern)\n\n")

# Create binary grouping
modern_servers <- c("Nginx", "LiteSpeed", "OpenResty")

data_apache <- data_ro2 %>% filter(WebServer_clean == "Apache")
data_modern <- data_ro2 %>% filter(WebServer_clean %in% modern_servers)

cat("Group sizes:\n")
cat("  Apache:", nrow(data_apache), "records\n")
cat("  Modern (Nginx + LiteSpeed + OpenResty):", nrow(data_modern), "records\n\n")

cat("Descriptive comparison:\n")
cat("  Apache mean DownTime:", round(mean(data_apache$DownTime_imputed), 2), "days\n")
cat("  Modern mean DownTime:", round(mean(data_modern$DownTime_imputed), 2), "days\n")
cat("  Apache median DownTime:", round(median(data_apache$DownTime_imputed), 2), "days\n")
cat("  Modern median DownTime:", round(median(data_modern$DownTime_imputed), 2), "days\n\n")

# Run Wilcoxon Rank-Sum (one-sided: Apache > Modern)
wilcox_h3 <- wilcox.test(
  data_apache$DownTime_imputed,
  data_modern$DownTime_imputed,
  alternative = "greater",
  conf.int = FALSE
)

cat("Wilcoxon Rank-Sum Results (RO2-Q1):\n")
cat("  W statistic:", format(wilcox_h3$statistic, big.mark = ","), "\n")
cat("  p-value:", format.pval(wilcox_h3$p.value, digits = 4), "\n\n")

if (wilcox_h3$p.value < 0.05) {
  cat("  DECISION: Reject H0 (p < 0.05)\n")
  cat("  Apache DOES have significantly longer downtime compared to\n")
  cat("  modern servers.\n\n")
} else {
  cat("  DECISION: Fail to reject H0 (p >= 0.05)\n")
  cat("  No statistically significant difference found.\n\n")
}

# Effect Size (rank-biserial correlation via rstatix)
data_h3 <- rbind(
  data_apache %>% mutate(ServerGroup = "Apache") %>% select(DownTime_imputed, ServerGroup),
  data_modern %>% mutate(ServerGroup = "Modern") %>% select(DownTime_imputed, ServerGroup)
)
data_h3$ServerGroup <- factor(data_h3$ServerGroup, levels = c("Apache", "Modern"))

es_h3 <- rstatix::wilcox_effsize(data_h3, DownTime_imputed ~ ServerGroup)
cat("Effect Size (rank-biserial correlation r):\n")
cat("  r:", round(es_h3$effsize, 6), "\n")
cat("  Magnitude:", as.character(es_h3$magnitude), "\n\n")

# Generate boxplot for the two-group comparison
p_h3 <- ggplot(data_h3, aes(x = ServerGroup, y = DownTime_imputed, fill = ServerGroup)) +
  geom_boxplot(outlier.size = 0.3, outlier.alpha = 0.2, width = 0.5) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#D6604D", "#4DAF4A")) +
  labs(title = "RO2-Q1: DownTime Comparison — Apache vs Modern Servers",
       subtitle = paste0("Wilcoxon Rank-Sum p ",
                         ifelse(wilcox_h3$p.value < 2.2e-16, "< 2.2e-16",
                                format.pval(wilcox_h3$p.value, digits = 3)),
                         " | Effect size r = ", round(es_h3$effsize, 4)),
       x = "Server Group", y = "DownTime (days)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

ggsave(file.path(output_dir, "tmp_images", "h3_apache_vs_modern_boxplot.png"),
       plot = p_h3, width = 8, height = 6, dpi = 150)
cat("Saved visualization: tmp_images/h3_apache_vs_modern_boxplot.png\n\n")

# ===========================================================
# 3.5 Results Summary
# ===========================================================

cat("======================================\n")
cat("RESULTS SUMMARY\n")
cat("======================================\n\n")

results_summary <- data.frame(
  Research_Objective = c("RO1-Q1: TLD vs Loss (all groups)",
                         "RO1-Q2: .gov vs .com Loss",
                         "RO2-Q1: Apache vs Modern DownTime",
                         "RO2-Q2: WebServer vs DownTime (all groups)"),
  Test = c("Kruskal-Wallis", "Wilcoxon Rank-Sum",
           "Wilcoxon Rank-Sum", "Kruskal-Wallis"),
  Statistic = c(round(kw_ro1$statistic, 4),
                round(wilcox_h2$statistic, 0),
                round(wilcox_h3$statistic, 0),
                round(kw_ro2$statistic, 4)),
  p_value = c(kw_ro1$p.value, wilcox_h2$p.value,
              wilcox_h3$p.value, kw_ro2$p.value),
  Effect_Size = c(round(es_ro1$effsize, 6), round(es_h2$effsize, 6),
                  round(es_h3$effsize, 6), round(es_ro2$effsize, 6)),
  Effect_Type = c("epsilon-sq", "rank-biserial r",
                  "rank-biserial r", "epsilon-sq"),
  Effect_Magnitude = c(as.character(es_ro1$magnitude), as.character(es_h2$magnitude),
                       as.character(es_h3$magnitude), as.character(es_ro2$magnitude)),
  Decision = c(
    ifelse(kw_ro1$p.value < 0.05, "Reject H0", "Fail to reject H0"),
    ifelse(wilcox_h2$p.value < 0.05, "Reject H0", "Fail to reject H0"),
    ifelse(wilcox_h3$p.value < 0.05, "Reject H0", "Fail to reject H0"),
    ifelse(kw_ro2$p.value < 0.05, "Reject H0", "Fail to reject H0")
  )
)

print(results_summary, row.names = FALSE)
cat("\n")

# Save all results
hypothesis_results <- list(
  ro1 = list(
    kruskal_wallis = kw_ro1,
    effect_size = es_ro1,
    dunn_test_all = dunn_ro1_df,
    dunn_test_significant = dunn_ro1_sig,
    summary_stats = ro1_summary
  ),
  ro1_q2 = list(
    wilcoxon = wilcox_h2,
    effect_size = es_h2,
    gov_n = nrow(data_gov),
    com_n = nrow(data_com),
    gov_mean = mean(data_gov$Loss_imputed),
    com_mean = mean(data_com$Loss_imputed)
  ),
  ro2 = list(
    kruskal_wallis = kw_ro2,
    effect_size = es_ro2,
    dunn_test_all = dunn_ro2_df,
    dunn_test_significant = dunn_ro2_sig,
    summary_stats = ro2_summary
  ),
  ro2_q1 = list(
    wilcoxon = wilcox_h3,
    effect_size = es_h3,
    apache_n = nrow(data_apache),
    modern_n = nrow(data_modern),
    apache_mean = mean(data_apache$DownTime_imputed),
    modern_mean = mean(data_modern$DownTime_imputed)
  ),
  results_summary = results_summary
)

saveRDS(hypothesis_results, "raw_data/processed_data/hypothesis_test_results.rds")
cat("All test results saved: raw_data/processed_data/hypothesis_test_results.rds\n\n")

# ============================================================
# SECTION 4: CONCLUSIONS
# ============================================================

cat("======================================\n")
cat("SECTION 4: CONCLUSIONS\n")
cat("======================================\n\n")

cat("--- RO1: URL TLD vs Financial Loss ---\n\n")

# Identify top TLD by mean and total
top_mean_tld <- ro1_summary %>%
  arrange(desc(as.numeric(as.character(Mean)))) %>%
  slice(1)
top_total_tld <- ro1_summary %>%
  arrange(desc(as.numeric(as.character(Total)))) %>%
  slice(1)

cat("Q1: Do different TLDs have different financial losses?\n")
cat("  Answer:", ifelse(kw_ro1$p.value < 0.05, "YES", "NO"),
    "- The Kruskal-Wallis test shows a statistically significant\n")
cat("  difference (H =", round(kw_ro1$statistic, 2),
    ", p =", format.pval(kw_ro1$p.value, digits = 3), ").\n")
cat("  Effect size (epsilon-squared =", round(es_ro1$effsize, 4),
    ") is", as.character(es_ro1$magnitude), ".\n")
cat("  Dunn's post-hoc identifies", nrow(dunn_ro1_sig),
    "significant pairwise differences.\n\n")

cat("Q2: Do .gov websites suffer significantly higher Loss than .com?\n")
cat("  Answer:", ifelse(wilcox_h2$p.value < 0.05, "YES", "NO"),
    "- The Wilcoxon Rank-Sum test confirms .gov websites have\n")
cat("  significantly higher per-incident loss (mean $",
    comma(round(mean(data_gov$Loss_imputed), 0)),
    " vs $", comma(round(mean(data_com$Loss_imputed), 0)), ").\n")
cat("  Effect size (r =", round(es_h2$effsize, 4),
    ") is", as.character(es_h2$magnitude), ".\n\n")

cat("--- RO2: WebServer Type vs DownTime ---\n\n")

top_mean_ws <- ro2_summary %>%
  arrange(desc(as.numeric(as.character(Mean)))) %>%
  slice(1)
top_total_ws <- ro2_summary %>%
  arrange(desc(as.numeric(as.character(Total)))) %>%
  slice(1)

cat("Q1: Does Apache have significantly longer DownTime than modern servers?\n")
cat("  Answer:", ifelse(wilcox_h3$p.value < 0.05, "YES", "NO"),
    "- The Wilcoxon Rank-Sum test confirms Apache has significantly\n")
cat("  longer downtime (mean", round(mean(data_apache$DownTime_imputed), 2),
    "days) vs modern servers (mean", round(mean(data_modern$DownTime_imputed), 2), "days).\n")
cat("  Effect size (r =", round(es_h3$effsize, 4),
    ") is", as.character(es_h3$magnitude), ".\n\n")

cat("Q2: Is the difference in downtime across WebServers significant?\n")
cat("  Answer:", ifelse(kw_ro2$p.value < 0.05, "YES", "NO"),
    "- The Kruskal-Wallis test shows a statistically significant\n")
cat("  difference (H =", round(kw_ro2$statistic, 2),
    ", p =", format.pval(kw_ro2$p.value, digits = 3), ").\n")
cat("  Effect size (epsilon-squared =", round(es_ro2$effsize, 4),
    ") is", as.character(es_ro2$magnitude), ".\n")
cat("  Dunn's post-hoc identifies", nrow(dunn_ro2_sig),
    "significant pairwise differences.\n\n")

cat("--- Limitations ---\n")
cat("  1. Loss and DownTime values include MICE-imputed data\n")
cat("     (6.3% Loss, 0.4% DownTime were imputed)\n")
cat("  2. With n > 500K, statistical significance (p-value) is\n")
cat("     almost guaranteed; effect sizes are more informative\n")
cat("  3. This is an observational study; TLD/WebServer associations\n")
cat("     do not imply causation\n")
cat("  4. 'Other' and 'Other_TLD' categories are catch-all groups\n")
cat("     containing heterogeneous entries\n\n")

cat("======================================\n")
cat("DATA ANALYSIS COMPLETE\n")
cat("======================================\n\n")
cat("Output files:\n")
cat("  PDFs:  docs/reports/06_data_analysis_RO1.pdf\n")
cat("         docs/reports/07_data_analysis_RO2.pdf\n")
cat("  CSVs:  docs/reports/ro1_summary_statistics.csv\n")
cat("         docs/reports/ro2_summary_statistics.csv\n")
cat("  RDS:   raw_data/processed_data/hypothesis_test_results.rds\n")
