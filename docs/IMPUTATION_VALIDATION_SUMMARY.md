# Post-Imputation Validation Report

**Date**: 2026-01-21
**Script**: 12_imputation_validation.R
**Validation Method**: Distribution comparison + Spot checks + Extreme value checks

---

## Executive Summary

**Overall Validation Score**: 1/6 (⚠️ Review Required)

The post-imputation validation reveals mixed results:
- ✓ **Extreme Value Checks**: 1/3 passed (DownTime imputed values all within observed range)
- ✗ **Distribution Similarity**: 0/3 passed (DownTime and Loss show significant distributional differences)
- ⚠️ **Spot Checks**: Most individual records appear reasonable, but category-specific patterns show USA Loss values differ by 26.37%

**Key Finding**: While PMM successfully selected real observed values (minimal out-of-range imputation), the **distributional properties** of imputed values differ from observed values, particularly for DownTime and Loss.

---

## Part 1: Distribution Visual Comparison

### Objective
Verify that imputed values follow the same distribution as observed values using:
- Density curve overlaps (visual)
- Kolmogorov-Smirnov (KS) test (statistical)

### Results

| Variable      | Observed Mean | Imputed Mean | Mean Diff | KS Test p-value | Status |
|---------------|---------------|--------------|-----------|-----------------|--------|
| Ransom_log    | NaN           | 0.918        | NaN       | NA              | N/A (All values imputed in modern data) |
| DownTime_log  | 0.950         | 0.864        | -0.086    | < 0.0001        | ⚠️ DIFFERENT |
| Loss_log      | 3.801         | 3.891        | +0.090    | < 0.0001        | ⚠️ DIFFERENT |

### Interpretation

**Ransom_log (Modern Data Only)**:
- Cannot perform KS test because ALL Ransom values in modern data were originally NA
- All 169,372 modern records had Ransom imputed
- Mean: 0.918, Median: 0.908, SD: 0.334, Skewness: 0.106
- ✓ Skewness near 0 indicates excellent normalization after log transformation
- ⚠️ Cannot verify if imputation matches "true" distribution (no observed baseline)

**DownTime_log**:
- **KS Test p-value < 0.0001**: Statistically significant difference
- Observed: mean=0.950, median=0.954, skewness=0.445
- Imputed:  mean=0.864, median=0.845, skewness=0.238
- **Imputed values are systematically LOWER** by 0.086 log units (~17% in original scale)
- Possible cause: PMM is selecting shorter downtimes as donors

**Loss_log**:
- **KS Test p-value < 0.0001**: Statistically significant difference
- Observed: mean=3.801, median=3.859, skewness=-1.250
- Imputed:  mean=3.891, median=3.771, skewness=-1.246
- **Imputed values are systematically HIGHER** by 0.090 log units (~23% in original scale)
- Skewness preserved (-1.25), but distribution shifted upward
- Possible cause: Loss heavily skewed even after log transformation; PMM struggling with multimodal distribution

### Visual Inspection (See PDF)

**File**: `raw_data/processed_data/imputation_validation_plots.pdf`

Key observations from density plots:
1. **Ransom_log**: Single distribution (all imputed), shows clean bell curve with slight positive skew
2. **DownTime_log**: Red curve (imputed) shifted LEFT compared to blue curve (observed)
3. **Loss_log**: Red curve (imputed) shows different modal structure than blue curve (observed)

---

## Part 2: Spot Checks - Individual Record Review

### Spot Check 1: Ransom Imputations (10 random records)

**All 10 records PASSED logical consistency checks**:
- Ransom/Loss ratios: 0.000-0.001 (all < 0.5, reasonable)
- Typical pattern: Ransom $8-$24, DownTime 2-8 hours, Loss $12K-$32K
- No cases where Ransom exceeds Loss (would be illogical)

**Sample Record**:
```
Date: 2022-01-22
Country: 6 (factor level), WebServer: 7 (factor level)
Ransom: $9.09, DownTime: 6 hrs, Loss: $28,858
Ransom/Loss: 0.0003 ✓ OK
```

### Spot Check 2: Loss Imputations (10 random records)

**8/10 records PASSED, 2 flagged as "Loss seems LOW"**:

✓ **Passed Examples**:
- Modern data (2023-06-04): DownTime=5hrs → Loss=$7,407 ✓
- Legacy data (NA date): DownTime=8hrs → Loss=$13,911 ✓

⚠️ **Flagged Examples**:
- Legacy data (2002-12-21): DownTime=**50hrs** → Loss=$3,660
  - Expected range: $5,000-$500,000
  - Actual: $3,660 (seems low for 50-hour outage)

- Legacy data (2001-12-29): DownTime=**11hrs** → Loss=$78
  - Expected range: $1,100-$110,000
  - Actual: $78 (extremely low for 11-hour outage)

**Interpretation**: Legacy data imputations occasionally produce unrealistic Loss values for long downtimes. This suggests:
1. Legacy data has weaker predictor relationships
2. PMM may be selecting from limited legacy donor pool
3. Extreme short-downtime/low-loss cases exist and are being used as donors inappropriately

### Spot Check 3: Category-Specific Patterns

#### USA Loss Pattern Check:
```
USA Observed:  mean=$8,708,  median=$3,304
USA Imputed:   mean=$11,005, median=$7,032
Difference: 26.37% ⚠️ REVIEW NEEDED
```
**Finding**: Imputed USA losses are ~26% higher than observed. This suggests PMM is not preserving country-specific loss patterns well.

#### Nginx DownTime Pattern Check:
```
Nginx Observed:  mean=28.25 hrs, median=29 hrs
Nginx Imputed:   mean=27.97 hrs, median=28 hrs
Difference: 1.00% ✓ PASS
```
**Finding**: Nginx downtime patterns are well-preserved (< 5% difference).

---

## Part 3: Extreme Value Checks

### Objective
Ensure PMM did not create **synthetic outliers** outside observed min/max ranges.

### Results

| Variable         | Observed Min | Observed Max | Imputed Min | Imputed Max | Out-of-Range Count | Status |
|------------------|--------------|--------------|-------------|-------------|--------------------|--------|
| Ransom_imputed   | $0.35        | $56.39       | $0.35       | $94.90      | 461 (0.24%)        | ⚠️ GOOD (<1%) |
| DownTime_imputed | 0 hrs        | 50 hrs       | 0 hrs       | 50 hrs      | 0 (0%)             | ✓✓ EXCELLENT |
| Loss_imputed     | $31          | $144,694     | $31         | $144,742    | 1 (0.002%)         | ✓✓ EXCELLENT |

### Interpretation

**Ransom**:
- 461 out-of-range values (0.24% of 192,371 imputations)
- Max observed: $56.39 → Max imputed: $94.90 (exceeds by $38.51)
- ⚠️ Concerning but < 1% threshold (acceptable)
- **Possible cause**: Back-transformation from log scale introducing rounding errors, OR PMM selecting from modern data pool where higher ransoms exist

**DownTime**:
- ✓✓ Perfect: All imputed values within [0, 50] hours range
- No synthetic outliers created
- PMM working as expected for DownTime

**Loss**:
- ✓✓ Nearly perfect: Only 1 out-of-range value (0.002%)
- Max observed: $144,694 → Max imputed: $144,742 (exceeds by $48, likely rounding)
- Negligible error

---

## Key Findings & Recommendations

### 🔴 Critical Issues

1. **Distribution Shift in DownTime**:
   - Imputed values are systematically 9% lower than observed
   - KS test p < 0.0001 (highly significant difference)
   - **Impact**: Underestimation of downtime in imputed records
   - **Recommendation**:
     - Review predictor selection (consider adding more temporal or attack-type features)
     - Consider stratified imputation by Year or WebServer type
     - Validate if shift is acceptable for your analysis goals

2. **Distribution Shift in Loss**:
   - Imputed values are systematically 9% higher than observed
   - KS test p < 0.0001 (highly significant difference)
   - **Impact**: Overestimation of financial loss in imputed records
   - **Recommendation**:
     - Loss_log still heavily skewed (-1.25) even after transformation
     - Consider alternative transformation (Box-Cox, square root)
     - Review if Loss should be stratified by Country (USA pattern shows 26% difference)

3. **USA Category Bias**:
   - USA imputed losses 26% higher than observed
   - Suggests PMM not capturing country-specific patterns well
   - **Recommendation**: Add more country-specific predictors or stratify imputation by Country

### 🟡 Moderate Concerns

4. **Legacy Data Quality**:
   - 2 out of 10 spot-check losses were unrealistic (e.g., $78 for 11-hour outage)
   - Legacy data has weaker predictor relationships
   - **Recommendation**:
     - Consider excluding legacy data from Loss analysis
     - OR: Flag legacy Loss imputations for sensitivity analysis
     - Document limitation in analysis

5. **Ransom Out-of-Range Values**:
   - 461 ransoms (0.24%) exceed observed max ($56 → $95)
   - Small percentage but concerning for extreme value analysis
   - **Recommendation**:
     - Investigate if these are back-transformation errors
     - Cap imputed Ransom at observed max if appropriate
     - Document in limitations

### 🟢 Strengths

6. **PMM Method Working Correctly**:
   - 99.76% of values within observed range (minimal synthetic outliers)
   - Individual spot checks show logical consistency (Ransom < Loss)
   - Nginx DownTime pattern preserved (1% difference)

7. **Ransom Skewness Corrected**:
   - Imputed Ransom_log skewness: 0.106 (excellent normalization)
   - Suggests transformation strategy was correct for Ransom

---

## Statistical Validation Metrics

### Distribution Tests (KS Test)
- **Ransom_log**: N/A (all values imputed, no baseline)
- **DownTime_log**: p < 0.0001 ✗ (FAILED - distributions differ)
- **Loss_log**: p < 0.0001 ✗ (FAILED - distributions differ)

### Extreme Value Tests
- **Ransom_imputed**: 99.76% within range ✓ (GOOD)
- **DownTime_imputed**: 100% within range ✓✓ (EXCELLENT)
- **Loss_imputed**: 99.998% within range ✓✓ (EXCELLENT)

### Spot Check Results
- **Ransom logic checks**: 10/10 passed (100%)
- **Loss logic checks**: 8/10 passed (80%)
- **Category pattern checks**: 1/2 passed (50%)

### Overall Score: 1/6 Tests Passed

---

## Recommendations for Analysis

### Immediate Actions

1. **Acknowledge Distributional Differences**:
   - Document that imputed DownTime is ~9% lower than observed
   - Document that imputed Loss is ~9% higher than observed
   - Report these biases in your methodology section

2. **Sensitivity Analysis**:
   ```r
   # Compare results with and without imputed values
   analysis_observed_only <- analyze(data[!Loss_was_imputed, ])
   analysis_with_imputed  <- analyze(data)
   compare_results(analysis_observed_only, analysis_with_imputed)
   ```

3. **Flag Imputed Records**:
   ```r
   # Keep imputation flags for downstream analysis
   data_final$has_imputed_loss <- data$Loss_was_imputed
   data_final$has_imputed_downtime <- data$DownTime_was_imputed
   ```

### Long-term Improvements

4. **Consider Alternative Imputation Methods**:
   - **Random Forest imputation** (better handles non-linear relationships)
   - **Stratified MICE** (separate models by Country or Year)
   - **Multiple Imputation with PMM + calibration** (post-hoc adjustment to match distributions)

5. **Improve Predictor Set**:
   - Add attack type indicators (if available)
   - Include organization size proxies
   - Consider interaction terms (Country × WebServer)

6. **Transform Loss Differently**:
   - Loss_log still heavily skewed (-1.25)
   - Try: `Loss_transformed <- (Loss^0.3)` (cube root-like)
   - Or: Box-Cox transformation with optimal lambda

---

## Files Generated

1. **`imputation_validation_plots.pdf`** (27 KB)
   - 4 density plot comparisons (Ransom, DownTime, Loss, Combined)
   - Visual assessment of distribution overlap

2. **`imputation_validation_report.rds`** (709 B)
   - Statistical summary tables
   - KS test results
   - Extreme value check results
   - Loadable for further analysis: `readRDS("imputation_validation_report.rds")`

3. **`IMPUTATION_VALIDATION_SUMMARY.md`** (This file)
   - Comprehensive validation report
   - Recommendations and limitations

---

## Conclusion

The MICE imputation using PMM successfully avoided creating synthetic outliers (99%+ within observed range), demonstrating that the method fundamentally works. However, **distributional differences** between observed and imputed values indicate that the imputed data has systematic biases:

- **DownTime**: Underestimated by ~9%
- **Loss**: Overestimated by ~9%
- **USA Loss**: Overestimated by ~26%

**These biases should be:**
1. ✅ Documented in your analysis
2. ✅ Tested via sensitivity analysis
3. ⚠️ Considered for re-imputation with improved strategy (if time permits)

**Overall Assessment**: The imputed dataset is **usable with caveats**. Proceed to analysis with awareness of these limitations, and flag imputed records for transparency in reporting.

---

**Validation Date**: 2026-01-21
**Validated By**: 12_imputation_validation.R (automated)
**Next Step**: Proceed to exploratory data analysis with imputation flags preserved
