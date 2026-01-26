![](media/image1.emf){width="1.9514982502187226in"
height="1.7833333333333334in"}

**GROUP ASSIGNMENT**

**TECHNOLOGY PARK MALAYSIA**

**CT127-3-2-PFDA**

**PROGRAMMING FOR DATA ANALYSIS**

**INTAKE CODE: APD2F2511**

**ASSIGNMENT HAND OUT DATE: 24^th^ November 2025**

**ASSIGNMENT HAND IN DATE: 7^th^ February 2026**

**LECTURER: DR. KULOTHUNKAN PALASUNDRAM**

**[Student Details:]{.underline}**

  ------------------------------------------------------------------------
  **No.**    **Name**                     **TP Number**
  ---------- ---------------------------- --------------------------------
  1\.        Choong Ti Huai               TP078539

  2\.        Daniel Chan Zit Fung         TP079018

  3\.        Gan Yew Joe                  TP077191

  4\.        Liu Wei                      TP085412
  ------------------------------------------------------------------------

Table of contents

Leave this page empty, don't do anything here. This will be the very
last step.

# 

# 1.0 Introduction

Leave this page empty, don't do anything here. This will be the last
step.

## Project Description

text

## Data Description

Text

Ti Huai to Daniel: Below is the dataset description copied from
assignment questions. You see yourself how to rewrite them in a proper
way lah (perhaps rewrite into sentences? Idk) ! I trust you😎!

\`\`\`

**Date : Website defacement date**

**Notify: Incident reported by a person / group**

**URL: URL of the defaced website.**

**IP: IP address of the server that was compromised**

**Country: Country where the server hosting the defaced website is
located.**

**WebServer : Web server version used by the server hosting the defaced
website.**

**Encoding : The character encoding used in the defacement message**

**Ransom: Amount paid in Thousands.**

**DownTime : Number of days when a system is unavailable.**

**Loss: Revenue lost caused by attack.**

\`\`\`

## Data Preparation

This section outlines the 14 steps, full pipeline, from messy data to
cleaned, analysis-ready data

**1. Dataset Merging**

Use rbind().

**2. Data Format Conversion**

Purpose: Convert data to appropriate formats for both cleaning and
analysis

Columns affected: Date, DownTime, Loss, Ransom

Date (DateTime to Date)

Numeric conversion (DownTime, Loss, Ransom)

**3. Distribution Check**

Purpose: Quick screening to determine if data follows normal or skewed
distribution

Descriptive Statistics, Visualization Check, Normality Diagnostic Tests,
Skewness Analysis

**4. Outlier Detection**

Purpose: Detect and flag outliers in two phases

Phase 1 focuses on physically impossible or logically invalid values
(negative, zero).

Phase 2 uses statistical methods to detect extreme outliers:

- Modified Z-Score (MAD-based): Robust to outliers

- Percentile method: Flag values beyond 99.9th percentile

**5. Outlier to NA**

Purpose: Convert flagged outliers to NA

Columns affected: Loss, Ransom, DownTime

**6. Multivariate Outlier Detection**

Purpose: Detect "weird combinations" using Isolation Forest

Method: Isolation Forest - detects structural anomalies in multivariate
data

Sources to attached: what is Isolation Forest?

**7. Multivariate outlier to NA**

Purpose: Convert flagged multivariate outliers to NA, preserve the flags
for later inspection.

Columns affected:

**8. Data Health Check**

Purpose: Visualize missing data patterns and post-cleaning distribution
quality.

Methods: naniar, VIM packages for missingness analysis

**9. Preprocessing for Imputation**

Purpose: data for missing value imputation

Steps involved: Variable categorization feature engineering, log
transformation

**10. Categorical Cleaning**

Purpose: Clean and standardize high-cardinality categorical variables

Methods: Version stripping, standardization, frequency lumping

Columns affected: WebServer, Country, Notify, URL, IP, Encoding,

**11. Final Quality Check**

Purpose: Comprehensive pre-imputation validation

Checks: 1) Information content, 2) Multivariate associations, 3) Final
missing patterns

**12. MICE Imputation**

Purpose: Impute missing values using Multivariate Imputation by Chained
Equations

Strategy: Separate imputation for Modern (2016+) and Legacy (\<2016)
data

Method: PMM (Predictive Mean Matching) - robust to skewness

**13. Imputation Validation**

Purpose: Visual validation and spot checks of MICE imputation quality

**14. Visual Comparison -- 2b confirmed**

## Assumptions

Short opening: ...Several assumptions were made...

### no remove duplicate

While the dataset contains duplicate records, we have opted to retain
them rather than performing de-duplication. These entries likely
represent distinct security incidents occurring in close succession.
Given that the \'time\' field lacks the precision (e.g., milliseconds)
to differentiate between them, removing these rows would lead to an
underestimation of the actual attack frequency and total downtime.

**Note**: "Date" section only mention the date, it didn't mention the
time (hours, minutes, seconds, milliseconds), it is possible that the
attackers held the attack multiple times in a day. The attackers held
the attacks at the same location, using the same servers, and that is
possible to cause the datasets have duplicate values in Downtime and
Loss.

### no pay ransom

NA in Ransom is set to 0, because victims decide not to pay ransom

To attach source: why should not pay for ransom.

Assumption 3

Assumption 4

Assumption 5

Assumptions 6

No negative value, all numerical values are "absoluted" to positive
values.

# 2.0 Analysis (Individual)

Short introduction here

## 2.1 Choong Ti Huai (TP078539)

text

### 2.1.1 Objectives

Text

**Descriptive Analytics:**

1.  To identify the most commonly targeted countries, attacker profiles,
    IP ranges, and encodings to discover which geographical regions are
    most vulnerable and experience the highest volume of cyber-attacks.

2.  To visualize temporal trends (daily/weekly/monthly) in attack
    frequency to detect peak attack periods or seasonal patterns to
    discover when cyber-attacks occur most frequently, whether during
    specific times of day, days of the week, or months of the year.

**Diagnostic Analytics:**

1.  To analyse whether specific web server technologies (e.g., Apache,
    Nginx, OpenResty) are associated with higher attack frequency or
    severity to identify which server software correlates with more
    severe outcomes (high ransom, long downtime, larger financial loss).

### 2.1.2 Analysis Questions

Text

### 2.1.3 Data Preparation

Describe any additional preparation work you did to perform or enhance
your individual analysis.

Include explanations for each step and justify your approach.

Provide R code snippets with explanations and outputs for each step
taken.

### 2.1.4 Data Analysis

Conduct exploratory and inferential analyses relevant to your
objectives.

Include summary statistics and visualizations with interpretations.

Clearly indicate which objective each analysis supports.

Include explanations for each step and justify your approach.

Provide R code snippets with explanations and outputs for each step
taken.

### 2.1.5 Hypothesis Formulation and Testing

Formulate hypotheses based on your findings and perform appropriate
statistical tests in R.

Interpret and explain your test results comprehensively.

Include explanations for each step and justify your approach.

Provide R code snippets with explanations and outputs for each step
taken.

## 2.2 Daniel Chan Zit Fung (TP079018)

Text

### 2.2.1 Objectives

**[Descriptive]{.underline}**

1.  To investigate which web server and specific encoding method is the
    most vulnerable to cyber-attacks.

2.  To identify the frequency of attacks, total loss and paid amount
    each year according to different countries.

### Analysis Questions

Text

### 2.2.3 Data Preparation

text

### 2.2.4 Data Analysis

text

### 2.2.5 Hypothesis Formulation and Testing

Text

## Gan Yew Joe (TP077191)

text

### 2.3.1 Objectives

Descriptive Analytics:

1.  To identify the relationship between the duration of downtime
    directly affecting the number of times the notifiers got attacked
    when it is under maintenance.

2.  To investigate whether the number of times a downtime occurs could
    cause an attack.

### Analysis Questions

Text

### Data Preparation

text

### Data Analysis

text

### Hypothesis Formulation and Testing

Text

## 2.4 Liu Wei (TP085412)

Text

### 2.4.1 Objectives

Objective 4.1: To analyze the reporting frequency, temporal patterns
(Date), and geographical coverage (Country) of different Notifiers to
understand their activity levels across attack severities (Ransom,
DownTime, Loss) and technical environments (WebServer, Encoding).

Objective 4.2: To explore the distribution of URL and IP addresses and
identify attack frequency per URL and IP to reveal vulnerability
characteristics based on hosting Country, WebServer types, and Encoding
formats, and examine their relationships with impact metrics (Ransom,
DownTime, Loss).

### 2.4.2 Analysis Questions

Text

### 2.4.3 Data Preparation

text

### 2.4.4 Data Analysis

text

### 2.4.5 Hypothesis Formulation and Testing

Text

# 3.0 Group Hypothesis

Short introduction here

## [Group Hypothesis Formulation]{.mark}

text

## Group Hypothesis Testing

Text

## Overall Conclusion

text

# 4.0 Summary

Short introduction here

## Limitations and Recommendations

text

## Title

Text

## Title

text

# Title 5

Short introduction here

## Title

text

## Title

Text

## Title

text

# Title 6

Short introduction here

## Title

text

## Title

Text

## Title

text

# Conclusion

References

Don't do anything here, leave it as the last step.

# Appendix
