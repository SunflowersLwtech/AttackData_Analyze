针对您这个特定的黑客攻击数据集（数据量大、极度偏态、包含长尾金融数据），以下是各类方案的推荐程度及理由：

### 1. 推荐选取的方案（Best Fits）

#### **可视化诊断方法：**

* **直方图 (Histogram) & 密度图 (Density Plot)**
* **理由**：这是必须的第一步。但要注意，由于 `Loss` 和 `Ransom` 跨度极大（从 0 到 10亿），必须配合 **对数缩放 (Log-scale)** 使用。它们能帮你直观识别数据是单峰还是多峰，以及长尾的具体形态。


* **箱线图 (Boxplot)**
* **理由**：能快速定位中位数和大致的异常区间。虽然在偏态数据中它会划出很多“异常点”，但它能帮你建立对数据离散程度的初步感官。



#### **单变量统计方法：**

* **修正 Z-Score 法 (Modified Z-score / MAD)** —— **重点推荐**
* **理由**：这是处理该数据集最稳健的统计方法。普通均值会被 10 亿级的 `Loss` 拉偏，而 MAD 使用**中位数**，不会受极端值影响，能更准确定位哪些点是真正的离群值。


* **百分位阈值法 (Percentile Thresholding)** —— **业务首选**
* **理由**：在安全和金融领域，我们通常关心“极端事件”。直接定义前 0.5% 或 1% 为异常值，非常符合业务直觉，且计算极快，适合 60 万行的数据量。



#### **多变量诊断方法：**

* **孤立森林 (Isolation Forest)** —— **算法首选**
* **理由**：它是处理大规模、高维异常检测的神器。它不假设数据分布（你的数据明显非正态），且计算效率极高。它能识别出那些“虽然数值没到顶峰，但组合起来很违和”的记录（例如：停机时间极短但损失极大的记录）。



---

### 2. 不推荐选取的方案及理由（Avoid These）

* **Z-Score 法 (Standard Z-Score)**
* **理由**：**该数据集严禁使用此法**。Z-Score 假设数据符合正态分布。你的数据（Loss, Ransom）是极度偏态的。极端大值会大幅拉高均值和标准差，导致 Z-Score 失效，甚至出现“明明很离谱的点，计算出的 Z 值却不大”的现象。


* **四分位距法 (IQR Method)**
* **理由**：**慎用/不推荐**。对于这种长尾分布，1.5*IQR 的准则过于严苛。在偏态分布中，它会将大量“虽然很大但属于正常长尾”的数据标记为异常，导致你丢失过多的有用信息。


* **马哈拉诺比斯距离 (Mahalanobis Distance)**
* **理由**：它依赖于均值和协方差矩阵，同样假设数据呈多元正态分布。对于包含极端值的非正态数据，它的结果会极不稳定。


* **DBSCAN / K-近邻距离 (k-NN)**
* **理由**：**计算成本过高**。这两种方法的时间复杂度通常在  或 。对于近 60 万行的数据，不经过采样直接跑这些算法会非常缓慢，且 DBSCAN 对参数（Epsilon）极其敏感，在密度不均的数据集上效果很差。



---

### 总结建议的操作路径：

1. **逻辑诊断（必做）**：先直接排除负数（逻辑错误）。
2. **可视化（必做）**：使用 `log10` 转换后的**直方图**观察分布。
3. **单变量处理**：使用 **Modified Z-Score** 识别各列的极端离群点并标记为 `NA`。
4. **多变量处理**：运行 **孤立森林**，抓取潜在的隐蔽异常组合。
5. **下一步**：在完成这些标记后，再使用插值法（如 R 的 `mice` 包）补全这些 `NA`。

📊 Analysis Results Summary
Key Findings:
All three numeric variables show SEVERE RIGHT SKEW:

Ransom: Mean/Median Ratio = 22.6

Mean: 198.44, Median: 8.79
Range: 0.07 to 3,000
DownTime: Mean/Median Ratio = 3.38

Mean: 20.31, Median: 6.0
Range: -5 to 9,999
⚠️ Has negative values (-5)
Loss: Mean/Median Ratio = 39.7

Mean: 443,440, Median: 11,166
Range: -99,999,999 to 1,000,000,000
⚠️ Has extreme negative values
Recommendations:
❌ DO NOT USE:

Z-Score method
Mahalanobis distance
✅ RECOMMENDED METHODS:

IQR (Interquartile Range) method
Percentile-based methods
Robust statistical methods
Consider log transformation
Output Files:
📄 Script: scripts/data_clean/02_distribution_check.R
📊 PDF Report: raw_data/processed_data/distribution_diagnostics.pdf (50MB)
The PDF contains 12 diagnostic plots (4 plots per variable) showing histograms, log-transformed distributions, boxplots, and Q-Q plots for detailed visual inspection.