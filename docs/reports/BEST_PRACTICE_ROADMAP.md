# 数据分析最佳实践路线图

## 基于网络攻击事件数据分析项目的后续工作指南

---

## 文档信息

| 项目 | 详情 |
|------|------|
| **基于项目** | 黑客攻击事件数据清洗与预处理 |
| **课程** | Programming for Data Analysis (112025-KLT) |
| **机构** | 亚太科技创新大学 (APU) |
| **报告类型** | 后续最佳实践路线图 |
| **生成日期** | 2026年1月24日 |

---

## 目录

1. [项目完成状态回顾](#1-项目完成状态回顾)
2. [后续分析路线图](#2-后续分析路线图)
3. [个人分析模块建议](#3-个人分析模块建议)
4. [小组假设整合策略](#4-小组假设整合策略)
5. [高级分析技术](#5-高级分析技术)
6. [可视化最佳实践](#6-可视化最佳实践)
7. [报告撰写指南](#7-报告撰写指南)
8. [代码质量与可重现性](#8-代码质量与可重现性)
9. [常见陷阱与规避](#9-常见陷阱与规避)
10. [提交前检查清单](#10-提交前检查清单)

---

## 1. 项目完成状态回顾

### 1.1 已完成工作总结

基于 `DATA_CLEANING_FULL_REPORT.md` 的分析，数据清洗阶段已经完成以下核心工作：

| 阶段 | 状态 | 关键成果 |
|------|------|----------|
| 数据获取与合并 | ✅ 完成 | 592,765 条记录成功合并 |
| 格式转换与标准化 | ✅ 完成 | 日期、数值列已标准化 |
| 单变量异常值检测 | ✅ 完成 | MAD + 百分位数方法 |
| 多变量异常值检测 | ✅ 完成 | 隔离森林算法 |
| 缺失机制分析 | ✅ 完成 | 确定为 MAR 机制 |
| 分类变量清洗 | ✅ 完成 | 高基数降维处理 |
| MICE 多重插补 | ✅ 完成 | 分层策略插补 |
| 质量验证 | ✅ 完成 | K-S 检验等验证 |

### 1.2 最终数据集状态

```
最终数据集: mice_imputed_data.RData
├── 总记录数: 592,765
├── 变量数: 18 (含新特征)
├── DownTime: 100% 完整
├── Loss: 100% 完整
└── Ransom: 62.5% 完整 (遗留数据故意不插补)
```

### 1.3 待完成作业要求

根据作业要求，以下章节需要继续完成：

| 章节 | 负责人 | 当前状态 |
|------|--------|----------|
| Chapter 1 - 引言 | 小组 | 数据准备部分可从清洗报告迁移 |
| Chapter 2 - 个人分析 | 各成员 | **待开始** |
| Chapter 3 - 小组假设 | 小组 | **待整合** |
| Chapter 4 - 总结 | 小组 | **待撰写** |

---

## 2. 后续分析路线图

### 2.1 整体时间线建议

```
Phase 1: 个人探索分析 (每人独立)
├── 目标定义 (0.5天)
├── 数据探索 (1天)
├── 假设制定与检验 (1.5天)
└── 可视化与解释 (1天)

Phase 2: 小组整合 (协作)
├── 个人发现汇报 (0.5天)
├── 综合假设构建 (1天)
└── 联合检验 (1天)

Phase 3: 报告撰写
├── 个人部分撰写 (2天)
├── 小组部分整合 (1天)
└── 审校与格式化 (1天)
```

### 2.2 推荐分析维度

基于数据集特性，建议从以下维度展开分析：

| 维度 | 核心变量 | 分析方向 |
|------|----------|----------|
| **时间维度** | Date, Year | 攻击趋势、季节性、年度对比 |
| **地理维度** | Country_clean | 地区分布、高危国家、区域对比 |
| **技术维度** | WebServer_clean, Encoding | 技术脆弱性、服务器风险 |
| **威胁维度** | Notify_clean | 黑客组织活跃度、攻击偏好 |
| **影响维度** | Loss, DownTime, Ransom | 经济影响、业务中断、勒索行为 |

### 2.3 变量关系矩阵

```
建议探索的变量关系:

影响变量内部:
├── Loss ~ DownTime (停机时间与损失的关系)
├── Ransom ~ Loss (勒索金额与实际损失)
└── Ransom ~ DownTime (勒索与停机关联)

影响 vs 分类:
├── Loss ~ Country (不同国家的损失差异)
├── DownTime ~ WebServer (服务器类型与恢复时间)
├── Ransom ~ Notify (不同组织的勒索行为)
└── Loss ~ Year (损失的时间趋势)

分类变量关联:
├── Country ~ WebServer (地区技术偏好)
├── Notify ~ Country (攻击者目标国家)
└── WebServer ~ Year (技术演进)
```

---

## 3. 个人分析模块建议

### 3.1 成员 1: 时间序列与趋势分析

**推荐研究方向**: 网络攻击的时间演变模式

**建议目标**:
1. 分析网络攻击事件的年度趋势
2. 识别攻击活动的季节性模式
3. 评估攻击影响（损失、停机）的时间变化

**建议分析问题**:
- 网络攻击事件数量是否逐年增加？
- 攻击活动是否存在季节性高峰？
- 平均经济损失是否随时间变化？
- 不同时期的攻击特征有何差异？

**推荐统计方法**:

```r
# 1. 年度趋势分析
yearly_summary <- data %>%
  group_by(Year) %>%
  summarise(
    attack_count = n(),
    avg_loss = mean(Loss_imputed, na.rm = TRUE),
    avg_downtime = mean(DownTime_imputed, na.rm = TRUE)
  )

# 可视化
ggplot(yearly_summary, aes(x = Year, y = attack_count)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(size = 3) +
  labs(title = "年度网络攻击事件趋势",
       x = "年份", y = "攻击事件数") +
  theme_minimal()

# 2. 季节性分析 (提取月份)
data$Month <- as.numeric(format(data$Date, "%m"))

monthly_pattern <- data %>%
  group_by(Month) %>%
  summarise(avg_attacks = n() / n_distinct(Year))

# 3. 线性回归检验趋势显著性
trend_model <- lm(attack_count ~ Year, data = yearly_summary)
summary(trend_model)

# 4. Mann-Kendall 趋势检验 (非参数)
library(Kendall)
MannKendall(yearly_summary$attack_count)
```

**建议假设**:
- H₀: 网络攻击数量没有显著时间趋势
- H₁: 网络攻击数量存在显著增长趋势

---

### 3.2 成员 2: 地理分布与区域风险分析

**推荐研究方向**: 网络攻击的地理分布特征与区域风险评估

**建议目标**:
1. 识别网络攻击的高风险国家/地区
2. 比较不同地区的攻击影响程度
3. 分析地理因素与攻击特征的关联

**建议分析问题**:
- 哪些国家是网络攻击的主要目标？
- 不同国家的平均经济损失是否存在差异？
- 地区分布是否与攻击类型/黑客组织相关？

**推荐统计方法**:

```r
# 1. 国家攻击频率分布
country_summary <- data %>%
  group_by(Country_clean) %>%
  summarise(
    attack_count = n(),
    total_loss = sum(Loss_imputed, na.rm = TRUE),
    avg_loss = mean(Loss_imputed, na.rm = TRUE),
    avg_downtime = mean(DownTime_imputed, na.rm = TRUE)
  ) %>%
  arrange(desc(attack_count))

# Top 10 国家可视化
top10_countries <- head(country_summary, 10)
ggplot(top10_countries, aes(x = reorder(Country_clean, attack_count),
                             y = attack_count)) +
  geom_bar(stat = "identity", fill = "coral") +
  coord_flip() +
  labs(title = "Top 10 受攻击国家",
       x = "国家", y = "攻击次数") +
  theme_minimal()

# 2. ANOVA 检验不同国家损失差异
aov_loss_country <- aov(Loss_log ~ Country_clean, data = data)
summary(aov_loss_country)

# 事后检验 (如果显著)
TukeyHSD(aov_loss_country)

# 3. Kruskal-Wallis 非参数检验 (如数据非正态)
kruskal.test(Loss_imputed ~ Country_clean, data = data)

# 4. 卡方检验: 国家与黑客组织关联
chisq.test(table(data$Country_clean, data$Notify_clean))
```

**建议假设**:
- H₀: 不同国家的平均经济损失无显著差异
- H₁: 至少有一个国家的平均经济损失显著不同

---

### 3.3 成员 3: 技术脆弱性分析

**推荐研究方向**: Web 服务器类型与安全风险的关系

**建议目标**:
1. 分析不同 Web 服务器类型的被攻击频率
2. 评估服务器类型与攻击影响的关联
3. 识别高风险技术配置

**建议分析问题**:
- 哪种 Web 服务器最容易被攻击？
- 不同服务器类型的恢复时间是否有差异？
- 字符编码类型是否与安全风险相关？

**推荐统计方法**:

```r
# 1. 服务器类型分布
server_summary <- data %>%
  group_by(WebServer_clean) %>%
  summarise(
    attack_count = n(),
    pct = n() / nrow(data) * 100,
    avg_downtime = mean(DownTime_imputed, na.rm = TRUE),
    avg_loss = mean(Loss_imputed, na.rm = TRUE)
  ) %>%
  arrange(desc(attack_count))

# 可视化
ggplot(server_summary, aes(x = reorder(WebServer_clean, attack_count),
                            y = attack_count, fill = avg_loss)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "yellow", high = "red") +
  coord_flip() +
  labs(title = "Web 服务器攻击分布与平均损失",
       x = "服务器类型", y = "攻击次数",
       fill = "平均损失") +
  theme_minimal()

# 2. ANOVA 检验服务器类型与停机时间
aov_downtime_server <- aov(DownTime_log ~ WebServer_clean, data = data)
summary(aov_downtime_server)

# 3. 双因素分析: 服务器 × 国家
aov_two_way <- aov(Loss_log ~ WebServer_clean * Country_clean,
                   data = data[data$Country_clean %in% c("USA", "CHINA", "UK"),])
summary(aov_two_way)

# 4. 相关性分析: 编码类型与损失
encoding_loss <- data %>%
  group_by(Encoding) %>%
  summarise(avg_loss = mean(Loss_imputed, na.rm = TRUE))
```

**建议假设**:
- H₀: 不同 Web 服务器类型的平均停机时间无显著差异
- H₁: 至少有一种服务器类型的停机时间显著不同

---

### 3.4 成员 4: 威胁行为者与勒索行为分析

**推荐研究方向**: 黑客组织行为模式与勒索策略分析

**建议目标**:
1. 识别最活跃的黑客组织及其攻击特征
2. 分析勒索行为与实际损失的关系
3. 研究攻击者目标选择的模式

**建议分析问题**:
- 哪些黑客组织最为活跃？
- 勒索金额与实际经济损失是否相关？
- 不同黑客组织的目标选择有何差异？

**推荐统计方法**:

```r
# 1. 黑客组织活跃度分析
notify_summary <- data %>%
  filter(Notify_clean != "Other_Groups") %>%
  group_by(Notify_clean) %>%
  summarise(
    attack_count = n(),
    has_ransom_pct = mean(has_ransom_demand, na.rm = TRUE) * 100,
    avg_ransom = mean(Ransom_imputed, na.rm = TRUE),
    avg_loss = mean(Loss_imputed, na.rm = TRUE)
  ) %>%
  arrange(desc(attack_count))

# Top 黑客组织可视化
ggplot(head(notify_summary, 15),
       aes(x = reorder(Notify_clean, attack_count), y = attack_count)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  labs(title = "Top 15 活跃黑客组织",
       x = "组织名称", y = "攻击次数") +
  theme_minimal()

# 2. 相关性分析: 勒索 vs 损失
# 使用 Spearman (非正态数据)
cor_test <- cor.test(data$Ransom_imputed, data$Loss_imputed,
                     method = "spearman", use = "complete.obs")
print(cor_test)

# 散点图可视化
ggplot(data[!is.na(data$Ransom_imputed),],
       aes(x = Ransom_imputed, y = Loss_imputed)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  scale_x_log10() + scale_y_log10() +
  labs(title = "勒索金额 vs 实际损失 (Log 尺度)",
       x = "勒索金额", y = "经济损失") +
  theme_minimal()

# 3. t 检验: 有勒索 vs 无勒索的损失差异
t.test(Loss_imputed ~ has_ransom_demand, data = data)

# 4. 卡方检验: 黑客组织 vs 目标国家
top_notifiers <- head(notify_summary$Notify_clean, 10)
top_countries <- c("USA", "CHINA", "UK", "GERMANY", "RUSSIA")

subset_data <- data %>%
  filter(Notify_clean %in% top_notifiers,
         Country_clean %in% top_countries)

chisq.test(table(subset_data$Notify_clean, subset_data$Country_clean))
```

**建议假设**:
- H₀: 勒索金额与实际经济损失无显著相关
- H₁: 勒索金额与实际经济损失存在显著相关

---

## 4. 小组假设整合策略

### 4.1 综合假设构建方法

小组假设应整合所有成员的发现，形成一个多因素假设。

**推荐整合框架**:

```
成员 1 发现: 攻击数量随时间增加
成员 2 发现: 美国是主要攻击目标
成员 3 发现: Apache 服务器风险较高
成员 4 发现: 勒索与损失正相关

↓ 整合

小组综合假设:
"网络攻击的经济影响受到时间趋势、地理位置、
技术配置和勒索行为的共同影响，
其中现代攻击(2016年后)对美国的Apache服务器
在有勒索要求时造成的损失显著更高"
```

### 4.2 综合假设检验方法

```r
# 多元线性回归: 综合检验多因素影响
comprehensive_model <- lm(
  Loss_log ~ Year + Country_clean + WebServer_clean +
             has_ransom_demand + DownTime_log,
  data = data
)

summary(comprehensive_model)

# 模型诊断
par(mfrow = c(2, 2))
plot(comprehensive_model)

# 方差膨胀因子检验多重共线性
library(car)
vif(comprehensive_model)

# 模型比较 (简单 vs 复杂)
simple_model <- lm(Loss_log ~ Year, data = data)
anova(simple_model, comprehensive_model)

# 效应量计算
library(effectsize)
eta_squared(comprehensive_model)
```

### 4.3 结果整合表格模板

| 假设来源 | 原假设 | 检验方法 | p值 | 结论 | 贡献于综合假设 |
|----------|--------|----------|-----|------|----------------|
| 成员 1 | 时间趋势 | Mann-Kendall | <0.001 | 支持 | 时间效应 |
| 成员 2 | 地区差异 | ANOVA | <0.001 | 支持 | 地理效应 |
| 成员 3 | 服务器差异 | ANOVA | 0.023 | 支持 | 技术效应 |
| 成员 4 | 勒索相关 | Spearman | <0.001 | 支持 | 行为效应 |
| **小组** | **综合效应** | **多元回归** | <0.001 | **支持** | **R²=0.34** |

---

## 5. 高级分析技术

### 5.1 推荐高级方法

为展示超越课堂示例的深度，建议考虑以下高级技术：

| 技术 | 适用场景 | R 包 |
|------|----------|------|
| 主成分分析 (PCA) | 降维、模式识别 | `prcomp`, `FactoMineR` |
| 聚类分析 | 攻击事件分类 | `kmeans`, `hclust`, `cluster` |
| 时间序列分解 | 趋势与季节性分离 | `decompose`, `forecast` |
| 逻辑回归 | 二分类预测 | `glm` |
| 随机森林 | 特征重要性 | `randomForest` |

### 5.2 PCA 示例

```r
# 对数值变量进行 PCA
numeric_vars <- data[, c("Ransom_log", "DownTime_log", "Loss_log", "Year")]
numeric_vars <- na.omit(numeric_vars)

# 标准化并执行 PCA
pca_result <- prcomp(numeric_vars, scale. = TRUE)

# 查看方差解释比例
summary(pca_result)

# 可视化
library(factoextra)
fviz_pca_var(pca_result, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
```

### 5.3 聚类分析示例

```r
# K-means 聚类
set.seed(123)
scaled_data <- scale(numeric_vars)
kmeans_result <- kmeans(scaled_data, centers = 4, nstart = 25)

# 添加聚类标签
data$cluster <- as.factor(kmeans_result$cluster)

# 可视化聚类
fviz_cluster(kmeans_result, data = scaled_data,
             palette = "jco",
             ellipse.type = "convex")

# 聚类特征分析
data %>%
  group_by(cluster) %>%
  summarise(
    avg_loss = mean(Loss_imputed, na.rm = TRUE),
    avg_downtime = mean(DownTime_imputed, na.rm = TRUE),
    count = n()
  )
```

---

## 6. 可视化最佳实践

### 6.1 推荐可视化类型

| 分析目的 | 推荐图表 | ggplot2 函数 |
|----------|----------|--------------|
| 分布展示 | 直方图、密度图、箱线图 | `geom_histogram`, `geom_density`, `geom_boxplot` |
| 趋势分析 | 折线图、面积图 | `geom_line`, `geom_area` |
| 比较分析 | 条形图、分组箱线图 | `geom_bar`, `geom_boxplot` |
| 关系分析 | 散点图、热力图 | `geom_point`, `geom_tile` |
| 组成分析 | 饼图、堆叠条形图 | `geom_bar` + `coord_polar` |

### 6.2 可视化代码模板

```r
# 标准主题设置
my_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.position = "bottom"
  )

# 颜色调色板
my_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")

# 示例: 分组箱线图
ggplot(data, aes(x = Country_clean, y = Loss_log, fill = is_modern)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values = c("FALSE" = "#ff7f0e", "TRUE" = "#1f77b4"),
                    labels = c("遗留数据", "现代数据")) +
  labs(title = "不同国家的经济损失分布",
       subtitle = "按数据时期分组",
       x = "国家", y = "经济损失 (Log10)",
       fill = "数据时期") +
  my_theme +
  coord_flip()
```

### 6.3 报告图表规范

1. **标题**: 每张图必须有清晰、描述性的标题
2. **轴标签**: 使用完整、专业的轴标签（含单位）
3. **图例**: 位置一致，标签清晰
4. **颜色**: 使用色盲友好的调色板
5. **分辨率**: 导出为高清 PNG 或 PDF（至少 300 DPI）

---

## 7. 报告撰写指南

### 7.1 Chapter 1 迁移建议

`DATA_CLEANING_FULL_REPORT.md` 中的以下内容可直接迁移到 Chapter 1：

| 清洗报告章节 | 映射到作业章节 | 迁移内容 |
|--------------|----------------|----------|
| 1. 项目概述 | 1.1 项目描述 | 背景、目标 |
| 1.2/1.3 数据概况 | 1.2 数据描述 | 字段说明、文件描述 |
| 2-13 章全部 | 1.3 数据准备 | 完整清洗流程 |
| 各章决策表 | 1.4 假设 | 关键假设汇总 |

### 7.2 个人部分撰写模板

```markdown
## 2.X [学生姓名] - [TP号码]

### 2.X.1 研究目标

本部分分析旨在:
1. [目标1]
2. [目标2]
3. [目标3]

### 2.X.2 分析问题

针对上述目标，提出以下引导性问题:

**目标1相关问题:**
- Q1.1: [问题]
- Q1.2: [问题]

**目标2相关问题:**
- Q2.1: [问题]
- Q2.2: [问题]

### 2.X.3 数据准备

[描述个人分析所需的额外数据准备步骤]

```r
# 代码示例
...
```

**输出结果:**
[结果截图或表格]

**说明:** [为什么需要这个步骤]

### 2.X.4 数据分析

#### 分析1: [标题] (支持目标X)

[分析描述]

```r
# 代码
```

**结果:**
[图表/表格]

**解释:**
[详细解读]

### 2.X.5 假设检验

#### 假设1:

- **H₀**: [原假设]
- **H₁**: [备择假设]

```r
# 检验代码
```

**检验结果:**
- 统计量: X
- p值: Y
- 显著性水平: α = 0.05

**结论:** [接受/拒绝原假设的解释]
```

### 7.3 报告格式规范

| 元素 | 规范 |
|------|------|
| 字体 | Times New Roman |
| 字号 | 正文 12pt，标题可更大 |
| 行距 | 1.5 倍 |
| 页边距 | 2.54cm (1英寸) |
| 图表编号 | Figure X.Y / Table X.Y |
| 代码格式 | 等宽字体，带语法高亮 |
| 页码 | 底部居中 |

---

## 8. 代码质量与可重现性

### 8.1 R 脚本组织建议

```r
# ====================================================
# 项目: 网络攻击事件数据分析
# 作者: [姓名] - [TP号码]
# 日期: 2026-01-XX
# 描述: [脚本功能描述]
# ====================================================

# ----- 1. 环境设置 -----
rm(list = ls())  # 清空环境
setwd("path/to/project")  # 设置工作目录

# 加载包
required_packages <- c("tidyverse", "mice", "ggplot2", "car")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ----- 2. 数据加载 -----
load("raw_data/processed_data/mice_imputed_data.RData")

# ----- 3. 数据探索 -----
# [代码...]

# ----- 4. 统计分析 -----
# [代码...]

# ----- 5. 可视化 -----
# [代码...]

# ----- 6. 结果保存 -----
# [代码...]

# ====== 脚本结束 ======
```

### 8.2 注释规范

```r
# 好的注释示例:

# 计算每个国家的平均损失，用于后续 ANOVA 分析
country_loss <- data %>%
  group_by(Country_clean) %>%
  summarise(avg_loss = mean(Loss_imputed, na.rm = TRUE))

# 执行单因素 ANOVA 检验国家对损失的影响
# H0: 所有国家的平均损失相同
# H1: 至少有一个国家的平均损失不同
aov_result <- aov(Loss_log ~ Country_clean, data = data)
```

### 8.3 可重现性检查

```r
# 在脚本开头记录环境信息
cat("R 版本:", R.version.string, "\n")
cat("运行时间:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("工作目录:", getwd(), "\n")

# 设置随机种子
set.seed(42)

# 在脚本结尾保存会话信息
sink("session_info.txt")
sessionInfo()
sink()
```

---

## 9. 常见陷阱与规避

### 9.1 数据处理陷阱

| 陷阱 | 问题 | 规避方法 |
|------|------|----------|
| 使用原始 Ransom/DownTime/Loss | 包含异常值，统计失真 | 使用 `_imputed` 或 `_log` 变量 |
| 忽略遗留数据缺失 | Ransom 遗留缺失率 53.9% | 分层分析或明确说明限制 |
| 直接比较不同尺度变量 | 数值范围差异大 | 使用 Log 变换或标准化 |

### 9.2 统计分析陷阱

| 陷阱 | 问题 | 规避方法 |
|------|------|----------|
| 未检验正态性就用参数检验 | 结果可能无效 | 先用 Shapiro-Wilk 检验，必要时用非参数 |
| 多重比较未校正 | 假阳性率膨胀 | 使用 Bonferroni 或 Tukey 校正 |
| p 值万能论 | 忽视效应量和实际意义 | 报告效应量 (Cohen's d, η²) |
| 因果推断 | 相关不等于因果 | 谨慎措辞，说明是关联分析 |

### 9.3 可视化陷阱

| 陷阱 | 问题 | 规避方法 |
|------|------|----------|
| 截断 Y 轴 | 夸大差异 | 从零开始或明确说明 |
| 3D 图表 | 难以准确读取 | 使用 2D 图表 |
| 颜色过多 | 难以区分 | 限制在 5-7 种颜色内 |
| 信息过载 | 图表混乱 | 一图一主题 |

---

## 10. 提交前检查清单

### 10.1 R 脚本检查

- [ ] 脚本从头到尾无错误运行
- [ ] 所有包都有安装检查逻辑
- [ ] 文件路径使用相对路径或可配置
- [ ] 注释充分且专业
- [ ] 变量命名一致且有意义
- [ ] 随机种子已设置（如有随机操作）

### 10.2 报告检查

- [ ] 封面页完整（标题、成员信息、日期）
- [ ] 目录自动生成且页码正确
- [ ] 格式符合要求（字体、字号、行距）
- [ ] 所有图表有编号和标题
- [ ] 所有图表在正文中有引用
- [ ] R 代码片段格式正确
- [ ] 假设检验结果解释完整
- [ ] 参考文献 APA 格式
- [ ] 页数不超过 100 页

### 10.3 内容检查

- [ ] Chapter 1 数据准备详细完整
- [ ] 每个成员的个人分析独立且完整
- [ ] 小组假设整合了所有成员发现
- [ ] 结论基于数据分析，非臆测
- [ ] 局限性讨论诚实客观
- [ ] 工作量矩阵填写完整
- [ ] AI 使用声明填写完整

### 10.4 最终提交

- [ ] R 脚本已压缩为 `RScript_GroupX.zip`
- [ ] 报告已命名为 `AnalysisReport_GroupX.docx/pdf`
- [ ] 通过 Moodle 成功提交
- [ ] 保留提交确认截图

---

## 附录: 推荐资源

### 在线学习资源

| 资源 | 链接 | 用途 |
|------|------|------|
| R for Data Science | https://r4ds.had.co.nz | tidyverse 学习 |
| ggplot2 官方文档 | https://ggplot2.tidyverse.org | 可视化参考 |
| STHDA 统计教程 | http://www.sthda.com/english/ | 统计方法教程 |
| Stack Overflow | https://stackoverflow.com/questions/tagged/r | 问题解答 |

### 推荐 R 包

```r
# 数据处理
install.packages(c("tidyverse", "data.table", "lubridate"))

# 统计分析
install.packages(c("car", "effectsize", "broom"))

# 可视化
install.packages(c("ggplot2", "ggpubr", "patchwork", "scales"))

# 缺失数据
install.packages(c("mice", "naniar", "VIM"))

# 报告生成
install.packages(c("rmarkdown", "knitr", "kableExtra"))
```

---

**文档生成日期**: 2026年1月24日

**生成工具**: Claude Code Assistant

**状态**: 最佳实践路线图 v1.0
