# 数据分析最佳实践路线图 V2

## 基于小组作业报告草稿的针对性指南

---

## 文档信息

| 项目 | 详情 |
|------|------|
| **基于项目** | 黑客攻击事件数据分析 (APD2F2511) |
| **课程** | CT127-3-2-PFDA Programming for Data Analysis |
| **讲师** | Dr. Kulothunkan Palasundram |
| **截止日期** | 2026年2月7日 |
| **报告类型** | 针对性最佳实践路线图 V2 |
| **生成日期** | 2026年1月24日 |

---

## 小组成员与分工

| 编号 | 姓名 | TP号码 | 研究方向 |
|------|------|--------|----------|
| 1 | Choong Ti Huai | TP078539 | 地理分布 + 时间趋势 + 服务器脆弱性 |
| 2 | Daniel Chan Zit Fung | TP079018 | 服务器/编码脆弱性 + 国家年度分析 |
| 3 | Gan Yew Joe | TP077191 | 停机时间与攻击关系 |
| 4 | Liu Wei | TP085412 | 通知者行为 + URL/IP 脆弱性分析 |

---

## 目录

1. [当前进度评估](#1-当前进度评估)
2. [Chapter 1 完善指南](#2-chapter-1-完善指南)
3. [个人分析详细指南](#3-个人分析详细指南)
4. [小组假设整合建议](#4-小组假设整合建议)
5. [代码实现模板](#5-代码实现模板)
6. [报告撰写规范](#6-报告撰写规范)
7. [时间管理建议](#7-时间管理建议)

---

## 1. 当前进度评估

### 1.1 已完成工作

基于 `DATA_CLEANING_FULL_REPORT.md` 和当前报告草稿:

| 工作项 | 状态 | 完成度 |
|--------|------|--------|
| 数据合并 (rbind) | ✅ 完成 | 100% |
| 格式转换 | ✅ 完成 | 100% |
| 分布检查 | ✅ 完成 | 100% |
| 异常值检测 (MAD + 百分位数) | ✅ 完成 | 100% |
| 多变量异常值检测 (隔离森林) | ✅ 完成 | 100% |
| 数据健康检查 | ✅ 完成 | 100% |
| 分类变量清洗 | ✅ 完成 | 100% |
| MICE 多重插补 | ✅ 完成 | 100% |
| 插补验证 | ✅ 完成 | 100% |

### 1.2 待完成工作

| 章节 | 负责人 | 当前状态 | 紧急程度 |
|------|--------|----------|----------|
| 1.1 Project Description | 小组 | 框架已有，内容待填 | 🟡 中 |
| 1.2 Data Description | Daniel | 已有提示，需重写 | 🟡 中 |
| 1.3 Data Preparation | 小组 | 14步已列出，需扩展 | 🟢 低 |
| 1.4 Assumptions | 小组 | 部分完成 | 🟡 中 |
| 2.1 Ti Huai 分析 | Ti Huai | 目标已定，待实现 | 🔴 高 |
| 2.2 Daniel 分析 | Daniel | 目标已定，待实现 | 🔴 高 |
| 2.3 Yew Joe 分析 | Yew Joe | 目标已定，待实现 | 🔴 高 |
| 2.4 Liu Wei 分析 | Liu Wei | 目标已定，待实现 | 🔴 高 |
| 3.0 小组假设 | 小组 | 未开始 | 🔴 高 |
| 4.0 Summary | 小组 | 未开始 | 🟡 中 |

---

## 2. Chapter 1 完善指南

### 2.1 Project Description 建议内容

```markdown
## 1.1 Project Description

### 背景
网站篡改(Website Defacement)是一种常见的网络攻击形式，攻击者通过入侵
Web服务器来修改网站内容。本项目旨在分析来自全球的网站篡改事件数据，
识别攻击模式、高风险因素，并为网络安全决策提供数据驱动的洞察。

### 数据集概览
- **数据来源**: 三个分散的数据文件 (CSV, Excel, TXT)
- **总记录数**: 592,765 条网站篡改事件
- **时间跨度**: 1998年 - 2025年
- **地理覆盖**: 360+ 个国家/地区

### 项目目标
1. 识别网络攻击的地理和时间分布模式
2. 分析技术因素(服务器类型、编码)与攻击风险的关系
3. 探索攻击者行为特征与攻击影响的关联
4. 提出基于数据的网络安全建议
```

### 2.2 Data Description 重写建议

当前状态是直接复制了作业说明，需要重写为专业描述：

```markdown
## 1.2 Data Description

本数据集记录了全球范围内的网站篡改安全事件，每条记录代表一次独立的
攻击事件。数据集包含以下10个变量：

### 事件标识变量
| 变量 | 类型 | 描述 | 示例 |
|------|------|------|------|
| Date | Date | 篡改事件发生日期 | 2020-03-15 |
| URL | Character | 被篡改网站的完整URL | example.com/page |
| IP | Character | 受害服务器的IP地址 | 192.168.1.100 |

### 攻击者与受害者特征
| 变量 | 类型 | 描述 | 唯一值数 |
|------|------|------|----------|
| Notify | Character | 报告攻击的个人/组织名称 | 9,906 |
| Country | Character | 受害服务器所在国家 | 360 |

### 技术环境变量
| 变量 | 类型 | 描述 | 唯一值数 |
|------|------|------|----------|
| WebServer | Character | Web服务器软件及版本 | 177 |
| Encoding | Character | 篡改信息使用的字符编码 | 50 |

### 攻击影响变量
| 变量 | 类型 | 描述 | 单位 |
|------|------|------|------|
| Ransom | Numeric | 勒索金额 | 千美元 |
| DownTime | Numeric | 系统不可用时间 | 天 |
| Loss | Numeric | 攻击造成的经济损失 | 美元 |
```

### 2.3 Assumptions 补充建议

当前假设需要补充：

```markdown
## 1.4 Assumptions

在数据准备和分析过程中，我们做出了以下假设：

### 假设 1: 保留重复记录
**决策**: 不删除看似重复的记录
**理由**:
- Date 字段仅包含日期，不含时间戳
- 同一天、同一服务器可能遭受多次攻击
- 删除重复可能低估实际攻击频率

### 假设 2: Ransom 缺失值处理
**决策**: 遗留数据(2016年前)的 Ransom 缺失不进行插补
**理由**:
- 遗留数据 Ransom 缺失率高达 53.9%
- 超过50%的缺失率会导致插补结果不可靠
- 保持数据诚实性优于强制填充

### 假设 3: 缺失机制为 MAR
**决策**: 假设缺失数据为 Missing At Random (MAR)
**理由**:
- 卡方检验显示缺失率与 Country、WebServer 显著相关
- 缺失率与时间(Year)呈强负相关
- 无证据表明缺失与缺失值本身相关

### 假设 4: 负值为数据录入错误
**决策**: 将负值视为异常值并转换为 NA
**理由**:
- Ransom、DownTime、Loss 在业务逻辑上不可能为负
- 负值可能是数据录入时的符号错误

### 假设 5: 极端值为异常
**决策**: 使用 MAD 方法(阈值=10)识别极端值
**理由**:
- 数据严重右偏，Z-Score 不适用
- MAD 方法对偏斜数据更鲁棒
- 阈值=10 为保守选择，避免过度清洗

### 假设 6: 现代/遗留数据分界
**决策**: 以2016年为分界点区分数据质量
**理由**:
- 2016年前后数据完整度有明显断层
- 现代数据完整度 87.2%，遗留数据仅 45.1%
```

---

## 3. 个人分析详细指南

### 3.1 Choong Ti Huai (TP078539) - 地理+时间+服务器分析

#### 已定义目标

**描述性分析:**
1. 识别最常被攻击的国家、攻击者档案、IP范围和编码
2. 可视化攻击频率的时间趋势(日/周/月)

**诊断性分析:**
1. 分析特定 Web 服务器技术是否与更高的攻击频率或严重性相关

#### 建议分析问题

```markdown
### 2.1.2 Analysis Questions

**目标1相关问题 (地理分布):**
- Q1.1: 哪10个国家是网络攻击的主要目标？
- Q1.2: 不同国家的平均经济损失是否存在显著差异？
- Q1.3: 攻击者(Notify)是否有特定的目标国家偏好？

**目标2相关问题 (时间趋势):**
- Q2.1: 网络攻击事件数量是否呈逐年增长趋势？
- Q2.2: 一周中哪天攻击最频繁？是否存在周末效应？
- Q2.3: 一年中哪些月份是攻击高峰期？

**目标3相关问题 (服务器脆弱性):**
- Q3.1: 哪种 Web 服务器类型被攻击次数最多？
- Q3.2: 不同服务器类型的平均停机时间是否有差异？
- Q3.3: 服务器类型与经济损失之间是否存在关联？
```

#### 推荐代码实现

```r
# ===== 2.1 Ti Huai 分析代码 =====

# ----- 目标1: 地理分布分析 -----

# Q1.1: Top 10 受攻击国家
top10_countries <- data %>%
  group_by(Country_clean) %>%
  summarise(
    attack_count = n(),
    total_loss = sum(Loss_imputed, na.rm = TRUE),
    avg_loss = mean(Loss_imputed, na.rm = TRUE),
    avg_downtime = mean(DownTime_imputed, na.rm = TRUE)
  ) %>%
  arrange(desc(attack_count)) %>%
  head(10)

# 可视化
ggplot(top10_countries, aes(x = reorder(Country_clean, attack_count),
                             y = attack_count, fill = avg_loss)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "yellow", high = "red",
                      labels = scales::dollar) +
  coord_flip() +
  labs(title = "Top 10 Most Targeted Countries",
       subtitle = "Color indicates average economic loss",
       x = "Country", y = "Number of Attacks",
       fill = "Avg Loss ($)") +
  theme_minimal()

# Q1.2: ANOVA 检验国家间损失差异
aov_country_loss <- aov(Loss_log ~ Country_clean, data = data)
summary(aov_country_loss)

# 效应量
library(effectsize)
eta_squared(aov_country_loss)

# ----- 目标2: 时间趋势分析 -----

# 提取时间特征
data$Year <- as.numeric(format(data$Date, "%Y"))
data$Month <- as.numeric(format(data$Date, "%m"))
data$Weekday <- weekdays(data$Date)

# Q2.1: 年度趋势
yearly_trend <- data %>%
  group_by(Year) %>%
  summarise(attack_count = n())

# Mann-Kendall 趋势检验
library(Kendall)
mk_result <- MannKendall(yearly_trend$attack_count)
print(mk_result)

# 可视化年度趋势
ggplot(yearly_trend, aes(x = Year, y = attack_count)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  labs(title = "Annual Trend of Cyber Attacks",
       x = "Year", y = "Number of Attacks") +
  theme_minimal()

# Q2.2: 周几效应
weekday_pattern <- data %>%
  group_by(Weekday) %>%
  summarise(avg_attacks = n() / n_distinct(Date))

# Q2.3: 月度模式
monthly_pattern <- data %>%
  group_by(Month) %>%
  summarise(avg_attacks = n() / n_distinct(Year))

# ----- 目标3: 服务器脆弱性 -----

# Q3.1: 服务器攻击频率
server_attacks <- data %>%
  group_by(WebServer_clean) %>%
  summarise(
    attack_count = n(),
    avg_downtime = mean(DownTime_imputed, na.rm = TRUE),
    avg_loss = mean(Loss_imputed, na.rm = TRUE)
  ) %>%
  arrange(desc(attack_count))

# Q3.2: ANOVA 检验服务器与停机时间
aov_server_downtime <- aov(DownTime_log ~ WebServer_clean, data = data)
summary(aov_server_downtime)

# 事后检验
TukeyHSD(aov_server_downtime)

# ----- 假设检验 -----

# 假设1: 不同国家的平均损失存在显著差异
# H0: μ_USA = μ_China = μ_UK = ...
# H1: 至少有一个国家的均值不同

# 已通过 aov_country_loss 检验
# 如果 p < 0.05，拒绝 H0

# 假设2: 攻击数量随时间显著增加
# H0: 无时间趋势 (tau = 0)
# H1: 存在时间趋势 (tau ≠ 0)
# 已通过 Mann-Kendall 检验
```

#### 建议假设

| 假设编号 | 原假设 (H₀) | 备择假设 (H₁) | 检验方法 |
|----------|-------------|---------------|----------|
| H1 | 不同国家的平均损失无显著差异 | 至少一个国家损失不同 | One-way ANOVA |
| H2 | 攻击数量无时间趋势 | 存在显著时间趋势 | Mann-Kendall |
| H3 | 服务器类型与停机时间无关联 | 存在显著关联 | One-way ANOVA |

---

### 3.2 Daniel Chan Zit Fung (TP079018) - 服务器/编码+国家年度分析

#### 已定义目标

**描述性分析:**
1. 调查哪种 Web 服务器和编码方法最易受攻击
2. 识别不同国家每年的攻击频率、总损失和支付金额

#### 建议分析问题

```markdown
### 2.2.2 Analysis Questions

**目标1相关问题 (技术脆弱性):**
- Q1.1: 哪种 Web 服务器被攻击次数最多？
- Q1.2: 哪种字符编码与高攻击频率相关？
- Q1.3: 服务器类型与编码类型之间是否存在交互效应？

**目标2相关问题 (国家年度分析):**
- Q2.1: 各主要国家的年度攻击趋势如何？
- Q2.2: 哪些国家的总损失最高？
- Q2.3: 勒索支付率在不同国家间是否有差异？
```

#### 推荐代码实现

```r
# ===== 2.2 Daniel 分析代码 =====

# ----- 目标1: 技术脆弱性分析 -----

# Q1.1: Web 服务器攻击分布
server_vuln <- data %>%
  group_by(WebServer_clean) %>%
  summarise(
    attack_count = n(),
    pct = n() / nrow(data) * 100
  ) %>%
  arrange(desc(attack_count))

# 饼图可视化 (Top 5 + Other)
top5_servers <- head(server_vuln, 5)
other_servers <- data.frame(
  WebServer_clean = "Other",
  attack_count = sum(server_vuln$attack_count[6:nrow(server_vuln)]),
  pct = sum(server_vuln$pct[6:nrow(server_vuln)])
)
pie_data <- rbind(top5_servers, other_servers)

ggplot(pie_data, aes(x = "", y = pct, fill = WebServer_clean)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Web Server Distribution in Attacks",
       fill = "Server Type") +
  theme_void() +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5))

# Q1.2: 编码类型分析
encoding_analysis <- data %>%
  group_by(Encoding) %>%
  summarise(
    attack_count = n(),
    avg_loss = mean(Loss_imputed, na.rm = TRUE)
  ) %>%
  arrange(desc(attack_count))

# Q1.3: 双因素分析 - 服务器 × 编码
# 选择 Top 5 服务器和 Top 5 编码
top5_enc <- head(encoding_analysis$Encoding, 5)
interaction_data <- data %>%
  filter(WebServer_clean %in% head(server_vuln$WebServer_clean, 5),
         Encoding %in% top5_enc)

# 双因素 ANOVA
aov_interaction <- aov(Loss_log ~ WebServer_clean * Encoding,
                       data = interaction_data)
summary(aov_interaction)

# 热力图可视化
interaction_summary <- interaction_data %>%
  group_by(WebServer_clean, Encoding) %>%
  summarise(attack_count = n(), .groups = "drop")

ggplot(interaction_summary, aes(x = WebServer_clean, y = Encoding,
                                 fill = attack_count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkred") +
  labs(title = "Attack Frequency by Server Type and Encoding",
       x = "Web Server", y = "Encoding", fill = "Attacks") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ----- 目标2: 国家年度分析 -----

# Q2.1: 主要国家年度趋势
top5_countries <- c("USA", "CHINA", "UK", "GERMANY", "RUSSIA")

country_yearly <- data %>%
  filter(Country_clean %in% top5_countries) %>%
  group_by(Country_clean, Year) %>%
  summarise(
    attack_count = n(),
    total_loss = sum(Loss_imputed, na.rm = TRUE),
    ransom_paid = sum(Ransom_imputed, na.rm = TRUE),
    .groups = "drop"
  )

# 分面折线图
ggplot(country_yearly, aes(x = Year, y = attack_count, color = Country_clean)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~Country_clean, scales = "free_y") +
  labs(title = "Annual Attack Trends by Country",
       x = "Year", y = "Number of Attacks") +
  theme_minimal() +
  theme(legend.position = "none")

# Q2.2: 国家总损失排名
country_loss <- data %>%
  group_by(Country_clean) %>%
  summarise(
    total_loss = sum(Loss_imputed, na.rm = TRUE),
    attack_count = n()
  ) %>%
  arrange(desc(total_loss)) %>%
  head(10)

# Q2.3: 勒索支付率分析
ransom_rate <- data %>%
  filter(Country_clean %in% top5_countries) %>%
  group_by(Country_clean) %>%
  summarise(
    total_attacks = n(),
    paid_ransom = sum(Ransom_imputed > 0, na.rm = TRUE),
    payment_rate = paid_ransom / total_attacks * 100
  )

# 卡方检验: 支付率是否因国家而异
ransom_table <- data %>%
  filter(Country_clean %in% top5_countries) %>%
  mutate(paid = Ransom_imputed > 0) %>%
  with(table(Country_clean, paid))

chisq.test(ransom_table)
```

#### 建议假设

| 假设编号 | 原假设 (H₀) | 备择假设 (H₁) | 检验方法 |
|----------|-------------|---------------|----------|
| H1 | 服务器类型与编码无交互效应 | 存在交互效应 | Two-way ANOVA |
| H2 | 不同国家的勒索支付率无差异 | 支付率因国家而异 | Chi-square |

---

### 3.3 Gan Yew Joe (TP077191) - 停机时间与攻击关系

#### 已定义目标

**描述性分析:**
1. 识别停机时间长度与通知者被攻击次数的关系
2. 调查停机次数是否会导致攻击

#### 建议分析问题

```markdown
### 2.3.2 Analysis Questions

**目标1相关问题 (停机时间分析):**
- Q1.1: 停机时间的分布特征是什么？
- Q1.2: 哪些因素与更长的停机时间相关？
- Q1.3: 不同严重程度的攻击，停机时间是否不同？

**目标2相关问题 (攻击频率分析):**
- Q2.1: 频繁被攻击的目标(URL/IP)是否有更长的累计停机时间？
- Q2.2: 停机时间与经济损失之间是否存在线性关系？
- Q2.3: 攻击频率与损失之间是否存在关联？
```

#### 推荐代码实现

```r
# ===== 2.3 Yew Joe 分析代码 =====

# ----- 目标1: 停机时间分析 -----

# Q1.1: 停机时间分布
summary(data$DownTime_imputed)

# 分布可视化
p1 <- ggplot(data, aes(x = DownTime_imputed)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Downtime",
       x = "Downtime (days)", y = "Frequency") +
  theme_minimal()

p2 <- ggplot(data, aes(x = DownTime_log)) +
  geom_histogram(bins = 50, fill = "coral", color = "white") +
  labs(title = "Distribution of Downtime (Log Scale)",
       x = "Log10(Downtime + 1)", y = "Frequency") +
  theme_minimal()

library(patchwork)
p1 + p2

# Q1.2: 影响停机时间的因素
# 多元回归分析
downtime_model <- lm(DownTime_log ~ Country_clean + WebServer_clean +
                      has_ransom_demand + Year, data = data)
summary(downtime_model)

# Q1.3: 按损失严重程度分析停机时间
downtime_by_severity <- data %>%
  filter(!is.na(loss_severity)) %>%
  group_by(loss_severity) %>%
  summarise(
    avg_downtime = mean(DownTime_imputed, na.rm = TRUE),
    median_downtime = median(DownTime_imputed, na.rm = TRUE),
    sd_downtime = sd(DownTime_imputed, na.rm = TRUE)
  )

# Kruskal-Wallis 检验 (非参数)
kruskal.test(DownTime_imputed ~ loss_severity, data = data)

# ----- 目标2: 攻击频率分析 -----

# Q2.1: 频繁被攻击目标的累计停机时间
# 按 IP 统计
ip_analysis <- data %>%
  group_by(IP) %>%
  summarise(
    attack_count = n(),
    total_downtime = sum(DownTime_imputed, na.rm = TRUE),
    avg_downtime = mean(DownTime_imputed, na.rm = TRUE)
  ) %>%
  filter(attack_count > 1) %>%  # 仅分析被多次攻击的IP
  arrange(desc(attack_count))

# 相关性分析
cor.test(ip_analysis$attack_count, ip_analysis$total_downtime,
         method = "spearman")

# Q2.2: 停机时间与损失的关系
cor_test <- cor.test(data$DownTime_log, data$Loss_log,
                     method = "pearson", use = "complete.obs")
print(cor_test)

# 散点图 + 回归线
ggplot(data, aes(x = DownTime_log, y = Loss_log)) +
  geom_point(alpha = 0.1, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship between Downtime and Loss",
       subtitle = paste("Pearson r =", round(cor_test$estimate, 3)),
       x = "Log10(Downtime + 1)", y = "Log10(Loss + 1)") +
  theme_minimal()

# 线性回归
loss_downtime_model <- lm(Loss_log ~ DownTime_log, data = data)
summary(loss_downtime_model)

# Q2.3: 攻击频率与损失
# 按 Notify 统计
notify_impact <- data %>%
  group_by(Notify_clean) %>%
  summarise(
    attack_count = n(),
    total_loss = sum(Loss_imputed, na.rm = TRUE),
    avg_loss = mean(Loss_imputed, na.rm = TRUE)
  ) %>%
  filter(Notify_clean != "Other_Groups")

cor.test(notify_impact$attack_count, notify_impact$total_loss,
         method = "spearman")
```

#### 建议假设

| 假设编号 | 原假设 (H₀) | 备择假设 (H₁) | 检验方法 |
|----------|-------------|---------------|----------|
| H1 | 停机时间与损失无线性相关 | 存在显著正相关 | Pearson 相关 |
| H2 | 不同损失等级的停机时间无差异 | 存在显著差异 | Kruskal-Wallis |

---

### 3.4 Liu Wei (TP085412) - 通知者行为+URL/IP分析

#### 已定义目标

**目标 4.1**: 分析不同 Notifiers 的报告频率、时间模式和地理覆盖

**目标 4.2**: 探索 URL 和 IP 地址的分布，识别脆弱性特征

#### 建议分析问题

```markdown
### 2.4.2 Analysis Questions

**目标1相关问题 (通知者行为):**
- Q1.1: 哪些通知者最活跃？他们的攻击频率如何？
- Q1.2: 不同通知者的攻击是否有地理偏好？
- Q1.3: 不同通知者造成的平均损失是否有差异？

**目标2相关问题 (URL/IP脆弱性):**
- Q2.1: 哪些顶级域名(TLD)最容易被攻击？
- Q2.2: 被重复攻击的 IP 有什么特征？
- Q2.3: URL/IP 的脆弱性是否与服务器类型相关？
```

#### 推荐代码实现

```r
# ===== 2.4 Liu Wei 分析代码 =====

# ----- 目标1: 通知者行为分析 -----

# Q1.1: 最活跃的通知者
notify_activity <- data %>%
  filter(Notify_clean != "Other_Groups") %>%
  group_by(Notify_clean) %>%
  summarise(
    attack_count = n(),
    first_attack = min(Date, na.rm = TRUE),
    last_attack = max(Date, na.rm = TRUE),
    active_years = as.numeric(difftime(last_attack, first_attack,
                                        units = "days")) / 365,
    avg_ransom = mean(Ransom_imputed, na.rm = TRUE),
    avg_loss = mean(Loss_imputed, na.rm = TRUE)
  ) %>%
  arrange(desc(attack_count)) %>%
  head(20)

# 可视化: Top 20 活跃通知者
ggplot(notify_activity, aes(x = reorder(Notify_clean, attack_count),
                             y = attack_count)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  labs(title = "Top 20 Most Active Notifiers",
       x = "Notifier", y = "Number of Attacks") +
  theme_minimal()

# Q1.2: 通知者的地理偏好 (卡方检验)
top10_notifiers <- head(notify_activity$Notify_clean, 10)
top5_countries <- c("USA", "CHINA", "UK", "GERMANY", "BRAZIL")

geo_preference <- data %>%
  filter(Notify_clean %in% top10_notifiers,
         Country_clean %in% top5_countries)

# 列联表
contingency_table <- table(geo_preference$Notify_clean,
                           geo_preference$Country_clean)

# 卡方检验
chisq_result <- chisq.test(contingency_table)
print(chisq_result)

# 马赛克图可视化
library(vcd)
mosaic(contingency_table, shade = TRUE,
       main = "Notifier-Country Association")

# Q1.3: 通知者损失差异 (ANOVA)
notify_loss_data <- data %>%
  filter(Notify_clean %in% top10_notifiers)

aov_notify_loss <- aov(Loss_log ~ Notify_clean, data = notify_loss_data)
summary(aov_notify_loss)

# ----- 目标2: URL/IP 脆弱性分析 -----

# Q2.1: TLD 攻击分布
tld_analysis <- data %>%
  group_by(URL_suffix) %>%
  summarise(
    attack_count = n(),
    pct = n() / nrow(data) * 100,
    avg_loss = mean(Loss_imputed, na.rm = TRUE)
  ) %>%
  arrange(desc(attack_count))

# 可视化
ggplot(head(tld_analysis, 10),
       aes(x = reorder(URL_suffix, attack_count), y = attack_count)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Attack Distribution by Top-Level Domain",
       x = "TLD", y = "Number of Attacks") +
  theme_minimal()

# Q2.2: 重复被攻击的 IP 分析
ip_repeat <- data %>%
  group_by(IP) %>%
  summarise(
    attack_count = n(),
    countries = n_distinct(Country_clean),
    servers = n_distinct(WebServer_clean),
    total_loss = sum(Loss_imputed, na.rm = TRUE)
  ) %>%
  filter(attack_count > 5) %>%  # 被攻击超过5次
  arrange(desc(attack_count))

# 重复攻击 IP 的特征
summary(ip_repeat)

# Q2.3: URL/IP 与服务器关联
# 检验 TLD 与 WebServer 的关联
tld_server <- data %>%
  filter(URL_suffix %in% c(".com", ".org", ".net", ".gov", ".edu"),
         WebServer_clean %in% c("Apache", "nginx", "Microsoft-IIS"))

chisq.test(table(tld_server$URL_suffix, tld_server$WebServer_clean))

# ----- 假设检验 -----

# 假设: 不同通知者的目标国家选择存在显著关联
# H0: 通知者与国家独立
# H1: 通知者与国家存在关联
# 通过卡方检验 chisq_result
```

#### 建议假设

| 假设编号 | 原假设 (H₀) | 备择假设 (H₁) | 检验方法 |
|----------|-------------|---------------|----------|
| H1 | 通知者与目标国家独立 | 存在显著关联 | Chi-square |
| H2 | 不同通知者的平均损失无差异 | 存在显著差异 | One-way ANOVA |
| H3 | TLD 与服务器类型独立 | 存在显著关联 | Chi-square |

---

## 4. 小组假设整合建议

### 4.1 综合假设构建

基于四位成员的研究方向，建议构建以下综合假设：

```
成员发现整合:
├── Ti Huai: 国家、时间、服务器影响攻击模式
├── Daniel: 服务器×编码交互、国家年度差异
├── Yew Joe: 停机时间与损失正相关
└── Liu Wei: 通知者有地理偏好

↓ 整合为综合假设

综合假设:
"网络攻击的经济损失受到多因素共同影响，包括：
目标国家(地理因素)、Web服务器类型(技术因素)、
攻击者特征(威胁因素)和停机时间(影响因素)，
其中这些因素之间存在交互效应"
```

### 4.2 综合假设检验代码

```r
# ===== 3.0 小组假设检验 =====

# 综合多元回归模型
comprehensive_model <- lm(
  Loss_log ~ Country_clean + WebServer_clean + Notify_clean +
             DownTime_log + Year + has_ransom_demand,
  data = data
)

summary(comprehensive_model)

# 模型诊断
par(mfrow = c(2, 2))
plot(comprehensive_model)

# 方差膨胀因子 (多重共线性检验)
library(car)
vif(comprehensive_model)

# 效应量
library(effectsize)
eta_squared(comprehensive_model)

# 模型比较: 逐步添加变量
model_1 <- lm(Loss_log ~ Country_clean, data = data)
model_2 <- lm(Loss_log ~ Country_clean + WebServer_clean, data = data)
model_3 <- lm(Loss_log ~ Country_clean + WebServer_clean + DownTime_log,
              data = data)
model_4 <- comprehensive_model

# ANOVA 模型比较
anova(model_1, model_2, model_3, model_4)

# AIC/BIC 比较
AIC(model_1, model_2, model_3, model_4)
BIC(model_1, model_2, model_3, model_4)
```

### 4.3 结果整合表格

```markdown
## 3.1 Group Hypothesis Formulation

本综合假设整合了全部四位成员的个人发现：

| 成员 | 个人假设核心发现 | 对综合假设的贡献 |
|------|------------------|------------------|
| Ti Huai | 国家间损失差异显著 | 地理因素 |
| Daniel | 服务器×编码存在交互 | 技术因素 |
| Yew Joe | 停机时间与损失正相关 | 影响因素 |
| Liu Wei | 通知者有目标偏好 | 威胁因素 |

**综合假设:**

H₀: 地理因素、技术因素、威胁因素和影响因素对经济损失无显著联合影响

H₁: 上述因素对经济损失存在显著联合影响

## 3.2 Group Hypothesis Testing

检验方法: 多元线性回归

| 预测变量 | 系数 | 标准误 | t值 | p值 | 显著性 |
|----------|------|--------|-----|-----|--------|
| (Intercept) | X.XX | X.XX | X.XX | <0.001 | *** |
| Country_USA | X.XX | X.XX | X.XX | X.XXX | * |
| WebServer_Apache | X.XX | X.XX | X.XX | X.XXX | ** |
| DownTime_log | X.XX | X.XX | X.XX | <0.001 | *** |
| ... | ... | ... | ... | ... | ... |

模型整体检验:
- F统计量: XXX.X
- p值: < 2.2e-16
- R²: 0.XX (解释了XX%的方差)
- Adjusted R²: 0.XX

## 3.3 Overall Conclusion

1. **拒绝原假设**: 综合模型显示多因素对损失有显著联合影响
2. **最重要因素**: DownTime_log (最大 β 系数)
3. **地理效应**: 美国、中国损失显著高于其他国家
4. **技术效应**: Apache 服务器相关损失较高
5. **实践意义**: 网络安全策略应同时考虑技术、地理和威胁因素
```

---

## 5. 代码实现模板

### 5.1 统一代码风格

```r
# ====================================================
# 文件: 02_analysis_[name].R
# 作者: [姓名] - [TP号码]
# 日期: 2026-01-XX
# 描述: [简要描述]
# ====================================================

# ----- 1. 环境设置 -----
# 清空环境 (可选，调试时使用)
# rm(list = ls())

# 加载必要包
library(tidyverse)  # 数据处理和可视化
library(car)        # 统计检验
library(effectsize) # 效应量计算

# 设置随机种子
set.seed(42)

# ----- 2. 数据加载 -----
load("raw_data/processed_data/mice_imputed_data.RData")

# 确认数据加载
cat("数据集维度:", dim(data), "\n")
cat("变量名:", names(data), "\n")

# ----- 3. 个人数据准备 (如需) -----
# [你的额外数据准备代码]

# ----- 4. 描述性分析 -----
# 4.1 目标1分析
# [代码]

# 4.2 目标2分析
# [代码]

# ----- 5. 假设检验 -----
# 5.1 假设1
# [代码]

# 5.2 假设2
# [代码]

# ----- 6. 可视化导出 -----
# 保存图表
ggsave("docs/reports/figures/[name]_fig1.png", width = 10, height = 6, dpi = 300)

# ----- 7. 结果汇总 -----
# [汇总代码]

# ====== 脚本结束 ======
cat("分析完成时间:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
```

### 5.2 可视化模板

```r
# 统一可视化主题
my_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# 统一颜色方案
my_colors <- c(
  "primary" = "#1f77b4",
  "secondary" = "#ff7f0e",
  "success" = "#2ca02c",
  "danger" = "#d62728",
  "warning" = "#9467bd"
)

# 使用示例
ggplot(data, aes(x = var1, y = var2)) +
  geom_point(color = my_colors["primary"], alpha = 0.5) +
  labs(title = "标题", subtitle = "副标题", x = "X轴", y = "Y轴") +
  my_theme
```

---

## 6. 报告撰写规范

### 6.1 格式要求回顾

| 元素 | 要求 |
|------|------|
| 字体 | Times New Roman |
| 字号 | 12pt |
| 行距 | 1.5 倍 |
| 页数限制 | 100 页以内 |
| 文件格式 | .docx 或 .pdf |

### 6.2 代码片段格式

在 Word 中插入代码时:

1. 使用等宽字体 (Courier New 或 Consolas)
2. 字号可略小 (10pt)
3. 添加灰色背景或边框
4. 包含输出结果

示例:
```
[灰色背景框]
# 计算各国攻击频率
country_attacks <- data %>%
  group_by(Country_clean) %>%
  summarise(count = n())

# 输出:
# # A tibble: 31 × 2
#    Country_clean  count
#    <fct>          <int>
#  1 USA           178234
#  2 CHINA          89123
#  ...
```

### 6.3 图表规范

每个图表必须包含:
1. **编号**: Figure 2.1.1, Table 2.1.1
2. **标题**: 描述性标题
3. **来源说明**: Source: Author's analysis
4. **正文引用**: 在正文中引用 (见 Figure 2.1.1)

### 6.4 假设检验结果报告模板

```markdown
#### 假设检验 1: 国家间损失差异

**假设陈述:**
- H₀: 不同国家的平均经济损失无显著差异 (μ₁ = μ₂ = ... = μₖ)
- H₁: 至少有一个国家的平均损失与其他国家不同

**检验方法:** 单因素方差分析 (One-way ANOVA)

**检验前提:**
- 独立性: 各组观测独立 ✓
- 正态性: 使用 Log 变换后近似正态 ✓
- 方差齐性: Levene 检验 p = 0.XXX ✓

**R 代码:**
```r
aov_result <- aov(Loss_log ~ Country_clean, data = data)
summary(aov_result)
```

**结果:**
| Source | df | Sum Sq | Mean Sq | F value | Pr(>F) |
|--------|-----|--------|---------|---------|--------|
| Country | 30 | XXX.X | XX.XX | 89.3 | <2e-16 *** |
| Residuals | 592734 | XXXX.X | X.XX | | |

**效应量:** η² = 0.XX (中等效应)

**结论:**
F(30, 592734) = 89.3, p < 0.001, η² = 0.XX

在 α = 0.05 的显著性水平下，我们拒绝原假设。
统计证据表明不同国家的平均经济损失存在显著差异。
事后检验显示美国和中国的损失显著高于其他国家。
```

---

## 7. 时间管理建议

### 7.1 剩余时间估算

```
当前日期: 2026年1月24日
截止日期: 2026年2月7日
剩余时间: 14 天
```

### 7.2 建议时间分配

| 阶段 | 任务 | 建议时间 | 截止日期 |
|------|------|----------|----------|
| Phase 1 | 个人分析完成 | 5 天 | 1月29日 |
| Phase 2 | 小组假设整合 | 3 天 | 2月1日 |
| Phase 3 | 报告撰写 | 4 天 | 2月5日 |
| Phase 4 | 审校与提交 | 2 天 | 2月7日 |

### 7.3 每日任务建议

**1月24-29日 (个人分析):**
- Day 1-2: 完成数据探索和描述性统计
- Day 3-4: 完成假设检验
- Day 5: 完成可视化和个人报告初稿

**1月30日-2月1日 (小组整合):**
- Day 6: 个人发现汇报会议
- Day 7: 构建综合假设
- Day 8: 完成综合假设检验

**2月2-5日 (报告撰写):**
- Day 9-10: 各成员撰写个人部分
- Day 11: 整合 Chapter 1 和 Chapter 3
- Day 12: 完成 Chapter 4 和格式化

**2月6-7日 (审校):**
- Day 13: 全员审校，交叉检查
- Day 14: 最终提交

---

## 附录: 快速参考

### A. 常用统计检验速查

| 研究问题 | 推荐检验 | R 函数 |
|----------|----------|--------|
| 两组均值比较 | t 检验 | `t.test()` |
| 多组均值比较 | ANOVA | `aov()` |
| 两分类变量关联 | 卡方检验 | `chisq.test()` |
| 两数值变量相关 | 相关分析 | `cor.test()` |
| 时间趋势 | Mann-Kendall | `Kendall::MannKendall()` |
| 多因素影响 | 多元回归 | `lm()` |

### B. 效应量解释标准

| 效应量 | 小 | 中 | 大 |
|--------|-----|-----|-----|
| Cohen's d | 0.2 | 0.5 | 0.8 |
| η² | 0.01 | 0.06 | 0.14 |
| r | 0.1 | 0.3 | 0.5 |
| R² | 0.02 | 0.13 | 0.26 |

### C. p 值报告规范

| p 值范围 | 报告方式 | 符号 |
|----------|----------|------|
| p ≥ 0.05 | 报告精确值 | ns |
| 0.01 ≤ p < 0.05 | p < 0.05 | * |
| 0.001 ≤ p < 0.01 | p < 0.01 | ** |
| p < 0.001 | p < 0.001 | *** |

---

**文档生成日期**: 2026年1月24日

**生成工具**: Claude Code Assistant

**版本**: V2.0 (基于实际报告草稿)
