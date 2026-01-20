# 项目文档文件夹

本文件夹包含项目的所有报告和文档文件。

## 文件夹结构

```
docs/
├── README.md                          # 本文件
├── outlier_detect.md                  # 异常值检测方法说明文档
├── DATA_CLEANING_PIPELINE_SUMMARY.md  # 数据清洗流程总结
├── DATA_CLEANING_REPORT_TEAM_SUMMARY.md # 团队数据清洗报告总结
├── EXECUTIVE_SUMMARY_ONE_PAGE.md      # 执行摘要（一页版）
├── IMPUTATION_VALIDATION_SUMMARY.md    # 插补验证总结
└── reports/                           # 报告文件子文件夹
    ├── *.pdf                          # 所有PDF可视化报告
    └── *.csv                          # 所有CSV统计报告
```

## 报告文件说明

### PDF可视化报告
- `distribution_diagnostics.pdf` - 分布诊断图
- `missing_data_analysis.pdf` - 缺失数据分析
- `multivariate_outlier_analysis.pdf` - 多元异常值分析
- `post_cleaning_distribution.pdf` - 清洗后分布图
- `final_association_check.pdf` - 最终关联性检查
- `final_missing_patterns.pdf` - 最终缺失模式
- `imputation_validation_plots.pdf` - 插补验证图表
- `before_after_comparison.pdf` - 清洗前后对比

### CSV统计报告
- `outlier_detection_summary.csv` - 异常值检测摘要
- `outlier_removal_report.csv` - 异常值移除报告
- `multivariate_outlier_summary.csv` - 多元异常值摘要
- `multivariate_outlier_removal_report.csv` - 多元异常值移除报告
- `categorical_cleaning_summary.csv` - 分类变量清洗摘要
- `before_after_statistics.csv` - 清洗前后统计对比

## 数据文件位置

所有数据文件（RData、CSV数据文件）保存在 `raw_data/processed_data/` 文件夹中。

## 作者

LiuWei TP085412
