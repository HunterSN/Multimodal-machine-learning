rm(list = ls())

library(dplyr)
library(tidyr)
library(class)
library(caret)
library(e1071)       # 用于SVM
library(gbm)         # 用于GBDT
library(klaR)        # 用于贝叶斯网络
library(pROC)        # 用于ROC曲线
library(ggplot2)     # 用于可视化

load("13_KNN_gradcam_weights.RData")
rm(list=ls()[!grepl('combined_data|TCMSPMOLTarget', ls())])

# Step 4-5: 加权聚合和归一化
aggregate_data <- combined_data %>%
  group_by(compound_id, target) %>%
  summarise(gradcam_weight = sum(gradcam_weight)) %>%
  mutate(normalized_weight = gradcam_weight / sum(gradcam_weight))

# Step 6: 合并理化特征
combined_data <- merge(aggregate_data, 
                       TCMSPMOLTarget[, c("molecule_name", "ob", "mw", "alogp", "caco2", "bbb", "hdon", "hacc", "FASA", "rbn", "tpsa")], 
                       by.x = "compound_id", 
                       by.y = "molecule_name") %>%
  distinct()

#### 数据预处理 (所有模型共用) ####
df_filtered <- combined_data %>%
  na.omit() %>%
  group_by(target) %>%
  filter(n() >= 2) %>%  # 移除样本数小于2的靶点
  ungroup()

# 特征与标签
features <- df_filtered[, c("normalized_weight", "ob", "mw", "alogp", "caco2", "bbb", "hdon", "hacc", "FASA", "rbn", "tpsa")]
features <- as.data.frame(lapply(features, as.numeric))
targets <- factor(df_filtered$target)  # 确保目标变量是因子

# 标准化
features_scaled <- scale(features)



load("14_herbMolTarget.RData")



library(openxlsx)
HYGenes = read.xlsx("DISEASES_Summary_GDA_CURATED_C0085580-C0020538.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)





save.image(file = "15_network.RData")







