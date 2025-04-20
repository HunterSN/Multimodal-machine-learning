rm(list = ls())

load('16_goGeneSim.RData')

library(proxy)
library(dplyr)
library(tibble)
library(tidyverse)
library(Matrix)

compound_targets <- df_filtered[,1:2]
disease_targets <- as.character(HYGenes$gene)
# 步骤1：创建特征矩阵 ----------------------------------------------------------
# 获取所有唯一靶点
all_targets <- union(unique(compound_targets$target), disease_targets) %>% sort()



compound_matrix <- compound_targets %>%
  distinct(compound_id, target) %>%  # 确保无重复
  mutate(present = 1) %>%  # 数值型标记
  pivot_wider(
    id_cols = compound_id,
    names_from = target,
    values_from = present,
    values_fill = 0  # 数值型填充
  ) %>%
  as.data.frame() %>%
  `rownames<-`(.[,"compound_id"]) %>%
  select(-compound_id)

# 检查结果
head(compound_matrix)
# 创建稀疏疾病行

disease_row <- sparseMatrix(
  i = rep(1, length(unique(disease_targets))),
  j = match(unique(disease_targets), all_targets),
  x = 1,
  dims = c(1, length(all_targets)),
  dimnames = list("Disease", all_targets)
)

# 获取所有可能的靶点（化合物+疾病）
all_targets <- union(colnames(compound_matrix), disease_targets) %>% 
  sort()

# 获取所有靶点（化合物和疾病的并集）
all_targets <- union(unique(compound_targets$target), unique(disease_targets)) %>% 
  sort()
# 直接添加缺失列
compound_matrix_full <- compound_matrix %>%
  as.data.frame() %>%
  tibble::rownames_to_column("compound_id")

# 添加缺失的靶点列并填充0
missing_cols <- setdiff(all_targets, colnames(compound_matrix_full))
compound_matrix_full[missing_cols] <- 0

# 设置行名和列顺序
compound_matrix_full <- compound_matrix_full %>%
  select(compound_id, all_of(all_targets)) %>%
  tibble::column_to_rownames("compound_id")

# 创建疾病行（确保列一致）
disease_row <- matrix(0, nrow = 1, ncol = length(all_targets),
                      dimnames = list("Disease", all_targets))
disease_row[, unique(disease_targets)] <- 1
  

# 合并矩阵
full_matrix <- rbind(compound_matrix_full, disease_row)




# 步骤2：计算各种距离 ----------------------------------------------------------
distance_types <- c("euclidean", "manhattan", "canberra", "binary")

# 初始化存储结果
distance_results <- data.frame(compound_id = rownames(compound_matrix))

for (d_type in distance_types) {
  # 计算距离矩阵
  dist_matrix <- dist(full_matrix, method = d_type) %>% 
    as.matrix()
  
  # 提取每个化合物到疾病行的距离
  distance_results[[d_type]] <- dist_matrix[rownames(compound_matrix), "Disease"]
}

# 步骤3：合并到原始数据 --------------------------------------------------------

distence <- data.frame(compound_id = rownames(compound_matrix))  # 示例数据

# 合并距离结果
distence <- distence %>%
  left_join(distance_results, by = "compound_id")

# 重命名列（可选）
colnames(distence)[-1] <- c("Euc", "Man", "Can", "Bin")




distance_cols <- c("Euc", "Man", "Can", "Bin") 



# 方法2：创建归一化副本（保留原始值）
df_normalized <- distence %>%
  mutate(across(all_of(distance_cols), list(
    minmax = ~ (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)),
    zscore = ~ scale(.),
    robust = ~ (. - median(., na.rm = TRUE)) / IQR(., na.rm = TRUE)
  )))

# 查看结果
glimpse(df_normalized)


df_filtered <- df_filtered %>%
  left_join(df_normalized,
    by = "compound_id"
  )



rm(list=ls()[!grepl('df_filtered|herbMolTarget|HYGenes', ls())])

save.image(file = "17_distances.RData")





