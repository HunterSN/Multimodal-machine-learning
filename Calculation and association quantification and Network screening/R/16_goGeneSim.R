rm(list = ls())

load('15_network.RData')


# 加载所需的库
library(clusterProfiler)
library(org.Hs.eg.db)
library(GOSemSim)
library(dplyr)
library(tibble) 

# 读取疾病靶点列表（假设文件名为'genes.csv'）
disease_genes <- HYGenes$gene


# 转换疾病靶点为ENTREZID
disease_entrez <- bitr(disease_genes, fromType = "SYMBOL",
                       toType = "ENTREZID",
                       OrgDb = org.Hs.eg.db) %>%
  pull(ENTREZID) %>%
  unique()

# 读取化合物靶点表格（假设数据存储在'compound_targets.txt'中）
compound_targets <- df_filtered[,1:2]

# 转换化合物靶点为ENTREZID并合并回原数据
compound_entrez <- bitr(compound_targets$target, 
                        fromType = "SYMBOL",
                        toType = "ENTREZID",
                        OrgDb = org.Hs.eg.db)

compound_targets_merged <- compound_targets %>%
  left_join(compound_entrez, by = c("target" = "SYMBOL")) %>%
  filter(!is.na(ENTREZID))  # 去除无法转换的靶点


# 初始化GO数据（使用分子功能MF本体）
d <- godata('org.Hs.eg.db', ont = "MF", computeIC = FALSE)



# 获取所有化合物靶点的唯一ENTREZID
comp_entrez_list <- compound_targets_merged %>%
  group_by(compound_id) %>%
  summarise(entrez = list(unique(ENTREZID))) %>%
  deframe()

# 预先计算所有可能的基因对相似性以提升效率（可选，大数据时推荐）
all_pairs <- expand.grid(comp_gene = unique(unlist(comp_entrez_list)),
                         disease_gene = disease_entrez)

all_pairs$sim <- mapply(function(g1, g2) {
  sim <- geneSim(g1, g2, semData = d, measure = "Wang", drop = "IEA", combine = "BMA")
  ifelse(is.na(sim), 0, sim)
}, all_pairs$comp_gene, all_pairs$disease_gene)

save.image(file = "16_goGeneSim_catch1.RData")


#######
rm(list = ls())
library(clusterProfiler)
library(org.Hs.eg.db)
library(GOSemSim)
library(dplyr)
library(tibble) 

load('16_goGeneSim_catch1.RData')

for (i in 1:nrow(all_pairs)) {
  all_pairs$sim[i] <- as.numeric(all_pairs$sim[[i]][1])
  print(i)
}


# 计算每个化合物的总得分
total_scores <- sapply(comp_entrez_list, function(genes) {
  sum(unlist(all_pairs[all_pairs$comp_gene %in% genes, "sim"]), na.rm = TRUE)
})


# 将总得分合并到df_filtered表格，新列命名为gosim
df_filtered <- df_filtered %>%
  left_join(
    data.frame(
      compound_id = names(total_scores),
      gosim = total_scores
    ),
    by = "compound_id"
  )

rm(list=ls()[!grepl('df_filtered|herbMolTarget|HYGenes', ls())])

save.image(file = "16_goGeneSim.RData")


