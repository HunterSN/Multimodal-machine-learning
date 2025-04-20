rm(list = ls())

library(openxlsx)
library(dplyr)


load('11_MolMatchTarget.RData')




 


# 读取蛋白质相互作用网络文件
hint <- read.table("HINT_hqb.txt", header = FALSE, sep = "\t")
hprd <- read.table("hprdPPI.txt", header = FALSE, sep = "\t")

# 设置基因列表
genes_list <- SymMapMOL$Gene.symbol

# 合并两个相互作用网络文件
ppi_network <- rbind(hint, hprd)

# 将相互作用的基因对命名为 'GeneA' 和 'GeneB'
colnames(ppi_network) <- c("GeneA", "GeneB")

# 找出基因列表中的基因参与的相互作用
interacting_genes <- ppi_network[ppi_network$GeneA %in% genes_list | ppi_network$GeneB %in% genes_list, ]

# 从相互作用对中提取与基因列表有相互作用的基因
related_genes <- unique(c(
  interacting_genes$GeneA[!interacting_genes$GeneA %in% genes_list],
  interacting_genes$GeneB[!interacting_genes$GeneB %in% genes_list]
))

symMap_all_genes <- unique(c(genes_list, related_genes))


intGeneSym = Reduce(intersect, list(CNNmodelAcc$target_geneName,
                                    targetFASAT$target_geneName,
                                    names(targetScore_10_100_geneSymbol),
                                    symMap_all_genes))



save.image(file = "12_DatebaseMatch.RData")











