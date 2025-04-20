rm(list = ls())

library(openxlsx)


#####DrugBank####
load('01_03_DrugBank_FASAT.RData')
load('10_CNNDateResult.RData')


drugRDKit <- subset(drugRDKit,!drugRDKit$子位置Smiles == "KeyError")




gene_map <- durgTargetNum[,c(4,2)]
gene_map = gene_map[!duplicated(gene_map),]
gene_map = merge(gene_map,targetFASAT[,c('uniprot_id','target_geneName')],by.x = "uniprot_id",by.y = "uniprot_id",all.x = T)
gene_map = gene_map[!duplicated(gene_map),]

CNNmodelAcc = merge(CNNmodelAcc,gene_map,by.x = 'ID',by.y = 'targetNum',all.x = T)



# 获取列名并替换
target_ids <- colnames(targetScore_10_100)
uniprot_gene_names <- gene_map$uniprot_id[match(target_ids, gene_map$targetNum)]

# 将新的基因名赋值给数据框的列名
targetScore_10_100_uniprot = targetScore_10_100
colnames(targetScore_10_100_uniprot) <- uniprot_gene_names

target_ids <- colnames(targetScore_10_100)
geneSymbol_gene_names <- gene_map$target_geneName[match(target_ids, gene_map$targetNum)]

# 将新的基因名赋值给数据框的列名
targetScore_10_100_geneSymbol = targetScore_10_100
colnames(targetScore_10_100_geneSymbol) <- geneSymbol_gene_names



rm(list=ls()[!grepl('CNNmodelAcc|drugRDKit|^targetScore_10_100_|drugFASAT|targetFASAT', ls())])


save.image(file = "11_MolMatchTarget_drugBank.RData")






#####TCMSP####
rm(list = ls())

TCMSPMOLTarget = read.xlsx("TCMSP_中药-化合物-靶点-疾病(未筛选)(已统计).xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)

TCMSPMOLTarget = TCMSPMOLTarget[,-c(1:7,9,24:26,29)]
TCMSPMOLTarget = TCMSPMOLTarget[!duplicated(TCMSPMOLTarget),]
TCMSPRDKit = read.table('P_08_TCMSPsdf_RDKit.txt',sep = "\t",header = T, stringsAsFactors = F)
#save.image(file = "11_MolMatchTarget_TCMSP.RData")

#####SymMap####
rm(list = ls())
# 加载必要的包
library(openxlsx)
library(dplyr)

# 设置包含 Excel 文件的文件夹路径
folder_path <- "SymMap_686个中药 - 化合物 - 靶点信息文件/"

# 获取文件夹内所有 .xlsx 文件的文件名
file_list <- list.files(path = folder_path, pattern = "*.xlsx", full.names = TRUE)
SymMapMOL = as.data.frame(c())
# 读取并合并所有文件

for (i in 1:length(file_list)) {
  print(i)
  column_to_save = c("TCMSP_id","Molecule_name","PubChem_CID","OB_score","CAS_id","Gene.symbol","Gene.name","Protein.name","Ensembl.id","UniProt.id")  
  catch = read.xlsx(file_list[i], sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  catch = subset(catch, select = column_to_save)
  catch <- catch %>%
    filter(!is.na(TCMSP_id) & TCMSP_id != "") 
  
  SymMapMOL = rbind(SymMapMOL,catch)
  SymMapMOL <- SymMapMOL %>%
    distinct()
  
}


#rm(list=ls()[!grepl('SymMap', ls())])


#save.image(file = "11_MolMatchTarget_SymMapMOL.RData")


#####汇总####
rm(list = ls())

library(openxlsx)
library(dplyr)



#SymMap
load('11_MolMatchTarget_SymMapMOL.RData')


SymMapDuoge = SymMapMOL[grep("\\|",SymMapMOL$TCMSP_id),1] %>% as.data.frame() %>% distinct()

library(plyr)
# 使用 strsplit 按 | 分割
SymMayChongfu <- strsplit(SymMapDuoge[,1], "\\|")

# 去掉每个分割结果的第一个元素
SymMayChongfuDEL <- lapply(SymMayChongfu, function(x) x[-1])


# 将剩余数据展平成一列，并去掉 NA 值
SymMayChongfuDEL <- unlist(SymMayChongfuDEL)

# 创建一个只有一列的新数据框
SymMayChongfuDEL <- data.frame(MOL = SymMayChongfuDEL, stringsAsFactors = FALSE)


SymMayChongfuSave <- rbind.fill(lapply(SymMayChongfu, function(x) as.data.frame(t(x), stringsAsFactors = FALSE)))[,1] %>% as.data.frame() %>% distinct()


SymMapMOL[, 1] <- sapply(strsplit(SymMapMOL[, 1], "\\|"), function(x) x[1])

#TCMSP
load('11_MolMatchTarget_TCMSP.RData')

#删除重复
TCMSPMOLTarget <- subset(TCMSPMOLTarget,!TCMSPMOLTarget$MOL_ID %in% SymMayChongfuDEL[,1])

TCMSPRDKit <- subset(TCMSPRDKit,!TCMSPRDKit$TCMSP_ID %in% SymMayChongfuDEL[,1])
TCMSPRDKit <- subset(TCMSPRDKit,!TCMSPRDKit$子位置Smiles == "KeyError")

rm(list=ls()[grepl('^(SymMapDuoge|SymMayChongfu)', ls())])

#DrugBank
load('11_MolMatchTarget_drugBank.RData')


#save.image(file = "11_MolMatchTarget.RData")











