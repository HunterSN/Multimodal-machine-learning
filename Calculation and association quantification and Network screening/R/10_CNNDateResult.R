rm(list = ls())

library(dplyr)

#CNNmodelAcc = read.table('P_06_cnncla_RDKit.txt',sep = "\t",header = T, stringsAsFactors = F)

#targetScore_10_100 = read.csv('P_07_gradients_10_100_data.csv',header = T,stringsAsFactors = F)

#save.image('10_CNNDateResult_catch01.RData')


load("10_CNNDateResult_catch01.RData")
load('09_CNNDataPrepare.RData')


names(targetScore_10_100) = CNNmodelAcc$ID
rownames(targetScore_10_100) = rownames(colsum)

rm(list=ls()[grepl('^catch', ls())])



# 找到cnn训练后全是0的列号（靶点号）
zero_col_indices <- which(colSums(targetScore_10_100 == 0) == nrow(targetScore_10_100))

# 找到cnn训练后全是0的行号
zero_row_indices <- which(rowSums(targetScore_10_100 == 0) == ncol(targetScore_10_100))


#CNNmodelAcc = CNNmodelAcc[-non_zero_col_indices,]
#targetScore_10_100 = targetScore_10_100[,-zero_col_indices]
#targetScore_10_100 = targetScore_10_100[-zero_row_indices,]


#save.image('10_CNNDateResult.RData')







