rm(list = ls())

library(reshape2)


drugRDKit = read.table('P_05_DrugBankRDKit.txt',sep = "\t",header = T,stringsAsFactors = F)

a = as.data.frame(cbind(drugRDKit$ID,drugRDKit$子位置Smiles,drugRDKit$子位置计数))
#b = split(a[,2],a[,1])  #按处方分类组成list

#c = as.data.frame(t(sapply(b, "[", i = 1:max(sapply(b, length)))))
#c = cbind(source = row.names(c), c)

#a2 = subset(a,a$V1 %in% as.data.frame(table(a$V1))[1:4,1])

drugRDKitMatrix <- dcast(a, V1 ~ V2)


rm(list=ls()[!grepl('drugRDKit', ls())])


#save.image('07_drugRDKit.RData')
