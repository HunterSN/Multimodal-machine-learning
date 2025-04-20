rm(list = ls())

library(dplyr)

####targetCNN_RDKit####
#药物结构指纹
load("07_drugRDKit.RData")

#a = read.csv('P_02_drugbank_smiles.csv',header = T)
#length(intersect(a$drugbank_id,drugRDKit$ID))
#药靶关系
load('01_03_DrugBank_FASAT.RData')


durgTargetNum = subset(targetFASAT, targetFASAT$ids %in% drugRDKit$ID)
durgTargetNum = durgTargetNum[,1:2]
durgTargetNum = durgTargetNum[!duplicated(durgTargetNum),]
durgTargetNum = na.omit(durgTargetNum)
durgTargetNum$drugNum = dense_rank(durgTargetNum$ids)
durgTargetNum$targetNum = dense_rank(durgTargetNum$uniprot_id)

drugRDKitMatrix = drugRDKitMatrix[,-2]
names(drugRDKitMatrix)[1] = 'ids'

column_to_delete = c("KeyError")  
drugRDKitMatrix = subset(drugRDKitMatrix, select = setdiff(colnames(drugRDKitMatrix),column_to_delete))


catch = drugRDKitMatrix[,2:ncol(drugRDKitMatrix)]
catch$rowSum = apply(catch,1,sum)
table(catch$rowSum)

targetCNN_RDKit = drugRDKitMatrix[which(catch$rowSum >0),]

catch = subset(durgTargetNum, select = c("ids","targetNum"))


targetCNN_RDKit = merge(catch,targetCNN_RDKit,all.x = T)
targetCNN_RDKit = targetCNN_RDKit[,-1]




####targetCNN_PSEAAC_PSSM####

#靶点PSSM
load("06_PSSM_dataProcess.RData")
ls()[grepl('PSSM', ls())]

PSSM_aac_pssm = PSSM_aac_pssm[,-ncol(PSSM_aac_pssm)]
PSSM_ab_pssm = PSSM_ab_pssm[,-ncol(PSSM_ab_pssm)]
PSSM_d_fpssm = PSSM_d_fpssm[,-ncol(PSSM_d_fpssm)]
PSSM_dp_pssm = PSSM_dp_pssm[,-ncol(PSSM_dp_pssm)]
PSSM_dpc_pssm = PSSM_dpc_pssm[,-ncol(PSSM_dpc_pssm)]
PSSM_edp = PSSM_edp[,-ncol(PSSM_edp)]
PSSM_eedp = PSSM_eedp[,-ncol(PSSM_eedp)]
PSSM_k_separated_bigrams_pssm = PSSM_k_separated_bigrams_pssm[,-ncol(PSSM_k_separated_bigrams_pssm)]
PSSM_pse_pssm = PSSM_pse_pssm[,-ncol(PSSM_pse_pssm)]
PSSM_pssm_ac = PSSM_pssm_ac[,-ncol(PSSM_pssm_ac)]
PSSM_pssm_cc = PSSM_pssm_cc[,-ncol(PSSM_pssm_cc)]
PSSM_pssm_composition = PSSM_pssm_composition[,-ncol(PSSM_pssm_composition)]
PSSM_rpm_pssm = PSSM_rpm_pssm[,-ncol(PSSM_rpm_pssm)]
PSSM_rpssm = PSSM_rpssm[,-ncol(PSSM_rpssm)]
PSSM_s_fpssm = PSSM_s_fpssm[,-ncol(PSSM_s_fpssm)]
PSSM_smoothed_pssm = PSSM_smoothed_pssm[,-ncol(PSSM_smoothed_pssm)]
PSSM_tpc = PSSM_tpc[,-ncol(PSSM_tpc)]
PSSM_tri_gram_pssm = PSSM_tri_gram_pssm[,-ncol(PSSM_tri_gram_pssm)]


targetPSSM = cbind(PSSM_aac_pssm,
                 PSSM_ab_pssm,
                 PSSM_d_fpssm,
                 PSSM_dp_pssm,
                 PSSM_dpc_pssm,
                 PSSM_edp,
                 PSSM_eedp,
                 PSSM_k_separated_bigrams_pssm,
                 PSSM_pse_pssm,
                 PSSM_pssm_ac,
                 PSSM_pssm_cc,
                 PSSM_pssm_composition,
                 PSSM_rpm_pssm,
                 PSSM_rpssm,
                 PSSM_s_fpssm,
                 PSSM_smoothed_pssm,
                 PSSM_tpc,
                 PSSM_tri_gram_pssm)


rm(list=ls()[grepl('^PSSM_', ls())])



#靶点PSEAAC
load("04_PSEAAC.RData")
targetPSEAAC = targetPSEAAC[,-ncol(targetPSEAAC)]

targetCNN_PSEAAC_PSSM = cbind(targetPSEAAC,targetPSSM)



load('08_targetDrugInt_fromFasta.RData')
targetCNN_PSEAAC_PSSM$uniprot_id = targetInf$targetID


catch2 = subset(durgTargetNum, select = c("uniprot_id","targetNum"))
catch2 = catch2[!duplicated(catch2),]

targetCNN_PSEAAC_PSSM = merge(catch2,targetCNN_PSEAAC_PSSM,all.x = T)

targetCNN_PSEAAC_PSSM = subset(targetCNN_PSEAAC_PSSM,targetCNN_PSEAAC_PSSM$targetNum %in% targetCNN_RDKit$targetNum)
targetCNN_PSEAAC_PSSM = targetCNN_PSEAAC_PSSM[,-1]



#write.csv(targetCNN_RDKit, "09_targetCNN_RDKit.csv", quote = F,row.names = F,fileEncoding = 'GBK')

#write.csv(targetCNN_PSEAAC_PSSM, "09_targetCNN_PSEAAC_PSSM.csv", quote = F,row.names = F,fileEncoding = 'GBK')

#save.image('09_CNNDataPrepare_catch1.RData')



####targetCNN_RDKit_PSEAAC_PSSM####
rm(list = ls())

load('09_CNNDataPrepare_catch1.RData')

targetCNN_RDKit_PSEAAC_PSSM = merge(targetCNN_RDKit,targetCNN_PSEAAC_PSSM,all.x = T)

#write.csv(targetCNN_RDKit_PSEAAC_PSSM, "09_targetCNN_RDKit_PSEAAC_PSSM.csv", quote = F,row.names = F,fileEncoding = 'GBK')

#save.image('09_CNNDataPrepare_catch2.RData')




####targetCNN_RDKit调整####
rm(list = ls())

load('09_CNNDataPrepare_catch2.RData')


catch = targetCNN_RDKit[,2:ncol(targetCNN_RDKit)]
catch$rowSum = apply(catch,1,sum)
table(catch$rowSum)

targetCNN_RDKit2 = targetCNN_RDKit[which(catch$rowSum >= quantile(catch$rowSum, probs = 0.75)),]


#write.csv(targetCNN_RDKit2, "09_targetCNN_RDKit2.csv", quote = F,row.names = F,fileEncoding = 'GBK')

catch2 = targetCNN_RDKit[,2:ncol(targetCNN_RDKit)]
colsum = as.data.frame(colSums(catch2))
targetSaved = rownames(colsum)[which(colsum[,1] >= 50)]

targetCNN_RDKit3 = subset(targetCNN_RDKit2, select = c(names(targetCNN_RDKit2)[1],targetSaved))


#write.csv(targetCNN_RDKit3, "09_targetCNN_RDKit3.csv", quote = F,row.names = F,fileEncoding = 'GBK')



#save.image('09_CNNDataPrepare_catch3.RData')


####targetCNN_RDKit_singletargettest####

rm(list = ls())

library(dplyr)


#药物结构指纹
load("07_drugRDKit.RData")

#a = read.csv('P_02_drugbank_smiles.csv',header = T)
#length(intersect(a$drugbank_id,drugRDKit$ID))
#药靶关系
load('01_03_DrugBank_FASAT.RData')


durgTargetNum = subset(targetFASAT, targetFASAT$ids %in% drugRDKit$ID)
durgTargetNum = durgTargetNum[,1:2]
durgTargetNum = durgTargetNum[!duplicated(durgTargetNum),]
durgTargetNum = na.omit(durgTargetNum)
durgTargetNum$drugNum = dense_rank(durgTargetNum$ids)
durgTargetNum$targetNum = dense_rank(durgTargetNum$uniprot_id)

drugRDKitMatrix = drugRDKitMatrix[,-2]
names(drugRDKitMatrix)[1] = 'ids'

column_to_delete = c("KeyError")  
drugRDKitMatrix = subset(drugRDKitMatrix, select = setdiff(colnames(drugRDKitMatrix),column_to_delete))

#write.csv(drugRDKitMatrix, "09_drugRDKitMatrix.csv", quote = F,row.names = F,fileEncoding = 'GBK')


catch = drugRDKitMatrix[,2:ncol(drugRDKitMatrix)]
catch$rowSum = apply(catch,1,sum)
table(catch$rowSum)

targetCNN_RDKit = drugRDKitMatrix[which(catch$rowSum >0),]

catch = subset(durgTargetNum, select = c("ids","targetNum"))

#write.csv(catch, "09_durgTargetNum.csv", quote = F,row.names = F,fileEncoding = 'GBK')

catch2 = as.data.frame(table(catch$targetNum))
catch2 = catch2[order(catch2$Freq,decreasing = T),]

#write.csv(catch2, "09_TargetCount.csv", quote = F,row.names = F,fileEncoding = 'GBK')


catch3 = catch
catch3$targetNum[which(catch3$targetNum != catch2$Var1[4])] = 0
catch3$targetNum[which(catch3$targetNum == catch2$Var1[4])] = 1

catch3 = catch3[!duplicated(catch3),]



targetCNN_RDKit = merge(catch3,targetCNN_RDKit,all.x = T)
targetCNN_RDKit = targetCNN_RDKit[,-1]
targetCNN_RDKit = na.omit(targetCNN_RDKit)

catch4 = subset(targetCNN_RDKit,targetCNN_RDKit$targetNum == 1)

catch5 = catch4[,2:ncol(catch4)]
colsum = as.data.frame(colSums(catch5))

for (i in 1:nrow(colsum)) {
  colsum$num[i] = i
}

#write.csv(targetCNN_RDKit, "09_targetCNN_RDKit_singletargettest.csv", quote = F,row.names = F,fileEncoding = 'GBK')

#save.image('09_CNNDataPrepare.RData')


