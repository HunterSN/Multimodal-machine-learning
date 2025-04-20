rm(list = ls())


drugbankSmile = read.csv('P_02_drugbank_smiles.csv',header = T,stringsAsFactors = F)


load('01_DrugBank.RData')

a = as.data.frame(dvobj[["drugs"]][["drug_interactions"]])




