rm(list = ls())

library(ftrCOOL)
library(seqinr)
library(Biostrings)
library(stringr)



a = readAAStringSet('protein_removeIllegal_Short_LongSequences.fasta')

targetInf = as.data.frame(a@ranges@NAMES)

catch = as.data.frame(c())
for (i in 1:nrow(targetInf)) {
  catch[i,1] = str_extract(targetInf[i,1],"\\|.+?\\ ") %>% str_sub(2,)
  catch[i,2] = str_extract(targetInf[i,1],"\\(D.+?\\)$") %>% str_sub(2,-2)
  
}



catch2 = strsplit(catch$V2,split = ";")
catch2 = as.data.frame(t(sapply(catch2, "[", i = 1:max(sapply(catch2, length)))))

catch2 = cbind(catch$V1,catch2)


#行转列


a = catch2

lei = as.numeric(ncol(a))
hang = as.numeric(nrow(a))

b= c()
b = as.data.frame(b)


zs = as.numeric(lei*hang)



for (i in 1:hang) {
  for (ii in 1:(lei-1)) {
    xu = as.numeric(as.numeric(nrow(b)) + 1)
    b[xu,1] = a[i,1]
    b[xu,2] = a[i,ii+1]
  }
  print(i)
}



bb = subset(b,b$V2 != "")

bb2 = bb[!duplicated(bb),]

names(bb2) = c('targetID','drugID')

names(catch) = c('targetID','drugID')

targetInf = catch
targetDrug_int = bb2






for (i in 1:nrow(targetInf)) {
  targetInf[i,1] = gsub(" ","",targetInf[i,1])
}


for (i in 1:nrow(targetDrug_int)) {
  targetDrug_int[i,1] = gsub(" ","",targetDrug_int[i,1])
  targetDrug_int[i,2] = gsub(" ","",targetDrug_int[i,2])
}

rm(list=ls()[!grepl('target', ls())])


#save.image('08_targetDrugInt_fromFasta.RData')



