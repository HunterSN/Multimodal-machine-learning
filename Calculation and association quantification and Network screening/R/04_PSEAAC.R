rm(list = ls())

library(ftrCOOL)

#load("01_03_DrugBank_FASAT.RData")
#targetPSEAAC = as.data.frame(targetFASAT[,c(1:5)])

#catch = targetFASAT$target_aminoAcidSequenceText[1]
#catch5 = gsub("\n","",gsub(">.+?\n","",catch))


library(seqinr)
library(Biostrings)

a = readAAStringSet('protein_removeIllegal_Short_LongSequences.fasta')


df = as.data.frame(a)
df$name = a@ranges@NAMES


catch = as.data.frame(c())
for (i in 1:nrow(df)) {
  print(i)
  mat <- PSEAAC(seqs = df[i,1], l = 1)
  mat = as.data.frame(mat)
  mat[1,ncol(mat) + 1] = df$name[i]
  catch = rbind(catch,mat)
  
}

targetPSEAAC = catch


rm(list=ls()[!grepl('targetPSEAAC', ls())])


save.image('04_PSEAAC.RData')











