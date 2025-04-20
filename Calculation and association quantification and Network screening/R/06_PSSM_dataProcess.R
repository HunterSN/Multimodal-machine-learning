rm(list = ls())

library(Biostrings)


for (i in 1:11) {
  print(i)
  file_name <- paste0("06_PSSM/05_PSSM_chunk_", i, ".fasta")
  
  a = readAAStringSet(file_name)
  df = as.data.frame(a)
  df$name = a@ranges@NAMES
  
  
  id <- list.files(path = paste0("06_PSSM/", i,"/originalFeatures"))
  orignalFeatures = list.files(path = paste0("06_PSSM/", i,"/originalFeatures/",id))
  hebing = c()
  for (m in 1:length(orignalFeatures)){
    hebing[m] <- gsub('?.csv','',orignalFeatures[m])
    hebing[m] = gsub(paste0(id,"_"), "", hebing[m])
    assign(paste0("NO_",i,"_",hebing[m]),read.csv(paste0(paste0("06_PSSM/", i,"/originalFeatures/",id),"/",orignalFeatures[m]), header = T,stringsAsFactors = F))
  }
  
  hebing2 = paste0("NO_",i,"_",hebing)
  
  
  
  
  for (n in 1:length(hebing2)) {
    catch = get(hebing2[n])
    catch$name = df$name
    assign(paste0("NO_",i,"_",hebing[n]),catch)
  }
  
}


#save.image('06_catch.RData')



for (x in 1:length(hebing)) {
  print(x)
  
  list=ls()[grepl(paste0("_",hebing[x],"$"), ls())]
  
  catch = as.list(c())
  for (y in 1:length(list)) {
    
    catch[[y]] = get(list[y])
    
  }
  
  assign(paste0("PSSM_",hebing[x]),do.call(rbind,catch))
  
  
}

rm(list=ls()[!grepl('PSSM', ls())])


#save.image('06_PSSM_dataProcess.RData')

