rm(list = ls())

library(seqinr)
library(Biostrings)

#a = readAAStringSet('protein.fasta')

a = readAAStringSet('05_PSSM_input_removeIllegal_ShortSequences.fasta')
df = as.data.frame(a)
df$name = a@ranges@NAMES



res2 = apply(df,1,function(x){
  paste0(">",x[2],"\n",x[1])
})



# 计算需要分割成多少个数据框
num_splits <- ceiling(length(res2) / 499)

# 使用split()函数将数据框分割成小块
split_dfs <- split(res2, cut(1:length(res2), breaks = num_splits, labels = FALSE))


# 循环遍历每个小块，将其保存成单独的文件
for (i in 1:num_splits) {
  file_name <- paste0("05_PSSM_chunk_", i, ".fasta")
  write.table(split_dfs[[i]], file = file_name, row.names = FALSE,quote = F)
}


#writeLines(res2)

#write.table(res2,file = "05_PSSM_input.fasta",row.names = F,quote = F)