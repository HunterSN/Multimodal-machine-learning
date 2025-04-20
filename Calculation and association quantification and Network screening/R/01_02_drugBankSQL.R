rm(list = ls())


library(drugbankR)
#drugbank_dataframe <- dbxml2df(xmlfile="full database.xml", version="5.1.11") 


#df2SQLite(dbdf=drugbank_dataframe, version="5.1.11")

# get the entire drugbank data.frame
all <- queryDB(type = "getAll", db_path="drugbank_5.1.11.db") 
#dim(all)

# retrieve all the valid drugbank ids
ids <- queryDB(type = "getIDs", db_path="drugbank_5.1.11.db") 
ids[1:4]

#save.image('01_02_drugBankSQL.RData')




# given drugbank ids, determine whether they are FDA approved
queryDB(ids = c("DB00001","DB00002"),type = "whichFDA", db_path="drugbank_5.1.11.db") 

# given drugbank ids, get their target gene/protein IDs 
queryDB(ids = c("DB00001","DB00002"),type = "getTargets", db_path="drugbank_5.1.11.db") 




