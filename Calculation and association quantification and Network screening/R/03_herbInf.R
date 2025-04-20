rm(list = ls())


library(openxlsx)

####tcmsp####
herbinf = read.xlsx("03_symmap_tcmsp/tcmsp_herbinf.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
herbMOL = read.xlsx("03_symmap_tcmsp/tcmsp_herbMOL.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
MOLinf = read.xlsx("03_symmap_tcmsp/tcmsp_MOLinf.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
MOLtarget = read.xlsx("03_symmap_tcmsp/tcmsp_MOLtarget.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
targetinf = read.xlsx("03_symmap_tcmsp/tcmsp_targetinf.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)

####symmap####

#herb = read.xlsx("03_symmap_tcmsp/SymMap v2.0, SMHB file.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
#TCM_symptom = read.xlsx("03_symmap_tcmsp/SymMap v2.0, SMTS file.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
#MM_symptom = read.xlsx("03_symmap_tcmsp/SymMap v2.0, SMMS file.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
#ingredient = read.xlsx("03_symmap_tcmsp/SymMap v2.0, SMIT file.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
#target = read.xlsx("03_symmap_tcmsp/SymMap v2.0, SMTT file.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
#disease = read.xlsx("03_symmap_tcmsp/SymMap v2.0, SMDE file.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
#syndrome = read.xlsx("03_symmap_tcmsp/SymMap v2.0, SMSY file.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)



#herb_key = read.xlsx("03_symmap_tcmsp/SymMap v2.0, SMHB key file.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
#TCM_symptom_key = read.xlsx("03_symmap_tcmsp/SymMap v2.0, SMTS key file.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
#MM_symptom_key = read.xlsx("03_symmap_tcmsp/SymMap v2.0, SMMS key file.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
#ingredient_key = read.xlsx("03_symmap_tcmsp/SymMap v2.0, SMIT key file.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
#target_key = read.xlsx("03_symmap_tcmsp/SymMap v2.0, SMTT key file.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
#disease_key = read.xlsx("03_symmap_tcmsp/SymMap v2.0, SMDE key file.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
#syndrome_key = read.xlsx("03_symmap_tcmsp/SymMap v2.0, SMSY key file.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
