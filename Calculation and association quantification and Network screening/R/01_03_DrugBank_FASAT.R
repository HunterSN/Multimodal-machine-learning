rm(list = ls())
library(jsonlite)

####Python03####
data <- fromJSON("drugBank_5_1_11.json")

load('01_02_drugBankSQL.RData')


drugFASAT = as.data.frame(ids)
drugFASAT$drugName = rownames(drugFASAT)

drugFASAT$durg_name = data[["drugbank"]][["drug"]][["name"]]
drugFASAT$durg_type = data[["drugbank"]][["drug"]][["@type"]]

drugFASAT$durgSequencesFormat = NA
drugFASAT$durgSequencesText = NA

for (i in 1:nrow(drugFASAT)) {
  print(i)
  result <- try(is.character(data[["drugbank"]][["drug"]][["sequences"]][["sequence"]][[i]][["@format"]][1]), silent = TRUE)
  if (inherits(result, "try-error")) {
    drugFASAT$durgSequencesFormat[i] = NA
  } else {
    if (is.character(data[["drugbank"]][["drug"]][["sequences"]][["sequence"]][[i]][["@format"]][1])) {
      drugFASAT$durgSequencesFormat[i] = data[["drugbank"]][["drug"]][["sequences"]][["sequence"]][[i]][["@format"]][1]
    } else {
      drugFASAT$durgSequencesFormat[i] = NA
    }
    
  }

  
  
  result <- try(is.character(data[["drugbank"]][["drug"]][["sequences"]][["sequence"]][[i]][[2]][1]), silent = TRUE)
  if (inherits(result, "try-error")) {
    drugFASAT$durgSequencesText[i] = NA
  } else {
    if (is.character(data[["drugbank"]][["drug"]][["sequences"]][["sequence"]][[i]][[2]][1])) {
      drugFASAT$durgSequencesText[i] = data[["drugbank"]][["drug"]][["sequences"]][["sequence"]][[i]][[2]][1]
    } else {
      drugFASAT$durgSequencesText[i] = NA
    }
    
  }
  
  
}



#####Python03####
targetFASAT = as.data.frame(c())

for (i in 1:nrow(drugFASAT)) {
  print(i)
  catch = as.data.frame(c())
  catch2 = data[["drugbank"]][["drug"]][["targets"]][["target"]][i]
  if(length(catch2[[1]][["polypeptide"]][["name"]]) == 0) {
    next
  } else {
    for (m in 1:length(catch2[[1]][["polypeptide"]][["name"]])) {
      catch[m,1] = drugFASAT$ids[i]
      if (is.character(catch2[[1]][["polypeptide"]][["@id"]][m])) {
        catch[m,2] = catch2[[1]][["polypeptide"]][["@id"]][m]
      } else {
        catch[m,2] = NA
      }
      
      if (is.character(catch2[[1]][["polypeptide"]][["name"]][m])) {
        catch[m,3] = catch2[[1]][["polypeptide"]][["name"]][m]
      } else {
        catch[m,3] = NA
      }

      if (is.character(catch2[[1]][["actions"]][[1]][m])) {
        catch[m,4] = catch2[[1]][["actions"]][[1]][m]
      } else {
        catch[m,4] = NA
      }
      
      if (is.character(catch2[[1]][["polypeptide"]][["gene-name"]][m])) {
        catch[m,5] = catch2[[1]][["polypeptide"]][["gene-name"]][m]
      } else {
        catch[m,5] = NA
      }

      if (is.character(catch2[[1]][["polypeptide"]][["amino-acid-sequence"]][["@format"]][m])) {
        catch[m,6] = catch2[[1]][["polypeptide"]][["amino-acid-sequence"]][["@format"]][m]
      } else {
        catch[m,6] = NA
      }

      if (is.character(catch2[[1]][["polypeptide"]][["amino-acid-sequence"]][2][[1]][m])) {
        catch[m,7] = catch2[[1]][["polypeptide"]][["amino-acid-sequence"]][2][[1]][m]
      } else {
        catch[m,7] = NA
      }
      
      
      if (is.character(catch2[[1]][["polypeptide"]][["gene-sequence"]][["@format"]][m])) {
        catch[m,8] = catch2[[1]][["polypeptide"]][["gene-sequence"]][["@format"]][m]
      } else {
        catch[m,8] = NA
      }
      result <- try(is.character(catch2[[1]][["polypeptide"]][["gene-sequence"]][2][[1]][m]), silent = TRUE)
      if (inherits(result, "try-error")) {
        catch[m,9] = NA
      } else {
        if (is.character(catch2[[1]][["polypeptide"]][["gene-sequence"]][2][[1]][m])) {
          catch[m,9] = catch2[[1]][["polypeptide"]][["gene-sequence"]][2][[1]][m]
        } else {
          catch[m,9] = NA
        }
        
      }
      
      
      if (is.character(catch2[[1]][["id"]][m])) {
        catch[m,10] = catch2[[1]][["id"]][m]
      } else {
        catch[m,10] = NA
      }

    }
    names(catch) = c('ids','uniprot_id', 'target_name', 'target_action', 
                     'target_geneName', 'target_aminoAcidSequenceFormat', 'target_aminoAcidSequenceText', 
                     'target_geneS2quenceFormat', 'target_geneSequenceText','Target_drugBank_ID')
    targetFASAT = rbind(targetFASAT,catch)
  }
}



rm(list=ls()[!grepl('FASAT', ls())])


save.image('01_03_DrugBank_FASAT.RData')




