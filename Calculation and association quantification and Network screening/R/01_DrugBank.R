rm(list = ls())



## load dbparser package
library(dbparser)
library(dplyr)
library(ggplot2)
library(XML)

## parse data from XML and save it to memory
dvobj <- parseDrugBank(db_path            = "full database.xml",
                       drug_options       = drug_node_options(),
                       parse_salts        = TRUE,
                       parse_products     = TRUE,
                       references_options = references_node_options(),
                       cett_options       = cett_nodes_options())

## load drugs data
drugs <- dvobj$drugs$general_information

## load drug groups data
drug_groups <- dvobj$drugs$groups

## load drug targets actions data
drug_targets_actions <- dvobj$cett$targets$actions


save.image('01_DrugBank.RData')



