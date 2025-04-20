rm(list = ls())

set.seed(12345)


library(stringr)
library(dplyr)
library(mice)
library(DMwR2)
library(dplyr)
library(anytime)
library(openxlsx)
library(tableone)
library(Matching)
library(survey)
library(reshape2)
library(ggplot2)
library(mice)
library(rpart)
library(DMwR2)


load('37_CRF_dataSummary.RData')


####基线指标####
#年龄，性别，高血压分级，高血压风险因素，住院天数，高血压病史，高血压药物种类，入院收缩压，入院舒张压

col_to_baseline = c('age',
                    'sex',
                    'HYClassification',
                    'HYRiskFactor',
                    'inHospitalDay',
                    'HYHistory',
                    'antiHYDrugs',
                    'SBP_in',
                    'DBP_in',
                    "IVS_in",
                    "LVPW_in",
                    "LA_in",
                    "LV_in",
                    "LVEF_in")

col_to_analysis = c("name",
                    "sex",
                    "age",
                    "marital",
                    "HYClassification",
                    "HYRiskFactor",
                    "inHospitalDay",
                    "antiHYDrugs",
                    "chiefHistory",
                    "HYHistory",
                    "smokeHistory",
                    "cigarettesDaily",
                    "quitSmokeHistory",
                    "drinkHistory",
                    "alcoholDaily",
                    "quitDrinkHistory",
                    "AST_in",
                    "TSH_in",
                    "MONOper_in",
                    "MONOabs_in",
                    "LDL_in",
                    "TG_in",
                    "HDL_in",
                    "SCR_in",
                    "UR_in",
                    "UA_in",
                    "MALB_in",
                    "FBG_in",
                    "GA_in",
                    "HbA1c_in",
                    "AST_in",
                    "Hb_in",
                    "FT4_in",
                    "FT3_in",
                    "TC_in",
                    "Pluse_in",
                    "SBP_in",
                    "DBP_in",
                    "IVS_in",
                    "LVPW_in",
                    "LA_in",
                    "LV_in",
                    "LVEF_in",
                    "AST_out",
                    "TSH_out",
                    "MONOper_out",
                    "MONOabs_out",
                    "LDL_out",
                    "TG_out",
                    "HDL_out",
                    "SCR_out",
                    "UR_out",
                    "UA_out",
                    "MALB_out",
                    "FBG_out",
                    "GA_out",
                    "HbA1c_out",
                    "AST_out",
                    "Hb_out",
                    "FT4_out",
                    "FT3_out",
                    "TC_out",
                    "Pluse_out",
                    "SBP_out",
                    "DBP_out",
                    "IVS_out",
                    "LVPW_out",
                    "LA_out",
                    "LV_out",
                    "LVEF_out")


test_analysis = subset(test_all, select = intersect(colnames(test_all),col_to_analysis))


con_analysis = subset(con_all, select = intersect(colnames(con_all),col_to_analysis))

col_to_supp = c("herbCount","herbInterval")
catch = subset(test_all, select = intersect(colnames(test_all),col_to_supp))
test_analysis = cbind(test_analysis,catch)


con_analysis$herbCount = 0
con_analysis$herbInterval = 0




####分类变量转数字####

#性别，婚姻，高血压分级，高血压风险因素
#sex，marital，HYClassification，HYRiskFactor

#性别 
table(test_analysis$sex)
table(con_analysis$sex)

#男1 女0

test_analysis$sex[which(test_analysis$sex == "男")] = "1"
test_analysis$sex[which(test_analysis$sex == "女")] = "0"

con_analysis$sex[which(con_analysis$sex == "男")] = "1"
con_analysis$sex[which(con_analysis$sex == "女")] = "0"

table(test_analysis$sex)
table(con_analysis$sex)



#婚姻
table(test_analysis$marital)
table(con_analysis$marital)
# 已婚1 未婚2 丧偶3 离婚4 离异4 

test_analysis$marital[which(test_analysis$marital == "已婚")] = "1"
test_analysis$marital[which(test_analysis$marital == "未婚")] = "2"
test_analysis$marital[which(test_analysis$marital == "丧偶")] = "3"
test_analysis$marital[which(test_analysis$marital == "离婚")] = "4"
test_analysis$marital[which(test_analysis$marital == "离异")] = "4"

con_analysis$marital[which(con_analysis$marital == "已婚")] = "1"
con_analysis$marital[which(con_analysis$marital == "未婚")] = "2"
con_analysis$marital[which(con_analysis$marital == "丧偶")] = "3"
con_analysis$marital[which(con_analysis$marital == "离婚")] = "4"
con_analysis$marital[which(con_analysis$marital == "离异")] = "4"

table(test_analysis$marital)
table(con_analysis$marital)

#高血压分级
table(test_analysis$HYClassification)
table(con_analysis$HYClassification)
#1级 1 2级 2 3级 3

test_analysis$HYClassification[which(test_analysis$HYClassification == "1级")] = "1"
test_analysis$HYClassification[which(test_analysis$HYClassification == "2级")] = "2"
test_analysis$HYClassification[which(test_analysis$HYClassification == "3级")] = "3"


con_analysis$HYClassification[which(con_analysis$HYClassification == "1级")] = "1"
con_analysis$HYClassification[which(con_analysis$HYClassification == "2级")] = "2"
con_analysis$HYClassification[which(con_analysis$HYClassification == "3级")] = "3"

table(test_analysis$HYClassification)
table(con_analysis$HYClassification)



#高血压风险因素
table(test_analysis$HYRiskFactor)
table(con_analysis$HYRiskFactor)

con_analysis$HYRiskFactor[grepl("雅施达",con_analysis$HYRiskFactor)] = "中危"
table(con_analysis$HYRiskFactor)


#低危 1   中危2 、高危3 高危 3 治疗后高危3  
#治疗后极高危4   、极高危4  很高危4  极高危4

test_analysis$HYRiskFactor[which(test_analysis$HYRiskFactor == "低危")] = "1"
test_analysis$HYRiskFactor[which(test_analysis$HYRiskFactor == "中危")] = "2"
test_analysis$HYRiskFactor[which(test_analysis$HYRiskFactor == "高危")] = "3"
test_analysis$HYRiskFactor[which(test_analysis$HYRiskFactor == "、高危")] = "3"
test_analysis$HYRiskFactor[which(test_analysis$HYRiskFactor == "治疗后高危")] = "3"
test_analysis$HYRiskFactor[which(test_analysis$HYRiskFactor == "治疗后极高危")] = "4"
test_analysis$HYRiskFactor[which(test_analysis$HYRiskFactor == "、极高危")] = "4"
test_analysis$HYRiskFactor[which(test_analysis$HYRiskFactor == "很高危")] = "4"
test_analysis$HYRiskFactor[which(test_analysis$HYRiskFactor == "极高危")] = "4"

con_analysis$HYRiskFactor[which(con_analysis$HYRiskFactor == "低危")] = "1"
con_analysis$HYRiskFactor[which(con_analysis$HYRiskFactor == "中危")] = "2"
con_analysis$HYRiskFactor[which(con_analysis$HYRiskFactor == "高危")] = "3"
con_analysis$HYRiskFactor[which(con_analysis$HYRiskFactor == "、高危")] = "3"
con_analysis$HYRiskFactor[which(con_analysis$HYRiskFactor == "治疗后高危")] = "3"
con_analysis$HYRiskFactor[which(con_analysis$HYRiskFactor == "治疗后极高危")] = "4"
con_analysis$HYRiskFactor[which(con_analysis$HYRiskFactor == "、极高危")] = "4"
con_analysis$HYRiskFactor[which(con_analysis$HYRiskFactor == "很高危")] = "4"
con_analysis$HYRiskFactor[which(con_analysis$HYRiskFactor == "极高危")] = "4"

table(test_analysis$HYRiskFactor)
table(con_analysis$HYRiskFactor)



test_analysis$typeNum = 1

con_analysis$typeNum = 0

for (i in 1:nrow(test_analysis)) {
  test_analysis$name[i] = paste(i,"001",sep = "")
}

for (i in 1:nrow(con_analysis)) {
  con_analysis$name[i] = paste(i,"000",sep = "")
}


for (i in 1:nrow(test_all)) {
  test_all$nameNum[i] = paste(i,"001",sep = "")
}

for (i in 1:nrow(con_all)) {
  con_all$nameNum[i] = paste(i,"000",sep = "")
}


#test_analysis$SBPchange = test_analysis$SBP_in - test_analysis$SBP_out
#test_analysis$DBPchange = test_analysis$DBP_in - test_analysis$DBP_out

#con_analysis$SBPchange = con_analysis$SBP_in - con_analysis$SBP_out
#con_analysis$DBPchange = con_analysis$DBP_in - con_analysis$DBP_out



#test_analysis$IVS_change = test_analysis$IVS_in - test_analysis$IVS_out
#con_analysis$IVS_change = con_analysis$IVS_in - con_analysis$IVS_out


#test_analysis$LVPW_change = test_analysis$LVPW_in - test_analysis$LVPW_out
#con_analysis$LVPW_change = con_analysis$LVPW_in - con_analysis$LVPW_out

#test_analysis$LA_change = test_analysis$LA_in - test_analysis$LA_out
#con_analysis$LA_change = con_analysis$LA_in - con_analysis$LA_out



#test_analysis$LV_change = test_analysis$LV_in - test_analysis$LV_out
#con_analysis$LV_change = con_analysis$LV_in - con_analysis$LV_out



####缺失值####


library(mice)
library(DMwR2)

md.pattern(test_analysis)

D_NA = function(x){
  x = dplyr::mutate_all(x,as.numeric)
  miceMod <- mice(x[, !names(x) %in% c("name","typeNum")], method="rf")  # 基于随机森林模型进行mice插值
  miceOutput <- complete(miceMod)  # 生成完整数据
  return(miceOutput)
}

D_NACol = function(x){
  out = x[, colSums(is.na(x)) == 0]
  return(out)
}



test_DNA = D_NA(test_analysis)
con_DNA = D_NA(con_analysis)


save.image('38_catch_DNA.RData')


####倾向性评分####
rm(list = ls())

load('38_catch_DNA.RData')


set.seed(12345)


library(dplyr)
library(anytime)
library(openxlsx)
library(tableone)
library(Matching)
library(survey)
library(reshape2)
library(ggplot2)
library(mice)
library(rpart)
library(DMwR2)



test_DNA$typeNum = 1

con_DNA$typeNum = 0


for (i in 1:nrow(test_DNA)) {
  test_DNA$nameNum[i] = paste(i,"001",sep = "")
}

for (i in 1:nrow(con_DNA)) {
  con_DNA$nameNum[i] = paste(i,"000",sep = "")
}




G1_F0 = rbind(test_DNA,con_DNA)
#G1_F0 = D_NACol(G1_F0)
anyNA(G1_F0)





paste(col_to_baseline, collapse = ' + ')


catVars = c("sex","marital","HYClassification","HYRiskFactor")
vars = setdiff(colnames(G1_F0[,-c(ncol(G1_F0)-1,ncol(G1_F0))]),catVars)



#接下来计算通过logit回归计算每个样本的倾向性分数（Propensity Score, PS），也就是被分配为GTXS的概率.
## Fit model
#G1_F0Model <- glm(formula = typeNum ~ age + sex + HYClassification + HYRiskFactor + inHospitalDay + HYHistory + antiHYDrugs + SBP_in + DBP_in,
#                  data = G1_F0)
G1_F0Model <- glm(formula = typeNum ~ age + sex + HYClassification + HYRiskFactor + inHospitalDay + HYHistory + antiHYDrugs + SBP_in + DBP_in,
                  data = G1_F0)

## Predicted probability of being assigned to GTXS
G1_F0$pGTXS <- predict(G1_F0Model, type = "response")
## Predicted probability of being assigned to no GTXS
G1_F0$pNoGTXS <- 1 - G1_F0$pGTXS
## Predicted probability of being assigned to the treatment actually assigned (either GTXS or no GTXS)
G1_F0$pAssign <- NA
G1_F0$pAssign[G1_F0$typeNum == 1]    <- G1_F0$pGTXS[G1_F0$typeNum   == 1]
G1_F0$pAssign[G1_F0$typeNum == 0] <- G1_F0$pNoGTXS[G1_F0$typeNum == 0]
## Smaller of pGTXS vs pNoGTXS for matching weight
G1_F0$pMin <- pmin(G1_F0$pGTXS, G1_F0$pNoGTXS)

# 1. 检查并处理零概率：
G1_F0$ratio <- G1_F0$pGTXS / G1_F0$pNoGTXS

# 添加一个很小的常数 epsilon，避免除以零和 log(0) 的情况
epsilon <- 1e-6 # 一个非常小的数，例如 0.000001

G1_F0$ratio[G1_F0$pGTXS == 0] <- epsilon / (G1_F0$pNoGTXS[G1_F0$pGTXS == 0] + epsilon)
G1_F0$ratio[G1_F0$pNoGTXS == 0] <- (G1_F0$pGTXS[G1_F0$pNoGTXS == 0] + epsilon) / epsilon

#处理两个都为0的情况，此时比值应为1
G1_F0$ratio[G1_F0$pGTXS == 0 & G1_F0$pNoGTXS == 0] <- 1


# 2. 计算 logit，现在可以避免 NaN：
G1_F0$logit_ratio <- log(G1_F0$ratio)

# 3. 在计算 logit 之后，删除相关列中包含 NA 值的行
G1_F0 <- G1_F0[!is.na(G1_F0$logit_ratio) & !is.na(G1_F0$typeNum), ]





#然后使用Matching包进行匹配一致性样本(1:1匹配)
listMatch <- Match(Tr = (G1_F0$typeNum == 1),      # Need to be in 0,1
                   ## logit of PS,i.e., log(PS/(1-PS)) as matching scale
                   X  = log(G1_F0$pGTXS / G1_F0$pNoGTXS),
                   ## 1:1 matching
                   M  = 1,
                   ## caliper = 0.2 * SD(logit(PS))
                   caliper  = 0.2,
                   replace  = FALSE,
                   ties     = TRUE,
                   version  = "fast")
summary(listMatch) #检查匹配结果

# determining if balance exists in any unmatched dataset and in matched datasets
mb <- MatchBalance(G1_F0Model$formula, data=G1_F0, match.out=listMatch, nboots=50)

#将匹配的样本提取出来：
GTXSMatched <- G1_F0[unlist(listMatch[c("index.treated","index.control")]), ]

#再看下现在匹配后的SMD，现在所有变量的SMD都小于10%了

tabMatched <- CreateTableOne(vars = vars, strata = 'typeNum', data = GTXSMatched, factorVars = catVars)
## Show table with SMD
print(tabMatched, smd = TRUE)

#然后给样本进行加权，使得各组中的倾向性评分基本一致，进而消除混杂因素，作为标准平衡数据参考。
#一般有两种加权方法：逆概率处理加权法（the inverse probability of treatment weighting，IPTW）
#和标准化死亡比加权法（the standardized mortality ratio weighting，SMRW）,
#本次我们是有IPTW的进阶版（PMID:26238958）


## Matching weight
G1_F0$mw <- G1_F0$pMin / G1_F0$pAssign
# IPTW:
G1_F0$mw1=ifelse(G1_F0$typeNum==1,1/(G1_F0$pGTXS),1/(1-G1_F0$pGTXS))

## Weighted data

GTXSSvy <- svydesign(ids = ~ 1, data = G1_F0, weights = ~ mw)


save.image('38_catch01.RData')


## Construct a table (This is a bit slow.)
tabWeighted <- svyCreateTableOne(vars = vars, strata = "typeNum", data = GTXSSvy, factorVars = catVars)
## Show table with SMD
print(tabWeighted, smd = TRUE)


rm(list=ls()[!grepl('tab|GTXS|con_|test_', ls())])


print(tabMatched, smd = TRUE)

table(GTXSMatched$typeNum)


save.image('38_CRF_PSM.RData')


####统计分析####
rm(list = ls())
load('38_CRF_PSM.RData')
print(tabWeighted, smd = TRUE)

print(tabMatched, smd = TRUE)
table(GTXSMatched$typeNum)

library(CBCgrps)

#删除全为NA的行或列
removeRowsAllNa  <- function(x){x[apply(x, 1, function(y) any(!is.na(y))),]}
removeColsAllNa  <- function(x){x[, apply(x, 2, function(y) any(!is.na(y)))]}

GTXSMatched2 = removeColsAllNa(GTXSMatched)

GTXSMatched2 = GTXSMatched2[,1:70]
#GTXSMatched2 = GTXSMatched
#GTXSMatched2[is.na(GTXSMatched2)] = 0

tab1 <-twogrps(GTXSMatched2, gvar = "typeNum",simulate.p.value=TRUE)

print(tab1, quote = T)
tab11 = as.data.frame(tab1[["Table"]])




rm(list = ls())

####统计分析2####

rm(list = ls())

load('38_CRF_PSM.RData')

save.image('38_CRF_PSM手改前.RData')

####手动修改####
 
rm(list = ls())
load('38_CRF_PSM手改前.RData')


write.xlsx(GTXSMatched, file = "38_手动修改_GTXSMatched.xlsx", colNames = TRUE)
library(openxlsx)
GTXSMatched = read.xlsx("38_手动修改_GTXSMatched.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
save.image('38_CRF_PSM.RData')

####手动修改后####
rm(list = ls())

load('38_CRF_PSM.RData')

GTXSMatched$sex[which(GTXSMatched$sex == "1")] = "男"
GTXSMatched$sex[which(GTXSMatched$sex == "0")] = "女"
GTXSMatched$marital[which(GTXSMatched$marital == "1")] = "婚姻_已婚"
GTXSMatched$marital[which(GTXSMatched$marital == "2")] = "婚姻_未婚"
GTXSMatched$marital[which(GTXSMatched$marital == "3")] = "婚姻_丧偶"
GTXSMatched$marital[which(GTXSMatched$marital == "4")] = "婚姻_离异"
GTXSMatched$HYClassification[which(GTXSMatched$HYClassification == "1")] = "高血压_1级"
GTXSMatched$HYClassification[which(GTXSMatched$HYClassification == "2")] = "高血压_2级"
GTXSMatched$HYClassification[which(GTXSMatched$HYClassification == "3")] = "高血压_3级"
GTXSMatched$HYRiskFactor[which(GTXSMatched$HYRiskFactor == "1")] = "风险_低危"
GTXSMatched$HYRiskFactor[which(GTXSMatched$HYRiskFactor == "2")] = "风险_中危"
GTXSMatched$HYRiskFactor[which(GTXSMatched$HYRiskFactor == "3")] = "风险_高危"
GTXSMatched$HYRiskFactor[which(GTXSMatched$HYRiskFactor == "4")] = "风险_很高危"


GTXSMatched$SBP_outOK = "SBP_outOK_否"
GTXSMatched$SBP_outOK[which(GTXSMatched$SBP_out <= 140)] = "SBP_outOK_是"

GTXSMatched$DBP_outOK = "DBP_outOK_否"
GTXSMatched$DBP_outOK[which(GTXSMatched$DBP_out <= 90)] = "DBP_outOK_是"

GTXSMatched$SBPchange = GTXSMatched$SBP_in - GTXSMatched$SBP_out
GTXSMatched$DBPchange = GTXSMatched$DBP_in - GTXSMatched$DBP_out
GTXSMatched$IVS_change = GTXSMatched$IVS_in - GTXSMatched$IVS_out
GTXSMatched$LVPW_change = GTXSMatched$LVPW_in - GTXSMatched$LVPW_out
GTXSMatched$LA_change = GTXSMatched$LA_in - GTXSMatched$LA_out
GTXSMatched$LV_change = GTXSMatched$LV_in - GTXSMatched$LV_out



####吸烟频率####
#Never：一生中抽了少于100支香烟
#Former：一生中抽了多于100支香烟，且现在不抽烟
#Current：一生中抽了多于100支香烟，且现在抽烟

GTXSMatched$smokeFre = "吸烟_Never"
GTXSMatched$smokeFre[which(GTXSMatched$quitSmokeHistory != 0)] = "吸烟_Former"
GTXSMatched$smokeFre[which(GTXSMatched$smokeHistory != 0 & GTXSMatched$quitSmokeHistory == 0)] = "吸烟_Current"


####饮酒频率####
#Never : 一生喝过少于12杯酒
#Former: 一生喝过超过12杯酒，但近1年未饮酒
#Current: 一生喝过超过12杯酒，但近1年有饮酒

GTXSMatched$drinkFre = "饮酒_Never"

GTXSMatched$drinkFre[which(GTXSMatched$quitDrinkHistory != 0)] = "饮酒_Former"
GTXSMatched$drinkFre[which(GTXSMatched$drinkHistory != 0 & GTXSMatched$quitDrinkHistory == 0)] = "饮酒_Current"



library(openxlsx)
#write.xlsx(GTXSMatched, file = "38_handjust_in_GTXSMatched.xlsx", colNames = TRUE)


#GTXSMatched = read.xlsx("38_handjust_in_GTXSMatched_OK.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)

library(tableone)
## PS matching
library(Matching)
## Weighted analysis
library(survey)
library(reshape2)
library(ggplot2)
## Right heart cath dataset
GTXS <- GTXSMatched
GTXS = dplyr::mutate_all(GTXS,as.character)
GTXS = dplyr::mutate_all(GTXS,as.numeric)
# 待统计的协变量：
vars <- names(GTXSMatched)


catVars = c("sex","marital","HYClassification","HYRiskFactor","HYClassification2","HYRiskFactor2","smokeFre","drinkFre","SBPOK","DBPOK")
## Construct a table
tabUnmatched <- CreateTableOne(vars = vars, strata = 'typeNum', data = GTXS, factorVars = catVars)
## Show table with SMD
print(tabUnmatched, smd = TRUE)
summary(tabUnmatched)
tabMat = print(tabUnmatched,  smd = TRUE,quote = F, noSpaces = T, printToggle = F)
tabMat = as.data.frame(tabMat)
tabMat$item = rownames(tabMat)


inlist = GTXSMatched$nameNum

conIn = subset(con_analysis,con_analysis$name %in% inlist)
testIn = subset(test_analysis,test_analysis$name %in% inlist)

conIn_ALL = subset(con_all,con_all$nameNum %in% inlist)
testIn_ALL = subset(con_all,con_all$nameNum %in% inlist)



