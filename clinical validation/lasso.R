rm(list = ls())

set.seed(1234)



library(openxlsx)
GTXSMatched = read.xlsx("38_2_GTXSMatched_adjust_symbol_alcohol_smok.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
GTXSMatched[is.na(GTXSMatched)] <- 0  # 将NA替换为0，保留原始缺失值



## 安装语句，缺失值处理
#if (!require("VIM")){install.packages("VIM", repos="https://cran.rstudio.com/")}

library(glmnet)
library(rms)
library(VIM)# 包中aggr()函数，判断数据缺失情况
library(survival)

#par(family = "STXihei")# 指定绘制图片中的字体
#(OS X 用的是 STXihei，Ubuntu 你自己找到字體的 family name 改就行了)
#两种改法，全局的和局部的：
#par(family='STXihei')
#plot(d, family='STXihei')

#调用数据，数据格式一行代表一条观测，一列代表一个变量；
d <-GTXSMatched
#View(d)
str(d)
aggr(d,prop=T,numbers=T) #判断数据缺失情况，红色表示有缺失。
#d <- na.omit(d) 按行删除缺失值

#告诉r哪个分类变量
##########################################################################
#用for循环语句将数值型变量转为因子变量
for(i in names(d)[c(4:9)]) {d[,i] <- as.factor(d[,i])}
str(d)
##筛选变量前，首先将自变量数据（因子变量）转变成矩阵（matrix）
x.factors <- model.matrix(~ d$sex
                          +d$HYClassification
                          +d$HYRiskFactor
                          +d$smokeFre
                          +d$drinkFre
                          +d$typeNum,
                          d)[,-1]
#将矩阵的因子变量与其它定量边量合并成数据框，定义了自变量。

col_index = c("sex",
              "HYClassification",
              "HYRiskFactor",
              "smokeFre",
              "drinkFre",
              "typeNum")

col_to_analysis = c("age",
                    "marital",
                    "inHospitalDay",
                    "herbCount",
                    "HYHistory",
                    "IVS_change",
                    "LVPW_change",
                    "LA_change",
                    "LV_change",
                    "sym_head",
                    "sym_digestion",
                    "sym_trunk",
                    "sym_limbs",
                    "sym_sleep",
                    "sym_breathing",
                    "sym_urinary",
                    "SBPchange",
                    "DBPchange")

col_to_analysis <- c("sex",
                     "age",
                     "marital",
                     "HYClassification",
                     "HYRiskFactor",
                     "smokeFre",
                     "drinkFre",
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
                     "LVEF_in")


paste(union(col_index,col_to_analysis), collapse = ' + ')

x=as.matrix(data.frame(x.factors,subset(d, select = col_to_analysis)))
#设置应变量，生存时间和生存状态（生存数据）
d$herbInterval[which(d$herbInterval <= 0)] = 0.1

d$sym_head[which(d$sym_head <= 0)] = 0
d$sym_head[which(d$sym_head > 0)] = 1

y <- data.matrix(Surv(d$herbInterval,d$sym_head))
#调用glmnet包中的glmnet函数，注意family那里一定要制定是“cox”，如果是做logistic需要换成"binomial"。
fit <-glmnet(x,y,family = "cox",alpha = 1)
plot(fit,label=T)
plot(fit,xvar="lambda",label=T)
#主要在做交叉验证,lasso
fitcv <- cv.glmnet(x,y,family="cox", alpha=1,nfolds=3)
plot(fitcv)
print(fitcv)
coef(fitcv, s="lambda.min")


############################################################
#改用ridge看一下结果
CV.fit <- cv.glmnet(x,y,family="cox", alpha=0,nfolds=5)
plot(CV.fit)
print(CV.fit)
#挑选出CV最小时的lambda值
coef(CV.fit, s="lambda.min")

####变量筛选完成，接下来保存预测值，验证
#利用筛选到变量在新的数据集中predict......

#############################################################
#############################################################
########### 假设筛选变量已完成，cox回归-列线图nomogram ######
d$HYHistory2 = mean(d$HYHistory[which(d$HYHistory > 0)])
d$HYHistory2[which(d$HYHistory > 0)] = d$HYHistory[which(d$HYHistory > 0)]

summary(d$herbCount[which(d$herbCount > 0)] )

d$herbCount2 = 2
d$herbCount2[which(d$herbCount > 0)] = d$herbCount[which(d$herbCount > 0)]/3

#拟合cox回归
dd<-datadist(d) #设置工作环境变量，将数据整合
options(datadist='dd') #设置工作环境变量，将数据整合
coxm <- cph(Surv(herbInterval,sym_head==1) ~ sex+
              smokeFre+
              drinkFre+
              HDL_in+
              UA_in+
              FT3_in+
              SBP_in+
              DBP_in,x=T,y=T,data=d,surv=T) 



summary(coxm)
cox.zph(coxm)#等比例风险假定检验
