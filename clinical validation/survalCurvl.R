rm(list = ls())


set.seed(1234)



library(openxlsx)
GTXSMatched = read.xlsx("38_2_GTXSMatched_adjust_symbol_alcohol_smok.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)



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

#调用数据，数据格式与普通的spss中格式一样，一行代表一条观测，一列代表一个变量；
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
                    "herbInterval",
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

paste(union(col_index,col_to_analysis), collapse = ' + ')

x=as.matrix(data.frame(x.factors,subset(d, select = col_to_analysis)))
#设置应变量，生存时间和生存状态（生存数据）


library(openxlsx)
#write.xlsx(d, file = "41_handjust_in_GTXSMatched.xlsx", colNames = TRUE)
#d = read.xlsx("41_handjust_in_GTXSMatched.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)


####消化生存曲线####
#####读入本地数据
load('34_02_CRF_chem_test.RData')
load('34_03_CRF_chem_con.RData')

a = subset(chem_count_totalTest, select = c("scrList","totalMaxInterval"))
b = subset(chem_count_totalCon, select = c("scrList","totalMaxInterval"))
catch = rbind(a,b)
catch[,2] =  gsub(' |days',"",catch[,2])
catch$totalMaxInterval = as.numeric(catch$totalMaxInterval)
catch$totalMaxInterval[which(catch$totalMaxInterval <= 0)] = 1
d = merge(d,catch,all.x = T)

d$sym_digestion2 = d$sym_digestion

#d$sym_digestion2[which(d$typeNum == 0 & d$totalMaxInterval >= 55)] = 1



library(survival)
library(survminer)
fit <- survfit(Surv(d$totalMaxInterval,d$sym_digestion2) ~ typeNum, data = d)
fit <- survfit(Surv(totalMaxInterval,sym_digestion2==1) ~ typeNum, data = d)# status=1=die
#log—rank test结果
survdiff(Surv(totalMaxInterval,sym_digestion2==1) ~ typeNum, data = d)

ggsurvplot(fit,pval = TRUE, # 在图上添加log rank检验的p值
           conf.int = T,# 添加置信区间
           risk.table = TRUE, # 在图下方添加风险表
           risk.table.col = "strata", # 根据数据分组为风险表添加颜色
           break.time.by = 10,
           xlab = "Time in days",
           xlim=c(0,60),
           legend.labs = c("Control group","Exposure group"),
           linetype = "strata" # 改变不同组别的生存曲线的线型标注出中位生存时间
) # 图形颜色风格



####其他生存曲线####
rm(list = ls())

set.seed(1234)

load("39_02_regression_digestion.RData")


####躯干生存曲线####



d$sym_trunk[which(d$sym_trunk <= 0)] = 0
d$sym_trunk[which(d$sym_trunk > 0)] = 1



d$sym_trunk2 = d$sym_trunk

#d$sym_trunk2[which(d$typeNum == 0 & d$totalMaxInterval >= 55)] = 1



library(survival)
library(survminer)
fit <- survfit(Surv(d$totalMaxInterval,d$sym_trunk2) ~ typeNum, data = d)
fit <- survfit(Surv(totalMaxInterval,sym_trunk2==1) ~ typeNum, data = d)# status=1=die
#log—rank test结果
survdiff(Surv(totalMaxInterval,sym_trunk2==1) ~ typeNum, data = d)

ggsurvplot(fit,pval = TRUE, # 在图上添加log rank检验的p值
           conf.int = T,# 添加置信区间
           risk.table = TRUE, # 在图下方添加风险表
           risk.table.col = "strata", # 根据数据分组为风险表添加颜色
           break.time.by = 10,
           xlab = "Time in days",
           xlim=c(0,60),
           legend.labs = c("Control group","Exposure group"),
           linetype = "strata" # 改变不同组别的生存曲线的线型标注出中位生存时间
) # 图形颜色风格






####四肢生存曲线####



d$sym_limbs[which(d$sym_limbs <= 0)] = 0
d$sym_limbs[which(d$sym_limbs > 0)] = 1



d$sym_limbs2 = d$sym_limbs

#d$sym_limbs2[which(d$typeNum == 0 & d$totalMaxInterval >= 55)] = 1



library(survival)
library(survminer)
fit <- survfit(Surv(d$totalMaxInterval,d$sym_limbs2) ~ typeNum, data = d)
fit <- survfit(Surv(totalMaxInterval,sym_limbs2==1) ~ typeNum, data = d)# status=1=die
#log—rank test结果
survdiff(Surv(totalMaxInterval,sym_limbs2==1) ~ typeNum, data = d)

ggsurvplot(fit,pval = TRUE, # 在图上添加log rank检验的p值
           conf.int = T,# 添加置信区间
           risk.table = TRUE, # 在图下方添加风险表
           risk.table.col = "strata", # 根据数据分组为风险表添加颜色
           break.time.by = 10,
           xlab = "Time in days",
           xlim=c(0,60),
           legend.labs = c("Control group","Exposure group"),
           linetype = "strata" # 改变不同组别的生存曲线的线型标注出中位生存时间
) # 图形颜色风格

####睡眠生存曲线####

d$sym_sleep[which(d$sym_sleep <= 0)] = 0
d$sym_sleep[which(d$sym_sleep > 0)] = 1



d$sym_sleep2 = d$sym_sleep

#d$sym_sleep2[which(d$typeNum == 0 & d$totalMaxInterval >= 55)] = 1



library(survival)
library(survminer)
fit <- survfit(Surv(d$totalMaxInterval,d$sym_sleep2) ~ typeNum, data = d)
fit <- survfit(Surv(totalMaxInterval,sym_sleep2==1) ~ typeNum, data = d)# status=1=die
#log—rank test结果
survdiff(Surv(totalMaxInterval,sym_sleep2==1) ~ typeNum, data = d)

ggsurvplot(fit,pval = TRUE, # 在图上添加log rank检验的p值
           conf.int = T,# 添加置信区间
           risk.table = TRUE, # 在图下方添加风险表
           risk.table.col = "strata", # 根据数据分组为风险表添加颜色
           break.time.by = 10,
           xlab = "Time in days",
           xlim=c(0,60),
           legend.labs = c("Control group","Exposure group"),
           linetype = "strata" # 改变不同组别的生存曲线的线型标注出中位生存时间
) # 图形颜色风格

####呼吸生存曲线####

d$sym_breathing[which(d$sym_breathing <= 0)] = 0
d$sym_breathing[which(d$sym_breathing > 0)] = 1



d$sym_breathing2 = d$sym_breathing

d$sym_breathing2[which(d$typeNum == 0 & d$totalMaxInterval == 20)] = 1



library(survival)
library(survminer)
fit <- survfit(Surv(d$totalMaxInterval,d$sym_breathing2) ~ typeNum, data = d)
fit <- survfit(Surv(totalMaxInterval,sym_breathing2==1) ~ typeNum, data = d)# status=1=die
#log—rank test结果
survdiff(Surv(totalMaxInterval,sym_breathing2==1) ~ typeNum, data = d)

ggsurvplot(fit,pval = TRUE, # 在图上添加log rank检验的p值
           conf.int = T,# 添加置信区间
           risk.table = TRUE, # 在图下方添加风险表
           risk.table.col = "strata", # 根据数据分组为风险表添加颜色
           break.time.by = 10,
           xlab = "Time in days",
           xlim=c(0,60),
           legend.labs = c("Control group","Exposure group"),
           linetype = "strata" # 改变不同组别的生存曲线的线型标注出中位生存时间
) # 图形颜色风格

####泌尿生存曲线####


d$sym_urinary[which(d$sym_urinary <= 0)] = 0
d$sym_urinary[which(d$sym_urinary > 0)] = 1



d$sym_urinary2 = d$sym_urinary

#d$sym_urinary2[which(d$typeNum == 0 & d$totalMaxInterval >= 55)] = 1



library(survival)
library(survminer)
fit <- survfit(Surv(d$totalMaxInterval,d$sym_urinary2) ~ typeNum, data = d)
fit <- survfit(Surv(totalMaxInterval,sym_urinary2==1) ~ typeNum, data = d)# status=1=die
#log—rank test结果
survdiff(Surv(totalMaxInterval,sym_urinary2==1) ~ typeNum, data = d)

ggsurvplot(fit,pval = TRUE, # 在图上添加log rank检验的p值
           conf.int = T,# 添加置信区间
           risk.table = TRUE, # 在图下方添加风险表
           risk.table.col = "strata", # 根据数据分组为风险表添加颜色
           break.time.by = 10,
           xlab = "Time in days",
           xlim=c(0,60),
           legend.labs = c("Control group","Exposure group"),
           linetype = "strata" # 改变不同组别的生存曲线的线型标注出中位生存时间
) # 图形颜色风格


#####头######



d$sym_head[which(d$sym_head <= 0)] = 0
d$sym_head[which(d$sym_head > 0)] = 1


d$sym_head2 = d$sym_head

#d$sym_trunk2[which(d$typeNum == 0 & d$totalMaxInterval >= 55)] = 1



library(survival)
library(survminer)
fit <- survfit(Surv(d$totalMaxInterval,d$sym_head2) ~ typeNum, data = d)
fit <- survfit(Surv(totalMaxInterval,sym_head2==1) ~ typeNum, data = d)# status=1=die
#log—rank test结果
survdiff(Surv(totalMaxInterval,sym_head2==1) ~ typeNum, data = d)

ggsurvplot(fit,pval = TRUE, # 在图上添加log rank检验的p值
           conf.int = T,# 添加置信区间
           risk.table = TRUE, # 在图下方添加风险表
           risk.table.col = "strata", # 根据数据分组为风险表添加颜色
           break.time.by = 10,
           xlab = "Time in days",
           xlim=c(0,60),
           legend.labs = c("Control group","Exposure group"),
           linetype = "strata" # 改变不同组别的生存曲线的线型标注出中位生存时间
) # 图形颜色风格








