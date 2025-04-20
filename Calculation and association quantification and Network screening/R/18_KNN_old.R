rm(list = ls())

load('17_distances.RData')
library(dplyr)

ZoutKNN = df_filtered
ZoutKNN$num = 1:nrow(ZoutKNN)

#确定分割点
ZoutKNN$cut = 0
ZoutKNN$cut[which(ZoutKNN$target %in% HYGenes$gene)] = 1

table(ZoutKNN$cut)

rownames(ZoutKNN) = 1:nrow(ZoutKNN)
ZoutKNN <- ZoutKNN %>%
  mutate(across(-c(1:2), as.numeric))

set.seed(1234)


par(mfrow = c(1,1))

#"gradcam_weight"    "normalized_weight" "ob"               
#"mw"                "alogp"             "caco2"             "bbb"               "hdon"             
#"hacc"              "FASA"              "rbn"               "tpsa"              "gosim"            
#"Euc"               "Man"               "Can"               "Bin"               "Euc_minmax"       
#"Euc_zscore"        "Euc_robust"        "Man_minmax"        "Man_zscore"        "Man_robust"       
#"Can_minmax"        "Can_zscore"        "Can_robust"        "Bin_minmax"        "Bin_zscore"       
#"Bin_robust"       
####KNN#####

KS = 100
#setInf = select(ZoutKNN,cut,weight,Euc,RWR,GoSimHY,GoSimHYSym)
setInf = ZoutKNN
library(kknn)
library(pROC)
library(modEvA)


library(class)

v = 10 #交叉次数

#分割原始数据（对原始数据均匀分成v重，以类别标签返回
#通过随机sample方法，如sample(5) --> 3 2 1 4 5,以sample产生的数据作为索引下标，提取cut类别标签数据。
#这样做到来随机抽取类别数据
grps = cut(1:nrow(setInf), v, labels = FALSE)[sample(1:nrow(setInf))]
print(grps)
# 对每份数据分别运行ML函数 对随机数据的1/v作为测试集，剩下的作为训练集
# 对于lapply函数，后面要加上传递给function的参数data, cl, k. lapply的返回值为list
pred = lapply(1:v,function(i, setInf){
  omit = which(grps == i)
  pcl = fitted(kknn(cut ~ gradcam_weight + ob +  gosim+Euc_zscore+Can_zscore, setInf[-omit,], setInf[omit,], k = KS ,distance = 2 ))
},setInf)
#整合测试结果
wh = unlist(pred)
length(setInf$cut)  # 原始数据的真实标签
length(wh)          # 预测结果

# 找出哪些样本被预测了（假设 wh 的顺序与 setInf 一致，但少了 NA 样本）
valid_rows <- !is.na(setInf$gradcam_weight) & !is.na(setInf$ob) & !is.na(setInf$bbb) & !is.na(setInf$gosim)
grps_clean <- grps[valid_rows]  # 只保留有效的分组标签

# 只保留有效的样本
setInf_clean <- setInf[valid_rows, ]
wh_clean <- wh  # 已经是 18008 个样本

#grps 顺序已经被打乱，重新从小到大排序
setInf_clean$grps = grps_clean
knnT = as.data.frame(table(wh_clean))
setInfF = setInf_clean[order(setInf_clean$grps),]

setInfFKNN = setInfF
knn_roc = roc(setInfFKNN$cut, as.numeric(wh_clean),levels = c('0', '1'),direction = "<")
plot(knn_roc)
knn_roc$auc
coords(knn_roc, "best")
aupr=AUC(obs=setInfFKNN$cut,pred=as.numeric(wh),curve = "ROC", simplif=TRUE, main = "ROC curve")
#aupr=AUC(obs=setInfFKNN$cut,pred=as.numeric(wh),curve = "PR", simplif=TRUE, main = "PR curve")



knn_roc$levels

#setInfFKNN$knn = wh
#write.csv(setInfFKNN, 'KNNout.csv', quote = F, row.names = F, fileEncoding = 'GBK')
setInfF_out = setInfF
setInfF_out$wh_KNN = as.numeric(wh)


####SVM交叉验证#####
setInf = ZoutKNN

library(e1071)
library(pROC)
library(modEvA)



library(class)

v = 10 #交叉次数

#分割原始数据（对原始数据均匀分成v重，以类别标签返回
#通过随机sample方法，如sample(5) --> 3 2 1 4 5,以sample产生的数据作为索引下标，提取cut类别标签数据。
#这样做到来随机抽取类别数据
grps = cut(1:nrow(setInf), v, labels = FALSE)[sample(1:nrow(setInf))]
print(grps)
# 对每份数据分别运行ML函数 对随机数据的1/v作为测试集，剩下的作为训练集
# 对于lapply函数，后面要加上传递给function的参数data, cl, k. lapply的返回值为list
pred = lapply(1:v,function(i, setInf){
  omit = which(grps == i)
  pcl = predict(svm(cut ~ gradcam_weight + ob +  gosim+Euc_zscore+Can_zscore, data = setInf[-omit,],  type = 'C-classification', kernel = 'radial'),newdata = setInf[omit,])
  
},setInf)
#整合测试结果
wh = unlist(pred)
length(setInf$cut)  # 原始数据的真实标签
length(wh)          # 预测结果

# 找出哪些样本被预测了（假设 wh 的顺序与 setInf 一致，但少了 NA 样本）
valid_rows <- !is.na(setInf$gradcam_weight) & !is.na(setInf$ob) & !is.na(setInf$bbb) & !is.na(setInf$gosim)
grps_clean <- grps[valid_rows]  # 只保留有效的分组标签

# 只保留有效的样本
setInf_clean <- setInf[valid_rows, ]
wh_clean <- wh  # 已经是 18008 个样本

#grps 顺序已经被打乱，重新从小到大排序
setInf_clean$grps = grps_clean
knnT = as.data.frame(table(wh_clean))
setInfF = setInf_clean[order(setInf_clean$grps),]

setInfFSVM = setInfF

#混淆矩阵
MLSVM= table(setInfFSVM$cut, wh, dnn =c("真实值","预测值"))
MLSVM = as.data.frame(MLSVM)
SVM_roc = roc(setInfFSVM$cut, as.numeric(wh),levels = c('0', '1'),direction = "<")
plot(SVM_roc)
SVM_roc$auc


####GBRT交叉验证#####


setInf = ZoutKNN

library(gbm)

library(pROC)
library(modEvA)

library(class)

v = 10 #交叉次数

#分割原始数据（对原始数据均匀分成v重，以类别标签返回
#通过随机sample方法，如sample(5) --> 3 2 1 4 5,以sample产生的数据作为索引下标，提取cut类别标签数据。
#这样做到来随机抽取类别数据
grps = cut(1:nrow(setInf), v, labels = FALSE)[sample(1:nrow(setInf))]
print(grps)
# 对每份数据分别运行ML函数 对随机数据的1/v作为测试集，剩下的作为训练集
# 对于lapply函数，后面要加上传递给function的参数data, cl, k. lapply的返回值为list
pred = lapply(1:v,function(i, setInf){
  omit = which(grps == i)
  mol_GBRTF = gbm(cut ~ gradcam_weight + ob +  gosim+Euc_zscore+Can_zscore,
                  distribution = "bernoulli",
                  data = setInf[-omit,],
                  var.monotone = NULL,
                  n.trees = 100,
                  interaction.depth = 1,
                  n.minobsinnode = 10,
                  shrinkage = 0.001,
                  bag.fraction = 0.5,
                  train.fraction = 1.0,
                  cv.folds=0,
                  keep.data = TRUE,
                  verbose = "CV",
                  class.stratify.cv=NULL,
                  n.cores = NULL)
  
  
  pcl = predict(mol_GBRTF,setInf[omit,],gbm.perf(mol_GBRTF,method = 'OOB'))
  
},setInf)
#整合测试结果
wh = unlist(pred)
#grps 顺序已经被打乱，重新从小到大排序
setInf$grps = grps
knnT = as.data.frame(table(wh))
setInfF = setInf[order(setInf$grps),]

setInfFGBRT = setInfF

#混淆矩阵
MLGBRT= table(setInfFGBRT$cut, wh, dnn =c("真实值","预测值"))
MLGBRT = as.data.frame(MLSVM)

GBRT_roc = roc(setInfFGBRT$cut, as.numeric(wh),levels = c('0', '1'),direction = "<")
plot(GBRT_roc)
GBRT_roc$auc


####Bayes交叉验证#####

setInf = ZoutKNN

library(klaR)
library(e1071)
library(pROC)
library(modEvA)
library(class)
library(caret) 



# 移除缺失值
setInf <- na.omit(setInf)

# 将目标变量转换为因子
setInf$cut <- as.factor(setInf$cut)

v <- 10  # 交叉验证次数

# 使用分层抽样创建交叉验证分组
set.seed(123)  # 确保可重复性
folds <- createFolds(setInf$cut, k = v, list = FALSE)
grps <- folds[sample(1:nrow(setInf))]

pred <- lapply(1:v, function(i) {
  omit <- which(grps == i)
  train_data <- setInf[-omit, ]
  test_data <- setInf[omit, ]
  
  # 检查训练集是否包含所有类别
  if (length(unique(train_data$cut)) < length(levels(setInf$cut))) {
    stop("训练集缺少某些类别，请调整交叉验证策略。")
  }
  
  # 训练朴素贝叶斯模型并添加平滑
  model <- NaiveBayes(cut ~ gradcam_weight + ob + bbb + gosim + Euc_zscore + Man_zscore + Can_zscore,
                      data = train_data, fL = 1)
  pcl <- predict(model, newdata = test_data)
  
  # 提取预测概率（假设二分类问题，选择第二列）
  pre_BayesF <- as.data.frame(pcl$posterior)[, 2]
  return(pre_BayesF)
})

# 后续处理（例如计算AUC）
actual <- setInf$cut[unlist(lapply(1:v, function(i) which(grps == i)))]
predictions <- unlist(pred)
roc_obj <- roc(actual, predictions)
auc(roc_obj)
#整合测试结果
wh = unlist(pred)
#grps 顺序已经被打乱，重新从小到大排序
setInf$grps = grps
knnT = as.data.frame(table(wh))
setInfF = setInf[order(setInf$grps),]

setInfFB = setInfF

#混淆矩阵
MLB= table(setInfFB$cut, wh, dnn =c("真实值","预测值"))
MLB = as.data.frame(MLB)



B_roc = roc(setInfFB$cut, as.numeric(wh),levels = c('0', '1'),direction = "<")

plot(B_roc)

B_roc$auc




####LG交叉验证#####

setInf = ZoutKNN

library(pROC)
library(modEvA)




library(class)

v = 10 #交叉次数

#分割原始数据（对原始数据均匀分成v重，以类别标签返回
#通过随机sample方法，如sample(5) --> 3 2 1 4 5,以sample产生的数据作为索引下标，提取cut类别标签数据。
#这样做到来随机抽取类别数据
grps = cut(1:nrow(setInf), v, labels = FALSE)[sample(1:nrow(setInf))]
print(grps)
# 对每份数据分别运行ML函数 对随机数据的1/v作为测试集，剩下的作为训练集
# 对于lapply函数，后面要加上传递给function的参数data, cl, k. lapply的返回值为list

setInf$cut = as.factor(setInf$cut)
setInf$cut = as.factor(setInf$cut)

pred = lapply(1:v,function(i, setInf){
  omit = which(grps == i)
  pcl = predict.glm(glm(cut ~ gradcam_weight + ob +  gosim+Euc_zscore+Can_zscore,setInf[-omit,],family=binomial(link="logit")),newdata = setInf[omit,])
},setInf)
#整合测试结果
wh = unlist(pred)
#grps 顺序已经被打乱，重新从小到大排序
setInf$grps = grps
knnT = as.data.frame(table(wh))
setInfF = setInf[order(setInf$grps),]

setInfFLG = setInfF


#混淆矩阵
MLLG= table(setInfFLG$cut, wh, dnn =c("真实值","预测值"))
MLLG = as.data.frame(MLLG)


LG_roc = roc(setInfFLG$cut, as.numeric(wh),levels = c('0', '1'),direction = "<")


plot(LG_roc)
LG_roc$auc
coords(LG_roc, "best")
aupr=AUC(obs=setInfFLG$cut,pred=as.numeric(wh),curve = "ROC", simplif=TRUE, main = "ROC curve")
aupr=AUC(obs=setInfFLG$cut,pred=as.numeric(wh),curve = "PR", simplif=TRUE, main = "PR curve")







#####smooth####
#knn_roc = roc(setInfFKNN$cut, as.numeric(wh),levels = c('0', '1'),direction = "<")
#SVM_roc = roc(setInfFSVM$cut, as.numeric(wh),levels = c('0', '1'),direction = "<")
#GBRT_roc = roc(setInfFGBRT$cut, as.numeric(wh),levels = c('0', '1'),direction = "<")
#B_roc = roc(setInfFB$cut, as.numeric(wh),levels = c('1', '0'),direction = "<")
#LG_roc = roc(setInfFLG$cut, as.numeric(wh),levels = c('0', '1'),direction = "<")

library(ggplot2)
library(ggsci)
library("scales")
mypal <- pal_npg("nrc", alpha = 0.7)(9)
mypal
show_col(mypal)




plot(knn_roc)
lines(SVM_roc)
lines(GBRT_roc)
lines(B_roc)
lines(LG_roc)

lwdS = 3

plot(smooth(knn_roc), col = mypal[1],
     print.auc=TRUE, #display pAUC value on the plot with following options:
     
     print.auc.pattern=paste("AUC of KNN =", round(knn_roc$auc[1],3)), 
     print.auc.x=0.7,print.auc.y=0.5,
     lwd=lwdS,
     main="ROC of models"
)
legend("bottomright", legend=c("KNN", "SVM", "GBDT",'Bayes','LR'), col=mypal, lwd=lwdS,)


plot.roc(SVM_roc,
         add=T,  # 增加曲线
         lwd=lwdS,
         print.auc=TRUE, print.auc.pattern=paste("AUC of SVM =", round(SVM_roc$auc[1],3)),print.auc.x=0.7,print.auc.y=0.4,
         smooth = T,col = mypal[2])  
plot.roc(smooth(GBRT_roc),
         add=T,  # 增加曲线
         lwd=lwdS,
         print.auc=TRUE, print.auc.pattern=paste("AUC of GBDT =", round(GBRT_roc$auc[1],3)),print.auc.x=0.7,print.auc.y=0.3,
         smooth = F,col = mypal[3])  
plot.roc(smooth(B_roc),
         add=T,  # 增加曲线
         lwd=lwdS,
         print.auc=TRUE,print.auc.pattern=paste("AUC of Bayes =", round(B_roc$auc[1],3)), print.auc.x=0.7,print.auc.y=0.2,
         smooth = T,col = mypal[4])  
plot.roc(smooth(LG_roc),
         add=T,  # 增加曲线
         lwd=lwdS,
         print.auc=TRUE, print.auc.pattern=paste("AUC of LR =", round(LG_roc$auc[1],3)),print.auc.x=0.7,print.auc.y=0.1,
         smooth = T,col = mypal[5])  


auc = c(as.numeric(knn_roc$auc[1]), SVM_roc$auc[1], GBRT_roc$auc[1], B_roc$auc[1], LG_roc$auc[1])

coords(LG_roc, "best")



#####unsmooth#####
plot(knn_roc, col = mypal[1],
     print.auc=TRUE, 
     
     print.auc.pattern=paste("AUC of KNN =", round(knn_roc$auc[1],3)), 
     print.auc.x=0.7,print.auc.y=0.5,
     lwd=lwdS,
     main="ROC of models"
)
legend("bottomright", legend=c("KNN", "GBDT",'Bayes'), col=mypal, lwd=lwdS,)



plot.roc(GBRT_roc,
         add=T,  # 增加曲线
         lwd=lwdS,
         print.auc=TRUE, print.auc.pattern=paste("AUC of GBDT =", round(GBRT_roc$auc[1],3)),print.auc.x=0.7,print.auc.y=0.3,
         smooth = F,col = mypal[3])  
plot.roc(B_roc,
         add=T,  # 增加曲线
         lwd=lwdS,
         print.auc=TRUE,print.auc.pattern=paste("AUC of Bayes =", round(B_roc$auc[1],3)), print.auc.x=0.7,print.auc.y=0.2,
         smooth = F,col = mypal[4])  







library(openxlsx)
write.xlsx(setInfF_out, file = "18_catchKNN_mitModel.xlsx", colNames = TRUE)

save.image(file = "18_catchKNN_mitModel.RData")


####出图####

rm(list = ls())
library(proxy)
library(dplyr)
library(kknn)
library(pROC)
library(modEvA)
library(class)


load('18_catchKNN_mitModel.RData')

library(plotROC)
library(tidyverse)
library(ggplot2)
library(ggsci)

mypal <- pal_npg("nrc", alpha = 0.7)(9)
mypal
library("scales")
show_col(mypal)






#####unsmooth#####
par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = TRUE) # 增加右侧边距，并允许绘制在边距之外

plot(knn_roc, col = mypal[1],
     print.auc=TRUE, 
     
     print.auc.pattern=paste("AUC of KNN =", round(knn_roc$auc[1],3)), 
     print.auc.x=0.4,print.auc.y=0.5,
     lwd=lwdS,
     main="ROC of models"
)
legend(x = -0.1, y = 0.7, # x 和 y 的值可以根据你的具体图形进行调整
       legend=c("KNN", "SVM", "GBDT",'Bayes','LR'),
       col=mypal,
       lwd=lwdS,
       bty = "n", # 可选: 去掉图例边框
       xjust = 0, # 左对齐图例文本
       yjust = 1, # 顶部对齐图例文本
       cex = 0.8) # 可选: 调整图例字体大小

plot.roc(SVM_roc,
         add=T,  # 增加曲线
         lwd=lwdS,
         print.auc=TRUE, print.auc.pattern=paste("AUC of SVM =", round(SVM_roc$auc[1],3)),print.auc.x=0.4,print.auc.y=0.4,
         smooth = F,col = mypal[2])  
plot.roc(GBRT_roc,
         add=T,  # 增加曲线
         lwd=lwdS,
         print.auc=TRUE, print.auc.pattern=paste("AUC of GBDT =", round(GBRT_roc$auc[1],3)),print.auc.x=0.4,print.auc.y=0.3,
         smooth = F,col = mypal[3])  
plot.roc(B_roc,
         add=T,  # 增加曲线
         lwd=lwdS,
         print.auc=TRUE,print.auc.pattern=paste("AUC of Bayes =", round(B_roc$auc[1],3)), print.auc.x=0.4,print.auc.y=0.2,
         smooth = F,col = mypal[4])  
plot.roc(LG_roc,
         add=T,  # 增加曲线
         lwd=lwdS,
         print.auc=TRUE, print.auc.pattern=paste("AUC of LR =", round(LG_roc$auc[1],3)),print.auc.x=0.4,print.auc.y=0.1,
         smooth = F,col = mypal[5])  


auc = c(as.numeric(knn_roc$auc[1]), SVM_roc$auc[1], GBRT_roc$auc[1], B_roc$auc[1], LG_roc$auc[1])

#rm(list=ls()[!grepl('setInfF_out', ls())])
#save.image(file = "18_KNN_out.RData")

