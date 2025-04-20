rm(list = ls())
load('17_distances.RData')
library(dplyr)
library(kknn)
library(iml)
library(pROC)
library(modEvA)

ZoutKNN = df_filtered
ZoutKNN$num = 1:nrow(ZoutKNN)
ZoutKNN$cut = 0
ZoutKNN$cut[which(ZoutKNN$target %in% HYGenes$gene)] = 1
table(ZoutKNN$cut)
rownames(ZoutKNN) = 1:nrow(ZoutKNN)
ZoutKNN <- ZoutKNN %>%
  mutate(across(-c(1:2), as.numeric))
set.seed(1234)
KS = 100 # 确保 KS 在这里被定义
# 选择用于模型的特征
features <- c("gradcam_weight", "ob", "gosim", "Euc_zscore", "Can_zscore")
target <- "cut"
data_for_model <- ZoutKNN %>% select(all_of(c(target, features))) %>% na.omit()
v = 10 # 交叉次数
grps = cut(1:nrow(data_for_model), v, labels = FALSE)[sample(1:nrow(data_for_model))]
knn_predict_function <- function(model, newdata) {
  predictions <- predict(model, newdata)
  # 对于分类任务，返回预测为类别 '1' 的概率
  if (is.factor(model$fitted.values)) {
    prob_one <- ifelse(predictions == levels(model$fitted.values)[2], 1, 0)
    return(prob_one)
  } else {
    return(as.numeric(predictions))
  }
}
shap_values_per_fold <- lapply(1:v, function(i, data) {
  omit <- which(grps == i)
  train_data <- data[-omit, ]
  test_data <- data[omit, ]
  
  knn_model_fold <- kknn(as.formula(paste(target, "~", paste(features, collapse = "+"))),
                         train = train_data,
                         k = KS,
                         distance = 2)
  
  # 修改 predictor 的创建
  predictor_fold <- iml::Predictor$new(model = knn_model_fold,
                                       data = train_data[, features],
                                       y = train_data[[target]],
                                       predict.function = function(x) {
                                         predictions <- predict(knn_model_fold, newdata = as.data.frame(x))
                                         if (is.factor(knn_model_fold$fitted.values)) {
                                           prob_one <- ifelse(predictions == levels(knn_model_fold$fitted.values)[2], 1, 0)
                                           return(prob_one)
                                         } else {
                                           return(as.numeric(predictions))
                                         }
                                       })
  
  shapley_values_fold <- iml::Shapley$new(predictor = predictor_fold, x.interest = test_data[, features])
  return(shapley_values_fold$results)
}, data = data_for_model)

# 合并所有 fold 的 SHAP 值
all_shap_values <- do.call(rbind, shap_values_per_fold)














