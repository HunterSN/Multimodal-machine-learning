rm(list = ls())
library(dplyr)
library(tidyr)
library(openxlsx)
library(nortest)  # 正态性检验

# 读取数据
df <- read.xlsx("39_indata_handjust_in_GTXSMatched.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
df[is.na(df)] <- 0  # 将NA替换为0，保留原始缺失值

# 正态性检验函数
check_normality <- function(data) {
  test_result <- ad.test(data)  # Anderson-Darling检验
  return(test_result$p.value > 0.05)  # 返回是否正态（p > 0.05）
}

# 数据描述函数
describe_data <- function(data, var) {
  if (is.numeric(data[[var]])) {
    # 连续变量 - 按组别(typeNum)统计
    data_grouped <- data %>%
      group_by(typeNum) %>%
      summarise(
        样本数 = sum(!is.na(.data[[var]])),
        均值 = ifelse(check_normality(.data[[var]]), mean(.data[[var]], na.rm = TRUE), NA),
        标准差 = ifelse(check_normality(.data[[var]]), sd(.data[[var]], na.rm = TRUE), NA),
        中位数 = median(.data[[var]], na.rm = TRUE),
        下四分位数 = quantile(.data[[var]], 0.25, na.rm = TRUE),
        上四分位数 = quantile(.data[[var]], 0.75, na.rm = TRUE),
        .groups = "drop"  # 解除group_by影响
      ) %>%
      mutate(变量 = var, 类别 = NA)  # 添加类别列，确保类型匹配
    return(data_grouped)
  } else {
    # 分类变量 - 按组别(typeNum)计算频数和百分比
    data_grouped <- data %>%
      group_by(typeNum, .data[[var]]) %>%
      summarise(频数 = n(), .groups = "drop") %>%
      group_by(typeNum) %>%
      mutate(百分比 = 频数 / sum(频数) * 100) %>%
      rename(类别 = .data[[var]]) %>%
      mutate(变量 = var)
    
    # 确保"类别"列为字符型，避免factor/ordered类型不匹配
    data_grouped$类别 <- as.character(data_grouped$类别)
    
    return(data_grouped)
  }
}

# 二分类变量分析
analyze_binary <- function(data, var) {
  table_data <- table(data$typeNum, data[[var]])
  if (any(chisq.test(table_data)$expected < 5)) {
    test <- fisher.test(table_data)
    method <- "Fisher精确检验"
  } else {
    test <- chisq.test(table_data)
    method <- "卡方检验"
  }
  
  # 计算频数和百分比
  freq_table <- data %>%
    group_by(typeNum, .data[[var]]) %>%
    summarise(频数 = n(), .groups = "drop") %>%
    group_by(typeNum) %>%
    mutate(百分比 = 频数 / sum(频数) * 100) %>%
    rename(类别 = .data[[var]])
  
  # 合并检验结果和频数表
  results <- freq_table %>%
    mutate(变量 = var, 方法 = method, p值 = test$p.value)
  
  return(results)
}

# 等级变量分析
analyze_ordinal <- function(data, var) {
  data_grouped <- data %>%
    group_by(typeNum, .data[[var]]) %>%
    summarise(频数 = n(), .groups = "drop") %>%
    group_by(typeNum) %>%
    mutate(百分比 = 频数 / sum(频数) * 100) %>%
    rename(类别 = .data[[var]]) %>%
    mutate(变量 = var)
  
  # 确保"类别"列为字符型，避免factor/ordered类型不匹配
  data_grouped$类别 <- as.character(data_grouped$类别)
  
  # 将等级变量转换为数值型
  data[[var]] <- as.numeric(data[[var]])
  
  # Mann-Whitney U检验
  test <- wilcox.test(data[[var]] ~ data$typeNum)
  
  
  
  # 合并检验结果和频数表
  results <- data_grouped %>%
    mutate(变量 = var, 方法 = "Mann-Whitney U检验", p值 = test$p.value)
  
  return(results)
}

#独立样本连续变量
analyze_continuous <- function(data, var, group_var = "typeNum") {
  group_0 <- data %>% filter(typeNum == 0) %>% pull(var)
  group_1 <- data %>% filter(typeNum == 1) %>% pull(var)
  
  # 正态性检验
  is_normal <- check_normality(group_0) && check_normality(group_1)
  
  # 根据正态性选择检验方法
  if (is_normal) {
    test <- t.test(group_0, group_1)
    method <- "独立样本t检验"
  } else {
    test <- wilcox.test(group_0, group_1)
    method <- "Mann-Whitney U检验"
  }
  
  # 计算描述性统计量（对照组）
  desc_group_0 <- data %>%
    filter(typeNum == 0) %>%
    summarise(
      样本数 = sum(!is.na(.data[[var]])),
      均值 = mean(.data[[var]], na.rm = TRUE),
      标准差 = sd(.data[[var]], na.rm = TRUE),
      中位数 = median(.data[[var]], na.rm = TRUE),
      下四分位数 = quantile(.data[[var]], 0.25, na.rm = TRUE),
      上四分位数 = quantile(.data[[var]], 0.75, na.rm = TRUE)
    )
  
  # 计算描述性统计量（暴露组）
  desc_group_1 <- data %>%
    filter(typeNum == 1) %>%
    summarise(
      样本数 = sum(!is.na(.data[[var]])),
      均值 = mean(.data[[var]], na.rm = TRUE),
      标准差 = sd(.data[[var]], na.rm = TRUE),
      中位数 = median(.data[[var]], na.rm = TRUE),
      下四分位数 = quantile(.data[[var]], 0.25, na.rm = TRUE),
      上四分位数 = quantile(.data[[var]], 0.75, na.rm = TRUE)
    )
  
  # 合并结果
  results <- data.frame(
    变量 = var,
    组别 = c("0", "1"),
    样本数 = c(desc_group_0$样本数, desc_group_1$样本数),
    均值 = c(desc_group_0$均值, desc_group_1$均值),
    标准差 = c(desc_group_0$标准差, desc_group_1$标准差),
    中位数 = c(desc_group_0$中位数, desc_group_1$中位数),
    下四分位数 = c(desc_group_0$下四分位数, desc_group_1$下四分位数),
    上四分位数 = c(desc_group_0$上四分位数, desc_group_1$上四分位数),
    方法 = method,
    p值 = test$p.value
  )
  
  return(results)
}

# 连续变量分析（治疗前后对比）
paired_analysis <- function(data, var_in, var_out, group_value) {
  pre <- data %>% filter(typeNum == group_value) %>% pull(var_in)
  post <- data %>% filter(typeNum == group_value) %>% pull(var_out)
  
  is_normal <- check_normality(pre - post)
  
  if (is_normal) {
    test <- t.test(pre, post, paired = TRUE)
    method <- "配对t检验"
  } else {
    test <- wilcox.test(pre, post, paired = TRUE)
    method <- "Wilcoxon符号秩检验"
  }
  
  # 计算治疗前的描述性统计量
  desc_pre <- data %>%
    filter(typeNum == group_value) %>%
    summarise(
      样本数 = sum(!is.na(.data[[var_in]])),
      均值 = mean(.data[[var_in]], na.rm = TRUE),
      标准差 = sd(.data[[var_in]], na.rm = TRUE),
      中位数 = median(.data[[var_in]], na.rm = TRUE),
      下四分位数 = quantile(.data[[var_in]], 0.25, na.rm = TRUE),
      上四分位数 = quantile(.data[[var_in]], 0.75, na.rm = TRUE)
    )
  
  # 计算治疗后的描述性统计量
  desc_post <- data %>%
    filter(typeNum == group_value) %>%
    summarise(
      样本数 = sum(!is.na(.data[[var_out]])),
      均值 = mean(.data[[var_out]], na.rm = TRUE),
      标准差 = sd(.data[[var_out]], na.rm = TRUE),
      中位数 = median(.data[[var_out]], na.rm = TRUE),
      下四分位数 = quantile(.data[[var_out]], 0.25, na.rm = TRUE),
      上四分位数 = quantile(.data[[var_out]], 0.75, na.rm = TRUE)
    )
  
  # 合并结果
  results <- data.frame(
    变量 = gsub("_in", "", var_in),
    组别 = group_value,
    样本数 = desc_pre$样本数,
    均值_in = desc_pre$均值,
    标准差_in = desc_pre$标准差,
    中位数_in = desc_pre$中位数,
    下四分位数_in = desc_pre$下四分位数,
    上四分位数_in = desc_pre$上四分位数,
    均值_out = desc_post$均值,
    标准差_out = desc_post$标准差,
    中位数_out = desc_post$中位数,
    下四分位数_out = desc_post$下四分位数,
    上四分位数_out = desc_post$上四分位数,
    方法 = method,
    p值 = test$p.value
  )
  
  return(results)
}

#####提取变量名#####
variables_binary <- c("sex","SBP_outOK","DBP_outOK")  # 二分类变量
variables_ordinal <- c("marital", "HYClassification", "HYRiskFactor","smokeFre","drinkFre")  # 等级变量
categorical_vars <- c("sex", "marital", "HYClassification", "HYRiskFactor")
df <- df %>%
  mutate(across(all_of(categorical_vars), as.factor))
df <- df %>%
  mutate(across(all_of(variables_ordinal), 
                ~ factor(.x, levels = sort(unique(.x)), ordered = TRUE)))

str(df[variables_ordinal]) 

variables_in <- colnames(df)[grep("_in$", colnames(df))]  # 连续变量（治疗前）
variables_out <- colnames(df)[grep("_out$", colnames(df))]  # 连续变量（治疗后）

####基线指标####


col_to_analysis <- c("sex",
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
                     "ALT_in",
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
                     "ALT_in",
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
                     "ALT_out",
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
                     "ALT_out",
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

####数据描述####
results_describe <- lapply(colnames(df), function(var) {
  if (var %in% col_to_analysis) {
    describe_data(df, var)
  }
}) %>% bind_rows()

####分析二分类变量_基线####
results_binary <- lapply(variables_binary, function(var) {
  analyze_binary(df, var)
}) %>% bind_rows()

####分析等级变量_基线####
results_ordinal <- lapply(variables_ordinal, function(var) {
  analyze_ordinal(df, var)
}) %>% bind_rows()

####连续变量治疗前后对比_疗效####
results_continuous <- lapply(variables_in, function(var_in) {
  var_out <- gsub("_in", "_out", var_in)
  rbind(
    paired_analysis(df, var_in, var_out, 0),  # 对照组
    paired_analysis(df, var_in, var_out, 1)   # 暴露组
  )
}) %>% bind_rows()

####治疗前暴露组与对照组的对比_基线####
a <- setdiff(col_to_analysis, variables_binary)
a <- setdiff(a, variables_ordinal)
a <- setdiff(a, variables_out)
results_pre_group <- lapply(a, function(var) {
  analyze_continuous(df, var)
}) %>% bind_rows()

####治疗后暴露组与对照组的对比_疗效####
results_post_group <- lapply(variables_out, function(var) {
  analyze_continuous(df, var)
}) %>% bind_rows()

#####创建正态性检验结果####
results_normality <- lapply(colnames(df), function(var) {
  if (is.numeric(df[[var]])) {
    is_normal <- check_normality(df[[var]])
    return(data.frame(变量 = var, 是否正态 = ifelse(is_normal, "是", "否")))
  }
}) %>% bind_rows()


# 独立样本的数据整理（暴露组与对照组对比）
singlecontinuous_adjust_table <- function(results) {
  results %>%
    mutate(
      组别_label = case_when(
        组别 == 0 ~ "对照组",
        组别 == 1 ~ "暴露组"
      ),
      值 = case_when(
        方法 %in% c("配对t检验", "独立样本t检验") ~ paste(均值, "±", 标准差),
        方法 %in% c("Wilcoxon符号秩检验", "Mann-Whitney U检验") ~ paste(中位数, "(", 下四分位数, ",", 上四分位数, ")"),
        TRUE ~ NA_character_
      ),
      P值 = ifelse(is.na(p值), "NA", round(p值, 3))
    ) %>%
    select(变量, 组别_label, 样本数, 值, 方法, P值) %>%
    spread(key = "组别_label", value = "值")
}



# 配对连续性变量（治疗前后）
paired_adjust_table <- function(data) {
  data %>%
    mutate(
      治疗前 = case_when(
        方法 %in% c("配对t检验", "独立样本t检验") ~ paste(均值_in, "±", 标准差_in),
        方法 %in% c("Wilcoxon符号秩检验", "Mann-Whitney U检验") ~ paste(中位数_in, "(", 下四分位数_in, ",", 上四分位数_in, ")"),
        TRUE ~ NA_character_
      ),
      治疗后 = case_when(
        方法 %in% c("配对t检验", "独立样本t检验") ~ paste(均值_out, "±", 标准差_out),
        方法 %in% c("Wilcoxon符号秩检验", "Mann-Whitney U检验") ~ paste(中位数_out, "(", 下四分位数_out, ",", 上四分位数_out, ")"),
        TRUE ~ NA_character_
      ),
      P值 = ifelse(is.na(p值), "NA", round(p值, 3))
    ) %>%
    select(组别, 变量, 样本数, 治疗前, 治疗后,方法, P值) %>%
    arrange(组别, 变量)
}


#分类变量
group_paired_adjust_table <- function(data) {
  # 创建新表格，保留频数和百分比
  result <- data %>%
    group_by(类别) %>%
    mutate(
      # 通过mutate，合并频数和百分比
      `0组(百分比)` = ifelse(typeNum == 0, paste0(频数, " (", sprintf("%.2f", 百分比), "%)"), NA),
      `1组(百分比)` = ifelse(typeNum == 1, paste0(频数, " (", sprintf("%.2f", 百分比), "%)"), NA),
      # 保留方法和P值
      方法 = first(方法),
      P值 = first(p值)
    ) %>%
    # 按类别分组后消除NA
    group_by(类别) %>%
    summarise(
      `0组(百分比)` = na.omit(`0组(百分比)`)[1],  # 获取非NA的第一个值
      `1组(百分比)` = na.omit(`1组(百分比)`)[1],  # 获取非NA的第一个值
      方法 = first(方法),
      P值 = first(p值)
    ) %>%
    ungroup() %>%
    # 删除 typeNum 列
    select(类别, `0组(百分比)`, `1组(百分比)`, 方法, P值)
  
  return(result)
}




# 调整并输出


results_binary_adjusted <- group_paired_adjust_table(results_binary)

results_ordinal_adjusted <- group_paired_adjust_table(results_ordinal)


results_pre_group_adjusted <- singlecontinuous_adjust_table(results_pre_group) #连续性变量_基线

results_post_group_adjusted <- singlecontinuous_adjust_table(results_post_group) #治疗后两组对比_疗效


# 调整后的表格
results_continuous_adjusted <- paired_adjust_table(results_continuous) #治疗前后对比_疗效



# 生成Excel表格
wb <- createWorkbook()
addWorksheet(wb, "正态性检验结果")
writeData(wb, sheet = "正态性检验结果", results_normality)

addWorksheet(wb, "二分类变量分析")
writeData(wb, sheet = "二分类变量分析", results_binary_adjusted)
addWorksheet(wb, "等级变量分析")
writeData(wb, sheet = "等级变量分析", results_ordinal_adjusted)

addWorksheet(wb, "连续性变量_基线")
writeData(wb, sheet = "连续性变量_基线", results_pre_group_adjusted)
addWorksheet(wb, "治疗后两组对比_疗效")
writeData(wb, sheet = "治疗后两组对比_疗效", results_post_group_adjusted)
addWorksheet(wb, "治疗前后对比_疗效")
writeData(wb, sheet = "治疗前后对比_疗效", results_continuous_adjusted)


addWorksheet(wb, "全_二分类变量分析")
writeData(wb, sheet = "全_二分类变量分析", results_binary)
addWorksheet(wb, "全_等级变量分析")
writeData(wb, sheet = "全_等级变量分析", results_ordinal)



addWorksheet(wb, "全_治疗前暴露组与对照组对比_基线")
writeData(wb, sheet = "全_治疗前暴露组与对照组对比_基线", results_pre_group)
addWorksheet(wb, "全_连续变量治疗前后对比_疗效")
writeData(wb, sheet = "全_连续变量治疗前后对比_疗效", results_continuous)
addWorksheet(wb, "全_治疗后暴露组与对照组对比_疗效")
writeData(wb, sheet = "全_治疗后暴露组与对照组对比_疗效", results_post_group)

addWorksheet(wb, "数据描述")
writeData(wb, sheet = "数据描述", results_describe)

saveWorkbook(wb, file = "39_统计分析结果.xlsx", overwrite = TRUE)


