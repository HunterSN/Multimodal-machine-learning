rm(list = ls())
# 加载必要的包
library(readxl)    # 用于读取Excel文件
library(dplyr)     # 用于数据操作
library(purrr)     # 用于函数式编程
library(writexl)   # 用于写入Excel文件

# 设置工作目录到包含所有Excel文件的文件夹
# 请将下面的路径替换为你的实际文件夹路径
folder_path <- "686个中药 - 化合物 - 靶点信息文件"  # 替换为你文件夹的实际路径

# 获取文件夹中的所有xlsx文件
file_list <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# 定义读取和提取函数
read_and_extract <- function(file_path) {
  data <- read_excel(file_path)
  required_cols <- c("Herb_id", "Molecule_name", "Gene symbol", "Gene name", "Protein name")
  extracted <- data %>% select(any_of(required_cols))
  return(extracted)
}

# 合并所有文件
herbMolTarget <- map_dfr(file_list, read_and_extract)

rm(list=ls()[!grepl('herbMolTarget', ls())])


save.image('14_herbMolTarget.RData')
