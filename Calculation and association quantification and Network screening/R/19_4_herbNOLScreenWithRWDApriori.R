rm(list = ls())

load('19_herbMOLScreen_catch1.RData')
# 加载必要包
library(dplyr)
library(arules)
library(igraph)
library(tidyr)
library(stringr)

library(dplyr)
library(igraph)

library(openxlsx)
RWDHerb = read.xlsx("19_2_RWDHerb_out.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)


# 按处方分组并整理为字符向量
trans_list <- RWDHerb %>%
  group_by(门诊流水号) %>%
  summarise(items = paste(unique(整理后草药), collapse = ",")) %>%
  pull(items) %>%
  strsplit(",")

# 转换为事务型数据
transactions <- as(trans_list, "transactions")


####运行Apriori算法####


# 设置参数：最小支持度0.01，最小置信度0.3
rules <-apriori(as(transactions,'transactions'),parameter = list(supp = 0,conf = 0,maxlen = 2,minlen = 2))


rules_df =inspect(rules)


rules_df$lhs =  gsub('[{]',"",rules_df$lhs)
rules_df$lhs =  gsub('[}]',"",rules_df$lhs)

rules_df$rhs =  gsub('[{]',"",rules_df$rhs)
rules_df$rhs =  gsub('[}]',"",rules_df$rhs)



####1. 数据整合：关联中药、化合物与靶点权重####


# 合并中药-化合物-靶点数据，关联wh_KNN
herb_mol_target_weight <- herbMolTarget %>%
  left_join(setInfF_clean, by = c("Molecule_name" = "compound_id")) %>%
  select(Herb_id, Molecule_name, target, wh_KNN) %>%
  na.omit()  # 去除无靶点或权重的记录

# 计算每个中药的靶点总权重（加权和）
herb_target_strength <- herb_mol_target_weight %>%
  group_by(Herb_id) %>%
  summarise(
    total_wh_KNN = sum(wh_KNN, na.rm = TRUE),  # 总权重
    avg_wh_KNN = mean(wh_KNN, na.rm = TRUE)    # 平均权重（可选）
  )

####构建融合共现权重与关联规则权重的中药网络####


library(dplyr)
library(igraph)

# 示例数据预览
head(RWDHerb)  # 列：门诊流水号, 整理后草药
head(rules_df) # 列：lhs, rhs, lift, support, confidence, ...



# 计算中药共现边列表
cooccur_edges <- RWDHerb %>%
  group_by(门诊流水号) %>%
  summarise(herbs = list(整理后草药)) %>%
  mutate(
    combo = lapply(herbs, function(x) {
      if (length(x) >= 2) {
        combn(x, 2, simplify = FALSE)
      } else {
        NULL
      }
    })
  ) %>%  # 明确闭合 mutate()
  unnest(combo) %>%
  mutate(
    from = sapply(combo, `[`, 1),
    to = sapply(combo, `[`, 2)
  ) %>%
  group_by(from, to) %>%
  summarise(cooccur = n()) %>%
  ungroup()
# 构建无向网络
g_herb <- graph_from_data_frame(cooccur_edges, directed = FALSE)
E(g_herb)$cooccur <- cooccur_edges$cooccur  # 边属性：cooccur

#处理关联规则权重（lift）
# 提取关联规则中的中药对及lift值
# 提取关联规则中的中药对及lift值
rules_df = rules_df[,names(rules_df) %in% c("lhs","rhs","lift")] 



rules_edges <- rules_df %>%
  # 过滤无效规则
  filter(
    !is.na(lhs), !is.na(rhs),
    lhs != "", rhs != "",
    grepl(".+\\s*=>\\s*.+", lhs),
    grepl(".+\\s*=>\\s*.+", rhs)
  ) %>%
  # 提取中药名称
  mutate(
    from = trimws(sub("\\s*=>\\s*.*", "", lhs)),
    to = trimws(sub(".*\\s*=>\\s*", "", rhs))
  ) %>%
  # 进一步清理
  filter(from != "", to != "", !is.na(lift)) %>%
  select(from, to, lift) %>%
  distinct()

# 过滤不存在的节点（确保中药名称与网络一致）
valid_edges <- rules_edges %>%
  filter(from %in% V(g_herb)$name & to %in% V(g_herb)$name)

# 清理关联规则中的中药名称（去空格、统一为小写）
valid_edges_clean <- valid_edges %>%
  mutate(
    from = trimws(tolower(from)),
    to = trimws(tolower(to))
  )

# 同步清理网络节点名称
V(g_herb)$name <- trimws(tolower(V(g_herb)$name))

# 提取网络中的有效节点
existing_nodes <- V(g_herb)$name

# 过滤valid_edges，确保from和to均存在于网络中
valid_edges_filtered <- valid_edges_clean %>%
  filter(from %in% existing_nodes & to %in% existing_nodes)



# 将valid_edges_filtered转换为igraph兼容的边列表
edges_to_add <- as.vector(t(as.matrix(valid_edges_filtered[, c("from", "to")])))


# 添加边并直接赋予lift属性
g_herb <- add_edges(
  graph = g_herb,
  edges = edges_to_add,
  attr = list(lift = valid_edges_filtered$lift)
)

# 合并重复边（若同一对中药多次出现，保留最大lift值）
g_herb <- simplify(
  g_herb,
  edge.attr.comb = list(lift = "max", cooccur = "first"),
  remove.loops = FALSE
)

                        
# 初始化未关联边的lift值为0
E(g_herb)$lift[is.na(E(g_herb)$lift)] <- 0


#### 融合共现权重与关联规则权重####
#将两种权重整合为综合权重（例如加权平均）。
# 定义权重融合公式（示例：cooccur占70%，lift占30%）
# 检查 cooccur 的方差
var_cooccur <- var(E(g_herb)$cooccur, na.rm = TRUE)
if (var_cooccur == 0) {
  cat("警告：cooccur 的方差为0，无法标准化。\n")
} else {
  cat("cooccur 的方差为:", var_cooccur, "\n")
}

# 检查 lift 的方差
var_lift <- var(E(g_herb)$lift, na.rm = TRUE)
if (var_lift == 0) {
  cat("警告：lift 的方差为0，无法标准化。\n")
} else {
  cat("lift 的方差为:", var_lift, "\n")
}

# 检查缺失值
sum_na_cooccur <- sum(is.na(E(g_herb)$cooccur))
sum_na_lift <- sum(is.na(E(g_herb)$lift))
cat("cooccur 的缺失值数量:", sum_na_cooccur, "\n")
cat("lift 的缺失值数量:", sum_na_lift, "\n")

safe_scale <- function(x) {
  if (var(x, na.rm = TRUE) == 0) {
    # 方差为零时返回0或原值
    return(rep(0, length(x)))  # 或 return(x)
  } else {
    return(scale(x))
  }
}

# 标准化 cooccur
scaled_cooccur <- safe_scale(E(g_herb)$cooccur)

# 标准化 lift
scaled_lift <- safe_scale(E(g_herb)$lift)

# 融合权重
E(g_herb)$weight <- 0.7 * scaled_cooccur + 0.3 * scaled_lift

summary(E(g_herb)$weight)
head(E(g_herb)$weight)





####2. 构建多维度中药网络####
#2.1 融合临床共现、关联规则与靶点权重

# 基础网络（基于临床共现和关联规则，沿用之前代码）
# g_herb: 包含共现权重(cooccur)和关联规则权重(lift)

# 添加靶点权重作为节点属性
V(g_herb)$wh_KNN_total <- herb_target_strength$total_wh_KNN[match(V(g_herb)$name, herb_target_strength$Herb_id)]

# 处理缺失值（无靶点数据的中药设为0或均值）
V(g_herb)$wh_KNN_total[is.na(V(g_herb)$wh_KNN_total)] <- 0

#2.2 动态调整边权重

#将靶点权重影响融入共现网络：

#基于靶点权重调整PageRank计算（更推荐）
V(g_herb)$pagerank <- page_rank(
  g_herb,
  weights = E(g_herb)$weight,
  personalized = V(g_herb)$wh_KNN_total  # 个性化向量，靶点权重影响PageRank
)$vector


####3综合评分公式优化####

library(dplyr)

# 计算中药频次
herb_freq <- RWDHerb %>%
  count(整理后草药, name = "frequency") %>%
  rename(Herb = 整理后草药)

# 示例输出
head(herb_freq)
# 标准化名称格式（去除空格、统一小写）
herb_freq$Herb <- trimws(tolower(herb_freq$Herb))
V(g_herb)$name <- trimws(tolower(V(g_herb)$name))

# 添加 frequency 属性到网络节点
V(g_herb)$frequency <- herb_freq$frequency[
  match(V(g_herb)$name, herb_freq$Herb)
]

# 处理缺失值（无频次记录的中药设为0）
V(g_herb)$frequency[is.na(V(g_herb)$frequency)] <- 0



#结合临床频次、网络拓扑与靶点权重：
# 标准化各指标（消除量纲差异）
V(g_herb)$frequency_scaled <- scale(V(g_herb)$frequency)
V(g_herb)$pagerank_scaled <- scale(V(g_herb)$pagerank)
V(g_herb)$wh_KNN_scaled <- scale(V(g_herb)$wh_KNN_total)

# 动态权重分配（根据数据特性调整系数）
alpha <- 0.4  # 临床频次
beta <- 0.3   # 网络拓扑（PageRank）
gamma <- 0.3  # 靶点权重

V(g_herb)$score <- 
  alpha * V(g_herb)$frequency_scaled + 
  beta * V(g_herb)$pagerank_scaled + 
  gamma * V(g_herb)$wh_KNN_scaled

# 筛选TOP中药
top_herbs <- V(g_herb)$name[order(-V(g_herb)$score)] %>% head(20)





####4. 验证与调参####
#4.1 结果对比
# 查看靶点权重对排名的影响

result <- data.frame(
  Herb = V(g_herb)$name,
  Frequency = V(g_herb)$frequency,
  PageRank = V(g_herb)$pagerank,
  wh_KNN = V(g_herb)$wh_KNN_total,
  Score = V(g_herb)$score
) %>% arrange(desc(Score))

# 获取临床高频中药（前20名）
clinical_top <- herb_freq %>%
  slice_max(frequency, n = 20) %>%
  pull(Herb)  # 注意列名已改为 "Herb"

# 计算重叠比例
overlap <- sum(result$Herb[1:20] %in% clinical_top) / 20
cat("Top 20 中药与临床高频中药的重叠比例:", overlap, "\n")
#4.2 参数调优
#通过网格搜索优化权重系数：
# 定义参数组合
params <- expand.grid(
  alpha = seq(0.2, 0.6, by=0.1),  # 更细粒度
  beta = seq(0.2, 0.5, by=0.1),
  gamma = seq(0.1, 0.4, by=0.1)
)

# 评估每个参数组合的重叠率
best_overlap <- 0
best_params <- list()

for (i in 1:nrow(params)) {
  alpha <- params$alpha[i] #临床使用频次（frequency_scaled）的权重。
  beta <- params$beta[i] #网络PageRank中心性（pagerank_scaled）的权重。
  gamma <- params$gamma[i] #靶点调控权重（wh_KNN_scaled）的权重
  
  scores <- alpha*V(g_herb)$frequency_scaled + 
    beta*V(g_herb)$pagerank_scaled + 
    gamma*V(g_herb)$wh_KNN_scaled
  
  current_top <- V(g_herb)$name[order(-scores)][1:20]
  overlap <- mean(current_top %in% clinical_top)
  
  if (overlap > best_overlap) {
    best_overlap <- overlap
    best_params <- list(alpha=alpha, beta=beta, gamma=gamma)
  }
}

# 引入F1-score（平衡准确率与召回率）
precision <- mean(current_top %in% clinical_top)
recall <- mean(clinical_top %in% current_top)
f1 <- 2 * (precision * recall) / (precision + recall)



####5. 可视化验证####

#5.1 网络拓扑可视化
library(ggraph)
library(showtext)
# 自动查找可用中文字体
font_add_google("Noto Sans SC", "notosans")  # 思源黑体
showtext_auto()
# 调整权重（确保正值）
E(g_herb)$weight_pos <- ifelse(
  E(g_herb)$weight <= 0, 
  0.01,  # 替换非正值
  E(g_herb)$weight
)

# 可视化
ggraph(g_herb, layout = "fr", weights = E(g_herb)$weight_pos) +
  geom_edge_link(
    aes(width = weight_pos, alpha = weight_pos),
    color = "grey50"
  ) +
  geom_node_point(
    aes(size = score, color = wh_KNN_total),
    show.legend = TRUE
  ) +
  geom_node_text(
    aes(label = name, filter = score > quantile(score, 0.9)),
    repel = TRUE, size = 3, family = "SimHei"
  ) +
  scale_edge_width(range = c(0.5, 3)) +
  scale_edge_alpha(range = c(0.1, 0.8)) +
  scale_color_gradientn(
    colours = c("blue", "green", "red"),
    name = "靶点权重"
  ) +
  labs(
    title = "中药-靶点网络拓扑",
    subtitle = "节点大小:综合评分, 颜色:靶点权重"
  ) +
  theme_void()


#5.2 指标相关性分析

library(corrplot)
cor_matrix <- cor(result[, c("Frequency", "PageRank", "wh_KNN")])
corrplot(cor_matrix, method = "number")



#save.image(file = "19_4_herbNOLScreenWithRWDApriori.RData")


load('19_4_herbNOLScreenWithRWDApriori.RData')



# 将 igraph 网络转换为边列表数据框
edges_df <- as_data_frame(g_herb, what = "edges") %>% 
  select(from, to, weight) %>% 
  rename(source = from, target = to)

# 确保权重列存在
if (!"weight" %in% colnames(edges_df)) {
  edges_df$weight <- E(g_herb)$weight  # 手动添加权重列
}

# 处理特殊字符（可选）
edges_df$source <- iconv(edges_df$source, to = "UTF-8")
edges_df$target <- iconv(edges_df$target, to = "UTF-8")

library(openxlsx)
write.xlsx(edges_df, file = "19_4_herbnetEdge.xlsx", colNames = TRUE)


# 保存节点属性
nodes_df <- data.frame(
  herb_id = V(g_herb)$name,
  frequency = V(g_herb)$frequency,
  pagerank = V(g_herb)$pagerank,
  wh_KNN_total = V(g_herb)$wh_KNN_total,
  score = V(g_herb)$score
)
library(openxlsx)
write.xlsx(result, file = "19_4_herbnetNodeScore.xlsx", colNames = TRUE)


library(dplyr)
library(arules)
library(igraph)
library(tidyr)
library(stringr)

library(dplyr)
library(igraph)
library(ggraph)
library(showtext)


library(dplyr)

library(dplyr)
library(igraph)




summary(g_full)

vcount(g_full)
ecount(g_full)
ecount(g_full)



