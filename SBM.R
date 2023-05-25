
##################################################################
# метод SBM

# задание предполагаемого количества кластеров, вершин графа
k <- 5
npc <- 20
n <- npc*k

# моделирование матрицы принадлежности к кластерам
Z <- diag(k)%x%matrix(1,npc,1)
# моделирование симметричной матрицы W размером k*k, элементы которой - вероятности связи вершин графа
W <- matrix(runif(k*k),k,k)
# моделирование матрицы смежности М(матричное представлением графа)
M <- 1*(matrix(runif(n*n),n,n)<Z%*%W%*%t(Z))

# Библиотека blockmodels в R - это пакет для кластеризации данных с помощью моделей блочных моделей (blockmodels)
library(blockmodels)

# создание модель SBM на основе матрицы смежности M
model <- BM_bernoulli('SBM',M)
# оценка блочной модели
model$estimate()
# итоговое количество кластеров
which.max(model$ICL)

# визуализация графа с помощью матрицы смежности
library("igraph")
library("RColorBrewer")

# визуализация изначального смоделированного графа
graph <- graph_from_adjacency_matrix(M,mode="undirected",diag=FALSE)
plot(graph, vertex.size = 10, vertex.color='plum2', edge.width=1)

# получение разметки графа(какой кластер был назначен каждой вершине)
sbmclust <- apply(model$memberships[[k]]$Z, 1, which.max) 

# визуализация представления кластерной структуры
graph1 <- graph_from_adjacency_matrix(M,mode="undirected",diag=FALSE)
for (i in 1:vcount(graph1)) {
  V(graph1)[i]$color <- ifelse(sbmclust[i] == 1,"gold", 
                               ifelse(sbmclust[i] == 2, "skyblue1",
                                      ifelse(sbmclust[i] == 3 ,"deeppink3",
                                             ifelse(sbmclust[i] == 4, "springgreen",
                                                    ifelse(sbmclust[i] == 5, "white", "black")))))
}

# установка атрибутов графа
graph1 <- set_graph_attr (graph1, "layout", layout_with_kk(graph1))
plot(graph1, vertex.size = 10 , edge.width = 1)

# Кластеризация графа на 5 кластеров с помощью алгоритма Лувена
# Кластеризация вершин графа на 5 кластеров с помощью алгоритма Лувена
clusters <- cluster_louvain(graph, 
                            weights = NULL, 
                            resolution = 1)

# Отображение графа с помощью цветовых меток для каждого кластера
plot(clusters, graph)
#####################################################################


