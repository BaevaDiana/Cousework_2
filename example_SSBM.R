# Пример работы Симметричной стохастической модели
# при n = 1000 и k = 5

# задание предполагаемого количества кластеров, вершин графа и вектора вероятностей
npc <- 200
k <- 5
p <- c(0.2, 0.2, 0.2, 0.2, 0.2)
n <- npc*k

Z <- diag(k)%x%matrix(1,npc,1)
# моделирование симметричной матрицы W размером k*k, элементы которой - вероятности связи вершин графа
W <- diag(1/50,k,k)
# симметризация матрицы
W[upper.tri(W)] <- 1/n
W[lower.tri(W)] <- 1/n

# моделирование матрицы смежности М(матричное представлением графа)
# симметризация матрицы
M <- 1*(matrix(runif(n*n),n,n)<Z%*%W%*%t(Z))
M[lower.tri(M)] <- t(M)[lower.tri(M)]

# Библиотека blockmodels в R - это пакет для кластеризации данных с помощью моделей блочных моделей (blockmodels). 
# Он предоставляет функции для создания и оценки моделей блочных моделей, включая метод SBM.
library(blockmodels)

# создание модель SBM на основе матрицы смежности M
model <- BM_bernoulli('SBM_sym',M)
# оценка блочной модели
model$estimate()
# итоговое количество кластеров
which.max(model$ICL)


# визуализация графа с помощью матрицы смежности
library("igraph")
library("RColorBrewer")

# визуализация изначального смоделированного графа
graph <- graph_from_adjacency_matrix (
  M[-c(129,522,921,542,610,859,253,698),
     -c(129,522,921,542,610,859,253,698) ] ,
  mode = "undirected", diag = FALSE)
plot(graph, vertex.size = 3, vertex.color = "plum2 ", edge.width = 1 , vertex.label = NA)


model$memberships[[k]]$Z
# получение разметки графа(какой кластер был назначен каждой вершине
sbm_clust <- apply(model$memberships[[k]]$Z, 1, which.max)

# визуализация представления кластерной структуры
graph1 <- graph_from_adjacency_matrix (
  M[-c(129,522,921,542,610,859,253,698),
    -c(129,522,921,542,610,859,253,698) ] ,
  mode = "undirected", diag = FALSE)
for (i in 1:vcount(graph1)) {
  V(graph1)[i]$color <- ifelse(sbm_clust[i] == 1,"gold", 
                               ifelse(sbm_clust[i] == 2, "skyblue1",
                                      ifelse(sbm_clust[i] == 3 ,"deeppink3",
                                             ifelse(sbm_clust[i] == 4, "springgreen",
                                                    ifelse(sbm_clust[i] == 5, "black", "white")))))
}
# установка атрибутов графа
graph1 <- set_graph_attr (graph1, "layout", layout_with_kk(graph1))
plot(graph1, vertex.size = 3 , edge.width = 1, vertex.label = NA)

     