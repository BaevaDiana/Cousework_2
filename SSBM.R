# Генерация графа, состоящего из n*k узлов, где 
# n - общее количество вершин
# k - предполагаемое количество кластеров



#####################################################################
# симметричный SBM
# SBM symmetric

# аналогичное задание предполагаемого количества кластеров, вершин графа и вектора вероятностей
npc <- 30
k <- 2
n <- npc*k

Z <- diag(k)%x%matrix(1,npc,1)
# аналогичное моделирование симметричной матрицы Ws размером ks*ks, элементы которой - вероятности связи вершин графа
W <- matrix(runif(k*k),k,k)
# симметризация матрицы(так как вероятности и связи между кластерами совпадают)
W[lower.tri(W)] <-t(W)[lower.tri(W)]

# аналогичное моделирование матрицы смежности Мs(матричное представлением графа)
M <- 1*(matrix(runif(n*n),n,n)<Z%*%W%*%t(Z))
# симметризация матрицы(так как вероятности и связи между кластерами совп
M[lower.tri(M)] <- t(M)[lower.tri(M)]

# создание модель SBM на основе матрицы смежности Ms
model <- BM_bernoulli('SBM_sym',M)
# оценка блочной модели
model$estimate()
# итоговое количество кластеров
which.max(model$ICL)

# визуализация графа с помощью матрицы смежности
library("igraph")
library("RColorBrewer")

# визуализация изначального смоделированного графа
graph <- graph_from_adjacency_matrix(M,mode="undirected",diag = FALSE)
plot(graph, vertex.size = 15, vertex.color='plum2', edge.width = 4)
model$memberships

# получение разметки графа(какой кластер был назначен каждой вершине
sbmclust <-apply(model$memberships[[4]]$Z, 1, which.max)

# визуализация представления кластерной структуры
graph1 <- graph_from_adjacency_matrix(M,mode="undirected",diag=FALSE)
V(graph1)$color <- ifelse(sbmclust==1,'lightblue3','deeppink3')

# установка атрибутов графа
graph1 <- set_graph_attr(graph1,'layot', layout_with_kk(graph1))
plot(graph1, vertex.size = 15, edge.width=4)
#####################################################################