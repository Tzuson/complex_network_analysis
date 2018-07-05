source("initialization.R")
source("functions/measures.R")
source("functions/tests.R")
c <- sqrt(2)


adj_matrix <- t(matrix(c(0,1,1,
                         1,0,1,
                         1,1,0),
                     nrow=3, ncol=3))
l_matrix <- t(matrix(c(0,1,1,
                       1,0,1,
                       1,1,0),
                     nrow=3, ncol=3))
print(l_matrix)



testgraph <- graph_from_adjacency_matrix(adj_matrix*l_matrix, mode="directed", weighted=TRUE)
plot(testgraph)
print(measures(testgraph,l_matrix,global_efficiency))


