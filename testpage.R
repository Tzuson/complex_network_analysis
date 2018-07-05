source("initialization.R")
source("functions/measures.R")
source("functions/tests.R")


matrix <- matrix(c(0,0,0,2.5,
                   1,0,0,0,
                   0,1,0,0,
                   0,0,1,0),
            nrow = 4, ncol = 4)
print(matrix)
testgraph <- graph_from_adjacency_matrix(matrix, mode="directed", weighted=TRUE)
plot(testgraph)

getal<-distances(testgraph, directed=TRUE)
print(getal)



