source("initialization.R")
source("functions/measures.R")
source("functions/tests.R")
Rcpp::sourceCpp("functions/testsincpp.cpp")
# c <- sqrt(2)
# 
# 
# adj_matrix <- t(matrix(c(0,1,1,
#                          1,0,1,
#                          1,1,0),
#                      nrow=3, ncol=3))
# l_matrix <- t(matrix(c(0,1,1,
#                        1,0,1,
#                        1,1,0),
#                      nrow=3, ncol=3))
# #print(l_matrix)
# 
# 
# 
# testgraph <- graph_from_adjacency_matrix(adj_matrix*l_matrix, mode="directed", weighted=TRUE)
# plot(testgraph)

print(sum(sapply(seq(20000), function(i){
  harmonic(i)
})))
print(sum(parsapply(seq(2000), function(i){
  harmonic(i)
})))

