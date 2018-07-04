source("initialization.R")
source("functions/measures.R")

testgraph <- graph_from_literal(A,B-C-D,B-E-D)
plot(testgraph)

global_efficacy(testgraph)

improvement_edge(testgraph, c(1,2), global_efficiency)
