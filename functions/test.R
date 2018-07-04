source("initialization.R")
source("functions/measures.R")
source("data/nsnet.R")

testgraph <- graph_from_literal(A-B-C-D-B)
plot(testgraph)

measures(testgraph, global_efficiency)


