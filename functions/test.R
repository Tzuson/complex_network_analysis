source("initialization")
source("functions/measures")
source("data/nsnet")

g <- graph_from_literal(A-B-C-D-B)
plot(g)

efficiency(g)
a <- igraph::distance_table(g)$res

testgraph <- graph_from_literal(A-B)
getal <- igraph::distance_table(testgraph)$res