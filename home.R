source("initialization.R")
source("data/nsnet.R")
source("functions/functions.R")
source("functions/measures.R")
source("functions/tests.R")



#measures(nsnet,global_efficiency)

#nsnet_plot(3*local_efficiency(nsnet))
v <- vcount(nsnet)
a <- matrix(1,nrow=v,ncol=v)


nodes <- vulnerability_nodes(nsnet, a, global_efficiency)
edges <- vulnerability_edges(nsnet, a, global_efficiency)

nsnet_plot(10*nodes, 10*edges)


measures(nsnet, matrix(1,nrow=v,ncol=v), global_efficiency)

