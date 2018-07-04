source("initialization.R")
source("data/nsnet.R")
source("functions/functions.R")
source("functions/measures.R")



#measures(nsnet,global_efficiency)

#nsnet_plot(3*local_efficiency(nsnet))

nodes <- vulnerability_nodes(nsnet, global_efficiency)
edges <- vulnerability_edges(nsnet, global_efficiency)


nsnet_plot(3*nodes, 3*edges)

print(stations$naam[which.max(nodes)])
print(trajecten$start[which.max(edges)])
print(trajecten$stop[which.max(edges)])
print(nodes[which.max(nodes)])
print(edges[which.max(edges)])