source("system/initialization.R")
source("system/parallel_programming.R")
source("system/functions.R")

# This network consists of directors of Norwegian companies and their
# connections from serving on the same board. The network is undirected and
# unweighted. Because of the large number of isolated and practically isolated
# nodes, the main cluster is taken, deleting the other nodes. For more
# information on the network, see
# https://toreopsahl.com/datasets/#norwegianbod 

# Reading data into R and making it an undirected graph
edges_nordirectors <-  read.csv("Network-Data/NORdirectors/raw_data/nordirectorslist.csv"
                            ,stringsAsFactors = FALSE) %>% 
  `colnames<-`(c("FROM","TO")) %>%
  normalize_edgelist()
  
# Undirected graph
nordirectors_uu <-  as.matrix(edges_nordirectors) %>%
  graph_from_edgelist() %>%
  as.undirected(mode="collapse")
V(nordirectors_uu)$name <- toString(1):toString(vcount(nordirectors_uu))

# Taking largest cluster
nordirectors_uu <- make_ego_graph(nordirectors_uu,order=10000,nodes=c(3),mode="all")[[1]]

# List of nodes
nodes_nordirectors <- 1:vcount(nordirectors_uu) %>%
  as.data.frame() %>%
  `colnames<-`(c("NODE")) %>%
  write.csv("Network-Data/NORdirectors/cooked_data/nodes_nordirectors.csv",row.names=FALSE)

# Adjusting list of edges
edges_nordirectors <- filter(edges_nordirectors
                             , FROM%in%as.integer(V(nordirectors_uu)$name)
                             , TO%in%as.integer(V(nordirectors_uu)$name)) %>%
  normalize_edgelist() %T>%
  write.csv("Network-Data/NORdirectors/cooked_data/edges_nordirectors.csv",row.names=FALSE)

V(nordirectors_uu)$name <- 1:vcount(nordirectors_uu)



