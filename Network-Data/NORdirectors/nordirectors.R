source("system/initialization.R")
source("system/functions.R")

# Reading data into R and making it an undirected graph
edges_nordirectors <-  read.csv("Network-Data/NORdirectors/raw_data/nordirectorslist.csv"
                            ,stringsAsFactors = FALSE) %>% 
  `colnames<-`(c("FROM","TO")) %>%
  normalize_edgelist()
  
# Undirected graph
nordirectors_uu <-  as.matrix(edges_nordirectors) %>%
  graph_from_edgelist() %>%
  as.undirected(mode="collapse")

# Taking largest cluster
nordirectors_uu <- make_ego_graph(nordirectors_uu,order=10000,nodes=c(3),mode="all")[[1]]

# List of nodes
nodes_nordirectors <- 1:vcount(nordirectors_uu) %>%
  as.data.frame() %>%
  `colnames<-`(c("NODE")) %>%
  write.csv("Network-Data/NORdirectors/cooked_data/nodes_nordirectors.csv",row.names=FALSE)

# Adjusting list of edges
edges_nordirectors <- filter(edges_nordirectors)

write.csv("Network-Data/NORdirectors/cooked_data/edges_nordirectors.csv",row.names=FALSE)