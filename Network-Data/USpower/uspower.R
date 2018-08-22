source("system/initialization.R")
source("system/parallell_programming.R")
source("system/functions.R")

# This network it the network of 4941 electricity poles, houses etc of
# north-west America. It thus has 4941 nodes and 6594 edges. It is undirected
# and unweighted. For more information, see
# https://toreopsahl.com/datasets/#uspowergrid

# Reading data into R and making it an edge list, then undirected graph
uspower_uu <-  read.csv("Network-Data/USpower/raw_data/uspowergrid.csv"
           ,stringsAsFactors = FALSE) %>% 
  subset(select = c("V1","V2")) %>%
  `colnames<-`(c("FROM","TO")) %>% 
  as.matrix() %>%
  normalize_edgelist() %T>%
  write.csv("Network-Data/USpower/cooked_data/edges_uspower.csv",row.names = FALSE) %>%
  
  # Undirected graph
  graph_from_edgelist() %>%
  as.undirected(mode="collapse")

# The node list of uspower
nodes_uspower <- as.data.frame(1:vcount(uspower_uu)) %>%
  `colnames<-`(c("NODE")) %>%
  write.csv("Network-Data/USpower/cooked_data/nodes_uspower.csv", row.names=FALSE)

