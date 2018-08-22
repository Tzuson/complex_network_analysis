source("system/initialization.R")
source("system/parallell_programming.R")
source("system/functions.R")

source("functions/measures_distance.R")
source("functions/tests_distance.R")

source("Network-Data/USair/usair2.R")


# Matrices loaded from memory
# We use a simple function for loading, because all matrices are loaded in the same way
# with the first column being the row names

#' Loads the appropriate matrix from "Network-Data/USair/cooked_data/"
#' 
#' @param name A string
#' @result A matrix derived from the data frame loaded
load_usair <- function(name){
  m <- read.csv(sprintf('Network-Data/USair/cooked_data/%s.csv',name), stringsAsFactors = FALSE) %>%
    `row.names<-`(.,.[,1]) %>% 
    .[,-1] %>% 
    as.matrix()
  return(m)
}# load_usair

# Names of matrices to be loaded (and used)
files_usair <- c("adj_usair_du", "l_usair"#, "w_usair"
                 # , "t_usair_du_min", "t_usair_dw_min"
                 # , "t_usair_uu_min", "t_usair_uw_min"
                 , "t_usair_du_sum", "t_usair_dw_sum"
                 , "t_usair_uu_sum", "t_usair_uw_sum")
for (file_usair in files_usair){
  assign(file_usair, load_usair(file_usair))
}# for

# Loading edge list and deleting its first row of non-used numbers
usair <- read.csv("Network-Data/USair/cooked_data/usair.csv", stringsAsFactors = FALSE) %>%
  .[,-1]

# Initiating the graphs
usair_uu <- graph_from_adjacency_matrix(adj_usair_du,mode="max",add.rownames = TRUE)
usair_du <- graph_from_adjacency_matrix(adj_usair_du,mode="directed",add.rownames = TRUE)
usair_uw <- graph_from_adjacency_matrix(adj_usair_du*l_usair,mode="max",weighted=TRUE,add.rownames = TRUE)
usair_dw <- graph_from_adjacency_matrix(adj_usair_du*l_usair,mode="directed",weighted=TRUE,add.rownames = TRUE)

names_usair <- V(usair_uu)$name
vul_ge_suffix_names <- c("vul_ge_uu","vul_ge_du","vul_ge_uw","vul_ge_dw"
               ,"vul_ge_uu_sum","vul_ge_du_sum","vul_ge_uw_sum","vul_ge_dw_sum"
               ,"vul_ge_uu_min","vul_ge_du_min","vul_ge_uw_min","vul_ge_dw_min")
vul_ge_usair <- matrix(0,nrow=count(usair_uu),ncol=length(vul_ge_suffix_names)) %>%
  `row.names<-`(names_usair) %>%
  `colnames<-`(vul_ge_suffix_names) %>%
  as.data.frame()
for (vul_ge_suffix in vul_ge_suffix_names){
  vul_ge_usair[,vul_ge_suffix] <- vulnerability_n
}# for


nodes_uu <- vulnerability_nodes(cl,usair_uu,performance=global_efficiency_unpar) %>%
  `names<-`(nodes_names) %T>%
  write.csv("Network-Data/USair/data/vulnerability_uu.csv") %T>%
  plot_usair(net=usair_uu
             , nodes=.
             , file_path="Network-Data/USair/pdf/usair_uu"
             , name="USair: \n unweighted, undirected")
nodes_du <- vulnerability_nodes(cl,usair_du,performance=global_efficiency_unpar)
nodes_uw <- vulnerability_nodes(cl,usair_uw,l=l_usair,performance=global_efficiency_unpar)
nodes_dw <- vulnerability_nodes(cl,usair_dw,l=l_usair,performance=global_efficiency_unpar)

nodes_uu_sum <- vulnerability_nodes(cl,usair_uu,t=t_usair_uu_sum,performance=global_efficiency_unpar)
nodes_du_sum <- vulnerability_nodes(cl,usair_du,t=t_usair_du_d_sum,performance=global_efficiency_unpar)
nodes_uw_sum <- vulnerability_nodes(cl,usair_uw,l=l_usair,t=t_usair_uw_sum,performance=global_efficiency_unpar)
nodes_dw_sum <- vulnerability_nodes(cl,usair_dw,l=l_usair,t=t_usair_dw_sum,performance=global_efficiency_unpar)

nodes_uu_min <- vulnerability_nodes(cl,usair_uu,t=t_usair_uu_min,performance=global_efficiency_unpar)
nodes_du_min <- vulnerability_nodes(cl,usair_du,t=t_usair_du_min,performance=global_efficiency_unpar)
nodes_uw_min <- vulnerability_nodes(cl,usair_uw,l=l_usair,t=t_usair_uw_min,performance=global_efficiency_unpar)
nodes_dw_min <- vulnerability_nodes(cl,usair_dw,l=l_usair,t=t_usair_dw_min,performance=global_efficiency_unpar)


plot_usair(usair_du
              , nodes=100*nodes_du
              , file_path="Network-Data/USair/pdf/usair_du"
              , name="USair: \n unweighted, directed")
plot_usair(usair_uw
              , nodes=100*nodes_uw
              , file_path="Network-Data/USair/pdf/usair_uw"
              , name="USair: \n weighted, undirected")
plot_usair(usair_dw
              , nodes=100*nodes_dw
              , file_path="Network-Data/USair/pdf/usair_dw"
              , name="USair: \n weighted, directed")

plot_usair(usair_uu
              , nodes=100*nodes_uu_sum
              , file_path="Network-Data/USair/pdf/usair_uu_sum"
              , name="USair: \n unweighted, undirected, transport=sum")
plot_usair(usair_du
              , nodes=100*nodes_du_sum
              , file_path="Network-Data/USair/pdf/usair_du_sum"
              , name="USair: \n unweighted, directed, transport=sum")
plot_usair(usair_uw
              , nodes=100*nodes_uw_sum
              , file_path="Network-Data/USair/pdf/usair_uw_sum"
              , name="USair: \n weighted, undirected, transport=sum")
plot_usair(usair_dw
              , nodes=100*nodes_dw_sum
              , file_path="Network-Data/USair/pdf/usair_dw_sum"
              , name="USair: \n weighted, directed, transport=sum")

plot_usair(usair_uu
              , nodes=100*nodes_uu_min
              , file_path="Network-Data/USair/pdf/usair_uu_min"
              , name="USair: \n unweighted, undirected, transport=min")
plot_usair(usair_du
              , nodes=100*nodes_du_min
              , file_path="Network-Data/USair/pdf/usair_du_min"
              , name="USair: \n unweighted, directed, transport=min")
plot_usair(usair_uw
              , nodes=100*nodes_uw_min
              , file_path="Network-Data/USair/pdf/usair_uw_min"
              , name="USair: \n weighted, undirected, transport=min")
plot_usair(usair_dw
              , nodes=100*nodes_dw_min
              , file_path="Network-Data/USair/pdf/usair_dw_min"
              , name="USair: \n weighted, directed, transport=min")


nodes1 <- matrix(0,nrow=vcount(usair_uu),ncol=12)
colnames(nodes1) <- c("nodes_uu","nodes_du","nodes_uw","nodes_dw"
                      , "nodes_uu_sum","nodes_du_sum","nodes_uw_sum","nodes_dw_sum"
                      , "nodes_uu_min","nodes_du_min","nodes_uw_min","nodes_dw_min")
row.names(nodes1) <- V(usair_uu)$name

nodes1[,1] <- nodes_uu
nodes1[,2] <- nodes_du
nodes1[,3] <- nodes_uw
nodes1[,4] <- nodes_dw

nodes1[,5] <- nodes_uu_sum
nodes1[,6] <- nodes_du_sum
nodes1[,7] <- nodes_uw_sum
nodes1[,8] <- nodes_dw_sum

nodes1[,9] <- nodes_uu_min
nodes1[,10] <- nodes_du_min
nodes1[,11] <- nodes_uw_min
nodes1[,12] <- nodes_dw_min

nodes1 <- 100*nodes1

nodes_minimum <- apply(nodes1,c(1),min)
nodes_maximum <- apply(nodes1,c(1),max)
nodes_mean <- apply(nodes1,c(1),mean)


nodes_data <- matrix(0,nrow=vcount(usair_uu),ncol=15)
colnames(nodes_data) <- c("nodes_uu","nodes_du","nodes_uw","nodes_dw"
                          , "nodes_uu_sum","nodes_du_sum","nodes_uw_sum","nodes_dw_sum"
                          , "nodes_uu_min","nodes_du_min","nodes_uw_min","nodes_dw_min"
                          , "minimum", "mean", "maximum")
row.names(nodes_data) <- V(usair_uu)$name

nodes_data[,1:12] <- nodes1
nodes_data[,13] <- nodes_minimum
nodes_data[,14] <- nodes_mean
nodes_data[,15] <- nodes_maximum

remove(nodes1)

nodes_data <- as.data.frame(nodes_data)

nodes_data <- signif(nodes_data,2)

nodes_rank <- matrix(0,nrow=vcount(usair_uu),ncol=12)
colnames(nodes_rank) <- c("nodes_uu","nodes_du","nodes_uw","nodes_dw"
                          , "nodes_uu_sum","nodes_du_sum","nodes_uw_sum","nodes_dw_sum"
                          , "nodes_uu_min","nodes_du_min","nodes_uw_min","nodes_dw_min")

for (i in 1:12){
  nodes_rank[,i] <- row.names(nodes_data)[order(nodes_data[,i],decreasing=TRUE)]
}# for









