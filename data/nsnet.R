source("initialization.R")

# Initialize unweighted graph
trajecten <- read.csv("data/trajecten.csv",stringsAsFactors = FALSE)
stations <-  read.csv("data/trajecten_stations.csv",stringsAsFactors = FALSE)

nsnet <- graph_from_data_frame(trajecten, directed=FALSE, vertices=stations)
l_nsnet <- matrix(1,nrow=vcount(nsnet),ncol=vcount(nsnet))

# Initializing weighted graph with length matrix
# Node 394 (Waddinxveen Triangel) does not have geospatial data, so it is ommited, as well as
# nodes 359 and 360 (Waddinxveen Triangel's neighbors)
c_lat <- 111267
c_lng <- 68677
adj_nsnet <- as_adjacency_matrix(nsnet, type = "both", sparse=FALSE)[-c(359,360,394),-c(359,360,394)]
l_nsnet_weighted <- sapply(V(nsnet),function(i,nsnet,stations){
  sapply(V(nsnet), function(j,i,nsnet,stations){
    sqrt(c_lat^2*(stations$geo_lat[i]-stations$geo_lat[j])^2+c_lng^2*(stations$geo_lng[i]-stations$geo_lng[j])^2)
  },i,nsnet,stations)
},nsnet,stations)
l_nsnet_weighted <- l_nsnet_weighted[-c(359,360,394),-c(359,360,394)]
nsnet_weighted <- graph_from_adjacency_matrix(adj_nsnet*l_nsnet_weighted, mode="undirected", weighted=TRUE)

#' Plot graph of nsnet
#' 
#' @param nodes Data on nodes 
#' @param edges Data on edges
#' @return A pdf with a plot of nsnet
nsnet_plot <- function(nodes, edges){
  nsrblauw <- "#003373"
  nsgeel <- "#f7d417"
  
  pdf("pdf/nsnet.pdf")
  set.seed(3)
  par(mar=rep(0,4),oma=rep(0,4))
  plot(nsnet,vertex.label=NA
       , vertex.size=2 + nodes
       , vertex.color=nsgeel
       , edge.color=nsrblauw
       , edge.width=1 + edges
       , vertex.frame.color=nsrblauw)
  dev.off()
}# nsnet_plot(nodes,edges)


#' Plot graph of nsnet
#' 
#' @param nodes Data on nodes 
#' @param edges Data on edges
#' @return A pdf with a plot of g_nsnet
nsnet_weighted_plot <- function(nodes, edges){
  nsrblauw <- "#003373"
  nsgeel <- "#f7d417"
  
  pdf("pdf/nsnet_weighted.pdf")
  set.seed(3)
  par(mar=rep(0,4),oma=rep(0,4))
  plot(nsnet_weighted,vertex.label=NA
       , vertex.size=2 + nodes
       , vertex.color=nsgeel
       , edge.color=nsrblauw
       , edge.width=1 + edges
       , vertex.frame.color=nsrblauw)
  dev.off()
}# nsnet_weighted_plot(nodes,edges)

