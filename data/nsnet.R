source("initialization.R")

# Initialize graph
trajecten <- read.csv("data/trajecten.csv",stringsAsFactors = FALSE)
stations <-  read.csv("data/trajecten_stations.csv",stringsAsFactors = FALSE)

nsnet <- graph_from_data_frame(trajecten, directed=FALSE, vertices=stations)
L <- components(g)


#' Plot graph of nsnet
#' 
#' @param nodes Data on nodes 
#' @param edges Data on edges
#' @return A pdf with a plot of nsnet
nsnet_plot <- function(nodes, edges){
  nsrblauw <- "#003373"
  nsgeel <- "#f7d417"
  
  pdf("nsnet.pdf")
  set.seed(3)
  par(mar=rep(0,4),oma=rep(0,4))
  plot(nsnet,vertex.label=NA
       , vertex.size=2 + nodes
       , vertex.color=nsgeel
       , edge.color=nsrblauw
       , edge.width=1 + edges
       , vertex.frame.color=nsrblauw)
  dev.off()
}# nsnet_plot(data)




