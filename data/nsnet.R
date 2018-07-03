source("initialization")

#' Initialize graph
#' @param g a graph
trajecten <- read.csv("02tidy/trajecten.csv",stringsAsFactors = FALSE)
stations <-  read.csv("02tidy/trajecten_stations.csv",stringsAsFactors = FALSE)

g <- graph_from_data_frame(trajecten,directed=FALSE,vertices=stations)
L <- components(g)


# Plot graph of network
# Input: vector of data on nodes
# Output: pdf of plot
nsnet_plot <- function(data){
  nsrblauw <- "#003373"
  nsgeel <- "#f7d417"
  
  pdf("nsnet.pdf")
  set.seed(3)
  par(mar=rep(0,4),oma=rep(0,4))
  plot(g,vertex.label=NA
       , vertex.size=2 + data
       , vertex.color=nsgeel
       , edge.color=nsrblauw
       , vertex.frame.color=nsrblauw)
  dev.off()
}# nsnet_plot(data)



