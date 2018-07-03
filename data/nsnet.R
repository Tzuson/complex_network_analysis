source("initialization")

# Initialize graph
trajecten <- read.csv("trajecten.csv",stringsAsFactors = FALSE)
stations <-  read.csv("trajecten_stations.csv",stringsAsFactors = FALSE)

g <- graph_from_data_frame(trajecten,directed=FALSE,vertices=stations)
L <- components(g)


#' Plot graph of nsnet
#' 
#' @param data Data on nodes
#' @return A pdf with a plot of nsnet
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



