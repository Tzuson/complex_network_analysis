source("system/initialization.R")
source("system/functions.R")

# For plot_nlrail we need a map
nlmap <- get_map(location=c("Netherlands"),zoom=7,maptype="toner-background")


#' @title Plot graph of NLrail on map 
#' 
#' @description Plots the airports with right geocoordinates on the map of the
#'   US. The colour of the nodes depend on the VALUE and goes from "blue"
#'   (lowest VALUE) to "red" (highest VALUE). The size is given by USAGE.
#'
#'   Furthermore, a histogram of the VALUE data is made, with binwidth
#'   \code{max(nodes$VALUE)-min(nodes$VALUE)/100}.
#'
#' @param nodes A data frame with for every node a NODE, LONGITUDE, LATITUDE and
#'   VALUE parameter
#' @param file_path A string giving the file path without file_name - optional -
#' @param file_name A string giving the file name without extension - optional -
#' @param name A string giving the name of the graph used in the titles - optional -
#' @param v_name A string giving the name of the VALUE parameter - optional -
#'
#' @return A pdf with a plot of NLrail and a pdf with a histogram of the VALUE
#'   data on the nodes
#'   
#' @examples 
#' plot_usair(nodes_nlrail)
plot_nlrail <- function(nodes, file_path="Network-Data/NLrail/pdf",file_name="nlrail",name="NLrail",v_name="Value"){
  # Right colours
  nsblauw <- "#003373"
  nsgeel <- "#f7d417"
  
  # Making edge list suitable for ggplot2
  edges_nlrail <- read.csv("Network-Data/NLrail/cooked_data/edges_nlrail.csv", stringsAsFactors = FALSE)
  edges_nlrail$LATITUDE1 <- nodes[edges_nlrail$FROM,"LATITUDE"]
  edges_nlrail$LONGITUDE1 <- nodes[edges_nlrail$FROM,"LONGITUDE"]
  edges_nlrail$LATITUDE2 <- nodes[edges_nlrail$TO,"LATITUDE"]
  edges_nlrail$LONGITUDE2 <- nodes[edges_nlrail$TO,"LONGITUDE"]
  
  ggedges <- as.data.frame(matrix(0,nrow=2*length(trajecten[,1]),ncol=4))
  colnames(ggedges) <- c("NODE","LATITUDE","LONGITUDE","GROUP")
  ggedges[seq.int(1,2*length(edges_nlrail[,1])-1,2),1:3] <- edges_nlrail[,c("FROM","LATITUDE1","LONGITUDE1")]
  ggedges[seq.int(1,2*length(edges_nlrail[,1])-1,2),4] <- 1:length(edges_nlrail[,1])
  ggedges[seq.int(2,2*length(edges_nlrail[,1]),2),1:3] <- edges_nlrail[,c("TO","LATITUDE2","LONGITUDE2")]
  ggedges[seq.int(2,2*length(edges_nlrail[,1]),2),4] <- 1:length(edges_nlrail[,1])
  
  # Making plot of NLrail 
  p <- ggmap(nlmap) +
    ggtitle(name) + xlab("Longitude") + ylab("Latitude") +
    geom_line(data=ggedges,aes(x=LATITUDE,y=LONGITUDE,group=GROUP),colour="#f7d417",size=1.5) +
    geom_point(data=nodes,aes(x=LONGITUDE,y=LATITUDE,colour=VALUE),size=1.5) +
    scale_color_gradient(low="blue", high="red") +
    scale_x_continuous(limits=c(3.2,7.1)) +
    scale_y_continuous(limits=c(50.75,53.5))
  ggsave(filename=sprintf('%s_map.pdf',file_name),plot=p,device="pdf",path=file_path)
  
  # Plotting and storing histogram of NLrail nodes data
  p <- ggplot(nodes,aes(VALUE)) +
    ggtitle(name) + xlab(v_name) + ylab("Frequency") +
    geom_histogram(binwidth = (max(VALUE)-min(VALUE))/100)
  ggsave(filename=sprintf('%s_hist.pdf',file_name),plot=p,device="pdf",path=file_path)
}# plot_nlrail