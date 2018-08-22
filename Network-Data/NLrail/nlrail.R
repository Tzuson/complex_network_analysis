source("system/initialization.R")
source("system/functions.R")

# Importing data
# Node 394 (Waddinxveen Triangel) does not have geospatial data, so it is ommited, as well as
# nodes 359 (Waddinxveen) and 360 (Waddinxveen Noord), its neighbors
stations <-  read.csv("Network-Data/NLrail/raw_data/trajecten_stations.csv",stringsAsFactors = FALSE) %>%
  .[-c(359,360,394),] 
row.names(stations) <- stations$naam
trajecten <- read.csv("Network-Data/NLrail/raw_data/trajecten.csv",stringsAsFactors = FALSE) %>%
  filter(start%in%stations$naam,stop%in%stations$naam)

# Making edge list
trajecten$lat1 <- stations[trajecten$start,"geo_lat"]
trajecten$lon1 <- stations[trajecten$start,"geo_lng"]
trajecten$lat2 <- stations[trajecten$stop,"geo_lat"]
trajecten$lon2 <- stations[trajecten$stop,"geo_lng"]

# For plot_nlrail we need a map
# nlmap <- get_map(location=c("Netherlands"),zoom=7,maptype="toner-background")

#' Initialize NLrail, weighted or unweighted
#' 
#' @param weighted A boolean
#' @return If unweighted: a list with 1) graph of NLrail, if weighted, a list with 1) a weighted
#' graph of NLrail, and 2) a matrix with Euclidean distances between every two nodes
initialize_nlrail <- function (weighted){
  # Initializing graph
  nlrail <- graph_from_data_frame(trajecten, directed=FALSE, vertices=stations)

  # Calculating the distances 
  if (weighted){
    adj_nlrail <- as_adjacency_matrix(nlrail, type = "both", sparse=TRUE)
    l_nlrail <- sapply(V(nlrail),function(i,nlrail,stations){
      sapply(V(nlrail), function(j,i,nlrail,stations){
        lat1 <- stations$geo_lat[i]
        lon1 <- stations$geo_lng[i]
        lat2 <- stations$geo_lat[j]
        lon2 <- stations$geo_lng[j]
        coordinates2distance(lat1,lon1,lat2,lon2)
      },i,nlrail,stations)
    },nlrail,stations)
    l_nlrail <- l_nlrail
    nlrail_weighted <- graph_from_adjacency_matrix(adj_nlrail*l_nlrail, mode="undirected", weighted=TRUE)
    
    return(list(nlrail_weighted,l_nlrail))
  }# if
  else {
    return(list(nlrail))
  }# else
}# initialize_nlrail


#' Plot graph of NLrail
#' 
#' @param net A graph
#' @param nodes Data on nodes - optional -
#' @param file_path A string (without .pdf)
#' @param file_name A string (without .pdf)
#' @param name A string
#' @return A pdf with a plot of NLrail
plot_nlrail <- function(net, nodes=NULL, file_path,file_name,name){
  # Data is zero when missing
  if (is.null(nodes)){
    nodes <- vector(mode="numeric", length = vcount(net))
  }# if
  data_stations <- stations
  data_stations$colour <- nodes

  # Right colours
  nsblauw <- "#003373"
  nsgeel <- "#f7d417"
  
  # Making edge list suitable for ggplot2
  edges <- as.data.frame(matrix(0,nrow=2*length(trajecten[,1]),ncol=4))
  colnames(edges) <- c("node","lat","lon","group")
  edges[seq.int(1,2*length(trajecten[,1])-1,2),1:3] <- trajecten[,c("start","lat1","lon1")]
  edges[seq.int(1,2*length(trajecten[,1])-1,2),4] <- 1:length(trajecten[,1])
  edges[seq.int(2,2*length(trajecten[,1]),2),] <- trajecten[,c("stop","lat2","lon2")]
  edges[seq.int(2,2*length(trajecten[,1]),2),4] <- 1:length(trajecten[,1])
  
  # Making plot of nlrail 
  p <- ggmap(nlmap) +
    ggtitle(name) + xlab("Longitude") + ylab("Latitude") +
    geom_line(data=edges,aes(x=lon,y=lat,group=group),colour="#f7d417",size=1.5) +
    geom_point(data=data_stations,aes(x=as.numeric(geo_lng)
                                 , y=as.numeric(geo_lat)
                                 , colour=colour),size=1.5) +
    scale_color_gradient(low="blue", high="red") +
    scale_x_continuous(limits=c(3.2,7.1)) +
    scale_y_continuous(limits=c(50.75,53.5))
  ggsave(filename=sprintf('%s_map.pdf',file_name),plot=p,device="pdf",path=file_path)
  
  # Plotting and storing histogram of NLrail nodes data
  nodes <- as.data.frame(nodes) %>%
    `colnames<-`(c("value"))
  p <- ggplot(nodes,aes(value)) +
    ggtitle(name) + xlab("Vulnerability") + ylab("Frequency") +
    geom_histogram(binwidth = 0.001)
  ggsave(filename=sprintf('%s_hist.pdf',file_name),plot=p,device="pdf",path=file_path)
}# plot_nlrail

