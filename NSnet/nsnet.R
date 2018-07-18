source("system/initialization.R")

# Used packages
suppressPackageStartupMessages({
  library(igraph)
  library(tidyr)
  library(dplyr)
  library(tidyverse)
  library(magrittr)
  library(stringdist)
  library(rvest)
  library(xml2)
})

#' Initialize nsnet, weighted or unweighted
#' 
#' @param weighted A boolean
#' @return If unweighted: a graph of NSnet, if weighted, a list with 1) a weighted
#' graph of NSnet, and 2) a matrix with Euclidean distances between every two nodes
initialize_nsnet <- function (weighted){
  trajecten <- read.csv("NSnet/trajecten.csv",stringsAsFactors = FALSE)
  stations <-  read.csv("NSnet/trajecten_stations.csv",stringsAsFactors = FALSE)
  
  nsnet <- graph_from_data_frame(trajecten, directed=FALSE, vertices=stations)
  
  # Node 394 (Waddinxveen Triangel) does not have geospatial data, so it is ommited, as well as
  # nodes 359 (Waddinxveen) and 360 (Waddinxveen Noord), its neighbors 
  # We use approximations for geocoordinates --> meters around
  # the Dom church in Utrecht (52*5'27"N 5*07'18"E), calculate distances in x- and y-direction,
  # and use Pythagoras to calculate Euclidian distance between two stations
  if (weighted){
    c_lat <- 111267 # ... m = 1 degree of latitude
    c_lng <- 68677  # ... m = 1 degree of longitude
    
    adj_nsnet <- as_adjacency_matrix(nsnet, type = "both", sparse=TRUE)[-c(359,360,394),-c(359,360,394)]
    l_nsnet <- sapply(V(nsnet),function(i,nsnet,stations){
      sapply(V(nsnet), function(j,i,nsnet,stations){
        sqrt(c_lat^2*(stations$geo_lat[i]-stations$geo_lat[j])^2+c_lng^2*(stations$geo_lng[i]-stations$geo_lng[j])^2)
      },i,nsnet,stations)
    },nsnet,stations)
    l_nsnet <- l_nsnet[-c(359,360,394),-c(359,360,394)]
    nsnet_weighted <- graph_from_adjacency_matrix(adj_nsnet*l_nsnet, mode="undirected", weighted=TRUE)
    
    return(list(nsnet_weighted,l_nsnet))
  }# if
  else {
    return(nsnet)
  }# else
}# initialize_nsnet



#' Plot graph of nsnet
#' 
#' @param net A graph
#' @param nodes Data on nodes - optional -
#' @param edges Data on edges - optional -
#' @param name A string name (without .pdf)
#' @return A pdf with a plot of nsnet
nsnet_plot <- function(net, nodes, edges, name){
  nsblauw <- "#003373"
  nsgeel <- "#f7d417"
  
  # Data is zero when missing
  if (missing(nodes)){
    nodes <- vector(mode="numerical", length = vcount(net))
  }# if
  if (missing(edges)){
    edges <- vector(mode="numerical", length = vcount(net))
  }# if
  
  # Plotting and storing image of NSnet
  file_name <- sprintf('NSnet/pdf/%s.pdf',name)
  pdf(file_name)
  par(mar=rep(0,4),oma=rep(0,4))
  plot(net,vertex.label=NA
       , vertex.size=2 + nodes
       , vertex.color=nsgeel
       , edge.color=nsblauw
       , edge.width=1 + edges
       , vertex.frame.color=nsblauw)
  dev.off()
}# nsnet_plot




