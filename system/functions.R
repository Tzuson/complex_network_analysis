source("system/initialization.R")
source("system/parallell_programming.R")


#' @title Calculates distance between two geocoordinates
#'
#' @description Calculating distance in km between two points on earth as a
#'   function of their geospatial coordinates using Haversine formula. Source:
#'   https://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula
#'
#'
#' @param lat1 Latitude degree of place 1
#' @param lon1 Longitude degree of place 1
#' @param lat2 Latitude degree of place 2
#' @param lon2 Longitude degree of place 2
#'
#' @result Distance in kilometers
#'
#' @examples
#' # Distance between Atlanta International airport and Denver International airport
#' coordinates2distance(33.64,-84.42,39.86,-104.67)
#' # 2624.497
coordinates2distance <- function(lat1,lon1,lat2,lon2){
  # Radius of the earth in km
  R <- 6371
  
  # Degree distances
  dlat <- deg2rad(lat2-lat1)
  dlon <- deg2rad(lon2-lon1)
  
  # Haversine formula
  a <- sin(dlat/2)*sin(dlat/2)+cos(deg2rad(lat1))*cos(deg2rad(lat2))*sin(dlon/2)*sin(dlon)
  b <- 2*atan2(sqrt(a),sqrt(1-a))
  
  # In kilometers
  distance <- R*b
  
  return(distance)
}# coordinates2distance


#' @title From degree to rad
#' 
#' @param deg Degrees
#' 
#' @result The degree in radials
#' 
#' @examples 
#' deg2rad(180)
#' # 3.141593
deg2rad <- function(deg) {
  return(deg*(pi/180))
}# deg2rad


#' @title Renumbers the vertex numbers to vertex ID's
#'
#' @description When the vertices in an edge list are given as numbers,
#'   \code{normalize_edgelist} will renumber them to make sure the vertex ID's
#'   start with 1 and end with the number of vertices, leaving no unused ID's in
#'   between.
#'
#' @param edge_list An edge list, with columns 1 and 2 the columns with vertices
#'   to be renumbered
#'
#' @return Same edge list with, but with normalized vertex ID's
#'   
#' @examples 
#' edge_list
#' #      [,1] [,2] [,3]
#' # [1,]  101  105  101
#' # [2,]  105  102  103 
#' normalize_edgelist(edge_list)
#' #      [,1] [,2] [,3]
#' # [1,]   1    2   101
#' # [2,]   2    3   103
normalize_edgelist <- function(edge_list){
  # Subtracting the minimum, such that our renumeration vector does not start
  # with a large number of zeros
  minimum <- min(min(edge_list[,1]),min(edge_list[,2]))
  edge_list[,1] <- edge_list[,1] - minimum + 1
  edge_list[,2] <- edge_list[,2] - minimum + 1
  
  # We make a vector with the nth entry being the new vertex ID of the vertex
  # with old vertex number n
  nodes <- unique(c(edge_list[,1],edge_list[,2]))
  map <- rep(0,length=max(nodes))
  map[nodes] <- seq_len(length(nodes))
  
  # Renumerating the vertex number using these numbers as indices for the vector
  # with new vertex IDs
  edge_list[,1] <- map[edge_list[,1]]
  edge_list[,2] <- map[edge_list[,2]]
  return(edge_list)
}# normalize_edgelist


#' @title Transport matrix from edge usage matrix
#'
#' @description Approximates the total amount that is being transported between
#'   every pair of nodes by calculating the sum over the of the amount
#'   transported over every edge along the shortest path between this pair of
#'   nodes
#'
#' @param cl A cluster of computer kernels to be used for the calculation
#' @param g An igraph graph
#' @param u_matrix A matrix with the usage (e.g. the number of passengers) of
#'   every edge
#'
#' @result A transport matrix with the sum (transport_sum) or minimum
#'   (transport_min) of usages over the shortest paths as transport and zero
#'   when no path (or self-loop)
#' 
#' @examples
#' g <- graph_from_literal(1--2--3)
#' u_matrix 
#' #      [,1] [,2] [,3]
#' # [,1]   0    1    0
#' # [,2]   1    0    1
#' # [,3]   0    0    0
#' transport_sum(cl,g,u_matrix)
#' #      [,1] [,2] [,3]
#' # [,1]   0    1    2
#' # [,2]   1    0    1
#' # [,3]   1    0    0
#' 
#' @family transport_sum, transport_min
transport_sum <- function(cl,g,u_matrix){
  t_matrix <- parSapply(cl,1:vcount(g),function(origin,g,u_matrix){
    paths <- shortest_paths(g,from=origin,to=V(g),mode="out")$vpath
    
    # Calculating sum of usages over every shortest path
    sapply(1:vcount(g),function(dest,origin,paths,g,u_matrix){
      path <- paths[[dest]]
      t <- 0
      if (length(path)<2){
        return(t)
      }# if
      for (node in 2:length(path)){
        t <- t + u_matrix[path[node-1],path[node]]
      }# for
      return(t)
    },origin,paths,g,u_matrix)# sapply
  },g,u_matrix)# parSapply
  
  t_matrix <- t(t_matrix)
  return(t_matrix)
}# transport_sum


#' @title Transport matrix from edge usage matrix
#'
#' @description Approximates the total amount that is being transported between
#'   every pair of nodes by calculating the minimum of the of the amounts
#'   transported over every edge along the shortest path between this pair of
#'   nodes
#'
#' @param cl A cluster of computer kernels to be used for the calculation
#' @param g An igraph graph
#' @param u_matrix A matrix with the usage (e.g. the number of passengers) of
#'   every edge
#'
#' @result A transport matrix with the sum (transport_sum) or minimum
#'   (transport_min) of usages over the shortest paths as transport and zero
#'   when no path (or self-loop)
#' 
#' @examples
#' g <- graph_from_literal(1--2--3)
#' u_matrix 
#' #      [,1] [,2] [,3]
#' # [,1]   0    1    0
#' # [,2]   1    0    1
#' # [,3]   0    0    0
#' transport_sum(cl,g,u_matrix)
#' #      [,1] [,2] [,3]
#' # [,1]   0    1    1
#' # [,2]   1    0    1
#' # [,3]   0    0    0
#' 
#' @family transport_sum, transport_min
transport_min <- function(cl,g,u_matrix){
  t_matrix <- parSapply(cl,1:vcount(g),function(origin,g,u_matrix){
    paths <- shortest_paths(g,from=origin,to=V(g),mode="out")$vpath
    
    # Calculating sum of usages over every shortest path
    sapply(1:vcount(g),function(dest,origin,paths,g,u_matrix){
      path <- paths[[dest]]
      if (length(path)<2){
        return(0)
      }# if
      t <- c(Inf)
      for (node in 2:length(path)){
        t <- c(t,u_matrix[path[node-1],path[node]])
      }# for
      return(min(t))
    },origin,paths,g,u_matrix)# sapply
  },g,u_matrix)# parSapply
  
  t_matrix <- t(t_matrix)
  return(t_matrix)
}# transport_min
