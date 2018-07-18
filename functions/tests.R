source("system/initialization.R")
source("system/parallell_programming.R")

source("functions/functions.R")


#' Network vulnerability per node, according to Gol'dshtein (2004) and 
#' Latora et al (2005).
#' 
#' @param cl A cluster of cores
#' @param g A graph
#' @param performance A function
#' @param l A matrix - optional -
#' @return The vulnerabilities of nodes
vulnerability_nodes <- function(cl, g, performance, l){
  n <- vcount(g)
  
  # Calculating for every node performance of
  # the network when the node is missing
  if (missing(l)){
    p <- performance(cl,g)
    vul <- parSapply(cl,1:n, function(i,g,p){
      subg <- delete_vertices(g,c(i))
      1-performance(cl,subg)/p
    },g,p)# parSapply
  }# if
  else {
    p <- performance(cl,g, l)
    vul <- parSapply(cl,1:n, function(i,g,l,p){
      subg <- delete_vertices(g,c(i))
      subl <- l[-i,-i]
      1-performance(cl,subg,subl)/p
    },g,l,p)
  }# else
  
  return(vul)
}# vulnerability_nodes


#' Network vulnerability per edge, according to Gol'dshtein (2004) and 
#' Latora et al (2005).
#' 
#' @param cl A cluster
#' @param g A graph
#' @param performance A function
#' @param l A matrix - optional -
#' @return The vulnerabilities of edges
vulnerability_edges <- function(cl, g, performance, l){
  e <- ecount(g)
  
  # Calculating for every edge performance of
  # the network when the edge is missing
  if (missing(l)){
    p <- performance(cl,g)
    vul <- parSapply(cl,1:e, function(i,g,p){
      subg <- delete_edges(g,c(i))
      1-performance(cl,subg)/p
    },g,p)# parSapply
  }# if
  else {
    p <- performance(cl,g, l)
    vul <- parSapply(cl,1:e, function(i,g,l,p){
      subg <- delete_edges(g,c(i))
      1-performance(cl,subg,l)/p
    },g,l,p)
  }# else
  
  return(vul)
}# vulnerability_edges


#' Network vulnerability for a list of nodes and edges, according to Gol'dshtein (2004) and 
#' Latora et al (2005).
#' 
#' @param cl A cluster
#' @param g A graph
#' @param nodes A list of names of new nodes
#' @param edges A list of edges
#' @param performance A function
#' @param l A matrix - optional -
#' @return The vulnerability due to the removed nodes and edges
#' @details 
#' For the edges list, we have edges[1] as the start and edges[2] as the end of the first edge,
#' then edges[3] as the start and edges[4] as the end of the second edge.
vulnerability <- function(cl, g, nodes, edges, performance, l){
  # First delete edges, then nodes, so names of edges, 
  # of original network can be used
  subg <- delete_edges(g, edges)
  subg <- delete_vertices(subg, nodes)
  
  if (missing(l)){
    p <- performance(cl,g)
    return(1-performance(subg)/p)
  }# if
  else {
    p <- performance(cl,g,l)
    subl <- l[-nodes, -nodes]
    return(1-performance(cl,subg,subl)/p)
  }#else
}# vulnerability


#' Network improvement for a list of nodes and edges, according to Gol'dshtein (2004) and 
#' Latora et al (2005).
#' 
#' @param cl A cluster
#' @param g A graph
#' @param nodes A list of names of nodes
#' @param edges A list of edges
#' @param performance A function
#' @param l A matrix - optional -
#' @return The improvement due to added nodes and edges
#' @details 
#' For the edges list, we have edges[1] as the start and edges[2] as the end of the first edge,
#' then edges[3] as the start and edges[4] as the end of the second edge.
#' NB The distances matrix l is of the NEW graph, not the old. 
improvement <- function(cl, g, nodes, edges, performance, l){
  if (missing(l)){
    p <- performance(cl,g)
    superg <- add.vertices(g, length(nodes), name=nodes)
    superg <- add.edges(subg, edges)
    return(performance(cl,superg)/p-1)
  }# if
  else {
    p <- performance(cl,g,l[V(g),V(g)])
    superg <- add.vertices(g, length(nodes), name=nodes)
    superg <- add.edges(subg, edges)
    return(performance(cl,superg,l)/p-1)
  }# else
}# improvement