source("initialization.R")
source("functions/functions.R")


#' Network vulnerability per node, according to Gol'dshtein (2004) and 
#' Latora et al (2005).
#' 
#' @param g A graph
#' @param performance A function
#' @return The vulnerabilities of nodes
vulnerability_nodes <- function(g, performance){
  nodes <- V(g)
  e <- performance(g)
  sapply(seq_along(nodes), function(i){
    h <- induced_subgraph(g, nodes[-i])
    1-performance(h)/e
  })
}# vulnerability_nodes(g, performance)


#' Network vulnerability per edge, according to Gol'dshtein (2004) and 
#' Latora et al (2005).
#' 
#' @param g A graph
#' @param performance A function
#' @return The vulnerabilities of edges
vulnerability_edges <- function(g, performance){
  edges <- E(g)
  e <- performance(g)
  sapply(seq_along(edges), function(i){
    h <- subgraph.edges(g, edges[-i], delete.vertices = FALSE)
    1-performance(h)/e
  })
}# vulnerability_edges(g, performance)


#' Network vulnerability for a list of nodes and edges, according to Gol'dshtein (2004) and 
#' Latora et al (2005).
#' 
#' @param g A graph
#' @param nodes A list of names of new nodes
#' @param edges A list of edges
#' @param performance A function
#' @return The vulnerability due to the removed nodes and edges
#' @details 
#' For the edges list, we have edges[1] as the start and edges[2] as the end of the first edge,
#' then edges[3] as the start and edges[4] as the end of the second edge.
vulnerability <- function(g, nodes, edges, performance){
  e <- performance(g)
  h <- delete.edges(g, edges)
  h <- delete.vertices(h, nodes)
  return(1-performance(h)/e)
}# vulnerability(g, nodes, edges, performance)


#' Network improvement for a list of nodes and edges, according to Gol'dshtein (2004) and 
#' Latora et al (2005).
#' 
#' @param g A graph
#' @param nodes A list of names of nodes
#' @param edges A list of edges
#' @param performance A function
#' @return The improvement due to added nodes and edges
#' @details 
#' For the edges list, we have edges[1] as the start and edges[2] as the end of the first edge,
#' then edges[3] as the start and edges[4] as the end of the second edge.
improvement <- function(g, nodes, edges, performance){
  e <- performance(g)
  h <- add.vertices(g, length(nodes), name=nodes)
  h <- add.edges(h, edges)
  return(performance(h)/e-1)
}# improvement(g, nodes, edges, performance)