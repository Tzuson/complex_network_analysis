source("initialization.R")
source("functions/functions.R")


#' Network vulnerability per node, according to Gol'dshtein (2004) and 
#' Latora et al (2005).
#' 
#' @param g A graph
#' @param l A matrix
#' @param performance A function
#' @return The vulnerabilities of nodes
vulnerability_nodes <- function(g, l, performance){
  nodes <- V(g)
  e <- performance(g, l)
  sapply(seq_along(nodes), function(i){
    subg <- induced_subgraph(g, nodes[-i])
    subl <- l[V(subg),V(subg)]
    1-performance(subg,subl)/e
  })
}# vulnerability_nodes(g, l, performance)


#' Network vulnerability per edge, according to Gol'dshtein (2004) and 
#' Latora et al (2005).
#' 
#' @param g A graph
#' @param l A matrix
#' @param performance A function
#' @return The vulnerabilities of edges
vulnerability_edges <- function(g, l, performance){
  edges <- E(g)
  e <- performance(g,l)
  sapply(seq_along(edges), function(i){
    subg <- subgraph.edges(g, edges[-i], delete.vertices = FALSE)
    1-performance(subg,l)/e
  })
}# vulnerability_edges(g, l, performance)


#' Network vulnerability for a list of nodes and edges, according to Gol'dshtein (2004) and 
#' Latora et al (2005).
#' 
#' @param g A graph
#' @param l A matrix
#' @param nodes A list of names of new nodes
#' @param edges A list of edges
#' @param performance A function
#' @return The vulnerability due to the removed nodes and edges
#' @details 
#' For the edges list, we have edges[1] as the start and edges[2] as the end of the first edge,
#' then edges[3] as the start and edges[4] as the end of the second edge.
vulnerability <- function(g, l, nodes, edges, performance){
  e <- performance(g,l)
  subg <- delete.edges(g, l, edges)
  subg <- delete.vertices(subg, l, nodes)
  subl <- l[V(subg), V(subg)]
  return(1-performance(subg,subl)/e)
}# vulnerability(g, l, nodes, edges, performance)


#' Network improvement for a list of nodes and edges, according to Gol'dshtein (2004) and 
#' Latora et al (2005).
#' 
#' @param g A graph
#' @param l A matrix
#' @param nodes A list of names of nodes
#' @param edges A list of edges
#' @param performance A function
#' @return The improvement due to added nodes and edges
#' @details 
#' For the edges list, we have edges[1] as the start and edges[2] as the end of the first edge,
#' then edges[3] as the start and edges[4] as the end of the second edge.
improvement <- function(g, l, nodes, edges, performance){
  e <- performance(g,l)
  subg <- add.vertices(g, length(nodes), name=nodes)
  subg <- add.edges(subg, edges)
  subl <- l[V(subg),V(subg)]
  return(performance(subg,subl)/e-1)
}# improvement(g, nodes, edges, performance)