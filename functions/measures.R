source("initialization.R")
source("functions/functions.R")

# Assumption: g a simple graph


#' Efficiency of a graph, according to Latora (2001)
#' 
#' @param g A graph 
#' @return The global efficiency (number)
global_efficiency <- function(g){
  n <- length(V(g))
  if (n<2){
    return(0)
  }# if
  d <- distances(g, v=V(g), to=V(g), mode="out")
  for (i in seq(n)){
      d[i,i] <- Inf
  }# for
  return(sum(1/d)/(n*(n-1)))
}# global_efficiency(g)


#' Local efficiency of the nodes of a graph
#' 
#' @param g A graph
#' @param directed A boolean
#' @return The local efficiencies of nodes (vector)
local_efficiency <- function(g){
  sapply(V(g),function(node){
    h <- induced_subgraph(g,c(neighbors(g,node)))
    global_efficiency(h)
  })
}# local_efficiency(g)


#' Topological information content
#' 
#' @param g A graph
#' @return The topological information content (number)
#' @details
#' The topological information content is defined as
#' the logarithm of the size of the automorphism group to the base of 2.
information_content <- function(g){
  log2(as.numeric(igraph::automorphisms(g)$group_size))
}# information_content(g)



#' Global efficacy of a graph
#' 
#' @param g A graph
#' @return The global efficacy (number)
#' @details:
#' The global efficacy of a graph is the sum over inverse shortest path lengths,
#' multiplied with the number of shortest paths, divided by n(n-1). 
#' (van der Loo, 2018)
global_efficacy <- function(g){
  n <- length(V(g))
  if (n<2){
    return(0)
  }# if
  d <- distances(g, v=V(g), to=V(g), mode="out")
  mu <- matrix(0, nrow=n, ncol=n)
  for (i in seq(n)){
    d[i,i] <- Inf
    mu[i,] <- all_shortest_paths(g, from=i, to=V(g), mode="out")$nrgeo
  }# for
  return(sum(mu/d)/(n*(n-1)))
}# global_efficacy(g)


#' The local efficacy
#' 
#' @param g A graph
#' @return The local efficiencies of nodes (vector)
local_efficacy <- function(g){
  sapply(V(g),function(node){
    h <- induced_subgraph(g,c(neighbors(g,node)))
    return(global_efficacy(h))
  })
}# local_efficacy(g)


#' Return a vector of graph characterization
#' 
#' @param g A graph
#' @param directed A boolean
#' @param performance A function
#' @return Graph characterization numbers (vector)
#' @details 
#' The characterizations are: global and local efficiency, global and local efficacy,
#' vulnerability of nodes and edges, and topological information content
measures <- function(g, performance){
  c(
    global_efficiency = global_efficiency(g)
    , local_efficiency = mean(local_efficiency(g))
    , global_efficacy = global_efficacy(g)
    , local_efficacy  = mean(local_efficacy(g))
    #, vulnerability_nodes = max(vulnerability_nodes(g, performance))
    #, vulnerability_edges = max(vulnerability_edges(g, performance))
    , topological_information_content = information_content(g)
  )
}# measures(g,performance)










