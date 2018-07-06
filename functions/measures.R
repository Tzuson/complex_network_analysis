source("initialization.R")
source("functions/functions.R")

# Assumption: g a simple graph


#' Efficiency of a graph, according to Latora (2001)
#' 
#' @param g A graph 
#' @param l A matrix
#' @return The global efficiency (number)
global_efficiency <- function(g,l){
  n <- vcount(g)
  if (n<2){
    return(0)
  }# if
  d <- distances(g, v=V(g), to=V(g), mode="out")
  diag(d) <- Inf
  diag(l) <- Inf
  return(sum(1/d)/(sum(1/l)))
}# global_efficiency(g,l)


#' Local efficiency of the nodes of a graph
#' 
#' @param g A graph
#' @param l A matrix
#' @return The local efficiencies of nodes (vector)
local_efficiency <- function(g,l){
  sapply(V(g),function(node){
    subg <- induced_subgraph(g,c(neighbors(g,node)))
    subl <- l[V(subg),V(subg)]
    global_efficiency(subg, subl)
  })
}# local_efficiency(g,l)


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
#' @param l A matrix
#' @return The global efficacy (number)
#' @details:
#' The global efficacy of a graph is the sum over inverse shortest path lengths,
#' multiplied with the number of shortest paths, divided by n(n-1). 
#' (van der Loo, 2018)
global_efficacy <- function(g,l){
  n <- vcount(g)
  if (n<2){
    return(0)
  }# if
  d <- distances(g, v=V(g), to=V(g), mode="out")
  mu <- matrix(0, nrow=n, ncol=n)
  for (i in seq(n)){
    d[i,i] <- Inf
    l[i,i] <- Inf
    mu[i,] <- all_shortest_paths(g, from=i, to=V(g), mode="out")$nrgeo
  }# for
  return(sum(mu/d)/(sum(1/l)))
}# global_efficacy(g,l)


#' The local efficacy
#' 
#' @param g A graph
#' @param l A matrix
#' @return The local efficiencies of nodes (vector)
local_efficacy <- function(g,l){
  sapply(V(g),function(node){
    subg <- induced_subgraph(g,c(neighbors(g,node)))
    subl <- l[V(subg),V(subg)]
    return(global_efficacy(subg,subl))
  })
}# local_efficacy(g,l)


#' Return a vector of graph characterization
#' 
#' @param g A graph
#' @param l A matrix
#' @param performance A function
#' @return Graph characterization numbers
#' @details 
#' The characterizations are: global and local efficiency, global and local efficacy,
#' vulnerability of nodes and edges, and topological information content
measures <- function(g, l, performance){
  c(
    global_efficiency = global_efficiency(g,l)
    , local_efficiency = mean(local_efficiency(g,l))
    , global_efficacy = global_efficacy(g,l)
    , local_efficacy  = mean(local_efficacy(g,l))
    , vulnerability_nodes = max(vulnerability_nodes(g, l, performance))
    , vulnerability_edges = max(vulnerability_edges(g, l, performance))
    , topological_information_content = information_content(g)
  )
}# measures(g, l, performance)










