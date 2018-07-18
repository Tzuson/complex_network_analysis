source("system/initialization.R")
source("system/parallell_programming.R")


# Assumption: g a simple graph


#' Efficiency of a graph, according to Latora (2001)
#' 
#' @param g A graph 
#' @param l A matrix - optional -
#' @return The global efficiency (number)
global_efficiency <- function(cl,g,l){
  # Efficiency is zero when n<2
  n <- vcount(g)
  if (n<2){
    return(0)
  }# if
  
  # Calculating distances
  d <- distances(g, v=V(g), to=V(g), mode="out")
  diag(d) <- Inf
  
  # Normalization depends on distances matrix l
  # if the graph is weighted
  if (missing(l)){
    return(sum(1/d)/(n*(n-1)))
  }# if
  else {
    diag(l) <- Inf
    return(sum(1/d)/sum(1/l))
  }# else
}# global_efficiency


#' Local efficiency of the nodes of a graph
#' 
#' @param cl A cluster
#' @param g A graph
#' @param l A matrix - optional -
#' @return The local efficiencies of nodes (vector)
local_efficiency <- function(cl,g,l){
  if (missing(l)){
    parSapply(cl,V(g),function(node,cl,g){
      subg <- induced_subgraph(g,c(neighbors(g,node)))
      global_efficiency(cl,subg)
    },cl,g)# parSapply
  }# if
  else {
    parSapply(cl,V(g),function(node,cl,g,l){
      subnodes <- c(neighbors(g,node))
      subg <- induced_subgraph(g,subnodes)
      subl <- l[subnodes,subnodes]
      global_efficiency(cl,subg,subl)
    },cl,g,l)# parSapply
  }# else
}# local_efficiency


#' Efficiency of the neighbourhoods of the nodes
#' 
#' @param cl A cluster
#' @param g A graph
#' @param c_neighbors A number
#' @param l A matrix - optional -
#' @result A vector with the global efficiencies of the neighbourhoods 
#' of the nodes with radius c_neighbors around the node
local_efficiency_generalised <- function(cl,g,c_neighbors,l){
  n <- vcount(g)
  d_g <- distances(g,v=V(g),to=V(g),mode="out")
  
  if (missing(l)){
    eff <- parSapply(cl,1:n, function(node,g,d_g,c_neighbors,n){
      node_neighbors <- c()
      for (nb in 1:n){
        if (d_g[node,nb] < c_neighbors && !(node==nb)){
          node_neighbors <- c(node_neighbors,nb)
        }# if
      }# for
      subg <- induced_subgraph(g,node_neighbors)
      global_efficiency(cl,subg)
    },g,d_g,c_neighbors,n)# parSapply
  }# if
  else {
    eff <- parSapply(cl,1:n, function(node,g,d_g,c_neighbors,n,l){
      node_neighbors <- c()
      for (nb in 1:n){
        if (d_g[node,nb] < c_neighbors && !(node==nb)){
          node_neighbors <- c(node_neighbors,nb)
        }# if
      }# for
      subg <- induced_subgraph(g,node_neighbors)
      subl <- l[node_neighbors,node_neighbors]
      global_efficiency(cl,subg,subl)
    },g,d_g,c_neighbors,n,l)# parSapply
  }# else
  return(eff)
}# local_efficiency_generalised


#' Topological information content
#' 
#' @param g A graph
#' @return The topological information content (number)
#' @details
#' The topological information content is defined as
#' the logarithm of the size of the automorphism group to the base of 2.
information_content <- function(cl,g){
  log2(as.numeric(automorphisms(g)$group_size))
}# information_content



#' Global efficacy of a graph
#' 
#' @param cl A cluster
#' @param g A graph
#' @param l A matrix - optional -
#' @return The global efficacy (number)
#' @details:
#' The global efficacy of a graph is the sum over inverse shortest path lengths,
#' multiplied with the number of shortest paths, divided by n(n-1). 
#' (van der Loo, 2018)
global_efficacy <- function(cl,g,l){
  # Efficiency is zero when n<2
  n <- vcount(g)
  if (n<2){
    return(0)
  }# if
  
  # Calculating distances
  d <- distances(g, v=V(g), to=V(g), mode="out")
  diag(d) <- Inf
  
  # Calculating number of shortest paths
  mu <- matrix(0, nrow=n, ncol=n)
  mu <- parSapply(cl, 1:n, function(i,g){
    all_shortest_paths(g, from=i, to=V(g), mode="out")$nrgeo
  },g)# parSapply
  mu <- t(mu)
  
  # Normalization depends on distances matrix l
  # if the graph is weighted
  if (missing(l)){
    return(sum(mu/d)/(n*(n-1)))
  }# if
  else {
    diag(l) <- Inf
    return(sum(mu/d)/(sum(1/l)))
  }# else
}# global_efficacy



#' Global efficacy of a graph (unparallellised)
#' 
#' @param g A graph
#' @param l A matrix - optional -
#' @return The global efficacy (number)
#' @details:
#' The global efficacy of a graph is the sum over inverse shortest path lengths,
#' multiplied with the number of shortest paths, divided by n(n-1). 
#' (van der Loo, 2018)
global_efficacy_unpar <- function(g,l){
  # Efficiency is zero when n<2
  n <- vcount(g)
  if (n<2){
    return(0)
  }# if
  
  # Calculating distances
  d <- distances(g, v=V(g), to=V(g), mode="out")
  diag(d) <- Inf
  
  # Calculating number of shortest paths
  mu <- matrix(0, nrow=n, ncol=n)
  mu <- sapply(1:n, function(i,g){
    all_shortest_paths(g, from=i, to=V(g), mode="out")$nrgeo
  },g)# parSapply
  mu <- t(mu)
  
  # Normalization depends on distances matrix l
  # if the graph is weighted
  if (missing(l)){
    return(sum(mu/d)/(n*(n-1)))
  }# if
  else {
    diag(l) <- Inf
    return(sum(mu/d)/(sum(1/l)))
  }# else
}# global_efficacy_unpar


#' The local efficacy
#' 
#' @param cl A cluster
#' @param g A graph
#' @param l A matrix - optional -
#' @return The local efficiencies of nodes (vector)
local_efficacy <- function(cl,g,l){
  if (missing(l)){
    parSapply(cl,V(g),function(node,cl,g){
      subg <- induced_subgraph(g,c(neighbors(g,node)))
      global_efficacy_unpar(subg)
    },cl,g)# parSapply
  }# if
  else {
    parSapply(cl,V(g),function(node,cl,g,l){
      subnodes <- neighbors(g,node)
      subg <- induced_subgraph(g,subnodes)
      subl <- l[subnodes,subnodes]
      global_efficacy_unpar(subg, subl)
    },cl,g,l)# parSapply
  }# else
}# local_efficacy


#' Return a vector of graph characterization
#' 
#' @param cl A cluster
#' @param g A graph
#' @param performance A function
#' @param l A matrix - optional -
#' @return Graph characterization numbers
#' @details 
#' The characterizations are: global and local efficiency, global and local efficacy,
#' vulnerability of nodes and edges, and topological information content
measures <- function(cl,g,performance,l){
  if (missing(l)){
    c(
      global_efficiency = global_efficiency(cl,g)
      , local_efficiency = mean(local_efficiency(cl,g))
      , global_efficacy = global_efficacy(cl,g)
      , local_efficacy  = mean(local_efficacy(cl,g))
      , vulnerability_nodes = max(vulnerability_nodes(cl,g,performance))
      , vulnerability_edges = max(vulnerability_edges(cl,g,performance))
      , topological_information_content = information_content(cl,g)
    )# c
  }# if
  else {
    c(
      global_efficiency = global_efficiency(cl,g,l)
      , local_efficiency = mean(local_efficiency(cl,g,l))
      , global_efficacy = global_efficacy(cl,g,l)
      , local_efficacy  = mean(local_efficacy(cl,g,l))
      , vulnerability_nodes = max(vulnerability_nodes(cl,g,performance,l))
      , vulnerability_edges = max(vulnerability_edges(cl,g,performance,l))
      , topological_information_content = information_content(cl,g)
    )
  }# else
}# measures










