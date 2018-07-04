source("initialization.R")
source("functions/functions.R")

# Assumption: g a simple, connected, undirected and unweighted graph


#' Efficiency of a graph, according to Latora (2001)
#' 
#' @param g A graph
#' @return The global efficiency (number)
global_efficiency <- function(g){
  n <- length(V(g))
  if (n >= 2){
    nd <- igraph::distance_table(g)$res
    d <- seq_along(nd)
    return(2*sum(nd/d)/(n*(n-1)))
  }# if (n>0)
  else {
    return(0)
  }# else
}# global_efficiency(g)


#' Local efficiency of the nodes of a graph
#' 
#' @param g A graph
#' @return The local efficiencies of nodes (vector)
local_efficiency <- function(g){
  sapply(V(g),function(node){
    h <- induced_subgraph(g,c(neighbors(g,node)))
    global_efficiency(h)
  })
}# local_efficiency(g)


#' Network vulnerability per node, according to Gol'dshtein (2004) and 
#' Latora et al (2005).
#' 
#' @param g A graph
#' @param performance A function
#' @return The vulnerabilities of nodes (vector)
vulnerability_nodes <- function(g, performance){
  nodes <- V(g)
  sapply(seq_along(nodes), function(i){
    h <- induced_subgraph(g, nodes[-i])
    1-performance(h)/performance(g)
  })
}# vulnerability_nodes(g, performance)


#' Network vulnerability per edge, according to Gol'dshtein (2004) and 
#' Latora et al (2005).
#' 
#' @param g A graph
#' @param performance A function
#' @return The vulnerabilities of edges (vector)
vulnerability_edges <- function(g, performance){
  edges <- E(g)
  sapply(seq_along(edges), function(i){
    h <- subgraph.edges(g, edges[-i], delete.vertices = FALSE)
    1-performance(h)/performance(g)
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
  h <- delete.edges(g, edges)
  h <- delete.vertices(h, nodes)
  return(1-performance(h)/performance(g))
}# vulnerability(g, nodes, edges, performance)


#' Network improvement for a list of nodes and edges, according to Gol'dshtein (2004) and 
#' Latora et al (2005).
#' 
#' @param g A graph
#' @param nodes A list of names of new nodes
#' @param edges A list of edges
#' @param performance A function
#' @return The improvement due to added nodes and edges
#' @details 
#' For the edges list, we have edges[1] as the start and edges[2] as the end of the first edge,
#' then edges[3] as the start and edges[4] as the end of the second edge.
improvement <- function(g, nodes, edges, performance){
  h <- add.vertices(g, length(nodes), name=nodes)
  h <- add.edges(h, edges)
  return(performance(h)/performance(g)-1)
}# improvement(g, nodes, edges, performance)


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
  nodes <- seq_along(V(g))
  n <- length(nodes)
  out <- sapply(nodes[-1],function(i){
    a <- sapply(seq_len(i-1), function(j){
      L <- igraph::all_shortest_paths(g,from=i, to=j)
      mu <- length(L$res)
      len <- length(L$res[[1]])-1L
      c(mu=mu,len=len)
    })
    sum(a[1,]/a[2,])
  })
  return(2*sum(out)/(n*(n-1)))
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
#' @param performance A function
#' @return Graph characterization numbers (vector)
#' @details 
#' The characterizations are: global and local efficiency, global and local efficacy,
#' vulnerability of nodes and edges, and topological information content
measures <- function(g,performance){
  c(
    global_efficiency = global_efficiency(g)
    , local_efficiency = mean(local_efficiency(g))
    #, global_efficacy = global_efficacy(g)
    #, local_efficacy  = mean(local_efficacy(g))
    , vulnerability_nodes = max(vulnerability_nodes(g,performance))
    , vulnerability_edges = max(vulnerability_edges(g,performance))
    , topological_information_content = information_content(g)
  )
}# measures(g,performance)










