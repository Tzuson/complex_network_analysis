source("system/initialization.R")
source("system/parallel_programming.R")
source("system/functions.R")


#' @title Network vulnerability per node, adjusted version of Gol'dshtein (2004) and
#' Latora et al (2005)
#'
#' @description The vulnerability of a single node \code{node} in a network for
#'   a certain performance function is defined as \code{vul(node) <-
#'   1-performance(g')/performance(g)}, with \code{g'} the graph of \code{g}
#'   without in- or outgoing edges from or to node \code{node}. In this way the
#'   vulnerability will not be negative and the node itself is still part of the
#'   network, it is however made impossible to reach it, lowering the
#'   performance of \code{g'} even further.
#'
#'   \code{performance} can be any function on the graph \code{g}, but always
#'   able to take as arguments l and t, as matrix or as NULL. Ignoring these
#'   variables is of course possible.
#'
#'   The length matrix (or l matrix) consists of the "real" distance between
#'   every pair of nodes, not only the nodes that are connected by an edge. The
#'   number of nodes passed along a path between a pair of nodes is not
#'   important anymore, only the distance traveled (see examples). It is assumed
#'   that the length between a pair of nodes connected by an edge is always
#'   smaller then the weight (e.g. length of the edge) of the edge. The
#'   efficiency is normalised by calculating the total efficiency of a full
#'   graph with the distances between every pair of nodes given by the l matrix.
#'   When omitted, the length between every pair of nodes is considerd 1, as
#'   well as the length of every edge.
#'
#'   The transport matrix (or t matrix) gives the importance of pairs of nodes,
#'   i.e. a t-value of 2, where all other values are 1 for the pair \code{[i,j]}
#'   means the efficiency of the path between these two points is considered
#'   twice as important than other paths. It is called transport matrix because
#'   the large number of passengers between two international airports in
#'   contrast to the small number of passengers between two regional airfields
#'   leads to the conclusion that it is more important to make the journey
#'   between these two international airports more efficient then the journey
#'   between the two regional ones. When omitted, every pair is deemed as
#'   equally important.
#'
#' @param cl A cluster of computer kernels to be used for the calculation
#' @param g An igraph graph
#' @param performance A function of g,l and t
#' @param l A length matrix - optional -
#' @param t A transport matrix - optional -
#'
#' @return A vector with the vulnerabilities of every node
#'
#' @examples
#' g1 <- graph_from_literal(1--2--3)
#' vulnerability_nodes(cl,g1,global_efficiency_unpar)
#' # [1] 0.6 1.0 0.6
#'
#' g2 <- graph_from_literal(1-+2-+3)
#' vulnerability_nodes(cl,g2,global_efficiency_unpar,l3)
#' # [1] 0.5 1.0 0.5
#'
#' adj3 <- matrix(c(0,1,0,1,0,1,0,1,0),nrow=3,ncol=3)
#' l3 <- matrix(c(0,1,2,1,0,1,2,1,0),nrow=3,ncol=3)
#' g3 <- graph_from_adjacency_matrix(adj3*l3,mode="undirected",weighted=TRUE)
#' vulnerability_nodes(cl,g3,global_efficiency_unpar,l3)
#' # [1] 0.6 1.0 0.6
#'
#' adj4 <- matrix(c(0,0,0,1,0,0,0,1,0),nrow=3,ncol=3)
#' g4 <- graph_from_adjacency_matrix(adj4*l3,mode="directed",weighted=TRUE)
#' vulnerability_nodes(cl,g4,global_efficiency_unpar,l3)
#' # [1] 0.6 1.0 0.6
#'
#' adj5 <- matrix(c(0,10,0,10,0,10,0,10,0),nrow=3,ncol=3)
#' g5 <- graph_from_adjacency_matrix(adj5*l3,mode="undirected",weighted=TRUE)
#' vulnerability_nodes(cl,g5,global_efficiency_unpar,l3)
#' # [1] 0.6 1.0 0.6
#'
#' t6 <- matrix(c(0,0,0,1,0,0,0,1,0),nrow=3,ncol=3)
#' vulnerability_nodes(cl,g5,global_efficiency_unpar,l3,t6)
#' # [1] 0.5 1.0 0.5
#' 
#' vulnerability_nodes(cl,g5,function(g,l,t){ecount(g)})
#' # [1] 0.5 1.0 0.5
#'
#' @family vulnerability_nodes, vulnerability_edges
vulnerability_nodes <- function(cl,g,performance,l=NULL,t=NULL){
  # Data of whole graph
  n <- vcount(g)
  p <- performance(g,l,t)
  
  # Deleting incident (in and out going) of particular node,
  # and calculating efficiency of resulting graph
  vul <- parSapply(cl,1:n,function(node,g,l,t,p,performance){
    subedges_delete <- incident(g,node,mode="all")
    subg <- delete_edges(g,subedges_delete)
    
    vul_node <- 1-performance(subg,l,t)/p
    return(vul_node)
  },g,l,t,p,performance)# parSapply

  return(vul)
}# vulnerability_nodes


#' @title Network vulnerability per edge, adjusted version of Gol'dshtein (2004)
#'   and Latora et al (2005)
#'
#' @description The vulnerability of a single edge \code{node} in a network for
#'   a certain performance function is defined as \code{vul(edge) <-
#'   1-performance(g')/performance(g)}, with \code{g'} the graph of \code{g}
#'   without edge \code{edge}.
#'
#'   \code{performance} can be any function on the graph \code{g}, but always
#'   able to take as arguments l and t, as matrix or as NULL. Ignoring these
#'   variables is of course possible.
#'
#'   The length matrix (or l matrix) consists of the "real" distance between
#'   every pair of nodes, not only the nodes that are connected by an edge. The
#'   number of nodes passed along a path between a pair of nodes is not
#'   important anymore, only the distance traveled (see examples). It is assumed
#'   that the length between a pair of nodes connected by an edge is always
#'   smaller then the weight (e.g. length of the edge) of the edge. The
#'   efficiency is normalised by calculating the total efficiency of a full
#'   graph with the distances between every pair of nodes given by the l matrix.
#'   When omitted, the length between every pair of nodes is considerd 1, as
#'   well as the length of every edge.
#'
#'   The transport matrix (or t matrix) gives the importance of pairs of nodes,
#'   i.e. a t-value of 2, where all other values are 1 for the pair \code{[i,j]}
#'   means the efficiency of the path between these two points is considered
#'   twice as important than other paths. It is called transport matrix because
#'   the large number of passengers between two international airports in
#'   contrast to the small number of passengers between two regional airfields
#'   leads to the conclusion that it is more important to make the journey
#'   between these two international airports more efficient then the journey
#'   between the two regional ones. When omitted, every pair is deemed as
#'   equally important.
#'
#' @param cl A cluster of computer kernels to be used for the calculation
#' @param g An igraph graph
#' @param performance A function of g,l and t
#' @param l A length matrix - optional -
#' @param t A transport matrix - optional -
#'
#' @return A vector with the vulnerabilities of every edge
#'
#' @examples
#' g1 <- graph_from_literal(1--2--3)
#' vulnerability_edges(cl,g1,global_efficiency_unpar)
#' # [1] 0.6 0.6
#'
#' g2 <- graph_from_literal(1-+2-+3)
#' vulnerability_edges(cl,g2,global_efficiency_unpar,l3)
#' # [1] 0.6 0.6
#'
#' adj3 <- matrix(c(0,1,0,1,0,1,0,1,0),nrow=3,ncol=3)
#' l3 <- matrix(c(0,1,2,1,0,1,2,1,0),nrow=3,ncol=3)
#' g3 <- graph_from_adjacency_matrix(adj3*l3,mode="undirected",weighted=TRUE)
#' vulnerability_edges(cl,g3,global_efficiency_unpar,l3)
#' # [1] 0.6 1.0 0.6
#'
#' adj4 <- matrix(c(0,0,0,1,0,0,0,1,0),nrow=3,ncol=3)
#' g4 <- graph_from_adjacency_matrix(adj4*l3,mode="directed",weighted=TRUE)
#' vulnerability_edges(cl,g4,global_efficiency_unpar,l3)
#' # [1] 0.6 0.6
#'
#' adj5 <- matrix(c(0,1,0,1,0,3,0,3,0),nrow=3,ncol=3)
#' g5 <- graph_from_adjacency_matrix(adj5,mode="undirected",weighted=TRUE)
#' vulnerability_edges(cl,g5,global_efficiency_unpar,l3)
#' # [1] 0.7894737 0.3684211
#'
#' t6 <- matrix(c(0,0,0,1,0,0,0,1,0),nrow=3,ncol=3)
#' vulnerability_edges(cl,g5,global_efficiency_unpar,l3,t6)
#' # [1] 0.75 0.25
#'
#' vulnerability_edges(cl,g5,function(g,l,t){ecount(g)})
#' # [1] 0.5 0.5
#'
#' @family vulnerability_nodes, vulnerability_edges
vulnerability_edges <- function(cl,g,performance,l=NULL,t=NULL){
  # Calculating data on whole graph
  e <- ecount(g)
  p <- performance(g,l,t)
 
  # Deleting incident (in and out going) of particular node,
  # and calculating efficiency of resulting graph
  vul <- parSapply(cl,1:e,function(edge,g,l,t,p){
    subg <- delete_edges(g,c(edge))
    
    vul_edge <- 1-performance(subg,l,t)/p
    return(vul_edge)
  },g,l,t,p)# parSapply
  return(vul)
}# vulnerability_edges


# #' @title Network vulnerability for a type of damage, adjusted version of
# #'   Gol'dshtein (2004) and Latora et al (2005)
# #'
# #' @description The vulnerability of a network for a type of damage, i.e. the
# #'   removal of a list of edges and the incident edges of a list of nodes, for a
# #'   certain performance function is defined as \code{vul(d) <-
# #'   1-performance(g')/performance(g)}, with \code{g'} the graph of \code{g}
# #'   without the deleted edges.
# #'
# #'   \code{performance} can be any function on the graph \code{g}, but always
# #'   able to take as arguments l and t, as matrix or as NULL. Ignoring these
# #'   variables is of course possible.
# #'
# #'   The length matrix (or l matrix) consists of the "real" distance between
# #'   every pair of nodes, not only the nodes that are connected by an edge. The
# #'   number of nodes passed along a path between a pair of nodes is not
# #'   important anymore, only the distance traveled (see examples). It is assumed
# #'   that the length between a pair of nodes connected by an edge is always
# #'   smaller then the weight (e.g. length of the edge) of the edge. The
# #'   efficiency is normalised by calculating the total efficiency of a full
# #'   graph with the distances between every pair of nodes given by the l matrix.
# #'   When omitted, the length between every pair of nodes is considerd 1, as
# #'   well as the length of every edge.
# #'
# #'   The transport matrix (or t matrix) gives the importance of pairs of nodes,
# #'   i.e. a t-value of 2, where all other values are 1 for the pair \code{[i,j]}
# #'   means the efficiency of the path between these two points is considered
# #'   twice as important than other paths. It is called transport matrix because
# #'   the large number of passengers between two international airports in
# #'   contrast to the small number of passengers between two regional airfields
# #'   leads to the conclusion that it is more important to make the journey
# #'   between these two international airports more efficient then the journey
# #'   between the two regional ones. When omitted, every pair is deemed as
# #'   equally important.
# #'
# #' @param cl A cluster of computer kernels to be used for the calculation
# #' @param g An igraph graph
# #' @param performance A function of g,l and t
# #' @param nodes A list of nodes to be deleted
# #' @param edges A
# #' @param l A length matrix - optional -
# #' @param t A transport matrix - optional -
# #'
# #' @return A vector with the vulnerabilities of every edge
# #'
# #' @examples
# #' g1 <- graph_from_literal(1--2--3)
# #' vulnerability_edges(cl,g1,global_efficiency_unpar)
# #' # [1] 0.6 0.6
# #'
# #' g2 <- graph_from_literal(1-+2-+3)
# #' vulnerability_edges(cl,g2,global_efficiency_unpar,l3)
# #' # [1] 0.6 0.6
# #'
# #' adj3 <- matrix(c(0,1,0,1,0,1,0,1,0),nrow=3,ncol=3)
# #' l3 <- matrix(c(0,1,2,1,0,1,2,1,0),nrow=3,ncol=3)
# #' g3 <- graph_from_adjacency_matrix(adj3*l3,mode="undirected",weighted=TRUE)
# #' vulnerability_edges(cl,g3,global_efficiency_unpar,l3)
# #' # [1] 0.6 1.0 0.6
# #'
# #' adj4 <- matrix(c(0,0,0,1,0,0,0,1,0),nrow=3,ncol=3)
# #' g4 <- graph_from_adjacency_matrix(adj4*l3,mode="directed",weighted=TRUE)
# #' vulnerability_edges(cl,g4,global_efficiency_unpar,l3)
# #' # [1] 0.6 0.6
# #'
# #' adj5 <- matrix(c(0,1,0,1,0,3,0,3,0),nrow=3,ncol=3)
# #' g5 <- graph_from_adjacency_matrix(adj5,mode="undirected",weighted=TRUE)
# #' vulnerability_edges(cl,g5,global_efficiency_unpar,l3)
# #' # [1] 0.7894737 0.3684211
# #'
# #' t6 <- matrix(c(0,0,0,1,0,0,0,1,0),nrow=3,ncol=3)
# #' vulnerability_edges(cl,g5,global_efficiency_unpar,l3,t6)
# #' # [1] 0.75 0.25
# #'
# #' vulnerability_edges(cl,g5,function(g,l,t){ecount(g)})
# #' # [1] 0.5 0.5
# #'
# #' @family vulnerability_nodes, vulnerability_edges, vulnerability
# vulnerability <- function(cl,g,performance,nodes,edges,l=NULL,t=NULL){
#   # Edges from/to deleted nodes + edges to delete
#   subedges_delete <- incident_edges(g1,nodes,mode="all")
#   subedges_delete <- unique(c(subedges_delete,edges))
#   subg <- delete_edges(g, edges)
# 
#   vul_node <- 1-performance(subg,l=subl,t=t)/performance(g,l,t)
#   return(vul_node)
# }# vulnerability