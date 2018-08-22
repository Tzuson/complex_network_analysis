source("system/initialization.R")
source("system/parallell_programming.R")
source("system/functions.R")


#' @title Efficiency of a graph, according to Latora (2001), optimised for large
#'   graphs
#'
#' @description The global efficiency is the normalized sum of the inverse
#'   distances of every pair of nodes, where the normalization constant is the
#'   sum of inverse distances of every pair of nodes of a full graph with the
#'   same number of nodes. In the case of an unweighted and undirected/directed
#'   graph the normalization constant will simply be \code{1/(n*(n-1))}, i.e.
#'   the number of non-self-loop edges.
#'
#'   This function calculates the distances matrix (from igraph) of the network
#'   g row by row. It then sums over these rows, forgetting the row data itself.
#'   At the end, all these sums are collected and added together, so it can be
#'   returned as global efficiency after normalization with
#'   \code{1/(n*(n-1))}.
#'
#' @param cl A cluster of computer kernels to be used for the calculation
#' @param g An igraph graph
#'
#' @return The global efficiency of the network
#'
#' @examples
#' g1 <- graph_from_literal(1--2--3)
#' global_efficiency(cl,g1)
#' # 0.8333333
#' 
#' g2 <- graph_from_literal(1-+2-+3)
#' global_efficiency(cl,g2)
#' # 0.4166667
#' 
#' @family global_efficiency, global_efficiency_unpar, local_efficiency
global_efficiency <- function(cl,g){
  n <- vcount(g)
  nodes <- V(g)
  
  # Efficiency is zero when n<2
  if (n<2){
    return(0)
  }# if
  
  # Calculating the rows of distance, setting the self-loop distance to Inf
  # (then the efficiency is zero and thus not counted)
  d_partition <- parSapply(cl,1:n,function(row,g,nodes){
    d_row <- distances(g,v=c(row),to=nodes,mode="out")
    d_row[,row] <- Inf
    return(sum(1/d_row))
  },g,nodes)# parSapply
  
  # Normalization constant is theoretical maximum of number of edges
  return(sum(d_partition)/(n*(n-1)))
}# global_efficiency


#' @title Efficiency of a graph, according to Latora (2001)
#'
#' @description The global efficiency is the normalized sum of the inverse
#'   distances of every pair of nodes, where the normalization constant is the
#'   sum of inverse distances of every pair, multiplied with the importance of
#'   this pair of nodes of a full graph with the same number of nodes.
#'
#'   In the case of an unweighted and undirected/directed graph the
#'   normalization constant will simply be \code{1/(n*(n-1))}, i.e. the number
#'   of non-self-loop edges.
#'
#'   When only the network is used, the assumption is that the network is
#'   unweighted, but it may be directed or undirected. In this case
#'   \code{global_efficiency_unpar} gives the same values as
#'   \code(global_efficiency) (it only takes longer for large networks). The
#'   normalization constant will simply be \code(1/(n*(n-1))).
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
#'   Note that if you use the length matrix, it is assumed the edges of the
#'   graph are weighted. Otherwise, length 1 is assumed for every edge. Note
#'   that for the global efficiency to be as Latora meant is, we need
#'   d_ij>=l_ij, with equality if there is an edge between i and j. However,
#'   this function is not restricted to this and accepts every d_ij and l_ij, as
#'   long as sum(l_ij) > 0 (otherwise we divide by zero).
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
#' @param g An igraph graph
#' @param l A length matrix - optional -
#' @param t A transport matrix - optional -
#'
#' @return The global efficiency of the network
#'
#' @examples
#' g1 <- graph_from_literal(1--2--3)
#' global_efficiency_unpar(g1)
#' # 0.8333333
#'
#' g2 <- graph_from_literal(1-+2-+3)
#' global_efficiency_unpar(g2)
#' # 0.4166667
#'
#' adj3 <- matrix(c(0,1,0,1,0,1,0,1,0),nrow=3,ncol=3)
#' l3 <- matrix(c(0,1,2,1,0,1,2,1,0),nrow=3,ncol=3)
#' g3 <- graph_from_adjacency_matrix(adj3*l3,mode="undirected",weighted=TRUE)
#' global_efficiency_unpar(g3,l3)
#' # 1
#'
#' adj4 <- matrix(c(0,0,0,1,0,0,0,1,0),nrow=3,ncol=3)
#' g4 <- graph_from_adjacency_matrix(adj4*l3,mode="directed",weighted=TRUE)
#' global_efficiency_unpar(g4,l3)
#' # 0.5
#'
#' adj5 <- matrix(c(0,10,0,10,0,10,0,10,0),nrow=3,ncol=3)
#' g5 <- graph_from_adjacency_matrix(adj5*l3,mode="undirected",weighted=TRUE)
#' global_efficiency_unpar(g5,l3)
#' # 0.1
#'
#' t6 <- matrix(c(0,0,0,1,0,0,0,1,0),nrow=3,ncol=3)
#' global_efficiency_unpar(g3,l3,t6)
#' # 1
#' 
#' @family global_efficiency
global_efficiency_unpar <- function(g,l=NULL,t=NULL){
  # Efficiency is zero when n<2
  n <- vcount(g)
  if (n<2){
    return(0)
  }# if
  
  # Calculating distances
  d <- distances(g, v=V(g), to=V(g), mode="out")
  diag(d) <- Inf
  
  # Calculating efficiency
  if (is.null(l)&&is.null(t)) {
    return(sum(1/d)/(n*(n-1)))
  } else if (!is.null(l)&&is.null(t)){
    diag(l) <- Inf
    return(sum(1/d)/sum(1/l))
  } else if (is.null(l)&&!is.null(t)){
    diag(t) <- 0*diag(t)
    return(sum(t/d)/sum(t))
  } else {
    diag(t) <- 0*diag(t)
    diag(l) <- Inf
    return(sum(t/d)/sum(t/l))
  }# if else
}# global_efficiency_unpar




#' @title Local efficiency of the nodes of a graph
#'
#' @description (For global efficiency, see \code{global_efficiency_unpar})
#'   Calculates the global efficiency of the \code{c_neighbors}-order
#'   neighbourhood of every node, without the ego node itself
#'
#' @param cl A cluster of computer kernels to be used for the calculation
#' @param g An igraph graph
#' @param c_neighbors An integer giving the order of the neighbourhood
#' @param l A length matrix - optional -
#' @param t A transport matrix - optional -
#'
#' @return A vector with the local efficienties of the nodes
#'
#' @examples
#' g1 <- graph_from_literal(1--2--3)
#' local_efficiency(cl,g1,1)
#' # [1] 0  0  0
#'
#' local_efficiency(cl,g1,2)
#' # [1] 1  0  1
#'
#' adj2 <- matrix(c(0,10,0,10,0,1,0,1,0),nrow=3,ncol=3)
#' l2 <- matrix(c(0,1,2,1,0,1,2,1,0),nrow=3,ncol=3)
#' g2 <- graph_from_adjacency_matrix(adj2*l2,mode="undirected",weighted=TRUE)
#' local_efficiency(cl,g2,2,l=l3)
#' # [1] 1.0  0.0  0.1
#' 
#' @family global_efficiency, global_efficiency_unpar, local_efficiency
local_efficiency <- function(cl,g,c_neighbors,l=NULL,t=NULL){
  parSapply(cl,1:vcount(g),function(node,g,l,t){
    # Calculating order=c_neighbors neighborhood of node
    subnodes <- ego(g,c_neighbors,nodes=c(node),mode="out")[[1]]
    subnodes <- subnodes[-which(subnodes==node)]
    subg <- induced_subgraph(g,subnodes)
    
    # Calculating global efficiency of neighborhood
    # depending on (possibly missing) l and t
    if (is.null(l)&&is.null(t)){
      global_efficiency_unpar(subg)
    } else if (is.null(l)&&!is.null(t)){
      subt <- t[subnodes,subnodes]
      global_efficiency_unpar(subg,t=subt)
    } else if (!is.null(l)&&is.null(t)){
      subl <- l[subnodes,subnodes]
      global_efficiency_unpar(subg,l=subl)
    } else {
      subl <- l[subnodes,subnodes]
      subt <- t[subnodes,subnodes]
      global_efficiency_unpar(subg,l=subl,t=subt)
    }# if 
  },g,l,t)# parSapply
}# local_efficiency