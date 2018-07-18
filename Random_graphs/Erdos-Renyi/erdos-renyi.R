source("system/initialization.R")


#' Generates a random Erdös-Rényi graph with edge probability p
#' 
#' @param n A natural number
#' @param p A probability 
#' @param directed A boolean
#' @param weighted A boolean
#' @result A random Erdös-Rényi graph with n nodes, probability p an edge
#' is in and normalised weight
erdos-renyi <- function(n,p,directed,weighted) {
  # Adjacency matrix, with "1" an edge between two nodes
  adj_matrix <- matrix(sample(0:1, size=n*n, prob=c(1-p,p), replace=TRUE)
                       ,nrow=n,ncol=n)
  
  # No self-loops
  diag(adj_matrix) <- 0
  
  # Weighted and/or directed, or not
  if (weighted){
    l_matrix <- matrix(runif(n*n),nrow=n,ncol=n)
    diag(l_matrix) <- 0
    
    if (directed){
      g <- graph_from_adjacency_matrix(adj_matrix*l_matrix, mode="directed", weighted=TRUE)
    }# if (directed)
    else {
      l_matrix[lower.tri(l_matrix)] <- t(l_matrix)[lower.tri(t(l_matrix))]
      g <- graph_from_adjacency_matrix(adj_matrix*l_matrix, mode="upper", weighted=TRUE)
    }# else (undirected)
  }# if (unweighted)
  else {
    l_matrix <- matrix(1,nrow=n,ncol=n)
    diag(l_matrix) <- 0
    
    if (directed){
      g <- graph_from_adjacency_matrix(adj_matrix, mode="directed")
    }# if (directed)
    else {
      g <- graph_from_adjacency_matrix(adj_matrix, mode="upper")
    }# else (undirected)
  }# else (unweighted)
  
  return(list(g,l_matrix))
}# erdos-renyi






