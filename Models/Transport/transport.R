source("system/initialization.R")
source("system/parallell_programming.R")


#' Generates a random matrix, where a sender has a larger 
#' probability to send more and a large receiver has
#' a larger probability to receive more
#' 
#' @param n A natural number
#' @param m A natural number
#' @param p A number
#' @result A random n*n matrix with zero diagonal
generate_preferential_transport_matrix <- function(n,m,p){
  # Start probabilities are equal for every node
  transport <- matrix(p,nrow=n,ncol=n)
  for (job in 1:m){
    i <- sample(x=1:n,size=1,prob=rowSums(transport))
    j <- sample(x=1:n,size=1,prob=colSums(transport))
    transport[i,j] <- transport[i,j] + 1
  }# for
  transport <- transport - p
  diag(transport) <- 0
  return(transport)
}# generate_preferential_transport_matrix


#' Generates a random matrix where a node with high out-degree
#' has more transport out and a node with high in-degree
#' has more transport in
#' 
#' @param g A graph
#' @param m A natural number
#' @param p A positive number
generate_preferential_destination_matrix <- function(g,m,p){
  n <- vcount(g)
  
  # Start probabilities are normalised node degrees
  p_out <- degree(g,v=V(g),mode="out",normalized=TRUE)
  p_in <- degree(g,v=V(g),mode="in",normalized=TRUE)
  
  transport <- matrix(0,nrow=n,ncol=n)
  for (job in 1:m){
    i <- sample(x=1:n,size=1,prob=(rowSums(transport)+p*p_out))
    j <- sample(x=1:n,size=1,prob=(colSums(transport)+p*p_in))
    transport[i,j] <- transport[i,j] + 1
  }# for
  diag(transport) <- 0
  return(transport)
}# generate_preferential_destination_matrix




