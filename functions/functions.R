source("system/initialization.R")
source("system/parallell_programming.R")

#' Harmonic function
#' 
#' @param n A natural number more then zero
#' @return The number 1/1 + ... + 1/n
harmonic <- function(n){
  sum(1/seq_len(n))
}# harmonic


#' Shannon entropy
#' 
#' @param p A list of probabilities
#' @return The Shannon entropy
shannon_entropy <- function(p){
  return(-sum(n*log2(p)))
}# shannon_entropy