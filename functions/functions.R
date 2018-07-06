source("initialization.R")


#' Harmonic function
#' 
#' @param n A natural number more then zero
#' @return The number 1/1 + ... + 1/n
harmonic <- function(n){
  sum(1/seq_len(n))
}# harmonic(n)


#' Shannon entropy
#' 
#' @param n A list of probabilities
#' @return The Shannon entropy
shannon_entropy <- function(n){
  return(-sum(n*log2(n)))
}# shannon_entropy(n)