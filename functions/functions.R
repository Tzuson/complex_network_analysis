source("initialization.R")


#' Harmonic function
#' 
#' @param n A natural number more then zero
#' @return The number 1/1 + ... + 1/n
harmonic <- function(n){
  sum(1/seq_len(n))
}# harmonic(n)