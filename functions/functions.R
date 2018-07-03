source("initialization")


#' Harmonic function
#' Input: number (natural number)
#' Output: number (1/1 + ... + 1/n)
harmonic <- function(n){
  sum(1/seq_len(n))
}# harmonic(n)