source("system/initialization.R")
source("system/parallell_programming.R")


#' Creates a random graph with the preferential attachment model
#' 
#' @param n A number
#' @param delta A number
#' @result A graph
generate_preferential_attachment <- function(n,delta){
  g <- graph_from_literal(1)
  p <- c(1)
  for (new in 2:n){
    node <- sample(x=1:(new-1),size=1,prob=p)
    g <- add_vertices(g,1)
    g <- add_edges(g,c(new,node))
    
    p[node] <- p[node]+delta
    p <- c(p,1)
  }# for
  return(g)
}# generate_preferential_attachment
