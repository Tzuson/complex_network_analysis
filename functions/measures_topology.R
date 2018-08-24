source("system/initialization.R")
source("system/parallel_programming.R")
source("system/functions.R")

setwd("functions/OACC");source("scripts/BDM2D.R");setwd("../../")


#' @title Topological information content
#' 
#' @description The 2log of the number of automorphisms of the graph
#' 
#' @param g An igraph graph
#' 
#' @return The topological information content 
#' 
#' @examples 
#' g <- graph_from_literal(1--2--3)
#' information_content(g)
#' # 1
#' g <- graph_from_literal(1--2-+3)
#' information_content(g)
#' # 0
information_content <- function(g){
  log2(as.numeric(automorphisms(g)$group_size))
}# information_content


#' @title The (normed) Kolmogorov complexity
#'
#' @description Calculates an upper bound of the Kolmogorov complexity of the
#'   adjacency matrix of a graph using the algorithm of (see details for
#'   citations), then normalizes it by the given normalization constant. The
#'   intended constant to be used is that of kolmogorov_norm.
#'
#' @param g An igraph graph
#' @param c_kolm A number to normalize the Kolmororov complexity
#' 
#' @return The (normalized) Kolmogorov complexity
#' 
#' @examples 
#' g <- graph_from_literal(1--2--3--4--1)
#' kolmogorov(g,1)
#' # 30.26577
#' kolmogorov(g,kolmogorov_norm(cl,4,4,FALSE,1000)[1])
#' # 1.044983 (for example)
#'
#' @details Bibliography Soler-Toscano F., Zenil H., Delahaye J.-P. and Gauvrit
#' N. (2014) Calculating Kolmogorov Complexity from the Output Frequency
#' Distributions of Small Turing Machines. PLoS ONE 9(5): e96223.
#' Gauvrit, N., Singmann H., Soler-Toscano F. and Zenil H.(2015) Algorithmic
#' complexity for psychology: A user-friendly implementation of the coding
#' theorem method, Behavior Research Methods, vol. 48, pp. 1–16.
#' Zenil H., Soler-Toscano F., Kiani N.A., Hernández-Orozco S., Rueda-Toicen A.
#' (2016) A Decomposition Method for Global Evaluation of Shannon Entropy and
#' Local Estimations of Algorithmic Complexity, arXiv:1609.00110.
#' 
#' @family kolmogorov, kolmogorov_norm
kolmogorov <- function(g,c_kolm){
  M <- as_adjacency_matrix(g,type="both",sparse=FALSE)
  return(bdm2D(M,4,4)/c_kolm)
}# kolmogorov


#' @title Kolmogorov complexity of random graphs
#'
#' @description Calculates the mean and standard deviation of the Kolmogorov
#'   complexity of \code(counter) random graphs (possible \code(directed)) each
#'   with \code(n) nodes and \code(m) edges, using the algorith developed by
#'   (see details for citation). Intended to be used with \code(kolmogorov) as
#'   norming constant \code(c_kolm). Because of the possible reuse of this
#'   constant \code(kolmogorov_norm) is separated from \code(kolmogorov).
#'
#' @param cl A cluster of computer kernels to be used for the calculation
#' @param n An integer giving the number of nodes of the random graphs (should
#'   be at least 4)
#' @param m An integer giving the number of edges of the random graphs (should
#'   be in 0:n*(n-1))
#' @param directed A boolean
#' @param counter An integer giving the number of graphs generated to calculate
#'   the constant
#'
#' @return A vector with [1] the mean Kolmogorov complexity of the generated
#'   graphs and [2] the standard deviation of these complexities
#'
#' @examples
#' kolmogorov_norm(cl,10,12,TRUE,1000)
#' # [1] 98.844902   7.287354
#' kolmogorov_norm(cl,4,0,TRUE,1000)
#' # [1] 22.00671   0.00000
#' kolmogorov_norm(cl,4,6,TRUE,1000)
#' # [1] 30.025844   1.824306
#' kolmogorov_norm(cl,4,12,TRUE,1000)
#' # [1] 23.23858   0.00000
#'
#' @details Bibliography Soler-Toscano F., Zenil H., Delahaye J.-P. and Gauvrit
#'   N. (2014) Calculating Kolmogorov Complexity from the Output Frequency
#'   Distributions of Small Turing Machines. PLoS ONE 9(5): e96223. Gauvrit, N.,
#'   Singmann H., Soler-Toscano F. and Zenil H.(2015) Algorithmic complexity for
#'   psychology: A user-friendly implementation of the coding theorem method,
#'   Behavior Research Methods, vol. 48, pp. 1–16. Zenil H., Soler-Toscano F.,
#'   Kiani N.A., Hernández-Orozco S., Rueda-Toicen A. (2016) A Decomposition
#'   Method for Global Evaluation of Shannon Entropy and Local Estimations of
#'   Algorithmic Complexity, arXiv:1609.00110.
#'   
#'   @family komogorov_norm, kolmogorov
kolmogorov_norm <- function(cl,n,m,directed,counter){
  dat <- parSapply(cl,1:counter,function(i,n,m,directed){
    mat <- as_adjacency_matrix(sample_gnm(n,m,directed=directed),type="both",sparse=FALSE)
    return(bdm2D(mat,4,4))
  },n,m,directed)
  return(c(mean(dat),sd(dat)))
}# kolmogorov_norm
