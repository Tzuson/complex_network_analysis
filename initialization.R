# Used packages
suppressPackageStartupMessages({
  library(igraph)
  library(tidyr)
  library(dplyr)
  library(tidyverse)
  library(magrittr)
  library(stringdist)
  library(rvest)
  library(xml2)
  library(Rcpp)
  library(parallel)
  library(doParallel)
  library(foreach)
})


#' Starts a cluster of cores
#' 
#' @param n A number (default = detectCores())
#' @return A cluster
#' @details Always use stop_cluster when finished
start_cluster <- function(n = parallel::detectCores()){
  parallelCluster <- parallel::makeCluster(n)
  doParallel::registerDoParallel(parallelCluster)
  clusterCall(parallelCluster, function(){
    source("initialization.R")
    source("functions/random_graphs.R")
    })# clusterCall
  return(parallelCluster)
}# start_cluster()



#' Stops a cluster of cores
#' 
#' @param parallelCluster A cluster
#' @return An empty cluster
#' @details Always use when finished with start_cluster
stop_cluster <- function(parallelCluster){
  if(!is.null(parallelCluster)) {
    parallel::stopCluster(parallelCluster)
    parallelCluster <- c()
    return(parallelCluster)
  }# if
}# stop_cluster(parallelCluster)



