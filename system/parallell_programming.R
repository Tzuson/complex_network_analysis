source("system/initialization.R")

#' Starts a cluster of cores
#' 
#' @param n A number (default = detectCores())
#' @param sources A list of source directory strings
#' @return A cluster
#' @details Always use stop_cluster when finished
start_cluster <- function(n = parallel::detectCores(),sources = c("system/initialization.R")){
  parallel_cluster <- parallel::makeCluster(n)
  doParallel::registerDoParallel(parallel_cluster)
  clusterCall(parallel_cluster, function(sources){
    for (name in sources){
      source(name)
    }# for
  },sources)# clusterCall
  return(parallel_cluster)
}# start_cluster


#' Stops a cluster of cores
#' 
#' @param parallel_cluster A cluster
#' @return An empty cluster
#' @details Always use when finished with start_cluster
stop_cluster <- function(parallel_cluster){
  if(!is.null(parallel_cluster)) {
    parallel::stopCluster(parallel_cluster)
    parallel_cluster <- c()
    return(parallel_cluster)
  }# if
}# stop_cluster