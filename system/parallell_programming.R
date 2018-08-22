source("system/initialization.R")

#' @title Starts a cluster of cores
#'
#' @description Convenience function to initialize a cluster with the maximum
#'   number of cores and preload them with the scripts most often used
#'
#' @param n A number of cores to be used (default = detectCores())
#' @param sources A list of source directory strings of R scripts to be called
#'   in the cluster
#'
#' @return A cluster
#'
#' @examples
#' # Initializes a cluster of one core with the extra script of transport
#' cl <- start_cluster(1,sources=c("system/initialization.R","system/functions.R","Models/Transport/transport.R"))
#'
#' @details Always use stop_cluster when finished
#' 
#' @family cluster
start_cluster <- function(n = parallel::detectCores()
                          , sources = c("system/initialization.R"
                                        , "system/functions.R"
                                        , "functions/measures_distance.R")){
  parallel_cluster <- parallel::makeCluster(n)
  doParallel::registerDoParallel(parallel_cluster)
  clusterCall(parallel_cluster, function(sources){
    for (name in sources){
      source(name)
    }# for
  },sources)# clusterCall
  return(parallel_cluster)
}# start_cluster


#' @title Stops a cluster of cores
#'
#' @description Convenience function to shut down a cluster nicely
#'
#' @param cl A cluster to be shut down
#'
#' @return An empty list
#'
#' @examples
#' cl <- stop_cluster(cl)
#' cl
#' # NULL
#'
#' @details Always use stop_cluster after finished with the cluster
#' 
#' @family cluster
stop_cluster <- function(parallel_cluster){
  if(!is.null(parallel_cluster)) {
    stopCluster(parallel_cluster)
    parallel_cluster <- c()
    return(parallel_cluster)
  }# if
}# stop_cluster