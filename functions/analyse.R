source("system/initialization.R")
source("system/parallel_programming.R")
source("system/functions.R")


#' @title Calculates the performance of random subgraphs
#'
#' @description 
#' Calculates the performance of random subgraphs for different number of
#' nodes in the subgraph. If \code{use_cluster == FALSE}, then for every
#' \code{size} in \code{sizes} \code{size} different nodes are randomly
#' selected and the subgraph is made from these nodes and the edges between
#' these nodes.
#'
#' If \code{use_cluster == TRUE} a node is randomly selected and its
#' neighbourhood, of order \code{sizes} is used as sample. The starting node
#' itself is in the neighbourhood if \code{use_ego == TRUE}, otherwise it is
#' omitted.
#'
#' Nodes adjacent to the subgraph, but not part of the subgraph itself are
#' deleted, as well as the edges incidident in both the subgraph and its
#' complement.
#'
#' @param cl A cluster of computer kernels to be used for the calculation
#' @param g An igraph  graph
#' @param performance A function of \code{g},\code{l} and \code{t}
#' @param counter An integer giving the number of samples/neighbourhoods to be
#'   seen
#' @param sizes An integer vector giving the different sizes for which samples
#'   should be taken. If \code{use_cluster == TRUE}, then \code{sizes} gives the
#'   order of the neighbourhoods, if \code{use_cluster == FALSE}, \code{sizes}
#'   should give the number of nodes used in every sample.
#' @param use_cluster A boolean - optional -. If \code{TRUE}, a node is randomly
#'   chosen and its neighbourhood (with order \code{sizes}) becomes the sample.
#'   If \code{FALSE}, a combination of \code{sizes} nodes is randomly choses as
#'   sample.
#' @param use_ego A boolean - optional -. Only used when \code{use_cluster ==
#'   TRUE}, else ignored. When \code{use_ego == TRUE}, the ego node itself is
#'   part of the neighbourhood. If \code{use_ego == FALSE}, the ego node is
#'   ommitted from the neighbourhood.
#' @param l A length matrix  (optional). A length matrix contains a physical
#'   distance between all nodes, whether connected through the network or not.
#' @param t A transport matrix (optional). A transport matrix contains for each
#'   pair of nodes \eqn{i}, \eqn{j}, the amount transported from \eqn{i} to
#'   \eqn{j}.
#'
#' @return A matrix of performances of random subgraphs, where the rows give the
#'   sizes
#'   
#' @examples 
#' g1 <- graph_from_literal(1--2--3--4--5--6--7--8--9--10)
#' sample_performance(cl,g1,function(g,l,t){ecount(g)},10,seq.int(1:3))
#' #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#' # [1,]   0    0    0    0    0    0    0    0    0     0
#' # [2,]   0    1    0    0    0    0    0    0    0     1
#' # [3,]   1    2    0    1    0    0    0    0    0     0
#' 
#' sample_performance(cl,g1,function(g,l,t){ecount(g)},10,seq.int(1:3),TRUE,FALSE)
#' #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#' # [1,]   0    0    0    0    0    0    0    0    0     0
#' # [2,]   1    1    2    2    2    2    2    1    2     1
#' # [3,]   3    2    2    2    2    3    4    3    2     4
#' 
#' sample_performance(cl,g1,function(g,l,t){ecount(g)},10,seq.int(1:3),TRUE,TRUE)
#' #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#' # [1,]   2    2    2    2    2    2    2    2    2     2
#' # [2,]   4    3    4    4    4    2    3    2    4     4
#' # [3,]   4    5    3    5    5    6    3    3    4     6
#' 
#' @family performance
sample_performance <- function(cl,g,performance,counter,sizes,use_cluster=FALSE,use_ego=FALSE,l=NULL,t=NULL){
  # Calculating exact data of g
  n <- vcount(g)
  
  # Calculating matrix with for every size (along rows) counter (along columns) measurements
  data <- parSapply(cl,1:counter,function(count,g,l,t,sizes,use_cluster,use_ego,performance,n){
    data_col <- sapply(sizes,function(size,g,l,t,use_cluster,use_ego,performance,n){
      if (use_cluster){
        if (use_ego){
          subnodes <- as.vector(ego(g,order=size,nodes=c(sample.int(n,size=1)),mode="all")[[1]])
        } else {
          node <- sample.int(n,size=1)
          subnodes <- as.vector(ego(g,order=size,nodes=c(node),mode="all")[[1]])
          subnodes <- subnodes[-which(subnodes==node)]
        }# if
        
      } else {
        subnodes <- sample.int(n,size=size)
      }# if
      
      subg <- induced_subgraph(g,subnodes)
      subl <- l[subnodes,subnodes]
      subt <- t[subnodes,subnodes]
      
      per_value <- performance(g=subg,l=subl,t=subt)
    },g,l,t,use_cluster,use_ego,performance,n)# sapply 
  },g,l,t,sizes,use_cluster,use_ego,performance,n)# parSapply
  
  return(data)
}# sample_performance


#' @title Wrapper and plotter for \code{sample_performance}
#'
#' @description Takes the matrix from \code{sample_performance} and turns it
#' into a plot. The x-axis gives the size of the sample, the y-axis the
#' performance. For every size a boxplot using ggplot2 is drawn given the
#' median, Q1, Q3 and outlying performances (outlying defined as lying further
#' that 1.5*distance(Q1,Q3) away from the median, in both directions). The data
#' is also stored in a .csv file in the same place with the same name.
#'
#' @param cl A cluster of computer kernels to be used for the calculation
#' @param g An igraph  graph
#' @param performance A function of \code{g},\code{l} and \code{t}
#' @param counter An integer giving the number of samples/neighbourhoods to be
#'   seen
#' @param sizes An integer vector giving the different sizes for which samples
#'   should be taken. If \code{use_cluster == TRUE}, then \code{sizes} gives the
#'   order of the neighbourhoods, if \code{use_cluster == FALSE}, \code{sizes}
#'   should give the number of nodes used in every sample.
#' @param file_path A string giving the path of the file the plot and data file
#'   should be put into, this string thus includes the file name, but excludes
#'   the suffix .pdf or .csv
#' @param netname A string giving the name of the network to be used in the
#'   title of the plot
#' @param p_name A string giving the name of the performance to be used in the
#'   plot
#' @param use_cluster A boolean - optional -. If \code{TRUE}, a node is randomly
#'   chosen and its neighbourhood (with order \code{sizes}) becomes the sample.
#'   If \code{FALSE}, a combination of \code{sizes} nodes is randomly choses as
#'   sample.
#' @param use_ego A boolean - optional -. Only used when \code{use_cluster ==
#'   TRUE}, else ignored. When \code{use_ego == TRUE}, the ego node itself is
#'   part of the neighbourhood. If \code{use_ego == FALSE}, the ego node is
#'   ommitted from the neighbourhood.
#' @param l A length matrix - optional -
#' @param t A transport matrix - optional -
analyse_sample_performance <- function(cl,g,performance,counter,sizes,file_path,netname,p_name
                                       ,use_cluster=FALSE,use_ego=FALSE,l=NULL,t=NULL){
  # Collecting data 
  data <- sample_performance(cl=cl,g=g,l=l,t=t
                             , counter=counter
                             , sizes=sizes
                             , use_cluster=use_cluster
                             , use_ego=use_ego
                             , performance=performance) %>%
    t()
  
  # ggplot2 uses the format of 'tidy data', thus we need to mutate our matrix to
  # a 2-column data frame. The first column will be the size, the second column
  # the performance
  data_plot <- matrix(0,nrow=counter*length(sizes),ncol=2)
  for (i in 1:length(sizes)){
    data_plot[(counter*(i-1)+1):(counter*i),1] <- sizes[i]
    data_plot[(counter*(i-1)+1):(counter*i),2] <- data[,i]
  }# for
  data_plot <- as.data.frame(data_plot)
  colnames(data_plot) <- c("size","performance")
  
  # The labels differ depending on the use of use_cluster and use_ego
  if (use_cluster){
    xlabel <- sprintf('Radius of neighboorhood (in number of edges, mean distance = %f)',mean_distance(g))
    if (use_ego){
      mainlabel <- sprintf('%s:\n counter=%i, %s of neighbourhood',netname,counter,p_name)
    } else {
      mainlabel <- sprintf('%s:\n counter=%i, %s of neighbours',netname,counter,p_name)
    }# if
  } else {
    xlabel <- sprintf('Number of nodes used (Total number of nodes = %i)',vcount(g))
    mainlabel <- sprintf('%s:\n counter=%i, %s of random subgraph',netname,counter,p_name)
  }# if
  ylabel <- sprintf('Absolute %s (Total = %f)', p_name,performance(g,l,t))
  
  # Plotting
  plot <- ggplot(data=data_plot) +
    geom_boxplot(aes(x=size,y=performance,group=size)) +
    labs(title = mainlabel, x = xlabel, y = ylabel)
  ggsave(sprintf('%s.pdf',file_path),plot=plot,device=pdf)
  
  # Outputting data in .csv
  colnames(data) <- sizes
  write.csv(data,sprintf('%s.csv',file_path))
}# analyse_sample_performance



