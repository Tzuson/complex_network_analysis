source("system/initialization.R")
source("system/parallel_programming.R")
source("system/functions.R")

#' @title Plot graph and histogram of NORdirectors
#'
#' @description Plots a graph of nordirectors, supplied with data on the nodes. This
#'   data will be mapped to the size of the nodes, i.e. higher number for a node
#'   means larger node size. The size of the nodes is determined via the formula
#'   \code{data <- 1 + nodes/mean(nodes)} if \code{mean(nodes) > 0}. If this is
#'   not the case, then every data point is zero, so we just take size 1.
#'
#'   Furthermore it plots a histogram of the data on the nodes (not of the size
#'   of the nodes). Both plots are stored in the same location.
#'
#' @param net A nordirectors graph
#' @param nodes A vector with data on the nodes - optional -. This data should
#'   be more or equal then zero
#' @param mapping A vectorized function from \code{nodes} to positive numbers to give the size of
#'   the nodes - optional -. Default is the identity.
#' @param file_path A string giving the map (without file name nor .pdf) -
#'   optional -
#' @param file_name A string giving the file name (without .pdf) - optional -
#' @param name A string giving the name of the network to be used in the title -
#'   optional - of the plots
#'
#' @return A pdf with a plot of NORdirectors, where the size depends on \code{nodes}
#'   and a pdf with a histogram of the data, if the data is not missing.
#'
#' @examples
#' plot_nordirectors(nordirectors_uu,1:vcount(nordirectors_uu),function(x){1+5*x^4/max(x^4)},name="NORdirectors_example")
plot_nordirectors <- function(net, nodes=NULL,mapping=function(x){x}, file_path="Network-Data/NORdirectors/pdf",file_name="nordirectors_uu",name="NORdirectors"){
  # Data is zero when missing
  do_hist <- TRUE
  if (is.null(nodes)){
    nodes <- vector(mode="numeric", length = vcount(net))
    do_hist <- FALSE
  }# if
  
  # Making plot of NORdirectors 
  pdf(file=sprintf('%s/%s_plot.pdf',file_path,file_name))
  par(mar=rep(0,4),oma=rep(0,4))
  plot(net,vertex.label=NA
       , vertex.size= mapping(nodes)
       , vertex.color="blue"
       , edge.color="lightblue"
       , edge.width=1
       , vertex.frame.color="darkblue"
       , edge.arrow.size = 0.5
       , main = sprintf('\n\n %s',name))
  dev.off()
  
  # Plotting and storing histogram of NORdirectors nodes data
  if (do_hist){
    nodes <- as.data.frame(nodes) %>%
      `colnames<-`(c("value"))
    p <- ggplot(nodes,aes(value)) +
      ggtitle(name) + xlab("Vulnerability") + ylab("Frequency") +
      geom_histogram(binwidth = (max(nodes)-min(nodes))/100)
    ggsave(filename=sprintf('%s_hist.pdf',file_name),plot=p,device="pdf",path=file_path)
  }# if
}# plot_nordirectors



