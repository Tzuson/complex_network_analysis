source("system/initialization.R")
source("system/functions.R")

#' Plot graph of NORdirectors
#' 
#' @param net A graph
#' @param nodes Data on nodes - optional -
#' @param file_path A string (without .pdf)
#' @param file_name A string (without .pdf)
#' @param name A string
#' @return A pdf with a plot of NORdirectors
plot_nordirectors <- function(net, nodes=NULL, file_path,file_name,name){
  # Data is zero when missing
  if (is.null(nodes)){
    nodes <- vector(mode="numeric", length = vcount(net))
  }# if
  
  # Making plot of NORdirectors 
  pdf(file=sprintf('%s/%s_plot.pdf',file_path,file_name))
  par(mar=rep(0,4),oma=rep(0,4))
  plot(net,vertex.label=NA
       , vertex.size=1 + nodes
       , vertex.color="blue"
       , edge.color="lightblue"
       , edge.width=1
       , vertex.frame.color="darkblue"
       , edge.arrow.size = 0.5
       , main = sprintf('\n\n %s',name))
  dev.off()
  
  # Plotting and storing histogram of NORdirectors nodes data
  nodes <- as.data.frame(nodes) %>%
    `colnames<-`(c("value"))
  p <- ggplot(nodes,aes(value)) +
    ggtitle(name) + xlab("Vulnerability") + ylab("Frequency") +
    geom_histogram(binwidth = 0.005)
  ggsave(filename=sprintf('%s_hist.pdf',file_name),plot=p,device="pdf",path=file_path)
}# plot_nordirectors



