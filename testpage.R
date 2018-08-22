source("system/initialization.R")
source("system/parallell_programming.R")
source("system/functions.R")

source("functions/measures_distance.R")
source("functions/tests_distance.R")

source("Models/Erdos-Renyi/erdos-renyi.R")
source("Network-Data/NLrail/nlrail.R")








adj_matrix <- t(matrix(c(0,1,1,0,
                         1,0,1,0,
                         1,1,0,0,
                         1,0,1,0),
                     nrow=4, ncol=4))
l_matrix <- t(matrix(c(0,1,1,2,
                       1,0,1,1,
                       1,1,0,1,
                       2,1,1,0),
                     nrow=4, ncol=4))
u_matrix <- t(matrix(c(0,10,1,0,
                       10,0,1,0,
                       2,2,0,0,
                       1,0,5,0),
                     nrow=4, ncol=4))
g1 <- graph_from_adjacency_matrix(adj_matrix, mode="directed")
g2 <- graph_from_adjacency_matrix(adj_matrix*l_matrix, mode="directed", weighted=TRUE)
plot(g2)


adj_matrix





L <- initialize_nlrail(FALSE)
nlrail <- L[[1]]
l_nlrail <- L[[2]]


vulnerability <- as.data.frame(vulnerability_nodes(cl,nlrail,performance=global_efficiency_unpar))
plot_nlrail(nlrail,nodes=vul,"Network-Data/NLrail/pdf","NLrail_unweighted","NLrail - Unweighted")

colnames(vulnerability) <- c("vul")

p <- ggplot(vulnerability,aes(vul)) +
  ggtitle("TITLE") + xlab("Vulnerability") + ylab("Frequency") +
  geom_histogram(binwidth = 0.001)
p



1+1













#' Collects counter relative performances per size of subgraphs,
#' calculates the average and sd and plots/write the output
#' 
#' @param cl A cluster
#' @param g A graph
#' @param l A matrix - optional -
#' @param t A matrix - optional -
#' @param counter An integer
#' @param sizes An integer vector
#' @param file_path A string: the file_path+name (without .pdf/.csv) of the files
#' @param netname A string: name of network in header of plot
#' @param performance An function
analyse_sample_performance_random <- function(cl,g,l=NULL,t=NULL,counter,sizes,file_path,netname,performance){
  # Collecting data
  data <- sapply(1:graphs,function(I,cl,g,l,t,counter,sizes,performance){
    sample_performance(cl,g,l,t,counter,sizes,performance)
  },cl,g,l,t,counter,sizes,performance)
  
  # Analysing data
  avg <- rowMeans(data)
  sdev <- apply(data,1, sd, na.rm = TRUE)
  
  # Plotting
  pdf(file = sprintf('%s.pdf',file_path))
  x <- sizes/vcount(g)
  plot(x, avg,ylim=range(c(avg-2*sdev, avg+2*sdev)),
       pch=19, xlab="Fraction of total nodes used", ylab="Performance (normalised, perfect=1, d(mean,bar)=2SD)",
       main=sprintf('%s: counter=%i, steps=%i',netname,counter,length(sizes)))
  # hack: we draw arrows but with very special "arrowheads"
  arrows(x, avg-2*sdev, x, avg+2*sdev, length=0.05, angle=90, code=3)
  dev.off()
  
  # Outputting data in .csv
  data_write <- matrix(c(x,avg,sdev),nrow=length(sizes),ncol=3)
  write.csv(data_write,sprintf('%s.csv',file_path))
}# analyse_sample_performance_random














