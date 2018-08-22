source("system/initialization.R")
source("system/parallell_programming.R")




#' 
#'
#'
#'
#'
data_erdos_renyi <- function(cl,n,directed,counter,x,performance){
  data <- parSapply(cl,x,function(p,n,directed,counter,performance){
    data_col <- sapply(1:counter,function(i,n,p,directed,performance){
      g <- erdos.renyi.game(n,p,FALSE)
      return(performance(g))
    },n,p,directed,performance)# sapply
    avg <- mean(data_col)
    sdev <- sd(data_col)
    return(c(p,avg,sdev))
  },n,directed,counter,performance)# parSapply
  data <- t(data)
  return(data)
}# data_erdos_renyi


analyse_erdos_renyi <- function(cl,n,directed,counter,start,stop,step,performance,name){
  x <- seq(from=start,to=stop,by=step)
  data <- data_erdos_renyi(cl,n,directed,counter,x,performance)
  write.csv(data,file=sprintf('%s.csv',name))
  
  pdf(file = sprintf('%s.pdf',name))
  
  # Plotting
  avg <- data[,2]
  sdev <- data[,3]
  
  plot(x, avg,ylim=range(c(avg-sdev, avg+sdev)),
       pch=19, xlab=sprintf('p (start=%g, stop=%g, step=%g)', start,stop,step),
       ylab="Mean global efficiency",
       main=sprintf('Erdos-Renyi: Mean global efficiency as function of edge probability \n (n=%i, counter=%i)',n,counter))
  # hack: we draw arrows but with very special "arrowheads"
  arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
  dev.off()
}# analyse_erdos_renyi


