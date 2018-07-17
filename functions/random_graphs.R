source("initialization.R")
#source("data/nsnet.R")
#source("functions/functions.R")
source("functions/measures.R")
#source("functions/tests.R")


#' Generates a random Erdös-Rényi graph with edge probability p
#' 
#' @param n A natural number
#' @param p A probability 
#' @param directed A boolean
#' @param weighted A boolean
#' @result A random Erdös-Rényi graph with n nodes, probability p an edge
#' is in and normalised weight
random_graph <- function(n,p,directed,weighted) {
  adj_matrix <- matrix(sample(0:1, size=n*n, prob=c(1-p,p), replace=TRUE)
                       ,nrow=n,ncol=n)
  diag(adj_matrix) <- 0
  
  if (weighted){
    l_matrix <- matrix(runif(n*n),nrow=n,ncol=n)
    diag(l_matrix) <- 0
    
    if (directed){
      g <- graph_from_adjacency_matrix(adj_matrix*l_matrix, mode="directed", weighted=TRUE)
    }# if (directed)
    else {
      l_matrix[lower.tri(l_matrix)] <- t(l_matrix)[lower.tri(t(l_matrix))]
      g <- graph_from_adjacency_matrix(adj_matrix*l_matrix, mode="upper", weighted=TRUE)
    }# else (undirected)
  }# if (unweighted)
  else {
    l_matrix <- matrix(1,nrow=n,ncol=n)
    diag(l_matrix) <- 0
    
    if (directed){
      g <- graph_from_adjacency_matrix(adj_matrix, mode="directed")
    }# if (directed)
    else {
      g <- graph_from_adjacency_matrix(adj_matrix, mode="upper")
    }# else (undirected)
  }# else (unweighted)
  
  return(list(g,l_matrix))
}# random_graph(n,p,directed,weighted)



generate_random <- function(cl, counter,steps,n,p,directed,weighted){
  # Generating #counter random graphs as sample
  data <- parSapply(cl, 1:counter, function(i,steps,n,p,directed,weighted){
    # Generating random graph
    L <- random_graph(n,p,directed,weighted)
    g <- L[[1]]
    
    # Calculating exact data of g
    d_exact <- distances(g,v=V(g),to=V(g),mode="out")
    diag(d_exact) <- Inf
    l_exact <- L[[2]]
    diag(l_exact) <- Inf
    e_exact <- sum(1/d_exact)/sum(1/l_exact)
    
    # Calculating approximated data using different numbers of nodes
    sapply(2:steps, function(j,steps,n,g,d_exact,l_exact,e_exact){
      q <- j*floor(n/steps)
      nodes <- sample.int(n, size = q, replace = FALSE)
      d_approx <- d_exact[nodes,nodes]
      l_approx <- l_exact[nodes,nodes]
      e_approx <- sum(1/d_approx)/sum(1/l_approx)
      return(e_approx/e_exact)
    },steps,n,g,d_exact,l_exact,e_exact)
    
  },steps,n,p,directed,weighted)# parSapply
  return(data)
}# generate_random(counter,steps,n,p,directed,weighted)




generate_graph <- function(cl, counter,steps,g,l_exact){
  # Calculating exact data of g
  n <- vcount(g)
  d_exact <- distances(g,v=V(g),to=V(g),mode="out")
  diag(d_exact) <- Inf
  diag(l_exact) <- Inf
  e_exact <- sum(1/d_exact)/sum(1/l_exact)
  
  # Generating counter random graphs as sample
  data <- parSapply(cl, 1:counter, function(i,steps,n,d_exact,l_exact,e_exact){
    # Calculating approximated data using different numbers of nodes
    sapply(2:steps, function(j,steps,n,d_exact,l_exact,e_exact){
      q <- j*floor(n/steps)
      nodes <- sample.int(n, size = q, replace = FALSE)
      d_approx <- d_exact[nodes,nodes]
      l_approx <- l_exact[nodes,nodes]
      e_approx <- sum(1/d_approx)/sum(1/l_approx)
      return(e_approx/e_exact)
    },steps,n,d_exact,l_exact,e_exact)# sapply
  },steps,n,d_exact,l_exact,e_exact)# parSapply
  return(data)
}# generate_graph(cl, counter,steps,g,l_exact)






analyse_random <- function(cl, counter,steps,n,p,directed,weighted){
  data <- generate_random(cl, counter,steps,n,p,directed,weighted)
  x <- sapply(2:steps,function(j,steps,n){j*floor(n/steps)/n},steps,n)
  avg <- rowMeans(data)
  sdev <- apply(data,1, sd, na.rm = TRUE)
  
  # pdf title creation
  if (directed && weighted){
    file_name <- sprintf('pdf/Erdos-Renyi_n=%i_p=%g_directed_weighted.pdf',n,p)
  } else if (directed && !weighted) {
    file_name <- sprintf('pdf/Erdos-Renyi_n=%i_p=%g_directed.pdf',n,p)
  } else if (!directed && weighted){
    file_name <- sprintf('pdf/Erdos-Renyi_n=%i_p=%g_weighted.pdf',n,p)
  } else {
    file_name <- sprintf('pdf/Erdos-Renyi_n=%i_p=%g.pdf',n,p)
  }# else
  pdf(file = file_name)
  
  # Plotting
  plot(x, avg,ylim=range(c(avg-sdev, avg+sdev)),
       pch=19, xlab="Fraction of total nodes used", ylab="Performance (normalised, 1 = perfect)",
       main=sprintf('Erdos-Renyi: counter=%i, steps=%i, n=%i, p=%g, directed=%d, weighted=%d'
                    ,counter,steps,n,p,directed,weighted))
  # hack: we draw arrows but with very special "arrowheads"
  arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
  dev.off()
}# analyse_random(cl, counter,steps,n,p,directed,weighted)





analyse_graph <- function(cl, counter,steps,g,l,name){
  n <- vcount(g)
  data <- generate_graph(cl, counter,steps,g,l)
  x <- sapply(2:steps,function(j,steps,n){j*floor(n/steps)/n},steps,n)
  avg <- rowMeans(data)
  sdev <- apply(data,1, sd, na.rm = TRUE)

  # pdf title creation
  file_name <- sprintf('pdf/%s_counter=%i_steps=%i.pdf',name,counter,steps)
  pdf(file = file_name)
  
  # Plotting
  plot(x, avg,ylim=range(c(avg-sdev, avg+sdev)),
       pch=19, xlab="Fraction of total nodes used", ylab="Performance (normalised, 1 = perfect)",
       main=sprintf('%s: counter=%i, steps=%i',name,counter,steps))
  # hack: we draw arrows but with very special "arrowheads"
  arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
  dev.off()
}# analyse_graph(cl, counter,steps,g,l,name)


