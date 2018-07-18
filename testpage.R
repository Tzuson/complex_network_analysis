source("system/initialization.R")
source("system/parallell_programming.R")

source("functions/functions.R")
source("functions/measures.R")
source("functions/tests.R")

source("NSnet/nsnet.R")
source("Random_graphs/Preferential_attachment/preferential_attachment.R")




# adj_matrix <- t(matrix(c(0,1,1,0,
#                          1,0,1,0,
#                          1,1,0,0,
#                          1,0,1,0),
#                      nrow=4, ncol=4))
# l_matrix <- t(matrix(c(0,1,2,1,
#                        1,0,1,1,
#                        1,1,0,1,
#                        1,1,1,0),
#                      nrow=4, ncol=4))
# g <- graph_from_adjacency_matrix(adj_matrix*l_matrix, mode="directed", weighted=TRUE)
# plot(g)

# adj_matrix <- t(matrix(c(0,1,1,
#                          1,0,1,
#                          1,1,0),
#                      nrow=3, ncol=3))
# l_matrix <- t(matrix(c(0,1,2,
#                        1,0,1,
#                        1,1,0),
#                      nrow=3, ncol=3))
# g <- graph_from_adjacency_matrix(adj_matrix*l_matrix, mode="directed", weighted=TRUE)
# plot(g)


sample_global_efficiency <- function(cl, counter,steps,g,l_exact){
  # Calculating exact data of g
  n <- vcount(g)
  d_exact <- distances(g,v=V(g),to=V(g),mode="out")
  diag(d_exact) <- Inf
  
  
  if (missing(l_exact)){
    e_exact <- sum(1/d_exact)/(n*(n-1))
    
    # Generating counter random graphs as sample
    data <- parSapply(cl, 1:counter, function(i,steps,n,d_exact,e_exact){
      # Calculating approximated data using different numbers of nodes
      sapply(2:steps, function(j,steps,n,d_exact,e_exact){
        q <- j*floor(n/steps)
        nodes <- sample.int(n, size = q, replace = FALSE)
        d_approx <- d_exact[nodes,nodes]
        e_approx <- sum(1/d_approx)/(q*(q-1))
        return(e_approx/e_exact)
      },steps,n,d_exact,e_exact)# sapply
    },steps,n,d_exact,e_exact)# parSapply
  }# if
  else {
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
  }
  return(data)
}# sample_global_efficiency


analyse_sample_global_efficiency <- function(cl, counter,steps,g,name,l){
  n <- vcount(g)
  data <- sample_global_efficiency(cl, counter,steps,g)
  x <- sapply(2:steps,function(j,steps,n){j*floor(n/steps)/n},steps,n)
  avg <- rowMeans(data)
  sdev <- apply(data,1, sd, na.rm = TRUE)
  
  # pdf title creation
  file_name <- sprintf('%s_counter=%i_steps=%i.pdf',name,counter,steps)
  pdf(file = file_name)
  
  # Plotting
  plot(x, avg,ylim=range(c(avg-sdev, avg+sdev)),
       pch=19, xlab="Fraction of total nodes used", ylab="Performance (normalised, 1 = perfect)",
       main=sprintf('Preferential_Attachment: nodes=1000, counter=%i, steps=%i',counter,steps))
  # hack: we draw arrows but with very special "arrowheads"
  arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
  dev.off()
}# analyse_sample_global_efficiency

cl <- start_cluster()
pref_att <- preferential_attachment(1000,1)
analyse_sample_global_efficiency(cl,100,25,pref_att,
                                 name ="Random_graphs/Preferential_attachment/pdf/pref_att_test")











cl <- start_cluster(sources=c("system/initialization.R","functions/measures.R"))





print(c(neighbors(g,2)))

cl <- stop_cluster(cl)

L <- initialize_nsnet(TRUE)
nsnet <- L[[1]]
l_nsnet <- L[[2]]

nsnet <- initialize_nsnet(FALSE)

data <- sapply(1:25,function(distance,nsnet){
  local_efficiency_generalised(cl,nsnet,distance)
},nsnet)
data <- t(data)
print(rowMeans(data))


data <- sapply(1:40,function(distance,nsnet){
  local_efficiency_generalised(cl,nsnet,distance)
},nsnet)
data <- t(data)




x <- 1:25
avg <- rowMeans(data)
sdev <- apply(data,1, sd, na.rm = TRUE)

# Plotting
plot(x, avg,ylim=range(c(avg-2*sdev, avg+2*sdev)),
     pch=19, xlab="Number of stations", ylab="Efficiency of neighbourhood",
     main="Mean efficiency of neighbourhood as function of diameter")
# hack: we draw arrows but with very special "arrowheads"
arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
arrows(x, avg-2*sdev, x, avg+2*sdev, length=0.05, angle=90, code=3)


global_efficiency(cl,nsnet,l_nsnet)


global_efficiency(cl,nsnet)









cl <- start_cluster()
for (p_counter in 1:1){
  for (directed in c(TRUE,FALSE)){
    for (weighted in c(TRUE,FALSE)){
      n <- 1000
      p <- p_counter/(2*n)
      name <- "Erdos-Renyi"
      
      graphs <- 100
      counter <- 100
      steps <- 20
      
      avg <- matrix(0,nrow=(steps-1),ncol=graphs)
      sdev <- matrix(0,nrow=(steps-1),ncol=graphs)
      for(i in 1:graphs){
        L <- random_graph(n,p,directed,weighted)
        g <- L[[1]]
        l <- L[[2]]
        data_graph <- generate_graph(cl,counter,steps,g,l)
        
        x <- sapply(2:steps,function(j,steps,n){j*floor(n/steps)/n},steps,n)
        
        avg[,i] <- rowMeans(data_graph)
        sdev[,i] <- apply(data_graph,1, sd, na.rm = TRUE)
      }# for
      
      avgavg <- rowMeans(avg)
      avgsdev <- apply(sdev,1, sd, na.rm = TRUE)
      sdevavg <- rowMeans(sdev)
      sdevsdev <- apply(sdev,1, sd, na.rm = TRUE)
      
      # pdf title creation
      if (directed && weighted){
        file_name_avg <- sprintf('Erdos-Renyi/Erdos-Renyi_average_n=%i_p=%g_directed_weighted.pdf',n,p)
        file_name_sdev <- sprintf('Erdos-Renyi/Erdos-Renyi_SD_n=%i_p=%g_directed_weighted.pdf',n,p)
      } else if (directed && !weighted) {
        file_name_avg <- sprintf('Erdos-Renyi/Erdos-Renyi_average_n=%i_p=%g_directed.pdf',n,p)
        file_name_sdev <- sprintf('Erdos-Renyi/Erdos-Renyi_SD_n=%i_p=%g_directed.pdf',n,p)
      } else if (!directed && weighted){
        file_name_avg <- sprintf('Erdos-Renyi/Erdos-Renyi_average_n=%i_p=%g_weighted.pdf',n,p)
        file_name_sdev <- sprintf('Erdos-Renyi/Erdos-Renyi_SD_n=%i_p=%g_weighted.pdf',n,p)
      } else {
        file_name_avg <- sprintf('Erdos-Renyi/Erdos-Renyi_average_n=%i_p=%g.pdf',n,p)
        file_name_sdev <- sprintf('Erdos-Renyi/Erdos-Renyi_SD_n=%i_p=%g.pdf',n,p)
      }# else
      
      # Plotting averages
      pdf(file = file_name_avg)
      plot(x, avgavg,ylim=range(c(avgavg-avgsdev, avgavg+avgsdev)),
           pch=19, xlab="Fraction of total nodes used", ylab="Performance (normalised, 1 = perfect)",
           main=sprintf('Erdos-Renyi: counter=%i, steps=%i, n=%i, p=%g, directed=%d, weighted=%d'
                        ,counter,steps,n,p,directed,weighted))
      # hack: we draw arrows but with very special "arrowheads"
      arrows(x, avgavg-avgsdev, x, avgavg+avgsdev, length=0.05, angle=90, code=3)
      dev.off()
      
      # Plotting standard deviations
      pdf(file = file_name_sdev)
      plot(x, sdevavg,ylim=range(c(sdevavg-sdevsdev, sdevavg+sdevsdev)),
           pch=19, xlab="Fraction of total nodes used", ylab="Standard Deviation",
           main=sprintf('Erdos-Renyi: counter=%i, steps=%i, n=%i, p=%g, directed=%d, weighted=%d'
                        ,counter,steps,n,p,directed,weighted))
      # hack: we draw arrows but with very special "arrowheads"
      arrows(x, sdevavg-sdevsdev, x, sdevavg+sdevsdev, length=0.05, angle=90, code=3)
      dev.off()
      
    }# for(weighted)
  }# for(directed)
}# for(p_counter 1:4)
cl <- stop_cluster(cl)





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

