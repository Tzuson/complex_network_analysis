source("initialization.R")
source("data/nsnet.R")
source("functions/functions.R")
source("functions/measures.R")
source("functions/testsPar.R")
source("functions/random_graphs.R")
# Rcpp::sourceCpp("functions/testsincpp.cpp")
# c <- sqrt(2)
# 
# 
# adj_matrix <- t(matrix(c(0,1,1,0,
#                          1,0,1,0,
#                          1,1,0,0,
#                          1,0,1,0),
#                      nrow=4, ncol=4))
# l_matrix <- t(matrix(c(0,1,1,1,
#                        1,0,1,1,
#                        1,1,0,1,
#                        10,1,1,0),
#                      nrow=4, ncol=4))
# g <- graph_from_adjacency_matrix(adj_matrix, mode="undirected")
# plot(g)






cl <- start_cluster()
nodes_nsnet_weighted <- vulnerability_nodesPar(cl,nsnet_weighted,l_nsnet_weighted,global_efficiency)
edges_nsnet_weighted <- vulnerability_edgesPar(cl,nsnet_weighted,l_nsnet_weighted,global_efficiency)
nodes_nsnet <- vulnerability_nodesPar(cl,nsnet,l_nsnet,global_efficiency)
edges_nsnet <- vulnerability_edgesPar(cl,nsnet,l_nsnet,global_efficiency)
global_efficiencyPar(nsnet_weighted,l_nsnet_weighted)
global_efficiencyPar(nsnet,l_nsnet)
nsnet_weighted_plot(20*nodes_nsnet_weighted,20*edges_nsnet_weighted)
nsnet_plot(20*nodes_nsnet,20*edges_nsnet)
cl <- stop_cluster(cl)

print(neighborhood(nsnet,nodes=394))
print(neighborhood(nsnet,nodes="Waddinxveen"))
print(neighborhood(nsnet,nodes="Waddinxveen Noord"))






# 
# 
# n <- vcount(g)
# paths_list <- vector(length=n, mode="list")
# paths_list <- lapply(1:n, function(i,g){
#   paths_list[[i]] <- all_shortest_paths(g,from=i,to=V(g), mode="out")$res
# },g)
# 
# print(paths_list)
# 
# paths_matrix <- lapply(1:n, function(i,paths_list){
#   lapply(1:n, function(j,i,paths_list){
#     l <- paths_list[i]
#     for (path in paths_list[[i]]){
#       if (!(j == path[length(path)])){
#         l[[-path]]
#       }# if
#     }# for
#   },i,pathslist)# function(j,paths_list[[i]])
# },paths_list)# function(i,paths_list)
# 
# 
# print(paths_list[[4]])
# l <- paths_list[[4]]
# for (path in seq.int(1,length(paths_list[[4]]),1)){
#   print(paths_list[[4]][[path]][length(paths_list[[4]][[path]])])
#   if (!(2 == paths_list[[4]][[path]][length(paths_list[[4]][[path]])])){
#     l[[path]] <- NULL
#   }# if
# }# for
# paths_list[[4]] <- l[-(which(lapply(l,is.null),arr.ind=TRUE))]
# print(paths_list[[4]])
# 
# print(paths_list[[4]])
# paths_list[[4]] <- lapply(paths_list[[4]], function(path){
#   print(path)
#   if (1 == path[length(path)]){
#     return(FALSE)
#   }# if
#   return(TRUE)
# })
# 
# 
# 
# 
# 
# n <- vcount(g)
# paths_list <- vector(length=n, mode="list")
# paths_list <- lapply(1:n, function(i,g){
#   paths_list[[i]] <- all_shortest_paths(g,from=i,to=V(g), mode="out")$res
# },g)
# 
# 
# print(paths_list[[4]])
# j <- 1
# paths_matrix[[4]][[j]] <- paths_list[[4]][-(which(sapply(paths_list[[4]],function(path,j){
#   if (j == path[length(path)]){
#     return(FALSE)
#   }# if
#   else {
#     return(TRUE)
#   }
# },j)   ))]#,arr.ind=TRUE
# print(paths_matrix[[4]][[j]])
# 
# paths_matrix <- lapply(1:4, function(i,paths_list){
#   lapply(1:4, function(j,i,l){
#     paths_list[[i]][-(which(sapply(paths_list[[i]],function(path,j){
#       if (j == path[length(path)]){
#         return(FALSE)
#       }# if
#       else {
#         return(TRUE)
#       }# else
#     },j)))]# paths_list[[i]]
#   },i,paths_list[[i]])# lapply(1:4, function(j,i,l))
# },paths_list)# lapply(1:4, function(i,paths_list))
# print(paths_matrix)


# system.time({
#   n <- vcount(nsnet)
#   #paths<-vector(length = n,mode="list")
#   paths <- c()
#   #paths <- c()
#   for (i in 1:n){
#     #sublist <- vector(length = n,mode="list")
#     sublist <- c()
#     for (j in 1:n){
#       sublist[[j]] <- all_shortest_paths(nsnet,from=i,to=c(j), mode="out")$res
#     }# for
#     paths[[i]]<-sublist
#   }# for
# })


# system.time({
#   cl <- start_cluster()
#   n <- vcount(nsnet)
#   paths <- vector(length=n, mode="list")
#   paths <- lapply(1:n, function(i,nsnet){
#     col <- lapply(1:n, function(j,i,nsnet){
#       paths[[i]][[j]] <- all_shortest_paths(nsnet,from=i,to=c(j), mode="out")$res
#     },i,nsnet)
#   },nsnet)
#   cl <- stop_cluster(cl)
# })

# data <- matrix(1:25,nrow=5,ncol=5)
# print(data)
# sdev <- apply(data,1, sd, na.rm = TRUE)
# print(sdev)


#
#
cl <- start_cluster()


for (i in 1:4){
  analyse_random(cl, 1000,25,1000,i/2000,TRUE,FALSE)
}# for


for (i in c(1,2.5,5,7.5,10)){
  analyse_graph(cl,i*1000,25,nsnet,l_nsnet,"NSnet")
}# for




cl <- start_cluster()
for (p_counter in 1:4){
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






sprintf('Erdos-Renyi: counter=%i, steps=%i, n=%i, p=%f, directed=%d, weighted=%d'
        ,counter,steps,n,p,directed,weighted)



cl <- stop_cluster(cl)




