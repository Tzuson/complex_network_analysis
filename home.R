source("initialization.R")
source("data/nsnet.R")
source("functions/functions.R")
source("functions/measures.R")
source("functions/tests.R")
source("functions/measuresPar.R")
source("functions/testsPar.R")


l <- matrix(1,nrow=vcount(nsnet),ncol=vcount(nsnet))


#measures(nsnet,global_efficiency)

#nsnet_plot(3*local_efficiency(nsnet))
# v <- vcount(nsnet)
# a <- matrix(1,nrow=v,ncol=v)
# cl <- start_cluster()
# 
# 
# nodes <- vulnerability_nodes(nsnet, a, global_efficiency)
# 
# sum(edges <- vulnerability_edges(nsnet, a, global_efficiency))
# sum(edges <- vulnerability_edgesPar(cl, nsnet, a, global_efficiency))
# 
# nsnet_plot(10*nodes, 10*edges)



# vulnerability_efficiency_nodesPar <- function(g, l){
#   # Making a 'matrix' of lists of shortest paths
#   n <- vcount(g)
#   paths <- vector(length=n, mode="list")
#   paths <- lapply(1:n, function(i,g){
#     col <- lapply(1:n, function(j,i,g){
#       paths[[i]][[j]] <- all_shortest_paths(g,from=i,to=c(j), mode="out")$res
#     },i,g)
#   },g)
# 
#   d <- distances(g, v=V(g), to=V(g), mode="out")
#   
#   diag(d) <- Inf
#   diag(l) <- Inf
#   e <- sum(1/d)/sum(1/l)
# 
#   # Calculating global efficiency when node n is missing
#   eff <- sapply(1:vcount(g), function(n,g,l,d,paths){# calculating efficiency without node n
#     d_new <- sapply(1:vcount(g), function(j,n,g,d,paths){# calculating sum(1/d) of the jth row of d
#       row <- sapply(1:vcount(g), function(i,j,n,g,d,paths){# calculating shortest path from i to j
#         if ((i==n) || (j==n)){
#           return(Inf)
#         }
#         for (path in paths[[i]][[j]]){# checking shortest paths from i to j
#           if (!(n %in% path)){# there is a path without n
#             return(d[i,j])
#           }# if
#         }# for (path in paths[[i]][[j]])
#         v1 <- i
#         v2 <- j
#         if (v1 > n){v1<-v1-1}
#         if (v2 > n){v2<-v2-1}
#         subg <- delete_vertices(g,c(n))
#         return(distances(subg,v=c(v1),to=c(v2),mode="out"))
#       },j,n,g,d,paths)# function(i,j,n,g,d,paths)
#       return(row)
#     },n,g,d,paths)# function(j,n,g,d,paths)
#     return(sum(1/d_new)/sum(1/l[-n,-n]))
#   }, g,l,d,paths)# function(n,g,d,paths)
#   vul <- 1-eff/e # vulnerability
#   return(vul)
# }# vulnerability_efficiency_nodesPar(g,l)


vulnerability_efficiency_nodesPar2 <- function(g, l){
  # Generating for every node a list of shortest paths originating 
  # in that node
  n <- vcount(g)
  paths_list <- lapply(1:n, function(i,g){
    all_shortest_paths(g,from=i,to=V(g), mode="out")$res
  },g)
  
  # Per origin, sort for destination
  paths <- lapply(1:n, function(i,paths_list){
    lapply(1:n, function(j,i,l){
      paths_list[[i]][-(which(sapply(paths_list[[i]],function(path,j){
        if (j == path[length(path)]){
          return(FALSE)
        }# if
        else {
          return(TRUE)
        }# else
      },j)))]# paths_list[[i]]
    },i,paths_list[[i]])# lapply(1:n, function(j,i,l))
  },paths_list)# lapply(1:n, function(i,paths_list))
  
  # General distances and numbers
  d <- distances(g, v=V(g), to=V(g), mode="out")
  diag(d) <- Inf
  diag(l) <- Inf
  e <- sum(1/d)/sum(1/l)
  
  # Efficiencies of damaged networks (damage is one node)
  eff <- sapply(1:vcount(g), function(n,g,l,d,paths){# calculating efficiency without node n
    
    # Which paths are altered?
    v_new <- c() 
    to_new <- c()
    
    # Checking every pair (i,j) if there is a unobstructed path
    sapply(1:vcount(g), function(j,n,g,paths,v_new,to_new){
      sapply(1:vcount(g), function(i,j,n,g,paths,v_new,to_new){
        # Is there a shortest path without n?
        for (path in paths[[i]][[j]]){# checking shortest paths from i to j
          if (!(n %in% path)){# there is a path without n
            break
          }# if
        }# for (path in paths[[i]][[j]])
        # If not, distance(v=i,to=j) has to be recalculated, so
        v_new <- c(v_new,i)
        to_new <- c(to_new,j)
      },j,n,g,paths,v_new,to_new)# function(i,j,n,g,paths,v_new,to_new)
    },n,g,paths,v_new,to_new)# function(j,n,g,paths,v_new,to_new)
    
    # Only recalculating altered paths
    dd_new <- distances(g,v=v_new,to=to_new,mode="out")
    d_new <- d
    d_new[v_new,to_new] <- dd_new
    
    # Efficiency of network without node n
    return(sum(1/d_new)/sum(1/l[-n,-n]))
  }, g,l,d,paths)# function(n,g,l,d,paths)
  
  # Calculating Vulnerability
  vul <- 1-eff/e 
  return(vul)
}# vulnerability_efficiency_nodesPar2(g,l)


system.time({
  x <-vulnerability_efficiency_nodesPar2(nsnet,l)
  # x <- vulnerability_nodes(nsnet,l,global_efficiency)
})
print(x[which.max(x)])

# x <- system.time({
#   for (i in 1:2500){
#     distances(nsnet,v=1:1,to=1:50,mode="out")
#   }# for
# })


# rm(list=ls())
# cat("\014") 
# source("initialization.R")
# source("data/nsnet.R")
# n <- 5
# i <- 5
# j <- 5
# #imax <- (i-1)*(394%/%n)+1
# imax <- 394
# #jmax <- (j-1)*(394%/%n)+1
# jmax <- 394
# system.time({
#   for (k in 1:25){
#     distances(nsnet,v=1:imax,to=1:jmax,mode="out")
#     # x <- 1:imax
#     # y <- 1:jmax
#   }# for
# })[1]
# 
# 
# 
# 
# get <- c(2.25,2.17,2.19,2.13,2.22)
# mean(get)
# sd(get)
# 
# 50%/%20
# 
# 
# 
# system.time({
# x <- sapply(1:394, function(i,nsnet){
#   mean(all_shortest_paths(nsnet, from=i, to=V(nsnet),mode="out")$nrgeo)
#   },nsnet)})
# #print(x$nrgeo)
# print(which.max(x))
# print(x[which.max(x)])
# print(which.min(x))
# print(x[which.min(x)])
# nsnet_plot(2*(x-1),0)
