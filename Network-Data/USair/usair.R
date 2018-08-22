source("system/initialization.R")
source("system/parallell_programming.R")
source("system/functions.R")

# Reading data into R
flights <- read.csv("USAirnet/flight_data.csv",stringsAsFactors = FALSE)
global_coordinates <- read.table("http://opsahl.co.uk/tnet/datasets/openflights_airports.txt"
                                 ,stringsAsFactors = FALSE)

# Flights without passengers (cargo) are omitted
passenger_flights <- flights[flights$PASSENGERS>0, ]

# Only national flights, no border crossing
passenger_flights <- filter(passenger_flights,ORIGIN_COUNTRY_NAME=="United States")
passenger_flights <- filter(passenger_flights,DEST_COUNTRY_NAME=="United States")

# No self-loops
passenger_flights <- filter(passenger_flights,ORIGIN!=DEST)

# List of airports
airports_origin <- unique(passenger_flights$ORIGIN)
airports_dest <- unique(passenger_flights$DEST)
airports <- unique(c(airports_origin,airports_dest))

# Edge list with passenger numbers
edges_usairnet <- unique(passenger_flights[,c(4,8)])
edges_usairnet["Passengers"] <- vector(mode="integer",length=length(edges_usairnet[,1]))
edges_usairnet[["Passengers"]] <- vector(mode="integer",length=length(edges_usairnet[,1]))

edges_usairnet[,3] <- parSapply(cl,1:length(edges_usairnet[,3]),function(pointer,passenger_flights,edges_usairnet){
  unique_usairnet <- filter(passenger_flights, ORIGIN==edges_usairnet[pointer,1]&DEST==edges_usairnet[pointer,2])
  passengers <- sum(unique_usairnet[,1])
},passenger_flights,edges_usairnet)

# Initializing graph as directed
usairnet_d <- graph_from_data_frame(passenger_flights[,c("ORIGIN","DEST")],directed=TRUE,airports)

# Only use the main cluster
usairnet_d <- induced_subgraph(usairnet_d,vids=ego(usairnet_d, order=10000,nodes=1)[[1]])

# Matrices from usairnet
adj_usairnet_d <- as_adjacency_matrix(usairnet_d, type = "both", sparse=FALSE)
l_usairnet <- parSapply(cl,V(usairnet_d),function(to,usairnet_d,global_coordinates){
  row <- sapply(V(usairnet_d), function(from,to,usairnet_d,global_coordinates){
    origin <- filter(global_coordinates, V5==V(usairnet_d)$name[from])
    lat_origin <- as.double(origin[7])
    lon_origin <- as.double(origin[8])
    
    destination <- filter(global_coordinates, V5==V(usairnet_d)$name[to])
    lat_destination <- as.double(destination[7])
    lon_destination <- as.double(destination[8])
    
    distance <- coordinates2distance(lat_origin,lon_origin,lat_destination,lon_destination)
    return(distance)
  },to,usairnet_d,global_coordinates)# sapply
  return(row)
},usairnet_d,global_coordinates)# parSapply

# There are airports we do not know the coordinates of
airports_delete <- V(usairnet_d)[is.na(l_usairnet[,1])]

adj_usairnet_d <- adj_usairnet_d[-airports_delete,-airports_delete]
l_usairnet <- l_usairnet[-airports_delete,-airports_delete]
edges_usairnet <- filter(edges_usairnet,ORIGIN%in%V(usairnet_d)$name)
edges_usairnet <- filter(edges_usairnet,DEST%in%V(usairnet_d)$name)

usairnet_d <- delete.vertices(usairnet_d,airports_delete)
# usairnet_dw <- graph_from_adjacency_matrix(adj_usairnet_d*l_usairnet,mode="directed",weighted=TRUE)

# Only use the main cluster
airports_delete <- V(usairnet_d)[-ego(usairnet_d, order=10000,nodes=1)[[1]]]

adj_usairnet_d <- adj_usairnet_d[-airports_delete,-airports_delete]
l_usairnet <- l_usairnet[-airports_delete,-airports_delete]
edges_usairnet <- filter(edges_usairnet,ORIGIN%in%row.names(l_usairnet))
edges_usairnet <- filter(edges_usairnet,DEST%in%row.names(l_usairnet))


usairnet_d <- delete.vertices(usairnet_d,airports_delete)
usairnet_dw <- graph_from_adjacency_matrix(adj_usairnet_d*l_usairnet,mode="directed",weighted=TRUE)

# Collapse networks to unweighted networks
usairnet <- as.undirected(usairnet_d,mode="collapse")
usairnet_w <- as.undirected(usairnet_dw,mode="collapse")

# Making a transport matrix
edges_usairnet <- edges_usairnet[-c(3157,3695),]
w_usairnet <- l_usairnet - l_usairnet
for (pointer in 1:length(edges_usairnet[,1])){
  w_usairnet[edges_usairnet[pointer,1],edges_usairnet[pointer,2]] <- edges_usairnet[pointer,3]
}# for


#' Plot graph of USAirnet
#' 
#' @param net A graph
#' @param nodes Data on nodes - optional -
#' @param edges Data on edges - optional -
#' @param file_path A string name (without .pdf)
#' @return A pdf with a plot of USAirnet
plot_usairnet <- function(net, nodes=NULL, edges=NULL, file_path,name){
  # Data is zero when missing
  if (is.null(nodes)){
    nodes <- vector(mode="numeric", length = vcount(net))
    nodes <- nodes + 1
  }# if
  if (is.null(edges)){
    edges <- vector(mode="numeric", length = ecount(net))
    edges <- edges + 1
  }# if
  
  # Plotting and storing image of USAirnet
  file_name <- sprintf('%s_map.pdf',file_path)
  pdf(file_name)
  par(mar=rep(0,4),oma=rep(0,4))
  plot(net,vertex.label=NA
       , vertex.size=1 + nodes
       , vertex.color="blue"
       , edge.color="lightblue"
       , edge.width=edges
       , vertex.frame.color="darkblue"
       , edge.arrow.size = 0.5
       , main = sprintf('\n\n %s',name))
  dev.off()
  
  # Plotting and storing histogram of USAirnet nodes data
  file_name <- sprintf('%s_hist.pdf',file_path)
  
  pdf(file_name)
  hist(nodes
       , breaks=100
       , xlab="Vulnerability (Binsize = 0.001)"
       , ylab="Frequency"
       , main=sprintf('\n Vulnerability of nodes of %s',name))
  dev.off()

  # Storing in .csv-file
  file_name <- sprintf('%s.csv',file_path)
  write.csv(nodes,file=file_name)
  
}# plot_usairnet