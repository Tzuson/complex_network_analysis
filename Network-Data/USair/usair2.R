source("system/initialization.R")
source("system/parallell_programming.R")
source("system/functions.R")

# For the distances we need the global coordinates
us_coordinates <- read.csv("Network-Data/USair/raw_data/us_coordinates.csv", stringsAsFactors = FALSE)

# First we make an edge list, useful for all our graphs
usair <- read.csv("Network-Data/USair/raw_data/flight_data.csv"
                  , stringsAsFactors = FALSE) %>%
  
  # We only use passenger flights, national airports and no self-loops
  filter(PASSENGERS > 0
         , ORIGIN_COUNTRY_NAME=="United States"
         , DEST_COUNTRY_NAME=="United States"
         , !(ORIGIN%in%c("ROR", "AWK", "TIQ", "UAM","PPG","JON"
                         ,"OFU","TAV","FAQ","MDY","SAW","GUM"
                         ,"ROP","SPN"))
         , !(DEST%in%c("ROR", "AWK", "TIQ", "UAM","PPG","JON"
                       ,"OFU","TAV","FAQ","MDY","SAW","GUM"
                       ,"ROP","SPN"))
         , ORIGIN!=DEST) %>%
  
  # Rest is not usefull anymore
  select(PASSENGERS,ORIGIN,DEST) %>%
  
  # We want an edge list with origin - destination - #passengers between these airports
  arrange(ORIGIN,DEST) %>%
  group_by(ORIGIN,DEST) %>%
  summarise(N_PASSENGERS = sum(PASSENGERS, na.rm = TRUE)) %>%
  select(ORIGIN,DEST,N_PASSENGERS) %>%
  `colnames<-`(c("ORIGIN","DEST","PASSENGERS"))

# List of airports
nodes_origin <- unique(usair$ORIGIN)
nodes_dest <- unique(usair$DEST)
nodes_usair <- unique(c(nodes_origin,nodes_dest))

# Initializing graph as directed
usair_du <- graph_from_data_frame(usair[,c("ORIGIN","DEST")],directed=TRUE,nodes_usair)

# Only use the main cluster (measured from Atlanta)
usair_du <- induced_subgraph(usair_du,vids=ego(usair_du, order=10000,nodes="ATL")[[1]])
usair <- filter(usair, ORIGIN%in%V(usair_du)$name, DEST%in%V(usair_du)$name)
write.csv(usair, "Network-Data/USair/cooked_data/usair.csv")

# Calculating matrices for usair (#Or using memory for already calculated ones)
adj_usair_du <- as_adjacency_matrix(usair_du, type = "both", sparse=FALSE)
write.csv(l_usair,file="Network-Data/USair/cooked_data/adj_usair_du.csv")

l_usair <- parSapply(cl,V(usair_du),function(to,usair_du,us_coordinates){
  row <- sapply(V(usair_du), function(from,to,usair_du,us_coordinates){
    origin <- filter(us_coordinates, AIRPORT==V(usair_du)$name[from])
    lat_origin <- as.double(origin[1,"LATITUDE"])
    lon_origin <- as.double(origin[1,"LONGITUDE"])
    
    destination <- filter(us_coordinates, AIRPORT==V(usair_du)$name[to])
    lat_destination <- as.double(destination[1,"LATITUDE"])
    lon_destination <- as.double(destination[1,"LONGITUDE"])
    
    distance <- coordinates2distance(lat_origin,lon_origin,lat_destination,lon_destination)
    return(distance)
  },to,usair_du,us_coordinates)# sapply
  return(row)
},usair_du,us_coordinates)# parSapply
write.csv(l_usair,file="Network-Data/USair/cooked_data/l_usair.csv")

# Making a passenger matrix
w_usair <- l_usair - l_usair
for (pointer in 1:length(usair[,1])){
  w_usair[usair[pointer,1],usair[pointer,2]] <- usair[pointer,3]
}# for
write.csv(w_usair,file="Network-Data/USair/cooked_data/w_usair.csv")

# Initiating the graphs
usair_uu <- graph_from_adjacency_matrix(adj_usair_du,mode="max")
usair_du <- graph_from_adjacency_matrix(adj_usair_du,mode="directed")
usair_uw <- graph_from_adjacency_matrix(adj_usair_du*l_usair,mode="max",weighted=TRUE)
usair_dw <- graph_from_adjacency_matrix(adj_usair_du*l_usair,mode="directed",weighted=TRUE)

# We use two different approximations for the transport matrix
t_usair_uu_sum <- transport_sum(cl,usair_uu,w_usair) %>%
  `rownames<-`(rownames(l_usair)) %>%
  `colnames<-`(colnames(l_usair)) %T>%
  write.csv(file="Network-Data/USair/cooked_data/t_usair_uu_sum.csv")
t_usair_du_sum <- transport_sum(cl,usair_du,w_usair) %>%
  `rownames<-`(rownames(l_usair)) %>%
  `colnames<-`(colnames(l_usair)) %T>%
  write.csv(file="Network-Data/USair/cooked_data/t_usair_du_sum.csv")
t_usair_uw_sum <- transport_sum(cl,usair_uw,w_usair) %>%
  `rownames<-`(rownames(l_usair)) %>%
  `colnames<-`(colnames(l_usair)) %T>%
  write.csv(file="Network-Data/USair/cooked_data/t_usair_uw_sum.csv")
t_usair_dw_sum <- transport_sum(cl,usair_dw,w_usair) %>%
  `rownames<-`(rownames(l_usair)) %>%
  `colnames<-`(colnames(l_usair)) %T>%
  write.csv(file="Network-Data/USair/cooked_data/t_usair_dw_sum.csv")

t_usair_uu_min <- transport_min(cl,usair_uu,w_usair) %>%
  `rownames<-`(rownames(l_usair)) %>%
  `colnames<-`(colnames(l_usair)) %T>%
  write.csv(file="Network-Data/USair/cooked_data/t_usair_uu_min.csv")
t_usair_du_min <- transport_min(cl,usair_du,w_usair) %>%
  `rownames<-`(rownames(l_usair)) %>%
  `colnames<-`(colnames(l_usair)) %T>%
  write.csv(file="Network-Data/USair/cooked_data/t_usair_du_min.csv")
t_usair_uw_min <- transport_min(cl,usair_uw,w_usair) %>%
  `rownames<-`(rownames(l_usair)) %>%
  `colnames<-`(colnames(l_usair)) %T>%
  write.csv(t_usair_uu_sum,file="Network-Data/USair/cooked_data/t_usair_uw_min.csv")
t_usair_dw_min <- transport_min(cl,usair_dw,w_usair) %>%
  `rownames<-`(rownames(l_usair)) %>%
  `colnames<-`(colnames(l_usair)) %T>%
  write.csv(file="Network-Data/USair/cooked_data/t_usair_dw_min.csv")









#' Plot graph of USair
#' 
#' @param net A graph
#' @param nodes Data on nodes - optional -
#' @param file_path A string name (without .pdf)
#' @return A pdf with a plot of USair
plot_usair <- function(net, nodes=NULL, file_path,name){
  # Data is zero when missing
  if (is.null(nodes)){
    nodes <- vector(mode="numeric", length = vcount(net))
    nodes <- nodes + 1
  }# if
  
  # Plotting and storing image of USair
  file_name <- sprintf('%s_map.pdf',file_path)
  pdf(file_name)
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
  
  # Plotting and storing histogram of USair nodes data
  file_name <- sprintf('%s_hist.pdf',file_path)
  
  pdf(file_name)
  hist(nodes
       , breaks=100
       , xlab="Vulnerability (Binsize = 0.001)"
       , ylab="Frequency"
       , main=sprintf('\n Vulnerability of nodes of %s',name))
  dev.off()
}# plot_usair