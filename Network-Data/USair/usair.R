source("system/initialization.R")
source("system/parallell_programming.R")
source("system/functions.R")

# USair consists of US airports on the mainland, Alaska, Hawaï and Puerto Rico.
# An edge is drawn if there was at least one passenger flown between these two
# airports. Only the main cluster (measured from Atlanta airport and the edges
# undirected) is used.
# 
# This network can be both directed and undirected, where
# the undirected is the collapsed directed version. Weights, when used, are the
# distances between airports in kilometers. The usage of edges are the total
# number of passengers. Raw data from the U.S. Department of Transportation
# (https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=292), Geography =
# all, Year = 2017, Period = All months.


# First we make an edge list, useful for all our graphs
edges_usair <- read.csv("Network-Data/USair/raw_data/flight_data.csv"
                  , stringsAsFactors = FALSE) %>%
  
  # We only use passenger flights, so their should be at least one passenger,
  # national airports and no self-loops
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
  `colnames<-`(c("FROM","TO","SIZE"))

# Temporary list of airports
nodes_from <- unique(edges_usair$FROM)
nodes_to <- unique(edges_usair$TO)
nodes_usair <- unique(c(nodes_from,nodes_to))

# Initializing graph as directed
usair_du <- graph_from_data_frame(edges_usair[,c("FROM","TO")],directed=TRUE,nodes_usair)

# Only use the main cluster (measured from Atlanta)
usair_du <- induced_subgraph(usair_du,vids=ego(usair_du, order=10000,nodes="ATL")[[1]])
edges_usair <- filter(edges_usair, FROM%in%V(usair_du)$name, TO%in%V(usair_du)$name) %T>%
  write.csv("Network-Data/USair/cooked_data/edges_usair.csv", row.names=FALSE)

# A list of nodes with their coordinates, where we only use the airports that
# are in the graph. The following is included: AirportID (NODE), latitude
# (LATITUDE), longitude (LONGITUDE) and number of passengers departing (SIZE)
nodes_usair <- read.csv("Network-Data/USair/raw_data/us_coordinates.csv", stringsAsFactors = FALSE) %>%
  filter(AIRPORT%in%V(usair_du)$name) %>%
  select(AIRPORT,LATITUDE,LONGITUDE) %>%
  group_by(AIRPORT) %>%
  summarise(LATITUDE = mean(LATITUDE, na.rm = TRUE), LONGITUDE = mean(LONGITUDE, na.rm = TRUE)) %>%
  left_join(summarise(group_by(edges_usair,FROM),USAGE = sum(USAGE)), by = c("AIRPORT" = "FROM")) %>%
  `colnames<-`(c("NODE","LATITUDE","LONGITUDE","SIZE"))
nodes_usair$SIZE[is.na(nodes_usair$SIZE)] <- 0
write.csv(nodes_usair,"Network-Data/USair/cooked_data/nodes_usair.csv",row.names=FALSE)

# Calculating matrices for usair 
adj_usair_du <- as_adjacency_matrix(usair_du, type = "both", sparse=FALSE)
write.csv(adj_usair_du,file="Network-Data/USair/cooked_data/adj_usair_du.csv",row.names=FALSE)

l_usair <- parSapply(cl,V(usair_du),function(to,usair_du,us_coordinates){
  row <- sapply(V(usair_du), function(from,to,usair_du,us_coordinates){
    origin <- filter(nodes_usair, FROM==V(usair_du)$name[from])
    lat_origin <- as.double(origin[1,"LATITUDE"])
    lon_origin <- as.double(origin[1,"LONGITUDE"])
    
    destination <- filter(nodes_usair, FROM==V(usair_du)$name[to])
    lat_destination <- as.double(destination[1,"LATITUDE"])
    lon_destination <- as.double(destination[1,"LONGITUDE"])
    
    distance <- coordinates2distance(lat_origin,lon_origin,lat_destination,lon_destination)
    return(distance)
  },to,usair_du,nodes_usair)# sapply
  return(row)
},usair_du,nodes_usair)# parSapply
write.csv(l_usair,file="Network-Data/USair/cooked_data/l_usair.csv",row.names=FALSE)

# Making a passenger matrix
w_usair <- l_usair - l_usair
for (pointer in 1:length(edges_usair[,"FROM"])){
  w_usair[edges_usair[pointer,"FROM"],edges_usair[pointer,"TO"]] <- edges_usair[pointer,"SIZE"]
}# for
write.csv(w_usair,file="Network-Data/USair/cooked_data/w_usair.csv",row.names=FALSE)

# Initiating the graphs
usair_uu <- graph_from_adjacency_matrix(adj_usair_du,mode="max")
usair_du <- graph_from_adjacency_matrix(adj_usair_du,mode="directed")
usair_uw <- graph_from_adjacency_matrix(adj_usair_du*l_usair,mode="max",weighted=TRUE)
usair_dw <- graph_from_adjacency_matrix(adj_usair_du*l_usair,mode="directed",weighted=TRUE)

# We use two different approximations for the transport matrix
t_usair_uu_sum <- transport_sum(cl,usair_uu,w_usair) %>%
  `rownames<-`(rownames(l_usair)) %>%
  `colnames<-`(colnames(l_usair)) %T>%
  write.csv(file="Network-Data/USair/cooked_data/t_usair_uu_sum.csv",row.names=FALSE)
t_usair_du_sum <- transport_sum(cl,usair_du,w_usair) %>%
  `rownames<-`(rownames(l_usair)) %>%
  `colnames<-`(colnames(l_usair)) %T>%
  write.csv(file="Network-Data/USair/cooked_data/t_usair_du_sum.csv",row.names=FALSE)
t_usair_uw_sum <- transport_sum(cl,usair_uw,w_usair) %>%
  `rownames<-`(rownames(l_usair)) %>%
  `colnames<-`(colnames(l_usair)) %T>%
  write.csv(file="Network-Data/USair/cooked_data/t_usair_uw_sum.csv",row.names=FALSE)
t_usair_dw_sum <- transport_sum(cl,usair_dw,w_usair) %>%
  `rownames<-`(rownames(l_usair)) %>%
  `colnames<-`(colnames(l_usair)) %T>%
  write.csv(file="Network-Data/USair/cooked_data/t_usair_dw_sum.csv",row.names=FALSE)

t_usair_uu_min <- transport_min(cl,usair_uu,w_usair) %>%
  `rownames<-`(rownames(l_usair)) %>%
  `colnames<-`(colnames(l_usair)) %T>%
  write.csv(file="Network-Data/USair/cooked_data/t_usair_uu_min.csv",row.names=FALSE)
t_usair_du_min <- transport_min(cl,usair_du,w_usair) %>%
  `rownames<-`(rownames(l_usair)) %>%
  `colnames<-`(colnames(l_usair)) %T>%
  write.csv(file="Network-Data/USair/cooked_data/t_usair_du_min.csv",row.names=FALSE)
t_usair_uw_min <- transport_min(cl,usair_uw,w_usair) %>%
  `rownames<-`(rownames(l_usair)) %>%
  `colnames<-`(colnames(l_usair)) %T>%
  write.csv(file="Network-Data/USair/cooked_data/t_usair_uw_min.csv",row.names=FALSE)
t_usair_dw_min <- transport_min(cl,usair_dw,w_usair) %>%
  `rownames<-`(rownames(l_usair)) %>%
  `colnames<-`(colnames(l_usair)) %T>%
  write.csv(file="Network-Data/USair/cooked_data/t_usair_dw_min.csv",row.names=FALSE)









