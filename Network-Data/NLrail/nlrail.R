source("system/initialization.R")
source("system/functions.R")

# NLrail is a network with dutch train stations as nodes and the rail
# connections between them as edges. It has 391 nodes and 425 edges. Some
# stations are omitted, mainly because they lack geocoordinates or are linked to
# stations lacking those coordinates.

# Importing data
# Node 394 (Waddinxveen Triangel) does not have geospatial data, so it is ommited, as well as
# nodes 359 (Waddinxveen) and 360 (Waddinxveen Noord), its neighbors
stations <-  read.csv("Network-Data/NLrail/raw_data/trajecten_stations.csv",stringsAsFactors = FALSE) %>%
  .[-c(359,360,394),] 
row.names(stations) <- stations$naam
trajecten <- read.csv("Network-Data/NLrail/raw_data/trajecten.csv",stringsAsFactors = FALSE) %>%
  filter(start%in%stations$naam,stop%in%stations$naam)

# Adding geocoordinates to the edges to make the l matrix
trajecten$lat1 <- stations[trajecten$start,"geo_lat"]
trajecten$lon1 <- stations[trajecten$start,"geo_lng"]
trajecten$lat2 <- stations[trajecten$stop,"geo_lat"]
trajecten$lon2 <- stations[trajecten$stop,"geo_lng"]

# Initializing graph
nlrail_uu <- graph_from_data_frame(trajecten, directed=FALSE, vertices=stations)

# Calculating the l matrix and adjacency matrix
adj_nlrail_uu <- as_adjacency_matrix(nlrail_uu, type = "both", sparse=FALSE) %T>%
  write.csv("Network-Data/NLrail/cooked_data/adj_nlrail_uu.csv", row.names = FALSE)
l_nlrail <- sapply(V(nlrail_uu),function(i,nlrail_uu,stations){
  sapply(V(nlrail_uu), function(j,i,nlrail_uu,stations){
    lat1 <- stations$geo_lat[i]
    lon1 <- stations$geo_lng[i]
    
    lat2 <- stations$geo_lat[j]
    lon2 <- stations$geo_lng[j]
    
    coordinates2distance(lat1,lon1,lat2,lon2)
  },i,nlrail_uu,stations)# sapply
},nlrail_uu,stations)# sapply
write.csv(l_nlrail,"Network-Data/NLrail/cooked_data/l_nlrail.csv", row.names = FALSE)

# Mutating stations into a standard nodes list with geocoordinates
nodes_nlrail <- select(stations,naam,geo_lat,geo_lng) %>%
  `row.names<-`(NULL) %>%
  `colnames<-`(c("NODE","LATITUDE","LONGITUDE")) %T>%
  write.csv("Network-Data/NLrail/cooked_data/nodes_nlrail.csv", row.names = FALSE)

# Mutating trajecten into standard edge list
edges_nlrail <- select(trajecten,start,stop) %>%
  `colnames<-`(c("FROM","TO")) %T>%
  write.csv("Network-Data/NLrail/cooked_data/edges_nlrail.csv", row.names = FALSE)




