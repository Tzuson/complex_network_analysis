source("system/initialization.R")
source("system/parallell_programming.R")
source("system/functions.R")

cl <- start_cluster(sources = c("system/initialization.R","functions/measures_distance.R","system/functions.R"))
use_main_cluster <- TRUE

# Flight data loading and cleaning
files_usair_ts <- sapply(1993:2017,function(year){sprintf('flight_data_%i',year)})
edges_usair_ts <- new.env()
for (file_usair_ts in files_usair_ts){
  # Reading data into R and filtering out information and places we do not need
  edges_usair_ts[[file_usair_ts]] <- read.csv(sprintf('Network-Data/USair_time_series/raw_data/%s.csv',file_usair_ts),stringsAsFactors = FALSE) %>%
    select(PASSENGERS,ORIGIN,ORIGIN_STATE_NM,ORIGIN_COUNTRY_NAME,DEST,DEST_STATE_NM,DEST_COUNTRY_NAME) %>%
    filter(PASSENGERS > 0
           , ORIGIN_COUNTRY_NAME=="United States"
           , DEST_COUNTRY_NAME=="United States"
           , ORIGIN_STATE_NM!="Alaska"
           , DEST_STATE_NM!="Alaska"
           , ORIGIN_STATE_NM!="Hawaii"
           , DEST_STATE_NM!="Hawaii"
           , !(ORIGIN%in%c("ROR", "AWK", "TIQ", "UAM","PPG","JON"
                           ,"OFU","TAV","FAQ","MDY","SAW","GUM"
                           ,"ROP","SPN"))
           , !(DEST%in%c("ROR", "AWK", "TIQ", "UAM","PPG","JON"
                         ,"OFU","TAV","FAQ","MDY","SAW","GUM"
                         ,"ROP","SPN"))
           , ORIGIN!=DEST) %>%
    select(ORIGIN,DEST,PASSENGERS) 
  
  # Some airports changed names over time
  # from=to: YUM=NYL, II2=LOT, DQF=AZA, NSF=ADW
  airports_usair_ts <- unique(c(edges_usair_ts[[file_usair_ts]][,"ORIGIN"],edges_usair_ts[[file_usair_ts]][,"DEST"]))
  names(airports_usair_ts) <- airports_usair_ts
  airports_usair_ts[c("YUM", "II2", "DQF", "NSF")] <- c("NYL", "LOT", "AZA", "ADW")
  edges_usair_ts[[file_usair_ts]][,"ORIGIN"] <- airports_usair_ts[edges_usair_ts[[file_usair_ts]][,"ORIGIN"]]
  edges_usair_ts[[file_usair_ts]][,"DEST"] <- airports_usair_ts[edges_usair_ts[[file_usair_ts]][,"DEST"]]
  
  # We only need the total number of passengers between each airport each year
  edges_usair_ts[[file_usair_ts]] <- group_by(edges_usair_ts[[file_usair_ts]],ORIGIN,DEST) %>%
    summarise(PASSENGERS = sum(PASSENGERS, na.rm = TRUE))
}# for

# If we use the main cluster, we want a list of every airport there is
# so we can calculate the distances matrix in one go
# If we use the airports that are operational every year, we only need those airports
# so we delete all others from the list of edges
if (use_main_cluster){
  airports_ORIGIN <- unique(unlist( lapply(files_usair_ts,function(file_usair_ts,edges_usair_ts){
    return(edges_usair_ts[[file_usair_ts]]$ORIGIN)
  },edges_usair_ts)))
  airports_DEST <- unique(unlist( lapply(files_usair_ts,function(file_usair_ts,edges_usair_ts){
    return(edges_usair_ts[[file_usair_ts]]$DEST)
  },edges_usair_ts)))
  airports_usair_ts <- unique(c(airports_ORIGIN,airports_DEST))
} else {
  airports_ORIGIN <- Reduce(intersect,lapply(files_usair_ts,function(file_usair_ts,edges_usair_ts){
    return(edges_usair_ts[[file_usair_ts]]$ORIGIN)
  },edges_usair_ts))
  airports_DEST <- Reduce(intersect,lapply(files_usair_ts,function(file_usair_ts,edges_usair_ts){
    return(edges_usair_ts[[file_usair_ts]]$DEST)
  },edges_usair_ts))
  airports_usair_ts <- Reduce(intersect,list(a=airports_ORIGIN,b=airports_DEST))
}# else

# If we use the airports that are in every year operational, we have to filter the other
# airports out of the edge list
for (file_usair_ts in files_usair_ts){
  edges_usair_ts[[file_usair_ts]] <- filter(edges_usair_ts[[file_usair_ts]]
                                            , ORIGIN%in%airports_usair_ts
                                            , DEST%in%airports_usair_ts)
}# for

# We need the coordinates of the airports to make l_usair_ts_c
us_coordinates <- read.csv("Network-Data/USair_time_series/raw_data/us_coordinates.csv"
                           , stringsAsFactors = FALSE) %>%
  filter(AIRPORT%in%airports_usair_ts) %>%
  arrange(AIRPORT) %>%
  group_by(AIRPORT) %>%
  summarise(LATITUDE = mean(LATITUDE, na.rm = TRUE), LONGITUDE = mean(LONGITUDE, na.rm = TRUE)) %>%
  select(AIRPORT,LATITUDE,LONGITUDE)

# Calculating distances between airports
l_usair_ts_c <- parSapply(cl,airports_usair_ts,function(to,airports_usair_ts,us_coordinates){
  row <- sapply(airports_usair_ts, function(from,to,airports_usair_ts,us_coordinates){
    origin <- filter(us_coordinates, AIRPORT==from)
    lat_origin <- as.double(origin[1,"LATITUDE"])
    lon_origin <- as.double(origin[1,"LONGITUDE"])
    
    destination <- filter(us_coordinates, AIRPORT==to)
    lat_destination <- as.double(destination[1,"LATITUDE"])
    lon_destination <- as.double(destination[1,"LONGITUDE"])
    
    distance <- coordinates2distance(lat_origin,lon_origin,lat_destination,lon_destination)
    return(distance)
  },to,airports_usair_ts,us_coordinates)# sapply
  return(row)
},airports_usair_ts,us_coordinates)# parSapply
write.csv(l_usair_ts_c,"Network-Data/USair_time_series/cooked_data/l_usair_ts_c.csv")


usair_ts_uu <- new.env()
usair_ts_du <- new.env()
usair_ts_uw <- new.env()
usair_ts_dw <- new.env()
l_usair_ts <- new.env()
# We make four different graphs four every month: Undirected/Unweighted, Directed/Unweighted,
# Undirected/Weighted, Directed/Weighted
for (file_usair_ts in files_usair_ts){
  # Calculating the adjacency matrix
  adj_usair_ts_du <- as_adjacency_matrix(graph_from_data_frame(edges_usair_ts[[file_usair_ts]][,c("ORIGIN","DEST")],directed=TRUE,vertices=airports_usair_ts))
  
  usair_ts_uu[[file_usair_ts]] <- graph_from_adjacency_matrix(adj_usair_ts_du,mode="max")
  usair_ts_du[[file_usair_ts]] <- graph_from_adjacency_matrix(adj_usair_ts_du,mode="directed")
  usair_ts_uw[[file_usair_ts]] <- graph_from_adjacency_matrix(adj_usair_ts_du*l_usair_ts_c,mode="max",weighted=TRUE)
  usair_ts_dw[[file_usair_ts]] <- graph_from_adjacency_matrix(adj_usair_ts_du*l_usair_ts_c,mode="directed",weighted=TRUE)
  
  # Only use the main cluster, calculated without directions, from Atlanta
  if (use_main_cluster){
    usair_ts_uu[[file_usair_ts]] <- induced_subgraph(usair_ts_uu[[file_usair_ts]]
                                                      , vids=ego(usair_ts_uu[[file_usair_ts]]
                                                                 , order=10000,nodes="ATL",mode="all")[[1]])
    usair_ts_du[[file_usair_ts]] <- induced_subgraph(usair_ts_du[[file_usair_ts]]
                                                      , vids=ego(usair_ts_du[[file_usair_ts]]
                                                                 , order=10000,nodes="ATL",mode="all")[[1]])
    usair_ts_uw[[file_usair_ts]] <- induced_subgraph(usair_ts_uw[[file_usair_ts]]
                                                      , vids=ego(usair_ts_uw[[file_usair_ts]]
                                                                 , order=10000,nodes="ATL",mode="all")[[1]])
    usair_ts_dw[[file_usair_ts]] <- induced_subgraph(usair_ts_dw[[file_usair_ts]]
                                                      , vids=ego(usair_ts_dw[[file_usair_ts]]
                                                                 , order=10000,nodes="ATL",mode="all")[[1]])
    l_usair_ts[[file_usair_ts]] <- l_usair_ts_c[V(usair_ts_uu[[file_usair_ts]])$name,V(usair_ts_uu[[file_usair_ts]])$name] %T>%
      write.csv(sprintf("Network-Data/USair_time_series/cooked_data/l_usair_ts_%i.csv", (which(sort(files_usair_ts) == file_usair_ts)+1992)))
    
    edges_usair_ts[[file_usair_ts]] <- filter(edges_usair_ts[[file_usair_ts]]
                                       , ORIGIN%in%V(usair_ts_uu[[file_usair_ts]])$name
                                       , DEST%in%V(usair_ts_uu[[file_usair_ts]])$name) %T>%
      write.csv(sprintf("Network-Data/USair_time_series/cooked_data/edges_usair_ts_%i.csv", (which(sort(files_usair_ts) == file_usair_ts)+1992)))
  }# if
}# for


# Finally, we need a nodes lists
nodes_usair_ts <- new.env()
for (file_usair_ts in files_usair_ts){
  nodes_usair_ts[[file_usair_ts]] <- filter(us_coordinates, AIRPORT%in%V(usair_ts_uu[[file_usair_ts]])$name) %>%
    arrange(AIRPORT) %>%
    left_join(.,
      group_by(edges_usair_ts[[file_usair_ts]],ORIGIN) %>% summarise(PASSENGERS = sum(PASSENGERS, na.rm = TRUE)),
              by = c("AIRPORT" = "ORIGIN")) %T>%
    write.csv(file=sprintf("Network-Data/USair_time_series/cooked_data/nodes_usair_ts_%i.csv", (which(sort(files_usair_ts) == file_usair_ts)+1992)))
  print(sprintf("nodes_usair_ts_%i.csv", (which(sort(files_usair_ts) == file_usair_ts)+1992)))
}# for

# Matrices with number of passengers over the nodes
w_usair_ts <- new.env()
for (file_usair_ts in files_usair_ts){
  w_usair_ts[[file_usair_ts]] <- 0*l_usair_ts[[file_usair_ts]] 
  for (pointer in 1:length(edges_usair_ts[[file_usair_ts]][,1])){
    w_usair_ts[[file_usair_ts]][edges_usair_ts[[file_usair_ts]][pointer,"ORIGIN"],edges_usair_ts[[file_usair_ts]][pointer,"DEST"]] <- edges_usair_ts[[file_usair_ts]][pointer,"PASSENGERS"]
  }# for
  write.csv(w_usair_ts[[file_usair_ts]],file=sprintf("Network-Data/USair_time_series/cooked_data/w_usair_ts_%i.csv",which(sort(files_usair_ts) == file_usair_ts)+1992))
  print(sprintf("w_usair_ts_%i.csv", (which(sort(files_usair_ts) == file_usair_ts)+1992)))
}# for

# Transport matrices
t_usair_ts_dw_min <- new.env()
for (file_usair_ts in files_usair_ts){
  t_usair_ts_dw_min[[file_usair_ts]] <- transport_min(cl,usair_ts_dw[[file_usair_ts]],w_usair_ts[[file_usair_ts]])
  write.csv(t_usair_ts_dw_min[[file_usair_ts]],file=sprintf("Network-Data/USair_time_series/cooked_data/t_usair_ts_dw_min_%i.csv",which(sort(files_usair_ts) == file_usair_ts)+1992))
  print(sprintf("t_usair_ts_dw_min_%i.csv", (which(sort(files_usair_ts) == file_usair_ts)+1992)))
}# for

# Transport matrices
t_usair_ts_uu_min <- new.env()
for (file_usair_ts in files_usair_ts){
  t_usair_ts_uu_min[[file_usair_ts]] <- transport_min(cl,usair_ts_uu[[file_usair_ts]],w_usair_ts[[file_usair_ts]])
  write.csv(t_usair_ts_uu_min[[file_usair_ts]],file=sprintf("Network-Data/USair_time_series/cooked_data/t_usair_ts_uu_min_%i.csv",which(sort(files_usair_ts) == file_usair_ts)+1992))
  print(sprintf("t_usair_ts_uu_min_%i.csv", (which(sort(files_usair_ts) == file_usair_ts)+1992)))
}# for



# # Making a transport matrix
# edges_usairnet <- edges_usairnet[-c(3157,3695),]
# w_usairnet <- l_usairnet - l_usairnet
# for (pointer in 1:length(edges_usairnet[,1])){
#   w_usairnet[edges_usairnet[pointer,1],edges_usairnet[pointer,2]] <- edges_usairnet[pointer,3]
# }# for


