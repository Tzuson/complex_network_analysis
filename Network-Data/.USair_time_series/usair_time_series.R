source("system/initialization.R")
source("system/parallell_programming.R")
source("system/functions.R")

source("functions/measures_distance.R")
source("functions/measures_topology.R")
source("functions/tests_distance.R")

cl <- start_cluster(sources = c("system/initialization.R","functions/measures_distance.R","functions/functions.R"))
use_main_cluster <- TRUE

# Flight data loading and cleaning
file_names <- sapply(1993:2017,function(year){sprintf('flight_data_%i',year)})
flight_data <- new.env()
for (file_name in file_names){
  # Reading data into R
  flight_data[[file_name]] <- read.csv(sprintf('USAirnet/raw_data/%s.csv',file_name),stringsAsFactors = FALSE)

  # Deleting column "X"
  flight_data[[file_name]]$X <- NULL

  # Omitting flights with zero passengers (cargo flights)
  flight_data[[file_name]] <- flight_data[[file_name]][flight_data[[file_name]]$PASSENGERS>0, ]

  #Only using domenistic flights, no-self loops and no Alaska & Hawaï
  flight_data[[file_name]] <- filter(flight_data[[file_name]],
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
                                     , ORIGIN!=DEST)
  
  # Some airports changed names over time
  # from=to: YUM=NYL, II2=LOT, DQF=AZA, NSF=ADW
  airport_names <- unique(c(flight_data[[file_name]][,"ORIGIN"],flight_data[[file_name]][,"DEST"]))
  names(airport_names) <- airport_names
  airport_names[c("YUM", "II2", "DQF", "NSF")] <- c("NYL", "LOT", "AZA", "ADW")
  flight_data[[file_name]][,"ORIGIN"] <- airport_names[flight_data[[file_name]][,"ORIGIN"]]
  flight_data[[file_name]][,"DEST"] <- airport_names[flight_data[[file_name]][,"DEST"]]
  
  # Deleting ORIGIN_COUNTRY_NAME and DEST_COUNTRY_NAME (both are "United States)
  flight_data[[file_name]] <- flight_data[[file_name]][,c("ORIGIN","DEST","PASSENGERS","MONTH")]

  gc()
}# for

# Split data sets into sets for every month
flight_data_test <- new.env()
file_names_test <- unlist(lapply(file_names,function(file_name){
  lapply(1:12,function(month,file_name){
    return(sprintf('%s_%i',file_name,month))
  }, file_name)# lapply
}))# lapply
for (file_name in file_names){
  for (month in 1:12){
    flight_data_test[[sprintf('%s_%i',file_name,month)]] <- filter(flight_data[[file_name]],MONTH==month)
  }# for
}# for
flight_data <- flight_data_test
file_names <- file_names_test

# We only use airports that are in all data sets
# Assumption: Only small airports will vanish, because medium to large airports
# are in business every year
airports_ORIGIN <- Reduce(intersect,lapply(file_names,function(file_name,flight_data){
  return(flight_data[[file_name]]$ORIGIN)
},flight_data))
airports_DEST <- Reduce(intersect,lapply(file_names,function(file_name,flight_data){
  return(flight_data[[file_name]]$DEST)
},flight_data))
airports_base <- Reduce(intersect,list(a=airports_ORIGIN,b=airports_DEST))

# We need a list of every airport available, such that
# our networks use the same nodes list
airports_ORIGIN <- unique(unlist( lapply(file_names,function(file_name,flight_data){
    return(flight_data[[file_name]]$ORIGIN)
  },flight_data)))
airports_DEST <- unique(unlist( lapply(file_names,function(file_name,flight_data){
  return(flight_data[[file_name]]$DEST)
},flight_data)))
airports_all <- unique(c(airports_ORIGIN,airports_DEST))

if (use_main_cluster){
  airports <- airports_all
} else {
  airports <- airports_base
}# else

# Coercing data into usable edge lists
for (file_name in file_names){
  # We do not need months anymore
  flight_data[[file_name]]$MONTH <- NULL
  
  # We only use the airports that are present in every file
  # in contrast to every airport in the main cluster (which depends on the year)
  flight_data[[file_name]] <- filter(flight_data[[file_name]],ORIGIN%in%airports&DEST%in%airports)
  
  # For every list, we remove duplicate edges and add the passengers together
  # Calculating a table for the indices of flight_data
  flight_data[[file_name]] <- flight_data[[file_name]][order(flight_data[[file_name]][,"ORIGIN"]),]
  flight_table <- table(flight_data[[file_name]][,"ORIGIN"])
  flight_classes <- c(0,cumsum(as.vector(flight_table)))
  names(flight_classes) <- c(0,row.names(flight_table))

  # Making an edge list with passengers column
  edges <- unique(flight_data[[file_name]][,c("ORIGIN","DEST")])
  edges["PASSENGERS"] <- vector(mode="integer",length=length(edges[,"ORIGIN"]))
  edges[["PASSENGERS"]] <- vector(mode="integer",length=length(edges[,"ORIGIN"]))

  # Calculating a table for the indices of the edges
  edges[2:(length(edges[,1])+1),] <- edges
  edges[1,] <- 0

  edges_table <- table(edges[,"ORIGIN"])
  edges_classes <- cumsum(as.vector(edges_table))
  names(edges_classes) <- row.names(edges_table)

  for (origin in 2:length(edges_classes)){
    for (dest in (edges_classes[origin-1]+1):edges_classes[origin]){
      unique_usairnet <- filter(flight_data[[file_name]][(flight_classes[edges[edges_classes[origin-1],"ORIGIN"]]+1):(flight_classes[edges[edges_classes[origin],"ORIGIN"]]),],DEST==edges[dest,"DEST"])
      edges[dest,"PASSENGERS"] <- sum(unique_usairnet[,"PASSENGERS"])
    }# for
  }# for

  # Deleting the first row
  edges <- edges[-c(1),]

  # Putting data back in flight_data
  flight_data[[file_name]] <- NULL
  flight_data[[file_name]] <- edges
}# for

# We need coordinates for the distances
us_coordinates <- read.csv("USAirnet/raw_data/us_coordinates.csv"
                           , stringsAsFactors = FALSE)[,c("AIRPORT","LATITUDE","LONGITUDE")]

# Calculating distances between airports
l_usairnet <- parSapply(cl,airports,function(to,airports,us_coordinates){
  row <- sapply(airports, function(from,to,airports,us_coordinates){
    origin <- filter(us_coordinates, AIRPORT==from)
    lat_origin <- as.double(origin[1,"LATITUDE"])
    lon_origin <- as.double(origin[1,"LONGITUDE"])
    
    destination <- filter(us_coordinates, AIRPORT==to)
    lat_destination <- as.double(destination[1,"LATITUDE"])
    lon_destination <- as.double(destination[1,"LONGITUDE"])
    
    distance <- coordinates2distance(lat_origin,lon_origin,lat_destination,lon_destination)
    return(distance)
  },to,airports,us_coordinates)# sapply
  return(row)
},airports,us_coordinates)# parSapply


flight_graphs_uu <- new.env()
flight_graphs_du <- new.env()
flight_graphs_uw <- new.env()
flight_graphs_dw <- new.env()
flight_l <- new.env()
# We make four different graphs four every month: Undirected/Unweighted, Directed/Unweighted,
# Undirected/Weighted, Directed/Weighted
for (file_name in file_names){
  # Calculating the adjacency matrix
  adj_du <- as_adjacency_matrix(graph_from_data_frame(flight_data[[file_name]],directed=TRUE,vertices=airports))

  flight_graphs_uu[[file_name]] <- graph_from_adjacency_matrix(adj_du,mode="max")
  flight_graphs_du[[file_name]] <- graph_from_adjacency_matrix(adj_du,mode="directed")
  flight_graphs_uw[[file_name]] <- graph_from_adjacency_matrix(adj_du*l_usairnet,mode="max",weighted=TRUE)
  flight_graphs_dw[[file_name]] <- graph_from_adjacency_matrix(adj_du*l_usairnet,mode="directed",weighted=TRUE)

  # Only use the main cluster, calculated without directions, from Atlanta
  if (use_main_cluster){
    flight_graphs_uu[[file_name]] <- induced_subgraph(flight_graphs_uu[[file_name]]
                                                      , vids=ego(flight_graphs_uu[[file_name]]
                                                      , order=10000,nodes="ATL",mode="all")[[1]])
    flight_graphs_du[[file_name]] <- induced_subgraph(flight_graphs_du[[file_name]]
                                                      , vids=ego(flight_graphs_du[[file_name]]
                                                      , order=10000,nodes="ATL",mode="all")[[1]])
    flight_graphs_uw[[file_name]] <- induced_subgraph(flight_graphs_uw[[file_name]]
                                                      , vids=ego(flight_graphs_uw[[file_name]]
                                                      , order=10000,nodes="ATL",mode="all")[[1]])
    flight_graphs_dw[[file_name]] <- induced_subgraph(flight_graphs_dw[[file_name]]
                                                      , vids=ego(flight_graphs_dw[[file_name]]
                                                      , order=10000,nodes="ATL",mode="all")[[1]])
    flight_l[[file_name]] <- l_usairnet[V(flight_graphs_uu[[file_name]])$name,V(flight_graphs_uu[[file_name]])$name]

    flight_data[[file_name]] <- filter(flight_data[[file_name]]
                                       , ORIGIN%in%V(flight_graphs_uu[[file_name]])$name
                                       &DEST%in%V(flight_graphs_uu[[file_name]])$name)
  }# if
}# for


# If we use the main cluster, every graph needs a different l_usairnet-matrix
if (use_main_cluster){
  flight_l <- new.env()
  for (file_name in file_names){
    flight_l[[file_name]] <- l_usairnet[V(flight_graphs_uu[[file_name]])$name,V(flight_graphs_uu[[file_name]])$name]
  }# for
}# if

# It is usefull to have a "size" of the airports, for example the number of departures
flight_passengers <- new.env()
for (file_name in file_names){
  flight_passengers[[file_name]] <- sort(V(flight_graphs_dw[[file_name]])$name)
  flight_passengers[[file_name]] <- as.data.frame(flight_passengers[[file_name]])
  row.names(flight_passengers[[file_name]]) <- flight_passengers[[file_name]][,1]
  colnames(flight_passengers[[file_name]]) <- c("DEPARTURES")
  flight_passengers[[file_name]][,"DEPARTURES"] <- 0
  
  f_table <- table(flight_data[[file_name]][,"ORIGIN"])
  f_classes <- c(0,cumsum(as.vector(f_table)))
  
  for (origin in 2:length(f_classes)){
    flight_passengers[[file_name]][names(f_table)[origin-1],"DEPARTURES"] <- sum(flight_data[[file_name]][(f_classes[origin-1]+1):f_classes[origin],"PASSENGERS"])
  }# for
}# for

# Calculating the global efficiency of the graphs
ge_uu <- sapply(file_names, function(file_name,flight_graphs_uu){
  global_efficiency_unpar(g=flight_graphs_uu[[file_name]])
  },flight_graphs_uu)# sapply

ge_du <- sapply(file_names, function(file_name,flight_graphs_du){
  global_efficiency_unpar(g=flight_graphs_du[[file_name]])
},flight_graphs_du)# sapply

ge_uw <- sapply(file_names, function(file_name,flight_graphs_uw,flight_l){
  global_efficiency_unpar(g=flight_graphs_uw[[file_name]],l=flight_l[[file_name]])
},flight_graphs_uw,flight_l)# sapply

ge_dw <- sapply(file_names, function(file_name,flight_graphs_dw,flight_l){
  global_efficiency_unpar(g=flight_graphs_dw[[file_name]],l=flight_l[[file_name]])
},flight_graphs_dw,flight_l)# sapply

# Calculating the vulnerabilities
vul_uu <- new.env()
for (file_name in file_names){
  vul_uu[[file_name]] <- vulnerability_nodes(cl,g=flight_graphs_uu[[file_name]],performance=global_efficiency_unpar)
  names(vul_uu[[file_name]]) <- V(flight_graphs_uu[[file_name]])$name
  vul_uu[[file_name]] <- vul_uu[[file_name]][sort(names(vul_uu[[file_name]]))] 
}# for

vul_du <- new.env()
for (file_name in file_names){
  vul_du[[file_name]] <- vulnerability_nodes(cl,g=flight_graphs_du[[file_name]],performance=global_efficiency_unpar)
  names(vul_du[[file_name]]) <- V(flight_graphs_du[[file_name]])$name
  vul_du[[file_name]] <- vul_du[[file_name]][sort(names(vul_du[[file_name]]))] 
}# for

vul_uw <- new.env()
for (file_name in file_names){
  vul_uw[[file_name]] <- vulnerability_nodes(cl,g=flight_graphs_uw[[file_name]],l=flight_l[[file_name]],performance=global_efficiency_unpar)
  names(vul_uw[[file_name]]) <- V(flight_graphs_uw[[file_name]])$name
  vul_uw[[file_name]] <- vul_uw[[file_name]][sort(names(vul_uw[[file_name]]))] 
}# for

vul_dw <- new.env()
for (file_name in file_names){
  vul_dw[[file_name]] <- vulnerability_nodes(cl,g=flight_graphs_dw[[file_name]],l=flight_l[[file_name]],performance=global_efficiency_unpar)
  names(vul_dw[[file_name]]) <- V(flight_graphs_dw[[file_name]])$name
  vul_dw[[file_name]] <- vul_dw[[file_name]][sort(names(vul_dw[[file_name]]))] 
}# for

# Calculating the number of nodes
vc_uu <- sapply(file_names, function(file_name,flight_graphs_uu){
  vcount(flight_graphs_uu[[file_name]])
},flight_graphs_uu)# sapply

vc_du <- sapply(file_names, function(file_name,flight_graphs_du){
  vcount(flight_graphs_du[[file_name]])
},flight_graphs_du)# sapply

vc_uw <- sapply(file_names, function(file_name,flight_graphs_uw){
  vcount(flight_graphs_uw[[file_name]])
},flight_graphs_uw)# sapply

vc_dw <- sapply(file_names, function(file_name,flight_graphs_dw){
  vcount(flight_graphs_dw[[file_name]])
},flight_graphs_dw)# sapply


plot(vc_uu,main="Number of airports of USAirnet\n Directed and Weighted"
     , xlab="Year",ylab="Number of airports",xaxt="n")
par(las=3)
axis(1, at=seq.int(1,301,60), labels=seq.int(1993,2018,5))


plot(ge_uu,main="Global efficiency of USAirnet\n Directed and Weighted"
     , xlab="Year",ylab="Global Efficiency",xaxt="n")
par(las=3)
axis(1, at=seq.int(1,301,60), labels=seq.int(1993,2018,5))


file_names2 <- sapply(1993:2017,function(year){sprintf('flight_data_%i_1',year)})


data_vul_dw <- sapply(file_names2,function(file_name,vul_dw){
  x <- vul_dw[[file_name]]
  return(c(min=min(x),w_min=names(x)[which.min(x)],max=max(x),w_max=names(x)[which.max(x)]
           , mean=mean(x),sd=sd(x)
           , q_1=quantile(x,prob=0.1)[[1]], q_2=quantile(x,prob=0.2)[[1]], q_3=quantile(x,prob=0.3)[[1]]
           , q_4=quantile(x,prob=0.4)[[1]], q_5=quantile(x,prob=0.5)[[1]], q_6=quantile(x,prob=0.6)[[1]]
           , q_7=quantile(x,prob=0.7)[[1]], q_8=quantile(x,prob=0.8)[[1]], q_9=quantile(x,prob=0.9)[[1]]
           , q_25=quantile(x,prob=0.25)[[1]], q_75=quantile(x,prob=0.75)[[1]], q_95=quantile(x,prob=0.95)[[1]]
           , q_99=quantile(x,prob=0.99)[[1]]))
},vul_dw)
data_vul_dw <- t(data_vul_dw)

plot(1993:2017,data_vul_dw[,"mean"],type="l")
plot(1993:2017,data_vul_dw[,"max"],type="l"
     , ylim=c(0.002,0.1), log="y"
     , xlab="Year", ylab="Vulnerability"
     , main="Distribution of Vulnerability over the years")
points(1993:2017,data_vul_dw[,"q_1"],type="l")
points(1993:2017,data_vul_dw[,"q_2"],type="l")
points(1993:2017,data_vul_dw[,"q_3"],type="l")
points(1993:2017,data_vul_dw[,"q_4"],type="l")
points(1993:2017,data_vul_dw[,"q_5"],type="l")
points(1993:2017,data_vul_dw[,"q_6"],type="l")
points(1993:2017,data_vul_dw[,"q_7"],type="l")
points(1993:2017,data_vul_dw[,"q_8"],type="l")
points(1993:2017,data_vul_dw[,"q_9"],type="l")
points(1993:2017,data_vul_dw[,"q_95"],type="l")
points(1993:2017,data_vul_dw[,"q_99"],type="l")

# Data for plotting 
flight_nodes_dw <- new.env()
for(file_name in file_names2){
  airports_data <- filter(us_coordinates,AIRPORT%in%V(flight_graphs_dw[[file_name]])$name)
  airports_data <- airports_data[sort(airports_data[,"AIRPORT"],index.return=TRUE)$ix,]
  
  airports_table <- table(airports_data[,"AIRPORT"])
  airports_classes <- cumsum(as.vector(airports_table))
  
  nodes <- data.frame(0)
  rownames(nodes) <- c("AIRPORT")
  nodes <- add_column(nodes,"LONGITUDE")
  nodes <- add_column(nodes,"LATITUDE")
  nodes <- add_column(nodes,"VULNERABILITY")
  nodes <- add_column(nodes,"DEPARTURES")
  colnames(nodes) <- c("AIRPORT","LATITUDE","LONGITUDE","VULNERABILITY","DEPARTURES")
  for (class in 1:length(airports_classes)){
    nodes[class,1:3] <- airports_data[airports_classes[class],]
  }# for
  nodes[,4] <- vul_dw[[file_name]] 
  nodes[,5] <- flight_passengers[[file_name]]
  flight_nodes_dw[[file_name]] <- nodes[,c("AIRPORT","LONGITUDE","LATITUDE","VULNERABILITY","DEPARTURES")]
}# for


# Plotting the airports on the map of the USA
years <- 1993:2017
names(years) <- file_names2
usmap <- get_map(location=c("United states"), zoom = 4)
for (file_name in file_names2){
  p <- ggmap(usmap)
  p <- p + ggtitle(sprintf('USAirnet: Directed - Weighted (%i)',years[file_name])) + xlab("Longitude") + ylab("Latitude")
  
  # p <- p + geom_line(data=flight_data[[file_name]])
  p <- p + geom_point(data=flight_nodes_dw[[file_name]],aes(x=as.numeric(LONGITUDE)
                                                            , y=as.numeric(LATITUDE)
                                                            , size=DEPARTURES
                                                            , colour=VULNERABILITY))
  
  
  p <- p + scale_color_gradient(low="blue", high="red")
  p <- p + scale_y_continuous(limits=c(24,51))
  
  ggsave(filename=sprintf('vul_%i.pdf',years[file_name]),plot=p,device="pdf",path="USAirnet/pdf/")
}# for






# # Making a transport matrix
# edges_usairnet <- edges_usairnet[-c(3157,3695),]
# w_usairnet <- l_usairnet - l_usairnet
# for (pointer in 1:length(edges_usairnet[,1])){
#   w_usairnet[edges_usairnet[pointer,1],edges_usairnet[pointer,2]] <- edges_usairnet[pointer,3]
# }# for


