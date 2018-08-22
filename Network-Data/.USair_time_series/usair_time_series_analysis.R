source("system/initialization.R")
source("system/parallell_programming.R")
source("system/functions.R")

source("functions/measures_distance.R")
source("functions/tests_distance.R")
source("functions/analyse.R")

cl <- start_cluster(sources = c("system/initialization.R","functions/measures_distance.R","system/functions.R"))

# Calculating the global efficiency of the graphs
ge_uu <- sapply(files_usair_ts, function(file_usair_ts,usair_ts_uu){
  global_efficiency_unpar(g=usair_ts_uu[[file_usair_ts]])
},usair_ts_uu)# sapply

ge_du <- sapply(files_usair_ts, function(file_usair_ts,usair_ts_du){
  global_efficiency_unpar(g=usair_ts_du[[file_usair_ts]])
},usair_ts_uu)# sapply

ge_uw <- sapply(files_usair_ts, function(file_usair_ts,usair_ts_uw){
  global_efficiency_unpar(g=usair_ts_uw[[file_usair_ts]],l=l_usair_ts[[file_usair_ts]])
},usair_ts_uu)# sapply

ge_dw <- sapply(files_usair_ts, function(file_usair_ts,usair_ts_dw){
  global_efficiency_unpar(g=usair_ts_dw[[file_usair_ts]],l=l_usair_ts[[file_usair_ts]])
},usair_ts_uu)# sapply

# Calculating the vulnerabilities
vul_ge_usair_uu <- new.env()
for (file_usair_ts in files_usair_ts){
  vul_ge_usair_uu[[file_usair_ts]] <- vulnerability_nodes(cl,g=usair_ts_uu[[file_usair_ts]]
                                                          , performance=global_efficiency_unpar)
  names(vul_ge_usair_uu[[file_usair_ts]]) <- V(usair_ts_uu[[file_usair_ts]])$name
  vul_ge_usair_uu[[file_usair_ts]] <- vul_ge_usair_uu[[file_usair_ts]][sort(names(vul_ge_usair_uu[[file_usair_ts]]))] 
}# for

vul_ge_usair_du <- new.env()
for (file_usair_ts in files_usair_ts){
  vul_ge_usair_du[[file_usair_ts]] <- vulnerability_nodes(cl,g=usair_ts_du[[file_usair_ts]]
                                                          , performance=global_efficiency_unpar)
  names(vul_ge_usair_du[[file_usair_ts]]) <- V(usair_ts_du[[file_usair_ts]])$name
  vul_ge_usair_du[[file_usair_ts]] <- vul_ge_usair_du[[file_usair_ts]][sort(names(vul_ge_usair_du[[file_usair_ts]]))] 
}# for

vul_ge_usair_uw <- new.env()
for (file_usair_ts in files_usair_ts){
  vul_ge_usair_uw[[file_usair_ts]] <- vulnerability_nodes(cl,g=usair_ts_uw[[file_usair_ts]]
                                                          , l=l_usair_ts[[file_usair_ts]]
                                                          , performance=global_efficiency_unpar)
  names(vul_ge_usair_uw[[file_usair_ts]]) <- V(usair_ts_uw[[file_usair_ts]])$name
  vul_ge_usair_uw[[file_usair_ts]] <- vul_ge_usair_uw[[file_usair_ts]][sort(names(vul_ge_usair_uw[[file_usair_ts]]))] 
}# for

vul_ge_usair_dw <- new.env()
for (file_usair_ts in files_usair_ts){
  vul_ge_usair_dw[[file_usair_ts]] <- vulnerability_nodes(cl,g=usair_ts_dw[[file_usair_ts]]
                                                          , l=l_usair_ts[[file_usair_ts]]
                                                          , performance=global_efficiency_unpar)
  names(vul_ge_usair_dw[[file_usair_ts]]) <- V(usair_ts_dw[[file_usair_ts]])$name
  vul_ge_usair_dw[[file_usair_ts]] <- vul_ge_usair_dw[[file_usair_ts]][sort(names(vul_ge_usair_dw[[file_usair_ts]]))] 
}# for

for (file_usair_ts in files_usair_ts){
  nodes_usair_ts[[file_usair_ts]]$VUL <- vul_ge_usair_dw[[file_usair_ts]]
  nodes_usair_ts[[file_usair_ts]]$YEAR <- which(files_usair_ts == file_usair_ts)+1992
}# for

map_usair_ts <- get_map(location = "United States", zoom = 4)
p <- ggmap(map_usair_ts) +
  geom_point(data=ggnodes,aes(x=LONGITUDE,y=LATITUDE,size=PASSENGERS,colour=VUL)) +
  # scale_size_continuous(range = c(0.25,2.5)) +
  scale_color_gradient(low="blue", high="red") +
  # gganimate
  ggtitle('USair: {frame_time}') + xlab("Longitude") + ylab("Latitude") +
  transition_time(YEAR) +
  ease_aes('linear')
animate(p,nframes = 750,fps = 30)

ll <- as.list(nodes_usair_ts)
ggnodes <- rbind.fill(ll)















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


