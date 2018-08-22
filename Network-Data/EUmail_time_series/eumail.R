source("system/initialization.R")
source("system/parallell_programming.R")
source("system/functions.R")

source("functions/measures_distance.R")
source("functions/measures_topology.R")
source("functions/tests_distance.R")

period <- 3600*24*30

# Reading data into R and making it a directed graph
eumail <- read.table("Network-Data/EUMail/raw_data/email-Eu-core-temporal.txt") %>%
  normalize_edgelist() %>%
  arrange(V3) %>%
  group_by(V3 %/% (period)) %>%
  `colnames<-`(c("FROM","TO","TIME","GROUP"))

eumail_graphs_uu <- new.env
eumail_names <- c()
for ()




as.matrix(.[,c("FROM","TO")]) 
graph_from_edgelist() 
as.undirected(mode="collapse")
  


uspowernet <-  read.table("http://opsahl.co.uk/tnet/datasets/USpowergrid_n4941.txt"
                          ,stringsAsFactors = FALSE) %>% 
  subset(select = c("V1","V2")) %>%
  `colnames<-`(c("FROM","TO")) %>% 
  as.matrix() %>%
  graph_from_edgelist() %>%
  as.undirected(mode="collapse")






read.csv("USAirnet/flight_data.csv",stringsAsFactors = FALSE)