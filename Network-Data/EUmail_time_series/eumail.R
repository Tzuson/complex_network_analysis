source("system/initialization.R")
source("system/parallell_programming.R")
source("system/functions.R")

source("functions/measures_distance.R")
source("functions/measures_topology.R")
source("functions/tests_distance.R")

time_period <- 3600*24*7

# Reading data into R and making it an undirected graph
eumail_ts <- read.table("Network-Data/EUMail_time_series/raw_data/email-Eu-core-temporal.txt") %>%
  normalize_edgelist() %>%
  arrange(V3) %>%
  mutate(V3 %/% time_period + 1) %>%
  `colnames<-`(c("FROM","TO","TIME","PERIOD")) %>%
  group_by(PERIOD) %>%
  select(FROM,TO,PERIOD) %>%
  arrange(FROM,TO) %>%
  group_by(PERIOD,FROM,TO) %>%
  summarise(MAILS = sum(FROM/FROM))

# nodes_eumail_ts <- sort(unique(c(eumail_ts$FROM,eumail_ts$TO)))

eumail_ts_graphs_uu <- new.env()
eumail_ts_names <- c()
for (time_period in 1:max(eumail_ts[,"PERIOD"])){
  eumail_ts_names <- c(eumail_ts_names,sprintf("eumail_ts_%i",time_period))
  eumail_ts_graphs_uu[[eumail_ts_names[time_period]]] <- filter(eumail_ts,PERIOD == time_period) %>%
    ungroup() %>%
    select(FROM,TO) %>%
    graph_from_data_frame(directed = FALSE)
}# for


eumail_ge <- parSapply(cl,eumail_ts_names,function(eumail_ts_name,eumail_ts_graphs_uu){
  global_efficiency_unpar(eumail_ts_graphs_uu[[eumail_ts_name]])
},eumail_ts_graphs_uu)# parSapply




