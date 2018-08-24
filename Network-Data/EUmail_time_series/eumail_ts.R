source("system/initialization.R")
source("system/parallel_programming.R")
source("system/functions.R")

# EUmail_ts is a time series of EU mailing networks. The nodes are persones and
# edges are email connections. The number of nodes and edges changes over time.
# For more information, see
# https://snap.stanford.edu/data/email-Eu-core-temporal.html.

# Period length for one graph, in seconds. High period length means less graphs
time_period <- 3600*24*30

# Reading data into R, group it by TIME (with period is time_period) and count
# the number of mails per edge
edges_eumail_ts_c <- read.table("Network-Data/EUMail_time_series/raw_data/email-Eu-core-temporal.txt") %>%
  normalize_edgelist() %>%
  arrange(V3) %>%
  mutate(V3 %/% time_period + 1) %>%
  `colnames<-`(c("FROM","TO","TIME","GROUP")) %>%
  group_by(GROUP) %>%
  select(FROM,TO,GROUP) %>%
  arrange(FROM,TO) %>%
  group_by(GROUP,FROM,TO) %>%
  summarise(SIZE = sum(FROM/FROM))

# Making edge lists for every graph, with FROM, TO and SIZE. Also making nodes
# lists with the vertex ID's, such that vertex ID's can be compared between
# different graphs
edges_eumail_ts <- new.env()
nodes_eumail_ts <- new.env()
names_eumail_ts <- c()
for (period in 1:max(edges_eumail_ts_c[,"GROUP"])){
  names_eumail_ts <- c(names_eumail_ts,sprintf("eumail_ts_%i",period))
  edges_eumail_ts[[names_eumail_ts[period]]] <- filter(edges_eumail_ts_c,GROUP==period) %>%
    ungroup() %>%
    select(FROM,TO,SIZE) %T>%
    write.csv(sprintf("Network-Data/EUmail_time_series/cooked_data/edges_eumail_ts_%i.csv",period),row.names = FALSE)
  nodes_eumail_ts[[names_eumail_ts[period]]] <- as.data.frame(sort(unique(c(unique(edges_eumail_ts[[names_eumail_ts[period]]]$FROM)
                                                       ,unique(edges_eumail_ts[[names_eumail_ts[period]]]$TO))))) %>%
    `colnames<-`(c("NODE")) %T>%
    write.csv(sprintf("Network-Data/EUmail_time_series/cooked_data/nodes_eumail_ts_%i.csv",period),row.names = FALSE)
}# for
names_eumail_ts %<>% as.data.frame() %>%
  `colnames<-`(c("NAME")) %T>%
  write.csv("Network-Data/EUmail_time_series/cooked_data/names_eumail_ts.csv", row.names = FALSE)









