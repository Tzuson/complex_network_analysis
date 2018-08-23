source("system/initialization.R")
source("system/parallell_programming.R")
source("system/functions.R")

cl <- start_cluster()

# We first need the names of the files with data
names_eumail_ts <- read.csv("Network-Data/EUmail_time_series/cooked_data/names_eumail_ts.csv", stringsAsFactors = FALSE)

# Making an environment with the graphs
eumail_ts_uu <- new.env()
eumail_ts_du <- new.env()
for (name_eumail_ts in names_eumail_ts$NAME){
  eumail_ts_uu[[name_eumail_ts]] <- read.csv(sprintf("Network-Data/EUmail_time_series/cooked_data/edges_%s.csv"
                                                     ,name_eumail_ts),stringsAsFactors = FALSE) %>%
    graph_from_data_frame(.,directed=FALSE
                          , vertices=read.csv(sprintf("Network-Data/EUmail_time_series/cooked_data/nodes_%s.csv"
                                                                   ,name_eumail_ts),stringsAsFactors = FALSE))
  eumail_ts_du[[name_eumail_ts]] <- read.csv(sprintf("Network-Data/EUmail_time_series/cooked_data/edges_%s.csv"
                                                     ,name_eumail_ts),stringsAsFactors = FALSE) %>%
    graph_from_data_frame(.,directed=TRUE
                          , vertices=read.csv(sprintf("Network-Data/EUmail_time_series/cooked_data/nodes_%s.csv"
                                                      ,name_eumail_ts),stringsAsFactors = FALSE))
}# for
