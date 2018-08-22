source("system/initialization.R")
source("system/parallell_programming.R")
source("system/functions.R")

cl <- start_cluster()

# We first need the names of the files with data
eumail_ts_names <- read.csv("Network-Data/EUmail_time_series/cooked_data/eumail_ts_names.csv", stringsAsFactors = FALSE)

# Making an environment with the graphs
eumail_ts_uu <- new.env()
eumail_ts_du <- new.env()
for (eumail_ts_name in eumail_ts_names$NAME){
  eumail_ts_uu[[eumail_ts_name]] <- read.csv(sprintf("Network-Data/EUmail_time_series/cooked_data/edges_%s.csv"
                                                     ,eumail_ts_name),stringsAsFactors = FALSE) %>%
    graph_from_data_frame(.,directed=FALSE
                          , vertices=read.csv(sprintf("Network-Data/EUmail_time_series/cooked_data/nodes_%s.csv"
                                                                   ,eumail_ts_name),stringsAsFactors = FALSE))
  eumail_ts_du[[eumail_ts_name]] <- read.csv(sprintf("Network-Data/EUmail_time_series/cooked_data/edges_%s.csv"
                                                     ,eumail_ts_name),stringsAsFactors = FALSE) %>%
    graph_from_data_frame(.,directed=TRUE
                          , vertices=read.csv(sprintf("Network-Data/EUmail_time_series/cooked_data/nodes_%s.csv"
                                                      ,eumail_ts_name),stringsAsFactors = FALSE))
}# for
