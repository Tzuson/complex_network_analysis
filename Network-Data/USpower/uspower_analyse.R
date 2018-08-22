source("system/initialization.R")
source("system/parallell_programming.R")
source("system/functions.R")

source("functions/measures_distance.R")
setwd("functions/OACC");source("scripts/BDM2D.R");setwd("../../")
source("functions/tests_distance.R")
source("functions/analyse.R")

source("Network-Data/USpower/uspower_functions.R")

# Loading data from cooked_data
uspower_uu <- read.csv("Network-Data/USpower/cooked_data/edges_uspower.csv") %>%
  as.matrix() %>%
  graph_from_edgelist(directed=TRUE) %>%
  as.undirected(mode="collapse")

cl <- start_cluster()

# Calculating and plotting vulnerabilities
nodes_uu <- as.data.frame(V(uspower_uu)$name) %>%
  (.$VULNERABILITY <- vulnerability_nodes(cl,uspower_uu,performance=global_efficiency_unpar)) %>%
  `colnames<-`(c("NAME","VULNERABILITY")) %T>%
  write.csv(.,file="Network-Data/USpower/data/vulnerability_uu.csv") %T>%
  plot_uspower(uspower_uu,.$VULNERABILITY,function(x){10*x},file_name="vulnerability_uu")


# Calculating samples of network
analyse_sample_performance(cl,uspower_uu,counter=100,sizes=seq.int(10,vcount(uspower),10)
                           , use_cluster = FALSE
                           , use_ego = TRUE
                           , file_path = "Network-Data/USpower/pdf/ge_uu_sample_r"
                           , netname = "USpower"
                           , p_name = "Global Efficiency"
                           , performance = global_efficiency_unpar)

analyse_sample_performance(cl,uspower_uu,counter=100,sizes=seq.int(1,40,1)
                           , use_cluster = TRUE
                           , use_ego = TRUE
                           , file_path = "Network-Data/USpower/pdf/ge_uu_sample_ce"
                           , netname = "USpower"
                           , p_name = "Global Efficiency"
                           , performance = global_efficiency_unpar)

analyse_sample_performance(cl,uspower_uu,counter=100,sizes=seq.int(1,40,1)
                           , use_cluster = TRUE
                           , use_ego = FALSE
                           , file_path = "Network-Data/USpower/pdf/ge_uu_sample_cu"
                           , netname = "USpower"
                           , p_name = "Global Efficiency"
                           , performance = global_efficiency_unpar)


# (Relative) Kolmogorov Complexity 
clusterCall(cl,function(){setwd("functions/OACC");source("scripts/BDM2D.R");setwd("../../")})

vec <- kolmogorov_norm(cl,vcount(uspower_uu),ecount(uspower_uu),FALSE,1e2)
k <- kolmogorov(uspower_uu,1)
k_rel <- c(k/(vec[1]-vec[2]),k/vec[1],k/(vec[1]+vec[2]))




