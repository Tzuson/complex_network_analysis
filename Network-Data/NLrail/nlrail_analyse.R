source("system/initialization.R")
source("system/parallell_programming.R")
source("system/functions.R")

source("functions/measures_distance.R")
setwd("functions/OACC");source("scripts/BDM2D.R");setwd("../../")
source("functions/tests_distance.R")
source("functions/analyse.R")


source("Network-Data/NLrail/nlrail.R")

cl <- start_cluster(sources=c("system/initialization.R","functions/measures_distance.R"))

# Initializing networks
L <- initialize_nlrail(FALSE)
nlrail_uu <- L[[1]]
L <- initialize_nlrail(TRUE)
nlrail_uw <- L[[1]]
l_nlrail <- L[[2]]

# Calculating and plotting vulnerabilities
nodes_uu <- as.data.frame(vulnerability_nodes(cl,nlrail_uu,performance=global_efficiency_unpar)) %>%
  `colnames<-`(c("Vulnerability")) %>%
  `row.names<-`(V(nlrail_uu)$name) %T>%
  write.csv(.,file="Network-Data/NLrail/data/vulnerability_uu.csv") %T>%
  plot_nlrail(nlrail_uu,.,"Network-Data/NLrail/pdf","NLrail_uu","NLrail - Unweighted")
  
nodes_uw <- as.data.frame(vulnerability_nodes(cl,nlrail_uw,l=l_nlrail,performance=global_efficiency_unpar)) %>%
  `colnames<-`(c("Vulnerability")) %>%
  `row.names<-`(V(nlrail_uw)$name) %T>%
  write.csv(file="Network-Data/NLrail/data/vulnerability_uw.csv") %T>%
  plot_nlrail(nlrail_uw,.,"Network-Data/NLrail/pdf","NLrail_uw","NLrail - Weighted")

# Calculating samples of network
analyse_sample_performance(cl,nlrail_uu,counter=100,sizes=seq.int(10,390,10)
                           , use_cluster = FALSE
                           , use_ego = TRUE
                           , file_path = "Network-Data/NLrail/pdf/NLrail_uu_sample_r"
                           , netname = "NLrail - Unweighted"
                           , p_name = "Global Efficiency"
                           , performance = global_efficiency_unpar)

analyse_sample_performance(cl,nlrail_uu,counter=100,sizes=seq.int(1,40,1)
                           , use_cluster = TRUE
                           , use_ego = TRUE
                           , file_path = "Network-Data/NLrail/pdf/NLrail_uu_sample_ce"
                           , netname = "NLrail - Unweighted"
                           , p_name = "Global Efficiency"
                           , performance = global_efficiency_unpar)

analyse_sample_performance(cl,nlrail_uu,counter=100,sizes=seq.int(1,40,1)
                           , use_cluster = TRUE
                           , use_ego = FALSE
                           , file_path = "Network-Data/NLrail/pdf/NLrail_uu_sample_cu"
                           , netname = "NLrail - Unweighted"
                           , p_name = "Global Efficiency"
                           , performance = global_efficiency_unpar)

analyse_sample_performance(cl,nlrail_uw,counter=100,sizes=seq.int(10,390,10)
                           , l = l_nlrail
                           , use_cluster = FALSE
                           , use_ego = TRUE
                           , file_path = "Network-Data/NLrail/pdf/NLrail_uw_sample_r"
                           , netname = "NLrail - Weighted"
                           , p_name = "Global Efficiency"
                           , performance = global_efficiency_unpar)

analyse_sample_performance(cl,nlrail_uw,counter=100,sizes=seq.int(1,40,1)
                           , l = l_nlrail
                           , use_cluster = TRUE
                           , use_ego = TRUE
                           , file_path = "Network-Data/NLrail/pdf/NLrail_uw_sample_ce"
                           , netname = "NLrail - Weighted"
                           , p_name = "Global Efficiency"
                           , performance = global_efficiency_unpar)

analyse_sample_performance(cl,nlrail_uw,counter=100,sizes=seq.int(1,40,1)
                           , l = l_nlrail
                           , use_cluster = TRUE
                           , use_ego = FALSE
                           , file_path = "Network-Data/NLrail/pdf/NLrail_uw_sample_cu"
                           , netname = "NLrail - Weighted"
                           , p_name = "Global Efficiency"
                           , performance = global_efficiency_unpar)


cl <- stop_cluster(cl)

# (Relative) Kolmogorov Complexity 
cl <- start_cluster(sources=c("system/initialization.R","functions/measures_distance.R"))
clusterCall(cl,function(){setwd("functions/OACC");source("scripts/BDM2D.R");setwd("../../")})

vec <- kolmogorov_norm(cl,vcount(nlrail_uu),ecount(nlrail_uu),FALSE,1e3)
k <- kolmogorov(nlrail,1)
k_relc(k/(vec[1]-vec[2]),k/vec[1],k/(vec[1]+vec[2]))




