source("system/initialization.R")
source("system/parallell_programming.R")
source("system/functions.R")

source("functions/measures_distance.R")
setwd("functions/OACC");source("scripts/BDM2D.R");setwd("../../")
source("functions/tests_distance.R")
source("functions/analyse.R")

source("Network-Data/NLrail/nlrail_functions.R")

# Loading data
edges_nlrail <- read.csv("Network-Data/NLrail/cooked_data/edges_nlrail.csv", stringsAsFactors = FALSE)
nodes_nlrail <- read.csv("Network-Data/NLrail/cooked_data/nodes_nlrail.csv", stringsAsFactors = FALSE)
adj_nlrail_uu <- read.csv("Network-Data/NLrail/cooked_data/adj_nlrail_uu.csv", stringsAsFactors = FALSE) %>%
  as.matrix() %>%
  `row.names<-`(nodes_nlrail$NODE) %>%
  `colnames<-`(nodes_nlrail$NODE)
l_nlrail <- read.csv("Network-Data/NLrail/cooked_data/l_nlrail.csv", stringsAsFactors = FALSE) %>%
  as.matrix() %>%
  `row.names<-`(nodes_nlrail$NODE) %>%
  `colnames<-`(nodes_nlrail$NODE)

# Initializing networks
nlrail_uu <- graph_from_data_frame(edges_nlrail,directed=FALSE,vertices=nodes_nlrail$NODE)
nlrail_uw <- graph_from_adjacency_matrix(adj_nlrail_uu*l_nlrail,mode="undirected",weighted=TRUE)

cl <- start_cluster()

# Calculating vulnerabilities
vul_ge_nlrail <- matrix(0,nrow=vcount(nlrail_uu),ncol=3) %>%
  `colnames<-`(c("NODE","vul_ge_uu","vul_ge_uw")) %>%
  as.matrix()
vul_ge_nlrail$NODE <- nodes_nlrail$NODE
vul_ge_nlrail$vul_ge_uu <- as.data.frame(vulnerability_nodes(cl,nlrail_uu,performance=global_efficiency_unpar))
vul_ge_nlrail$vul_ge_uw <- as.data.frame(vulnerability_nodes(cl,nlrail_uw,l=l_nlrail,performance=global_efficiency_unpar))
write.csv(.,file="Network-Data/NLrail/data/vul_ge.csv", row.names = FALSE)


# Plotting maps and histogram
for (vul_ge_suffix in c("vul_ge_uu","vul_ge_uw")){
  nodes_nlrail$VALUE <- vul_ge_nlrail[,vul_ge_suffix]
  plot_nlrail(nodes_nlrail,file_name=vul_ge_suffix,v_name=vul_ge_suffix)
}# for


# Calculating samples of network
analyse_sample_performance(cl,nlrail_uu,counter=100,sizes=seq.int(10,390,10)
                           , use_cluster = FALSE
                           , use_ego = TRUE
                           , file_path = "Network-Data/NLrail/pdf/vul_ge_uu_sample_r"
                           , netname = "NLrail - Unweighted"
                           , p_name = "Global Efficiency"
                           , performance = global_efficiency_unpar)

analyse_sample_performance(cl,nlrail_uu,counter=100,sizes=seq.int(1,40,1)
                           , use_cluster = TRUE
                           , use_ego = TRUE
                           , file_path = "Network-Data/NLrail/pdf/vul_ge_uu_sample_ce"
                           , netname = "NLrail - Unweighted"
                           , p_name = "Global Efficiency"
                           , performance = global_efficiency_unpar)

analyse_sample_performance(cl,nlrail_uu,counter=100,sizes=seq.int(1,40,1)
                           , use_cluster = TRUE
                           , use_ego = FALSE
                           , file_path = "Network-Data/NLrail/pdf/vul_ge_uu_sample_cu"
                           , netname = "NLrail - Unweighted"
                           , p_name = "Global Efficiency"
                           , performance = global_efficiency_unpar)

analyse_sample_performance(cl,nlrail_uw,counter=100,sizes=seq.int(10,390,10)
                           , l = l_nlrail
                           , use_cluster = FALSE
                           , use_ego = TRUE
                           , file_path = "Network-Data/NLrail/pdf/vul_ge_uw_sample_r"
                           , netname = "NLrail - Weighted"
                           , p_name = "Global Efficiency"
                           , performance = global_efficiency_unpar)

analyse_sample_performance(cl,nlrail_uw,counter=100,sizes=seq.int(1,40,1)
                           , l = l_nlrail
                           , use_cluster = TRUE
                           , use_ego = TRUE
                           , file_path = "Network-Data/NLrail/pdf/vul_ge_uw_sample_ce"
                           , netname = "NLrail - Weighted"
                           , p_name = "Global Efficiency"
                           , performance = global_efficiency_unpar)

analyse_sample_performance(cl,nlrail_uw,counter=100,sizes=seq.int(1,40,1)
                           , l = l_nlrail
                           , use_cluster = TRUE
                           , use_ego = FALSE
                           , file_path = "Network-Data/NLrail/pdf/vul_ge_uw_sample_cu"
                           , netname = "NLrail - Weighted"
                           , p_name = "Global Efficiency"
                           , performance = global_efficiency_unpar)


# (Relative) Kolmogorov Complexity 
clusterCall(cl,function(){setwd("functions/OACC");source("scripts/BDM2D.R");setwd("../../")})

vec <- kolmogorov_norm(cl,vcount(nlrail_uu),ecount(nlrail_uu),FALSE,1e3)
k <- kolmogorov(nlrail,1)
k_relc(k/(vec[1]-vec[2]),k/vec[1],k/(vec[1]+vec[2]))




