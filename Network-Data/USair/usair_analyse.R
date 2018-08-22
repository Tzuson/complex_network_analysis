source("system/initialization.R")
source("system/parallell_programming.R")
source("system/functions.R")

source("functions/measures_distance.R")
source("functions/tests_distance.R")

source("Network-Data/USair/usair_functions.R")

# Names of matrices to be loaded (and used)
files_usair <- c("adj_usair_du", "l_usair", "w_usair"
                 , "t_usair_du_min", "t_usair_dw_min"
                 , "t_usair_uu_min", "t_usair_uw_min"
                 , "t_usair_du_sum", "t_usair_dw_sum"
                 , "t_usair_uu_sum", "t_usair_uw_sum")
for (file_usair in files_usair){
  assign(file_usair, load_usair(file_usair))
}# for

# Loading nodes and edges lists
edges_usair <- read.csv("Network-Data/USair/cooked_data/edges_usair.csv", stringsAsFactors = FALSE) 
nodes_usair <- read.csv("Network-Data/USair/cooked_data/nodes_usair.csv", stringsAsFactors = FALSE) 

# Initiating the graphs
usair_uu <- graph_from_adjacency_matrix(adj_usair_du,mode="max",add.rownames = TRUE)
usair_du <- graph_from_adjacency_matrix(adj_usair_du,mode="directed",add.rownames = TRUE)
usair_uw <- graph_from_adjacency_matrix(adj_usair_du*l_usair,mode="max",weighted=TRUE,add.rownames = TRUE)
usair_dw <- graph_from_adjacency_matrix(adj_usair_du*l_usair,mode="directed",weighted=TRUE,add.rownames = TRUE)

# Calculating vulnerabilities for the different versions
# Initializing data frame for vulnerabilities
names_usair <- sort(V(usair_uu)$name)
vul_ge_suffix_names <- c("vul_ge_uu","vul_ge_du","vul_ge_uw","vul_ge_dw"
                         ,"vul_ge_uu_min","vul_ge_du_min","vul_ge_uw_min","vul_ge_dw_min"
                         ,"vul_ge_uu_sum","vul_ge_du_sum","vul_ge_uw_sum","vul_ge_dw_sum")
vul_ge_usair <- matrix(0,nrow=vcount(usair_uu),ncol=(length(vul_ge_suffix_names)+1)) %>%
  `colnames<-`(c("node",vul_ge_suffix_names)) %>%
  as.data.frame()

# Calculating, plotting and storing vulnerabilities
# Vulnerabilities without transport
vul_ge_usair[,"vul_ge_uu"] <- vulnerability_nodes(cl,usair_uu,performance=global_efficiency_unpar)
vul_ge_usair[,"vul_ge_du"] <- vulnerability_nodes(cl,usair_du,performance=global_efficiency_unpar)
vul_ge_usair[,"vul_ge_uw"] <- vulnerability_nodes(cl,usair_uw,l=l_usair,performance=global_efficiency_unpar)
vul_ge_usair[,"vul_ge_dw"] <- vulnerability_nodes(cl,usair_dw,l=l_usair,performance=global_efficiency_unpar)

# Vulnerabilities with transport (sum)
vul_ge_usair[,"vul_ge_uu_sum"] <- vulnerability_nodes(cl,usair_uu,t=t_usair_uu_sum,performance=global_efficiency_unpar)
vul_ge_usair[,"vul_ge_du_sum"] <- vulnerability_nodes(cl,usair_du,t=t_usair_du_sum,performance=global_efficiency_unpar)
vul_ge_usair[,"vul_ge_uw_sum"] <- vulnerability_nodes(cl,usair_uw,l=l_usair,t=t_usair_uw_sum,performance=global_efficiency_unpar)
vul_ge_usair[,"vul_ge_dw_sum"] <- vulnerability_nodes(cl,usair_dw,l=l_usair,t=t_usair_dw_sum,performance=global_efficiency_unpar)

# Vulnerabilities with transport (min)
vul_ge_usair[,"vul_ge_uu_min"] <- vulnerability_nodes(cl,usair_uu,t=t_usair_uu_min,performance=global_efficiency_unpar)
vul_ge_usair[,"vul_ge_du_min"] <- vulnerability_nodes(cl,usair_du,t=t_usair_du_min,performance=global_efficiency_unpar)
vul_ge_usair[,"vul_ge_uw_min"] <- vulnerability_nodes(cl,usair_uw,l=l_usair,t=t_usair_uw_min,performance=global_efficiency_unpar)
vul_ge_usair[,"vul_ge_dw_min"] <- vulnerability_nodes(cl,usair_dw,l=l_usair,t=t_usair_dw_min,performance=global_efficiency_unpar)

write.csv(vul_ge_usair,file="Network-Data/USair/data/vul_ge_usair.csv", row.names=FALSE)

# Plotting maps and histogram
for (vul_ge_suffix in vul_ge_suffix_names){
  nodes_usair$VALUE <- vul_ge_usair[,vul_ge_suffix]
  plot_usair(nodes_usair,file_name=vul_ge_suffix,v_name=vul_ge_suffix)
}# for


















