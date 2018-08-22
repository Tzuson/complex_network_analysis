source("system/initialization.R")
source("system/parallell_programming.R")
source("system/functions.R")

source("functions/measures_distance.R")
source("functions/tests_distance.R")
source("functions/analyse.R")

source("NSnet/nsnet.R")

cl <- start_cluster(sources=c("system/initialization.R","system/functions.R","functions/measures_distance.R"))
clusterCall(cl,function(){setwd("functions/OACC");source("scripts/BDM2D.R");setwd("../../")})









analyse_sample_performance2(cl,uspowernet,NULL,NULL,100,seq.int(500,4500,500),FALSE,FALSE
                            ,"USPowernet/pdf/USPowernet_randomsample_r_test"
                            ,"USPowernet","Global Efficiency", global_efficiency_unpar)