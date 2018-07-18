source("system/initialization.R")
source("system/parallell_programming.R")

source("functions/functions.R")
source("functions/measures.R")
source("functions/tests.R")

source("NSnet/nsnet.R")

nsnet <- initialize_nsnet(FALSE)
L <- initialize_nsnet(TRUE)
nsnet_weighted <- L[[1]]
l_nsnet <- L[[2]]

cl <- start_cluster(sources=c("system/initialization.R","functions/measures.R","functions/tests.R"))


measures(cl,nsnet_weighted,global_efficiency,l_nsnet)



cl <- stop_cluster(cl)



