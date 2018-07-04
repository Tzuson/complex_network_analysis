source("initialization.R")
source("data/nsnet.R")
source("functions/functions.R")
source("functions/measures.R")
source("functions/tests.R")



#measures(nsnet,global_efficiency)

#nsnet_plot(3*local_efficiency(nsnet))

print(global_efficacy(nsnet))
print(global_efficiency(nsnet))

nsnet_plot(10*nodes, 10*edges)


