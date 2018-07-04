source("initialization.R")
source("functions/measures.R")
source("functions/tests.R")

testgraph <- graph_from_literal(A,B-C-D,B-E-D)
plot(testgraph)

global_efficacy(testgraph)

