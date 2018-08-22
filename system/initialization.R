# Used packages
suppressPackageStartupMessages({
  # Graph calculations and parallel computing
  library(igraph)
  library(parallel)
  library(doParallel)
  
  # Data manipulation and mutating into usable node- and edgelists
  library(tidyverse)
  library(magrittr)
  
  # Import of data from the internet and basic cleaning
  library(stringdist)
  library(rvest)
  library(xml2)
  library(tnet)
  
  # Plotting some networks on maps 
  library(ggmap)
  library(mapproj)
})






