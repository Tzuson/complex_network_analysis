source("system/initialization.R")
source("system/parallel_programming.R")
source("system/functions.R")

# We use a simple function for loading, because all matrices are loaded in the same way

#' @title Loads the appropriate matrix from "Network-Data/USair/cooked_data/"
#'
#' @description It loads the matrix from a .csv file, putting the colnames also
#'   in the row names and coercing it into a matrix.
#'
#' @param name A string naming the matrix in "Network-Data/USair/cooked_data/", without .csv
#' 
#' @result A matrix derived from the data frame loaded
#' 
#' @examples 
#' l_usair <- load_usair(l_usair)
load_usair <- function(name){
  m <- read.csv(sprintf('Network-Data/USair/cooked_data/%s.csv',name), stringsAsFactors = FALSE) %>%
    `row.names<-`(.,colnames(.)) %>% 
    as.matrix()
  return(m)
}# load_usair


# For plotting to work, we need a map of the US
map_usair <- get_map(location = c(-120,50),zoom=3)


#' @title Plot USair in specific way on map of USA
#'
#' @description Plots the airports with right geocoordinates on the map of the
#'   US. The colour of the nodes depend on the VALUE and goes from "blue"
#'   (lowest VALUE) to "red" (highest VALUE). The size is given by USAGE.
#'
#'   Furthermore, a histogram of the VALUE data is made, with binwidth
#'   \code{max(nodes$VALUE)-min(nodes$VALUE)/100}.
#'
#' @param nodes A data frame with for every node a NODE, LONGITUDE, LATITUDE, SIZE and
#'   VALUE parameter
#' @param file_path A string giving the file path without file_name - optional -
#' @param file_name A string giving the file name without extension - optional -
#' @param name A string giving the name of the graph used in the titles - optional -
#' @param v_name A string giving the name of the VALUE parameter - optional -
#'
#' @return A pdf with a plot of USair and a pdf with a histogram of the VALUE
#'   data on the nodes
#'   
#' @examples 
#' plot_usair(nodes_usair)
plot_usair <- function(nodes, file_path="Network-Data/USair/pdf",file_name="usair",name="USair",v_name="Value"){
  # Plotting and storing image of USair
  p <- ggmap(map_usair) +
    ggtitle(sprintf("USair %s",v_name)) + xlab("Longitude") + ylab("Latitude") +
    geom_point(data=nodes_usair,aes(x=LONGITUDE,y=LATITUDE,size=SIZE,colour=VALUE)) +
    scale_size_continuous(range = c(0.25,2.5)) +
    scale_color_gradient(low="blue", high="red") +
    scale_x_continuous(limits=c(-170,-70)) +
    scale_y_continuous(limits=c(15,70))
  ggsave(filename=sprintf('%s_map.pdf',file_name),plot=p,device="pdf",path=file_path)
  
  # Plotting and storing histogram of USair nodes data
  h <- ggplot(data=nodes_usair) +
    ggtitle("USair") + xlab(v_name) + ylab("Frequency") +
    geom_histogram(aes(x=VUL),binwidth=(max(nodes$VALUE)-min(nodes$VALUE)/100))
  ggsave(filename=sprintf('%s_hist.pdf',file_name),plot=h,device="pdf",path=file_path)
  
}# plot_usair