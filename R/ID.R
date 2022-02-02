source("./R/Matcher.R")


#' Subclass of Matcher. It is used to compute the Identity match between two networks.
#' 
#' @name ID
#' 
#' @param x first network (igraph class type)
#' @param y second network (igraph class type)
#' 
#' @return list of the given networks with class type c('matcher', 'Id')
#' 
ID <- function(x, y){
  network_lst <- list(x, y)
  attr(network_lst, "class") <- c(attr(network_lst, "class"), "matcher", "Id")
  return(network_lst)
}