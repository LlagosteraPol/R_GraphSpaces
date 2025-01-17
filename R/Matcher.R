# TODO: Finish implementation
# TODO: Review the documentation and upgrade it with better explanations
# TODO: Check wich functions needs to be exported

node_dis <- function(obj, x, y){
  UseMethod("node_dis")
}


node_sim <- function(obj){
  UseMethod("node_sim")
}


edge_dis <- function(obj){
  UseMethod("edge_dis")
}


edge_sim <- function(obj){
  UseMethod("edge_sim")
}


the_dis <- function(obj){
  UseMethod("the_dis")
}


the_sim <- function(obj){
  UseMethod("the_sim")
}


get_Instance <- function(obj){
  UseMethod("get_Instance")
}


#' Computes the distance between two networks. Allows different type of attributes on nodes and edges.
#' 
#' @name the_dis.matcher
#' 
#' @param obj list with two networks 
#' 
#' @return distance between the given networks
#' 
the_dis.matcher <- function(obj){
  # TODO: Finish implementation
  # TODO: Check class of obj
  
  g1 <- obj[[1]]
  g2 <- obj[[2]]
  dis <- 0
  n <- gorder(g1)
  
  g1_coords <- cbind(vertex_attr(g1, 'xcoord'), vertex_attr(g1, 'ycoord'))
  colnames(g1_coords) <- c('xcoord', 'ycoord')
  g2_coords <- cbind(vertex_attr(g2, 'xcoord'), vertex_attr(g2, 'ycoord'))
  colnames(g2_coords) <- c('xcoord', 'ycoord')
  
  if(length(g1_coords) < length(g2_coords)){
    g1_coords <- rbind(g1_coords, matrix(data = 0, ncol = 2, nrow = length(g1_coords) - length(g2_coords)))
  }else if(length(g1_coords) > length(g2_coords)){
    g2_coords <- rbind(g2_coords, matrix(data = 0, ncol = 2, nrow = length(g2_coords) - length(g1_coords)))
  }
  
  for(n in 1:n){
    dis = dis + node_dis(obj, 
                         vertex_attr(graph = g1, index = V(g1)[n]), 
                         vertex_attr(graph = g2, index = V(g2)[n]))
    #for(j in 1:n){
      
    #}
  }
  dis = dis + abs(gsize(g1) - gsize(g2))
  dis
}


#' Computes the distance between two networks. The type of distance depends on the class of the given object. Allows different type 
#' of attributes on nodes and edges.
#' 
#' @name dis
#' 
#' @param obj list with two networks 
#' 
#' @return distance between the given networks
#' 
dis <- function(obj){
  # TODO: Finish implementation for list of graphs
  if(length(obj) == 1){
    print("TODO: implement when given only one graph")
  }else{
    if(length(obj[1]) > 1){
      print("TODO: implement when second argument is a set of graphs")
    }else{
      the_dis(obj)
    }
  }
}
