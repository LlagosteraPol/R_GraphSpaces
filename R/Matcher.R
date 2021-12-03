
the_dis <- function(obj){
  UseMethod("the_dis")
}

the_sim <- function(obj){
  UseMethod("the_sim")
}

get_Instance <- function(obj){
  UseMethod("get_Instance")
}

the_dis.matcher <- function(obj){
  g1 <- obj[0]
  g2 <- obj[1]
  g1_coords <- cbind(vertex_attr(g1, 'coordx'), vertex_attr(g1, 'coordy'))
  colnames(g1_coords) <- c('coordx', 'coordy')
  g2_coords <- cbind(vertex_attr(g2, 'coordx'), vertex_attr(g2, 'coordy'))
  colnames(g2_coords) <- c('coordx', 'coordy')
  
  if(length(g1_coords) < length(g2_coords)){
    g1_coords <- rbind(g1_coords, matrix(data = 0, ncol = 2, nrow = length(g1_coords) - length(g2_coords)))
  }else if(length(g1_coords) > length(g2_coords)){
    g2_coords <- rbind(g2_coords, matrix(data = 0, ncol = 2, nrow = length(g2_coords) - length(g1_coords)))
  }
  
  for(n in length(g1_coords)){
    dis = dis + self.measure.node_dis(V(g1)[n], V(g2)[n])
  }
  
  dis
}


dis <- function(obj){
  if(length(obj) == 1){
    print("TODO: implement when given only one graph")
  }else{
    if(length(obj[1] > 1)){
      print("TODO: implement when second argument is a set of graphs")
    }else{
      the_dis(args[0], args[1])
    }
  }
}