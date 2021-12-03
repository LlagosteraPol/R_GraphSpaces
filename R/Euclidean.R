euclidean <- function(obj){
  UseMethod("euclidean")
}

euclidean <- function(obj){
  attr(obj, 'class') <- c(class(obj), "euclidean")
  return(obj)
}

the_dis.euclidean <-function(obj){
  x <- obj.x
  y <- obj.y
  
  if(length(x) > length(y)){
    y <- c(y, rep(0,length(y) - length(x)))
  }else{
    x <- c(x, rep(0,length(x) - length(y)))
  }
  sum((x - y)^2)
}


the_sim.euclidean <-function(obj){
  x <- obj.x
  y <- obj.y
}

node_dis <- function(obj){
  UseMethod("node_dis")
}

node_dis.euclidean <-function(obj){
  the_dis(obj)
}


node_sim <- function(obj){
  UseMethod("node_sim")
}

node_sim.euclidean <-function(obj, x, y){
  the_sim(obj, x, y)
}


edge_dis <- function(obj){
  UseMethod("edge_dis")
}

edge_dis.euclidean <-function(obj, x, y){
  the_dis(obj, x, y)
}


edge_sim <- function(obj){
  UseMethod("edge_sim")
}

edge_sim.euclidean <-function(obj, x, y){
  the_sim(obj, x, y)
}

get_Instance.euclidean <-function(obj, x, y){
  return("euclidean")
}