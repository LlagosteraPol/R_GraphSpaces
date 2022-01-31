# ToDo: Maybe no need to create this class, can be done as function of 'Euclidean'

sqeuclidean <- function(obj){
  if(is(obj, "matcher")){
    if(length(class(obj)) >= 3){
      stop("The class is already initialized.")
    }else{
      attr(obj, 'class') <- c(class(obj), "sqeuclidean")
      return(obj) # class = c(matcher', <matcher_sub_class>, 'sqeuclidean')
    }
  }else{
    stop("The object needs to belong to one of the 'matcher' type classes.")
  }
}


node_dis.euclidean <-function(obj, x, y){
  sqrt(eu_the_dis(x, y))
}


node_sim.sqeuclidean <-function(obj, x, y){
  eu_the_sim(obj, x, y)
}


edge_dis.sqeuclidean <-function(obj, x, y){
  sqrt(eu_the_dis(x, y))
}


edge_sim.sqeuclidean <-function(obj, x, y){
  eu_the_sim(x, y)
}


get_Instance.sqeuclidean <-function(obj){
  return("sqeuclidean")
}