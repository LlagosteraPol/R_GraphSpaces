euclidean <- function(obj){
  UseMethod("euclidean")
}

euclidean <- function(obj){
  if(is(obj, "matcher")){
    if(length(class(obj)) >= 3){
      stop("The class is already initialized.")
    }else{
      attr(obj, 'class') <- c(class(obj), "euclidean")
      return(obj) # class = c(matcher', <matcher_sub_class>, 'euclidean')
    }
  }else{
    stop("The object needs to belong to one of the 'matcher' type classes.")
  }
}

eu_the_dis <- function(x, y){
  # TODO: Finish implementation
  dis <- 0
  
  if(is.null(x) || is.null(y)){
    return(0)
  }
  
  if(is.null(x) && is.null(y)){
    print('Give me at least one non empty vector!')
    return(0)
  }
  # two lists
  if(typeof(x) == "list" && typeof(y) == "list"){
    nx <- length(x)
    ny <- length(y)
    n <- nx
    dis <- 0
    
    if( nx == 0 && ny == 0 ){
      return(0)
    }else if ( nx == 0 ){
      return(eu_the_sim(y, y))
    }else if (ny == 0){
      return(eu_the_sim(x, x))
    }else if(nx < ny){
      n <- ny
      x <- c(x, rep(0,length(y) - length(x)))
    }else if(nx > ny) {
      y <- c(y, rep(0,length(x) - length(y)))
    }
    for(i in 1:n){
      dis <- dis + (x[[i]] - y[[i]])^2
    }
    return(dis)
  }
  # One list one integer
  if(typeof(x) == "list" && typeof(y) != "list"){
    print("Needs implementation")
  }
  # One integer one list
  if(typeof(x) != "list" && typeof(y) == "list"){
    print("Needs implementation")
  }
  # two integer
  if(typeof(x) != "list" && typeof(y) != "list"){
    return( sum((x - y)^2) )
  }
  # TODO: Check when two equal arrays for pandas DF
}


eu_the_sim <- function(x, y){
  # TODO: Finish implementation
  if(is.null(x) || is.null(y)){
    return(0)
  }
  
  if(is.null(x) && is.null(y)){
    print('Give me at least one non empty vector!')
    return(0)
  }
  # two lists
  if(typeof(x) == "list" && typeof(y) == "list"){
    if(length(x) <= length(y)){
      x <- c(x, rep(0,length(y) - length(x)))
    }else{
      y <- c(y, rep(0,length(x) - length(y)))
    }
    print("Needs implementation")
  }
  # One list one integer
  if(typeof(x) == "list" && typeof(y) != "list"){
    print("Needs implementation")
  }
  # One integer one list
  if(typeof(x) != "list" && typeof(y) == "list"){
    print("Needs implementation")
  }
  # two integer
  if(typeof(x) != "list" && typeof(y) != "list"){
    return( x * y )
  }
}

node_dis <- function(obj, x, y){
  UseMethod("node_dis")
}

node_dis.euclidean <-function(obj, x, y){
  eu_the_dis(x, y)
}


node_sim <- function(obj){
  UseMethod("node_sim")
}

node_sim.euclidean <-function(obj, x, y){
  eu_the_sim(obj, x, y)
}


edge_dis <- function(obj){
  UseMethod("edge_dis")
}

edge_dis.euclidean <-function(obj, x, y){
  eu_the_dis(obj, x, y)
}


edge_sim <- function(obj){
  UseMethod("edge_sim")
}

edge_sim.euclidean <-function(obj, x, y){
  eu_the_sim(obj, x, y)
}

get_Instance.euclidean <-function(obj, x, y){
  return("euclidean")
}