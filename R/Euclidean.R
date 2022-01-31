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
  # TODO: Check if the data type is not integer or list. Maybe Dataframe as in
  # python Pandas DataFrame.
  
  if(is.null(x) || is.null(y)){
    return(0)
  }
  else if(is.null(x) && is.null(y)){
    print('Give me at least one non empty vector!')
    return(0)
  }
  # two integer
  else if(typeof(x) != "list" && typeof(y) != "list"){
    return( sum((x - y)^2) )
  }
  # One list one integer
  else if(typeof(x) == "list" && typeof(y) != "list"){
    n <- length(x)
    y <- c(y, rep(0, n - 1))
  }
  else if(typeof(x) != "list" && typeof(y) == "list"){
    n <- length(y)
    x <- c(x, rep(0, n - 1))
  }
  # two lists
  else if(typeof(x) == "list" && typeof(y) == "list"){
    nx <- length(x)
    ny <- length(y)
    
    if( nx == 0 && ny == 0 ){
      return(0)
    }else if ( nx == 0 ){
      return(eu_the_sim(y, y))
    }else if (ny == 0){
      return(eu_the_sim(x, x))
    }else if(nx <= ny){
      n <- ny
      x <- c(x, rep(0, n - nx))
    }else{
      n <- nx
      y <- c(y, rep(0, n - ny))
    }
  }
  dis <- 0
  for(i in 1:n){
    dis <- dis + (x[[i]] - y[[i]])^2
  }
  return(dis)
}


eu_the_sim <- function(x, y){
  # TODO: Check if the data type is not integer or list. Maybe Dataframe as in
  # python Pandas DataFrame.
  
  if(is.null(x) || is.null(y)){
    return(0)
  }
  else if(is.null(x) && is.null(y)){
    print('Give me at least one non empty vector!')
    return(0)
  }
  # two integer
  else if(typeof(x) != "list" && typeof(y) != "list"){
    return( x * y )
  }
  # One list one integer
  else if(typeof(x) == "list" && typeof(y) != "list"){
    n <- length(x)
    y <- c(y, rep(0, n - 1))
  }
  else if(typeof(x) != "list" && typeof(y) == "list"){
    n <- length(y)
    x <- c(x, rep(0, n - 1))
  }
  # two lists
  else if(typeof(x) == "list" && typeof(y) == "list"){
    if(length(x) <= length(y)){
      x <- c(x, rep(0,length(y) - length(x)))
    }else{
      y <- c(y, rep(0,length(x) - length(y)))
    }
  }
  sim <- 0
  for(i in 1:n){
    sim <- sim + (x[[i]] * y[[i]])
  }
  return(sim)
}


node_dis.euclidean <-function(obj, x, y){
  eu_the_dis(x, y)
}


node_sim.euclidean <-function(obj, x, y){
  eu_the_sim(x, y)
}


edge_dis.euclidean <-function(obj, x, y){
  eu_the_dis(x, y)
}


edge_sim.euclidean <-function(obj, x, y){
  eu_the_sim(x, y)
}


get_Instance.euclidean <-function(obj){
  return("euclidean")
}