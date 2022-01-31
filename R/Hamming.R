hm_the_dis <- function(x, y){
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
    return( compute_hamming_distance(x, y) )
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
      x <- rep(0, ny)
      return( compute_hamming_distance( c(y, rep(0, ny)), y ) )
    }else if (ny == 0){
      y <- rep(0, nx)
      return( compute_hamming_distance( x, c(y, rep(0, nx)) ) )
    }else if(nx <= ny){
      n <- ny
      x <- c(x, rep(0, n - nx))
    }else{
      n <- nx
      y <- c(rep(0, n - ny), y)
    }
  }
  return( compute_hamming_distance( x, y ) )
}

node_dis.hamming  <-function(obj, x, y){
  hm_the_dis(x, y)
}


edge_dis.hamming <-function(obj, x, y){
  hm_the_dis(x, y)
}

get_Instance.hamming <-function(obj){
  return("hamming")
}

#' Function code extracted from https://johanndejong.wordpress.com/2015/10/02/faster-hamming-distance-in-r-2/
compute_hamming_distance <- function(x, y = NULL){
  if (is.null(y)) {
    uniqs <- unique(as.vector(x))
    u <- x == uniqs[1]
    H <- t(u) %*% U
    for ( uniq in uniqs[-1] ) {
      u <- x == uniq
      H <- H + t(u) %*% u
    }
  } else {
    uniqs <- union(x, y)
    h <- t(x == uniqs[1]) %*% (y == uniqs[1])
    for ( uniq in uniqs[-1] ) {
      h <- h + t(x == uniq) %*% (y == uniq)
    }
  }
  nrow(x) - h
}