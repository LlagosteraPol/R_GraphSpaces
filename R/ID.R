source("./R/Matcher.R")


ID <- function(obj){
  if(length(class(obj)) >= 2){
    stop("The class is already initialized.")
  }else{
    attr(obj, "class") <- c(attr(obj, "class"), "Matcher", "Id")
    return(obj) # class = c('Matcher', 'Id')
  }
}