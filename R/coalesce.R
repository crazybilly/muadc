#' coalesce
#' 
#' a function, like the one in SQL, which takes arguments and returns the first non-NA value.
coalesce<-function(...){
  Reduce(function(x,y) {
    i<-which(is.na(x))
    x[i]<-y[i]
    x
  },
  list(...))
}