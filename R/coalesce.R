#' coalesce
#' 
#' a function, like the one in SQL, which takes arguments and returns the first non-NA value.
#' @param ... objects to be used
#' @return returns an object with the length of the first argument. Elements are not recycled.
#' @export
coalesce<-function(...){
  Reduce(function(x,y) {
    i<-which(is.na(x))
    x[i]<-y[i]
    x
  },
  list(...))
}