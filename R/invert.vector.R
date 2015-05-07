#' invert.vector 
#' 
#' Inverts a linear vector x, setting min(x) = min(x) and vice versa. Values in between are assumed to be linear values along the scale between min(x) and max(x).
#' @param x a numeric vector to be inverted
#' @return a numeric vector with length = length(x)
#' @export

invert.vector <- function(x) {
  
  # determine the median using only the min and max values
  # ie. the median if all values were to exists in the vector
  med  <- median(
    c(seq( min(x),max(x)))
  )
  
  # calculate the distance from the median and add twice that value to x
  ((med - x)*2) + x
  
}