#' FillNA
#' 
#' @description Replaces NA values in a vector with a value, 0 by default. This is really just a convinence wrapper around replace() to minimize typing.
#'
#' @param x a vector with NA values to be filled
#' @param fill a value to replace any NA values in x. Note that if the class of fill does not match the class of x, x will be coerced to an acceptable class.
#'
#' @return a vector with length of x
#' @export
 
fillna  <- function(x,fill = 0) {
  
  replace(x, is.na(x), fill)
  
}