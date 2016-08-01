#' is.blank
#' 
#' tests a vector, returning TRUE if values are NA, NULL, blank strings or NAN. Also searches for the string "()-", often used as an empty phone number.
#' @param x a vector to test for blank values
#' @return a vector of logical values, where TRUE indicates blank values
#' @export
 
is.blank  <- function(x) {
  
  is.na(x) | !nzchar(x) | is.null(x) | is.nan(x) | grepl("^\\(\\)-$",x)
  
}