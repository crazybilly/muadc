#' yesno.as.logical
#' 
#' @description Convert a vector of Yes/No or Yes/NA values to logical. 
#'
#' @param v  a character vector to convert
#'
#' @return a logical vector. Values of "Y" and "Yes" are converted to TRUE. All other values, including NA, are converted to FALSE.
#' 
#' @export
#'
#' @examples
#' 
#' foo  <- c("Y", NA, "Y", NA)
#' yesno.as.logical(foo)
#' 
#' foo  <- c("Y", NA, "Y", "Yes",NA, "No", "N","something else")
#' yesno.as.logical(foo)

yesno.as.logical  <- function(v) {
  
  (v == "Y" | grepl("yes",v, ignore.case = T) ) & !is.na(v)
  
}