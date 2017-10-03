#' Str_detect with NA handling
#'
#' @param string 	Input vector. Either a character vector, or something coercible to one.
#' @param pattern Pattern to look for. See \link[stringr]{str_detect} .
#' @param default a default value to be used if string is NA
#'
#' @return a logical vector, assuming default is set to logical. Using a string or numeric value will coerce the value to the class of default.
#' @import stringr
#' @export
#'
str_detect2  <- function(string, pattern, default = F) {
  
  muadc::coalesce(stringr::str_detect(string, pattern), default)
  
  
}