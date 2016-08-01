#' Bannerize a Pidm
#' 
#' @description turn a vector into Banner pidms, by padding it out to 8 characters with zeros. Coerces numeric values to character.
#'
#' @param x a vector to convert to Banner pidms
#'
#' @return a character vector where x is padded out to 8 characters with zeros
#' @export
#'
bannerize.pidm  <- function(x) {
  
  stringr::str_pad(x, 8, side = 'left', pad = '0')
  
}