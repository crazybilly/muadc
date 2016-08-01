#' Format as currency
#' 
#' Given a numeric vector, format it as currency, implicitly changing it to a string.
#' 
#' @param x a numeric vector to be formatted
#' @return a vector of strings formated as currency, with commas and dollar signs.
#' @export



as.currency  <- function(x) {
  paste0("$",
         formatC(x 
                 , format   = 'f'
                 , big.mark = ','
                 , digits   =  2
                 )
        )
}