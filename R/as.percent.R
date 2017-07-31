#' Format as percent
#' 
#' Given a numeric vector, format it as percent, implicitly changing it to a string.
#' 
#' @param x a numeric vector to be formatted
#' @param digits a single numerical value determining how many digits should be shown
#' @return a vector of strings formated as percent, with commas and percent signs. 
#' @export



as.percent <- function(x, digits = 2) {
  paste0(
         formatC(x*100 
                 , format   = 'f'
                 , big.mark = ','
                 , digits   =  digits
         )
         , "%"
  )
}
