#' Search for a string in column names
#' 
#' Performs a grep regex search on the column names of a data frame, returning the values and their column number.
#' @param str a regex string to find. 
#' @param x the object to search. Expect a data frame
#' @param ignorecase should the search ignore case. Default = T
#' @export
#' @examples
#' grepnames("lifetimeg",hallp)
#' grepnames("event",hallp,ignorecase=F)

grepnames  <- function(str,x,ignorecase=T) {
  df  <- as.data.frame(
    grep(  str
           , names(x)
           ,ignore.case = T,value = F)
    , grep(  str
             , names(x)
             ,ignore.case = T,value = T)
  )
  names(df)  <- c("index")
  return(df)
}
