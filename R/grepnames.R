#' Search for a string in column names
#' 
#' Performs a grep regex search on the column names of a data frame, returning the values and their column number.
#' @param str a regex string to find. Defaults to ".*"
#' @param x the object to search. Expect a data frame
#' @param ignorecase should the search ignore case. Default = T
#' @export

grepnames  <- function(str=".*",x,ignorecase=T,sortalpha=F) {
  df  <- data.frame(
      colname= grep(  str
             , names(x)
             ,ignore.case = T,value = T)
      , index = grep(  str
            , names(x)
            ,ignore.case = T,value = F)
      , stringsAsFactors = F
  )
  #names(df)  <- c("index")
  
  if(sortalpha) {
    df  <- df[with(df,order(colname)),]
  }
  
  return(df)
}
