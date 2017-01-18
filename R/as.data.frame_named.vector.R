#' Coerce to a Data Frame
#' 
#' @description converts a named vector to a 2 column data frame with the names in the first column and values in the second columns. Throws an error if no names are found. Row names are stripped out of the final data frame.
#' 
#' @param x a named vector
#' @param valname a character string to be used as the new column name. Defaults to the name of x
#' 
#' @return a two column data frame with nrow = length(x)
#' @export



as.data.frame_named.vector <- function(x,valname=deparse(substitute(x))) {
  
  
  names <- names(x)
  if(is.null(names)) {
    names  <- row.names(x)
  }
  
  if( is.null(names) | length(x) == 0 ) {
    stop("no names found in x")
  }
  
  df  <- data.frame( 
        names = names
      , val   = x
      , stringsAsFactors = F
    )
  
  if ("package:dplyr" %in% search() ) {
    df  <- dplyr::tbl_df(df)
  }
  
  df  <- stats::setNames(df, c('names',valname))
  row.names(df)  <- NULL
  
  return(df)
  
}