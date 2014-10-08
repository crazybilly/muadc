#' renormalize
#' 
#' renormalize a denormalized list into a Boyce-Codd style data frame suitable for relational databases.
#' @param x a datafame to analyze
#' @param idcol a string describing the column name holding ids or the numeric value of the column's index
#' @param listcol a string describing the column name holding the values to be re-normalized or the numeric value of the column's index
#' @param sep a string describing the seperator between values in listcol. Defaults to ",".
#' @param listcolname the name of the new column of re-normalized values. Defaults to 'normalized'.
#' @return returns a 2-column data frame. The first column is a vector of ids from the original data frame. The second column is the renormalized data.

renormalize  <- function (x,idcol,listcol,sep=",",listcolname='normalized'){
  
  collist  <- strsplit(x[,listcol], sep)
  normalizedlist <- data.frame(id = rep(x[,idcol], sapply(collist, length)), item = unlist(collist))
  
  names(normalizedlist)  <- c(idcol,listcolname)
  
  return(normalizedlist)
  
}