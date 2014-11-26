#' renormalize
#' 
#' renormalize a denormalized list into a Boyce-Codd style data frame suitable for relational databases.
#' @param x a datafame to analyze
#' @param idcol a string describing the column name holding ids or the numeric value of the column's index
#' @param listcol a string describing the column name holding the values to be re-normalized or the numeric value of the column's index
#' @param sep a string describing the seperator between values in listcol. Defaults to ",".
#' @param listcolname the name of the new column of re-normalized values. Defaults to 'normalized'.
#' @param na.replace a string to use in place of NA values of x. Defaults to a blank string. Use NA to force renormalize() to throw errors if there are NA values in x.
#' @return returns a 2-column data frame. The first column is a vector of ids from the original data frame. The second column is the renormalized data.
#' @export

renormalize  <- function (x,idcol,listcol,sep=",",listcolname='normalized',na.replace = ""){
  
  # lots of potential problems here
  #     1. when x$listcol has NAs in it, strsplit bombs out
  #     2. when x$listcol is not a character, strplit bombs out
  #     3. there MIGHT be some coercion problems, too
  
  
  collist  <- strsplit(x[,listcol], sep)
  normalizedlist <- data.frame(id = rep(x[,idcol], sapply(collist, length)), item = unlist(collist))
  
  names(normalizedlist)  <- c(idcol,listcolname)
  
  return(normalizedlist)
  
}