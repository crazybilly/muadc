#' renormalize
#' 
#' renormalize a denormalized list into a Boyce-Codd style data frame suitable for relational databases.
#' @param x a datafame to analyze
#' @param idcol a string describing the column name holding ids or the numeric value of the column's index
#' @param listcol a string describing the column name holding the values to be re-normalized or the numeric value of the column's index. Note that listcol data will be coerced to a string. NA values and values which do not coerce properly will be replaced with empty strings.
#' @param sep a string describing the seperator between values in listcol. Defaults to ",".
#' @param listcolname the name of the new column of re-normalized values. Defaults to 'normalized'.
#' @param na.replace a string to use in place of NA values of x. Defaults to a blank string. 
#' @return returns a 2-column data frame. The first column is a vector of ids from the original data frame. The second column is the renormalized data. Rows from x where listcol is NA are removed.
#' @export

renormalize  <- function (x,idcol,listcol,sep=",",listcolname='normalized',na.replace = ""){
  
  # lots of potential problems here
  #     1. when x$listcol has NAs in it, strsplit bombs out
  #     2. when x$listcol is not a character, strplit bombs out
  #     3. there MIGHT be some coercion problems, too
  
  # remove rows where is.na(listcol)
  x  <- x[!is.na(x[[listcol]]),]
  
  idcoldata  <-  x[[idcol]]
  listcoldata  <-  x[[listcol]]
  
  # coerce listcol to character
  listcoldata  <- as.character(listcoldata)
  
  
 # replace NA values (introduced via coercion) with empty strings.
    nasinlist  <- sum(is.na(listcoldata))
    if(nasinlist > 0) {
        warning(paste('listcol contains', nasinlist, 'NA values. Converting these values to empty strings...'))
    }
    listcoldata[is.na(listcoldata)]  <- na.replace
  
  
 # split the data on the delimiter 
  collist  <- strsplit(listcoldata,sep)
  
  # pull data into a new, normalized data fram
  normalizedlist <- data.frame('id' = rep(idcoldata, sapply(collist, length)), 'item' = unlist(collist), stringsAsFactors = F )
  
  # rename the list columns of the new data frame and return it
  setNames(normalizedlist,  c(idcol,listcolname) )
   
  
}