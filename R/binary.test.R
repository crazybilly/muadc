#' Binary
#'  
#' checks if a vector is empty or not, return a factor with optional descriptive names
#' @param x an data frame to be analyzed. The first column must be pidm.
#' @param i the numerical index of the column to be analyzed.
#' @param newname a string for the name of the new column
#' @param emptyname a string to be used as the factor level for empty values. Defaults to FALSE.
#' @param not emptyname a string to be used as the factor level for values that are not empty. Defaults to TRUE
#' @return Returns a data frame with the first column of the original data frame (hopefully pidm) and the new column.
#' @export
# 
binary.test  <- function (x,i,newname,emptyname=F,notemptyname=T) {
  
  y  <- x[,c(1,i)] %>%
    mutate(
      newcol = factor(
        ifelse(x[,i]=="" | is.na(x[,i]) | grepl("\\(\\)-",x[,i]), emptyname ,notemptyname)
        , levels=c(emptyname,notemptyname)
        #, ordered = T
      )
    ) %>%
    select(pidm,newcol)
  names(y)  <- c("pidm",newname)
  
  return(y)
}