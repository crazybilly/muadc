#' write.tidy
#' 
#' writes a tidy csv file. Primarily a convience wrapper around write.csv, defaults to not writing rownames and NA = ""
#' @param x the object to be written, preferably a matrix or data frame. If not, it is attempted to coerce x to a data frame. 
#' @param file either a character string naming a file or a connection open for writing. "" indicates output to the console.
#' @param row.names either a logical value indicating whether the row names of x are to be written along with x, or a character vector of row names to be written.
#' @param na the string to use for missing values in the data.
#' @param ... other values to be passed to write.csv
#' @export
write.tidy  <- function(x, file = "", row.names = FALSE, na = "", ... ) {
  write.csv(x, file, row.names=row.names, na=na, ... )
}