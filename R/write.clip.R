#' Write data to the clipboard
#' 
#' A convience wrapper for write.table that writes tab-seperated data to the Windows clipboard. Does not work in *nix.
#' @param x an object to write
#' @param size the file size to use for the clipboard. Defaults to 4096 which is reasonably large.
#' @param sepr a string to seperate columns, passed to write.table as sep=sepr. Defaults to tab-seperated columns. 
#' @param na the string to replace NA values. Defaults to "".
#' @param row.names a boolean values determining whether  row names be written to the file. Defaults to FALSE.
#' @param ... other arguments to pass to write.table()
#' @export
write.clip <- function(x,size='4096', sep='\t', na= "", row.names=F, ...) {
  
  destination = paste('clipboard-',size)
  
  write.table(x, file = destination, row.names = row.names, sep = sep, na = na, ...)
   
}


