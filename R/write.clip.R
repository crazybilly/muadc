#' Write data to the clipboard
#' 
#' A convience wrapper for write.table that writes tab-seperated data to the Windows clipboard. Does not work in *nix.
#' @param x an object to write
#' @param size the file size to use for the clipboard. Defaults to 4096 which is reasonably large.
#' @param sepr a string to seperate columns, passed to write.table as sep=sepr. Defaults to tab-seperated columns. 
#' @param na the string to replace NA values. Defaults to "".
#' @param row.names a logical value determining whether  row names be written to the file. Defaults to FALSE.
#' @param excel a logical value determing whether Excel should be opened immediately after copying the data to the clipboard.
#' @param ... other arguments to pass to write.table()
#' @export
write.clip <- function(x,size='4096', sep='\t', na= "", row.names=F, excel = F, ...) {
  
  destination = paste('clipboard-',size)
  
  write.table(x, file = destination, row.names = row.names, sep = sep, na = na, ...)
  
  
  if(excel) {
    shell.exec('Excel')
  }
   
}


