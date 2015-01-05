#' Write data to the clipboard
#' 
#' A convience wrapper for write.table that writes tab-seperated data to the Windows clipboard. Does not work in *nix.
#' @param x an object to write
#' @param sepr a string to seperate columns, passed to write.table as sep=sepr. Defaults to \t, ie. tab-seperated columns. 
#' @param na the string to replace NA values. Defaults to "".
#' @param ... other arguments to pass to write.table()
#' @export
write.clip <- function(x,sep='\t', na= "", ...) {
  write.table(x,file = 'clipboard-128',row.names=F,sep=sep, na=na, ...)
}


