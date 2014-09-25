#' Write data to the clipboard
#' 
#' A convience wrapper for write.table that writes tab-seperated data to the clipboard. Does not work in *nix.
#' @param x an object to write
#' @param sepr a string to seperate columns, passed to write.table as sep=sepr
#' @export
write.clip <- function(x,sepr='\t') {
  write.table(x,file = 'clipboard-128',row.names=F,sep=sepr)
}


