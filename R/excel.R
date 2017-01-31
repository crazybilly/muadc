#' Excel
#' 
#' @description open a file in Excel. This is convience wrapper around `shell.exec(normalizePath(file) )`.
#'
#' @param file a character string describing the path of the file to open. If the file does not open with Excel by default in Windows, it will open in the default program.
#'
#' @return returns NULL if successfull.
#' @export
#'
excel  <- function(file) {
  
  
  shell.exec(normalizePath(file) )
  
  
}