#' See Outpout Directory
#'
#' @description Opens a Windows directory to the path given to the dir argument, by default, the output/ directory. If the given directory does not exist, a warning is thrown and the current working directory is opened.
#' 
#' @param dir the path to a directory to open
#'
#' @return
#' @export
#'
seeoutput  <- function( dir = 'output') {
  
  origdir  <- dir
  
  if(!dir.exists(dir) ) {
    or
    dir  <-  getwd()
  }
  
  warning(paste(origdir, "does not exist, opening working directory."))
  
  shell.exec(dir)
  
}