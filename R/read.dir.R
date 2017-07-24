#' Read Files in a Directory
#' 
#' @description Read all the files in a directory into a list object.
#'
#' @param path a character vector of full path names
#' @param pattern an optional regular expression. Only file names which match the regular expression will be returned. The names of the list items will have pattern removed from them
#' @param sep the field separator character for data frame-like files. Defaults to comma separated values.
#' @param ... other arguments to pass to list.files()
#'
#' @return a list with one object per file. Files are read via read.tidy(). As such, files that are not data frame-ish may not be read correctly.
#' @export
#'
read.dir  <- function(path = ".", pattern = NULL, sep = ",", ... ) {
  
  
  allfiles  <- list.files(path = path, pattern = pattern , full.names = T, ... ) 
  
  objectnames  <- allfiles %>% 
    tolower() %>% 
    str_replace(path, "") %>% 
    str_replace_all("\\/", "") %>% 
    str_replace_all(" ", "_") 
  
  if(!is.null(pattern)) {
    objectnames  <- objectnames %>% 
    str_replace( pattern, "") 
  }
  
  results  <- lapply(allfiles, function(x) read.tidy(x, sep = sep) ) %>% 
    setNames(objectnames)
  
  
  
  
}