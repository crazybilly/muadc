#' Identify the Most Recently Edited File
#'
#' @param path a character vector of full path names; the default corresponds to the working directory, `getwd()`. Tilde expansion (see `path.expand``) is performed. Missing values will be ignored.
#' @param pattern an optional regular expression. Only file names which match the regular expression will be returned.
#' @param invert a logical value. If FALSE, returns the most recently modified file. If TRUE, returns the first modified file.
#' @param messagename a logical value determing whether the results should be messaged on the console. Useful in semi-interactive cases when output might not be immediately available, for example, within a dplyr chain.
#' @param ...  other arguments to pass to `list.files`
#'
#' @return a character string with the full name of the most recently modified file in path
#' @export
#'
file.newest  <- function( path = ".", pattern = NULL, invert = F, messagename = F, ... ) {
  
  # list all files 
  allfiles  <- list.files(path, pattern = pattern, ... , full.names = T)
  
  # get the info about them
  fileinfodf  <- file.info(allfiles)
  
  # do we want the most recently modified file or the first modified file  
  if(!invert) {
    sortorder  <- order(as.numeric(fileinfodf$mtime))
  } else {
    sortorder  <- order(-as.numeric(fileinfodf$mtime))
  }
  
  # sort the info data frame based on sortorder
  fileinfodf  <- fileinfodf[sortorder,]
  
  
  # get rid of NA values (which we get if we use a pattern)
  fileinfodf  <- fileinfodf[!is.na(fileinfodf$size),]
  
  
  # get the index of the last row
  i  <- nrow(fileinfodf)
  
  
  # if you asked for notification about what file is being used, generate it
  if(messagename) {
    
    oldestornewest  <- ifelse(invert, 'Oldest', 'Newest')
    
    message(paste(oldestornewest, "file:   ", rownames(fileinfodf)[i] ))
  } 
  
  # return the row name, which is the filename
  rownames(fileinfodf)[i]
  
  
}
