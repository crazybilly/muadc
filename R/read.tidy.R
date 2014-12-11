#' Read csvs into tidy format
#'
#' This is a convience wrapper for read.csv which lowercases column names and removes punctuation. It also has an option to only read the csv if the object does not exist.
#' @param x the filename to load. Should be a csv
#' @param ifexists should the function check if the object exists. Default = F
#' @param existsname what object should the function look for, assuming ifexists = T. If ifexists == T and existname is not empty, the function will return the object with name = existsname.
#' @param ... additional parameters to pass to read.csv
#' @export 
#' @examples
#' read.tidy("hallp.csv",ifexist=T,"hallp")

read.tidy <- function(x,ifexists=F,existsname,...){
  if( ifexists ) {
    if( !exists(existsname) ) {
      df <- read.csv(x,stringsAsFactors=F,...)
      names(df) <- gsub("\\.|,|-| |_","",names(df))
      names(df) <- tolower(names(df))
      
      return(df)
    } else { get(existsname) }
  }
  else {
    df <- read.csv(x,stringsAsFactors=F,...)
    names(df) <- gsub("\\.|,|-| |_","",names(df))
    names(df) <- tolower(names(df))
    
    return(df)
  }
  
}
