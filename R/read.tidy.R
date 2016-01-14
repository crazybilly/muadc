#' Read csvs into tidy format
#'
#' This is a convience wrapper for read.csv which lowercases column names and removes punctuation. It also has an option to only read the csv if the object does not exist. 
#' 
#' @param x the filename to load. Should be a csv. Defaults to reading from the clipboard.
#' @param sep the field separator character. If x == 'clipboard', sep defaults to tab seperated files. Otherwise, it defaults to ',' to read csv data.
#' @param ifexists should the function check if the object exists. Default = F
#' @param existsname what object should the function look for, assuming ifexists = T. If ifexists == T and existname is not empty, the function will return the object with name = existsname.
#' @param ... additional parameters to pass to read.csv
#' @return Returns a data frame will lower cased names and all punctuation removed. If dplyr is already loaded, it returns a data frame as a tbl_df--otherwise, it returns a simple data frame.
#' @export 

read.tidy <- function(x='clipboard',sep = '\t', ifexists=F,existsname,...){
  
  df  <- NULL

  if( sep != '\t' ) {
    sep  <- sep 
  } else if ( x == 'clipboard') {
    sep  <- '\t'
  } else {
    sep  <- ','
  }
  
  
   
  if( ifexists ) {
    if( !exists(existsname) ) {
      df <- read.csv(x,sep = sep, stringsAsFactors=F,...)
      clean.df(df)
      
    } else { 
      df  <- get(existsname) 
      clean.df(df)
      }
  }
  else {
    df <- read.csv(x,sep = sep, stringsAsFactors=F,...)
    clean.df(df)
    
  }
  
  
}
