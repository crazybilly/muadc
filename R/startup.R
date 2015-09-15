#' startup
#' 
#' do the things I do at startup all the time
#' 
#' @param load a boolean value determining whether .RData ought to be loaded
#' @param xlsx a boolean value determining whether the xlsx package ought to be loaded
#' @param MySQL a boolean value determing whether the MySQL package ought to be loaded

startup  <- function(load=F, xlsx=F,MySQL=F) {
  
  require(muadc)
  require(plyr)
  require(lubridate)
  require(stringr)
  require(dplyr)
  
  if(xlsx) {
    require(xlsx)
  }
  
  if(MySQL) {
    require(MySQL)
  }
  
  
  if( load) {
    load('.RData')
  }
  
  
}