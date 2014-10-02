#' libraryprep
#'
#' installs a bunch of packages that I use all the time
#'
#' @param xlsx should the xlsx package be installed and loaded
#' @param RMySQL should the RMySQL package be installed and loaded
#' @param ... a character vector of futher packages to be installed and loaded



libraryprep <- function(xlsx=F,RMySQL=F,...)
  
#   devtools::install_github("hadley/devtools") 
#   devtools::install_github("hadley/dplyr")
#   devtools::install_github("crazybilly/muadc")
#   
  
  require(muadc)
  require(shiny)
  require(stringr)
  require(lubridate)
  require(plyr)
  require(dplyr)
  
  
  if( xlsx ) {
    require(xlsx)
  }
  
  if( RMySQL ) {
    require(RMySQL) 
  }
  
  if( ... ) {
    require(...)
  }
