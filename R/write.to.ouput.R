#' Write To Output Directory
#'
#' @param df a data frame to write to disk
#' @param ... additional arguments passed to `write.csv()`
#'
#' @return
#' @details The data frame will be written to the output directory with a filename of "output/the-object-name.R.YYYY-MM-DD.csv" with today's date. 
#' @export
#'
write.to.ouput  <- function(df, ...) {
  
  if( !dir.exists("output")) {
    
    makeoutput  <- rstudioapi::showQuestion("Create output?", "The output directory does not exist. Do you want to create it and the data directory?")
    
    if(makeoutput) {
      skel()
    } else {
      stop('No output directory')
    }
     
  }
  
  
  
  todaysdate  <- as.character(lubridate::today())
  
  newfilename  <- deparse(substitute(df)) %>% 
    stringr::str_replace_all("_", "-") %>% 
    paste0("output/",, ".R.", todaysdate, ".csv")
  
  
  write.tidy(df, newfilename, ...)
  
  
  
  
}