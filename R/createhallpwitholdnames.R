#' createhallpwitholdnames
#' 
#' @description Creates a copy of hallp with old column names, ie. prior to Fall 2015.
#'
#' @return Returns a collected copy of hallptbl as a dataframe.
#' @export
createhallpwitholdnames <- function() {
  
  
  
  if( !exists('hallptbl' )) {
    initcommitsdb()
  }
  
  hallpnames <- hallptbl %>% 
    head(1)  %>% 
    collect  %>% 
    names  %>% 
    data.frame( current = . , stringsAsFactors = F)  %>% 
    left_join( oldhallpnames %>% select(newname, oldname), by = c('current' = 'newname'))  %>% 
    mutate(oldname = ifelse(is.na(oldname),current,oldname) )
  
  hallpwitholdnames  <- hallptbl %>% 
    collect  %>% 
    stats::setNames(hallpnames$oldname)
  
  invisible(hallpwitholdnames)
  
}
