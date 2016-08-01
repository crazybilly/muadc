#' Source with Beep
#' 
#' @description Source an R file and beep when you're finished
#'
#' @param file a file to source
#' @param echo should the call to source(file) be made with the echo argument set to TRUE
#' @param ...  additional arguments to pass to `source()`
#' @param beep a numerical value to pass to `beepr::beep()`, indicating what what sound should indicate the process is finished
#'
#' @return
#' @export
#'
sourcebeep  <- function( file, echo = T, ..., beep = 1 ) {
  
  source(file, ..., echo=echo )
  beepr::beep( beep)
  
}