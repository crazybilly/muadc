#' Count.NA
#'
#' @description Count the number of NA values in a data frame or vector.
#' 
#' @param an object to count. All bets are off if you don't pass a data frame or a vector.
#'
#' @return If x is a vector, returns a single numeric count of the NAs in the vector. If x is a data frame, returns a two-column data frame with counts for each column of the data frame.
#' 
#' @export
#' 
count.na  <- function(x) {
  classx  <- class(x)
  
  if( any(grepl('data.frame', class(x)) ) ) {
    
    nacounts  <- sapply(x, function(y) {
                  sum(is.na(y))
                  })
    
    as.data.frame_named.vector(nacounts, valname = 'n_NAs')  
    
    
  } else {
    
    sum(is.na(x))
    
  }
  
}