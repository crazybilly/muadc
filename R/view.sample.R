#' view.sample
#' 
#' get a quick view of gigantic data. 
#' @param x an object to view. view.sample() is designed to work best with data frames but will work with vectors as well.
#' @param transpose a boolean value determining whether or not the data frame should be transposed. Defaults to TRUE so long data frames can be easily examined.
#' @param sample.n an integer value determining how many rows should be sampled
#' @param sample.method a string describing the way rows should be chosen from x. Valid values include:
#'    - `head` uses head() to pull the first sample.n rows
#'    - `sample` randomly chooses sample.n rows
#'    - `tail` uses tail() to pull the last sample.n rows
#' @param output a boolean value determing whether the results should be sent to R's standard output. The default value, FALSE, sends the output to RStudio's `View()` function.
#' @return unless stdout==TRUE, does not return a value. Otherwise, returns the sampled data frame
#' @export
 
view.sample  <- function(x,transpose = TRUE,sample.n=3,sample.method='head',stdout=FALSE) {
  
  
  if (sample.method == 'head') {
    y  <- head(x,sample.n)
  } else if( sample.method == 'sample') {
    y  <- x[sample(nrow(x), size=sample.n),]
  } else if( sample.method == 'tail') {
    y  <- tail(x,sample.n)
  } else { 
    stop("unknown sample.method - use 'head', 'sample' or 'tail'")
  }

  if (transpose == T) {
   z  <- t(y)
  } else {
    z  <- y 
  }
  
  
  if (stdout == TRUE)  {
    z
  } else {
    View(z)
  }
  
}