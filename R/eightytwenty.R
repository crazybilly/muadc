#' Determine the 80/20 cutoff of a vector
#' 
#' Given a numerical vector, above what value is there more total value, ie. where is the "80/20" split. Returns the value, and the 80/20 split values.
#' 
#' @param x a numeric vector to be analysed.
#' @return a data frame with the following columns:
#'  \enumerate{
#'    \item x - the cutoff value
#'    \item p.t - the percentage of total value (ie. the "80")
#'    \item p.f - the percentage of observations above (ie. the "20")
#'    }
eightytwenty <- function(x) {
  
  # grab the data from the input and create a freq table as dataframe
  tab 	<- as.data.frame(table(x))
  tab$x <- as.numeric(levels(tab$x))[tab$x]  
  
  #add the $ totals in 
  tab$t <- tab$x * tab$Freq
  
  # sort the data by x asc
  tab 	<- tab[order(tab$x),]
  
  #create cumulative totals
  
  # $ totals
  tab$c.t <- cumsum(tab$t)
  # frequency totals
  tab$c.f <- cumsum(tab$Freq)
  
  # create percentage of totals
  
  # $ total percentages
  tab$p.t <- tab$c.t/max(tab$c.t)
  # frequency percentages
  tab$p.f <- tab$c.f/max(tab$c.f)
  
  #sum percentages
  tab$psum <- tab$p.f + tab$p.t
  
  #compare to 1
  tab$dif <- 1-tab$psum
  
  #return the row closest to 1
  results <- subset(tab,abs(dif)==min(abs(dif)),select=c(x,p.t,p.f,dif))
  
  
  if ( results$dif != 0 ) {
    remainderhalf <- results$dif/2
    results$p.t <- results$p.t + remainderhalf
    results$p.f <- results$p.f + remainderhalf
  }
  
  
  #round the resulting percentages off
  results$p.t <- round(results$p.t,digits=2)
  results$p.f <- round(results$p.f,digits=2)
  
  results <- subset(results,select = c(x,p.t,p.f))
  
  return(results)
}
