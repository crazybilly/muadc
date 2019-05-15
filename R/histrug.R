#' Plot a histogram with a rug
#' 
#' a convience wrapper to put a rug underneath a histogram. Also calls muplot for formatting tweaks
#' 
#' @param df either a data frame containing the column to be plotted, or a numeric vector to plot
#' @param col if df is not null, then the unquoted name of a column in df to plot
#' 
#' @export
histrug <- function(df, col) {
  
  muplot()
  
  if(is.vector(df)) {
    
    hist(x)
    rug(x)
    
  } else {
    
    colq  <- dplyr::enquo(col)
    
    thevector <-  df %>% 
      dplyr::pull(!!colq)
    
    hist(thevector)
    rug(thevector)
      
    
    
  }
  
  
  
}
