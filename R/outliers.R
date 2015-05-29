#' Determine the upper and lower outliers
#' 
#' Given a numeric vector, return the upper and lower bounds for outliers. Assumes IQR * 1.5 as bounds.
#' @param x a numeric vector to be analysed.
#' @param na.rm logical; if true, NA values  are removed before IQR and quantiles are computed.
#' @export
#' @return a list with two values:
#' \enumerate{
#'  \item bottom - the lower bound
#'  \item top - the upper bound
#'  }
#'  
outliers <- function(x, na.rm = T) {
  #calculate the thresholds
  iqr <- IQR(x, na.rm = na.rm)
  thirdq <- quantile(x, na.rm = na.rm)[4]
  firstq <- quantile(x, na.rm = na.rm)[2]
  outlier.top <- as.numeric((iqr*1.5) + thirdq)
  outlier.bottom <- as.numeric(firstq - (iqr*1.5))
  
  #if the threshold is smaller/larger than the min/max, there are no outliers, return NA
  if( outlier.bottom < quantile(x, na.rm = na.rm)[1]) { outlier.bottom <- NA }
  if( outlier.top > quantile(x, na.rm = na.rm)[5]) { outlier.top <- NA }
  
  #return a list
  outlier <- list("bottom" = outlier.bottom,"top" = outlier.top)
  
  return(outlier)
}
