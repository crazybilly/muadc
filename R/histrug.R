#' Plot a histogram with a rug
#' 
#' a convience wrapper to put a rug underneath a histogram. Also calls muplot for formatting tweaks
#' @param x a numeric vector to plot
histrug <- function(x) {
  muplot()
  hist(x)
  rug(x)
}
