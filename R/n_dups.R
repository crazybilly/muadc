#' Count Duplicates
#'
#' @param x a vector to count duplicates
#'
#' @return a integer number of duplicate values found in x
#' @export
#'
n_dups  <- function(x) {
  sum(duplicated(x))
}