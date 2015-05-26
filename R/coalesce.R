#' @title Replace NA values
#' @description Returns the first non-NA argument, similar to the SQL function 
#'   coalesce. If a vector or matrix is passed as first argument,
#'   the remaining arguments are recycled to generate a vector/matrix of
#'   the same dimension, and coalescing is done element by element.
#'   Taken from krlmlr's [misc package](https://github.com/krlmlr/kimisc), referenced at [StackOverflow](http://stackoverflow.com/questions/19253820/how-to-implement-coalesce-efficiently-in-r).
#' @param x The first value to coalesce.
#' @param ... Other values to coalesce.
#' @return A vector of the same length as {x}.
#' @export
coalesce <- function(x, ...) {
  x.len <- length(x)
  ly <- list(...)
  for (y in ly) {
    y.len <- length(y)
    if (y.len == 1) {
      x[is.na(x)] <- y
    } else {
      if (x.len %% y.len != 0)
        warning('object length is not a multiple of first object length')
      pos <- which(is.na(x))
      x[pos] <- y[(pos - 1) %% y.len + 1]
    }
  }
  x
}
