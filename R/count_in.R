#' Count in 
#' 
#' @description compare two objects to see how many rows are shared between them
#'
#' @param x, y tbls to compare
#' @param by  a character vector of variables to join by. If `NULL`, the default, `*_join()` will do a natural join, using all variables with common names across the two tables
#'
#' @return a 3 row data frame with the number of ID variables that are in x but not in y, number in both, and number that are in y but not in x.
#' 
#' @export
#'
count_in  <- function(x, y, by= NULL) {
  
  n_x_not_in_y  <- anti_join( x, y, by = by) %>% nrow()
  n_in_both     <- inner_join(x, y, by = by) %>% nrow()
  n_y_not_in_x  <- anti_join( y, x, by = by) %>% nrow()
  
  
  c(x_not_in_y = n_x_not_in_y, in_both = n_in_both, y_not_in_x = n_y_not_in_x) %>% 
    as.data.frame_named.vector(valname = 'n')
  
}
