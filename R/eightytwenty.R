#' Determine the 80/20 cutoff of a vector
#' 
#' Given a numerical vector, above what value is there more total value, ie. where is the "80/20" split. Returns the value, and the 80/20 split values.
#' 
#' @param x a numeric vector to be analysed.
#' @return a data frame with the following columns:
#'  \enumerate{
#'    \item cutoff - the cutoff value
#'    \item perc_of_total_x - the percentage of total value (ie. the "80")
#'    \item perc_of_rows  - the percentage of observations above the cutoff (ie. the "20")
#'    }
#' @export
eightytwenty <- function(x) {
  
    # set x in ascending order, and cumulative total
    df <- data.frame( x = x[order(x)], cumtotal = cumsum(x[order(x)]))
    
    #set rownum
    df$rownum <- 1:length(x)
    
    # calculate what % of the total is the cumulative total
    df$perc_total <- df$cumtotal/sum(x, na.rm = T)
    
    # calculate what % of the number of rows is the current rows
    df$perc_rows <- df$rownum/length(x)
    
    # add the percentages together
    df$perc_plus_perc <- df$perc_total + df$perc_rows
    
    # compare the added percentages to 1 and take the absolute value
    df$nearly1 <- abs(df$perc_plus_perc - 1)
    
    # grab the row where the added percentages are closest to zero
    cutoff <- df[df$nearly1 == min(df$nearly1),]
    
    cutoff$perc_of_rows     <- round(1-cutoff$perc_rows,2)
    cutoff$perc_of_total_x  <- round(1-cutoff$perc_total,2)
    
    cutoff$cutoff  <- cutoff$x
    
    cutoff[,c(10,9,8)]
  }