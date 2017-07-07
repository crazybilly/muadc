#' Make a data frame into a delimited text Table
#' 
#' @description collapse a data frame into a tab-seperated text table, suitable for a .tsv file. Particularly useful for stringing multiple data frames together into a single report.
#'
#' @param df a data frame to convert to a vector
#' @param sep a delimiter. Defaults to tab-seperated text.
#'
#' @return a character vector with one entry per row of the data frame. Each row is collasped to a single entry with variables separated by tabs. NA and Inf values are replaced with blanks.
#' 
#' @export
#' 
df.as.text  <- function(df, sep = "\t") {
  stringr::str_replace_all( 
    
    c(
        paste0(names(df),collapse="\t")
      , purrr::map_chr(1:nrow(df), ~ paste0(unlist(df[.x,]), collapse = sep) )
    )
    , "NA|Inf",""
  )
}





