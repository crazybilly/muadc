#' Clean the names of a data frame
#' 
#' Clean up a data frame by moving all column names to lower case, removing spaces and punctuation. Also, when dplyr is available, return the data frame as a tbl_df.
#' 
#' @param df a data frame 
#' @param keep.underscore a logical value indicating whether underscores be kept or removed
#' 
#' @return a data frame, or if dplyr is available, a tbl_df. 
#' @export

clean.df <- function(df, keep.underscore = TRUE) {
  
  if( keep.underscore) {
    # remove everything except alphanumeric and underscore
    names(df)  <- gsub("\\W", "", names(df) )
  } else {
    # remove punctuation and then remove the underscores, too
    names(df)  <- gsub("_", "",  gsub("\\W", "", names(df) ) )
  }
    
  names(df) <- tolower(names(df))
  
  # if dplyr's already loaded, make the table a dplyr data_frame
  #   if not, don't worry about it--it's only cosmetic anyway
  if ("package:dplyr" %in% search() ) {
    df  <- dplyr::tbl_df(df)
  }
  
  df
  
}
