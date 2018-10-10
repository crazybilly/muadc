#' Clean the names of a data frame
#' 
#' Clean up a data frame by changing all column names to lower case, removing spaces and punctuation. 
#' 
#' @param df a data frame 
#' @param keep.underscore a logical value indicating whether underscores be kept or removed
#' @param pidmpattern a regular expression to be matched against the names of df. Set to `NULL` to turn off pattern mathing.
#' 
#' @details If no column named `pidm` is found and pidmpattern is not `NULL`, then we look if there's only 
#' 
#' @return a data frame of class tbl_df. 
#' @export

clean.df <- function(df, keep.underscore = TRUE, pidmpattern = 'bannerid|banner_id') {
  
  if( keep.underscore) {
    # remove everything except alphanumeric and underscore
    names(df)  <- gsub("\\W", "", names(df) )
  } else {
    # remove punctuation and then remove the underscores, too
    names(df)  <- gsub("_", "",  gsub("\\W", "", names(df) ) )
  }
    
  names(df) <- tolower(names(df))
  
  # if you don't have a column named pidm already, look for other bannerid and use that?
  if( !is.null(pidmpattern) & !any(names(df) == 'pidm')  ) {
    
    n_pidmcols  <- sum(grepl(pidmpattern, names(df) ))
    
    # don't rename if you have more than one matching col
    if( n_pidmcols > 1) {
      
      warning(n_pidmcols, " columns match pidmpattern regex: ", pidmpattern, ". Possible pidm columns not renamed.")
      
    } else {
      
      coltorename = names(df)[grepl(pidmpattern, names(df)) ]
      
      message("Renaming ", coltorename, " to pidm ...")
      dplyr::rename_at(df, dplyr::vars(matches(pidmpattern)), dplyr::funs(gsub(., ".*", "pidm")) )
    }
    
  }
  
  
  dplyr::tbl_df(df)
  
}

