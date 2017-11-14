#' Add Spouses
#'
#' @param df a data frame with at least a pidm column
#' @param spid if df is a local data frame, spid will be passed to the .id argument of bind_rows()
#'
#' @return a data frame with the same columns as df but with additional rows for any spouses. Spouse rows have identical rows to the original data. Union is used, so duplicates are removed.
#' @export
#'
addspouses  <- function(df, spid = 'spouse') {
  
  df_is_lazy  <- any(grepl("sql", class(df)))
  
  spouses  <- df %>% 
    left_join(hallptbl %>% select(pidm, sppidm), by = 'pidm', copy = T) %>% 
    select(-pidm) %>% 
    select(pidm = sppidm, everything()) %>% 
    filter(!is.na(pidm))
  
  if( df_is_lazy ) { 
    union( df ,spouses)
  } else {
    bind_rows('primary' = df , "spouse" = spouses %>% collect, .id = spid)
  }
  
  
}
