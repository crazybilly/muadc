#' Get Primary People
#' @description turn a data frame of people into a data frame of only primary. Grabs any primary people that are on the list, plus the primary spouse if they're not already on the list.
#'
#' @param df a data frame
#' @param y a hallp-like data frame or database table
#' @param by a character vector of variables to join by
#'
#' @return a one columns data frame of only primary people's pidms
#' @export
#'
getprimaries  <- function(df, y = hallptbl, by = 'pidm') {
  
  idq  <- rlang::sym(by)
  
  
  # get a list of everybody
  withprimarypidm  <- df %>%
    distinct(!!idq) %>%
    left_join(y %>% select(pidm, primarypidm), by = by, copy = T) 
  
  
  primarypidms  <- withprimarypidm %>%
    distinct(primarypidm) %>%
    rename(pidm = primarypidm) 
  
  return(primarypidms)
  # 
  # existingcolumns  <- names(df)
  # hallpnames  <- head(y) %>% collect %>% names()
  # 
  # yinx  <- hallpnames %in% existingcolumns
  # ysyms  <- syms(hallpnames[yinx]) 
  # 
  # ysource  <- y %>% 
  #   select(!!!ysyms)
  # 
  # 
  # allcols  <- primarypidms %>% 
  #   left_join(df, by = by) %>% 
  #   left_join(ysource, by = by, copy = T) 
  # 
  # 
  # sharedcolnames  <- existingcolumns[existingcolumns %in% hallpnames][existingcolumns[existingcolumns %in% hallpnames] != by]
  # 
  # sharedcols  <- sharedcolnames %>% 
  #   map(~ muadc::coalesce( 
  #     allcols[paste0(.x, ".x")]
  #     , allcols[paste0(.x, ".y")]  
  #   )
  #   ) %>% 
  #   bind_cols()
  # 
  # unsharedcols  <- allcols %>%
  #   select(-matches("\\..$"))
  # 
  # finalcols  <- bind_cols(
  #   unsharedcols
  #   , sharedcols
  # )
  # 
  # names(finalcols)  <- gsub(".x$|.y$", "", names(finalcols) )
  # 
  # existingcolq  <- syms(existingcolumns)
  # 
  # finalcols %>% 
  #   select(!!!existingcolq)
  # 
  # 
}


