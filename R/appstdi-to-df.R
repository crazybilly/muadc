#' APPSTDI to Data Frame
#'
#' @param appstdi a character vector read in from an appstdi.lis file (via `readr::read_lines()`) 
#'
#' @return a data frame with one line per person. Multiple degree lines are dropped.
#' @export
#' 
#' @import stringr
#' @import dplyr
#'
appstdi_to_df   <- function(appstdi) {
  
  namerowsi    <- stringr::str_detect(appstdi, "^00\\d\\d\\d\\d\\d\\d")
  actionrowsi  <- stringr::str_detect(appstdi, "^(\\f)*                        .*20\\d\\d")
  
  namerows  <- appstdi[namerowsi] %>% 
    stringr::str_split("  ", simplify = T) %>% 
    dplyr::as_tibble(.name_repair = ~c("pidm", "name"))
  
  actionrows  <- appstdi[actionrowsi]
  
  action  <- actionrows %>% 
    stringr::str_replace("^\\s+(...).*", "\\1")
  
  dnrc  <- actionrows %>% 
    stringr::str_replace("^\\s+(...)\\s+(....).*", "\\2")
  
  classyr  <- actionrows %>% 
    stringr::str_replace("^\\s+(...)\\s+(....).*(20..).*", "\\3")
  
  
  namerows$action  <- action
  namerows$dnrc    <- dnrc
  namerows$classyr <- classyr
  
  namerows %>% 
    mutate(pidm = rlang::as_integer(as.numeric(pidm)) )
  
}
