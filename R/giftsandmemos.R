#' Create a List of Gifts and memos
#'  
#'
#' @param collectdata a logical value describing whether or not the data should be collected from the database
#'
#' @details If collectdata == T, returns a data frame of all gifts and memos with one line per transaction. If collectdata is FALSE, a tbl_sql with 6 columns:
#' \itemize{
#'  \item{"pidm"}{ constituent's id}
#'  \item{"desg"}{ the designation number to which the gift was given}
#'  \item{"campaign"}{ the campaign in which the gift was given}
#'  \item{"fisc_code"}{ the fiscal year in which the gift was given}
#'  \item{"amt"}{ the gift amount or the amount which was memo credited to the constituent}
#'  \item{"dt"}{ the date the gift or memo was receipted}
#' }
#'     
#' @return a 6 column data frame
#'     
#' @export
#'
giftsandmemos  <- function( collectdata = F) {
  
  if( !exists("giftstbl" )) {
    initcommitsdb()
  }
  
  
  gifts  = giftstbl %>% 
    select(
      pidm = PIDM
      , desg = GIFT_DESG
      , campaign = CAMPAIGN
      , fisc_code = FISC_CODE
      , amt = GIFT_AMT
      , dt  = GIFT_DATE
    )
  
  memos =  memostbl %>% 
    select(
      pidm = memopidm
      , desg
      , campaign
      , fisc_code
      , amt = memo_amt
      , dt = gift_date
    )
  
  if( collectdata ) {
    bind_rows(
      gift = gifts %>% collect() 
      , memo = memos %>% collect() 
      , .id  = 'type'
    )
  } else {
    gifts %<>% mutate(type = 'gift')
    memos %<>% mutate(type = 'memo')
    dplyr::union_all(gifts,memos)
  }
  
  
  
  
}