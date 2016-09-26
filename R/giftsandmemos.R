#' Create a List of Gifts and memos
#'  
#'
#' @param collectdata a logical value describing whether or not the data should be collected from the database
#'
#' @return if collectdata == T, a data frame of all gifts and memos with one line per transaction. If collectdata is FALSE, a tbl_sql with 6 columns:
#'     - pidm - constituent's id
#'     - desg - the designation number to which the gift was given
#'     - campaign - the campaign in which the gift was given
#'     - fisc_code - the fiscal year in which the gift was given
#'     - amt - the gift amount or the amount which was memo credited to the constituent
#'     - dt - the gift date
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
    dplyr::union(gifts,memos)
  }
  
  
  
  
}