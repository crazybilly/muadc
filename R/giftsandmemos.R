#' Create a List of Gifts and Memos
#'  
#' @description Build a list of all gifts (or commitments) and memos.
#'
#' @param onedollar a logical value describing whether or not gifts of $1 or less should be considered
#' @param collectdata a logical value describing whether or not the data should be collected from the database
#'
#' @details If collectdata == T, returns a data frame of all gifts/commitments and memos with one line per transaction. If collectdata is FALSE, a tbl_sql with 6 columns:
#' \itemize{
#'  \item{"pidm"}{ constituent's id}
#'  \item{"desg"}{ the designation number to which the gift was given}
#'  \item{"campaign"}{ the campaign in which the gift was given}
#'  \item{"fisc_code"}{ the fiscal year in which the gift was given}
#'  \item{"amt"}{ the gift amount or the amount which was memo credited to the constituent}
#'  \item{"dt"}{ the date the gift or memo was receipted}
#' }
#'     
#' @return a 6 column data_frame or a tbl_sql
#'     
#' @export
#'
giftsandmemos  <-  function( onedollar = F, collectdata = F) {
    
    if( !exists("giftstbl" )) {
      initcommitsdb()
    }
    
    
    gifts  = giftstbl %>% 
      select(
        pidm 
        , desg
        , campaign
        , fisc_code
        , amt = gift_amt
        , dt  = gift_date
      )
    
    
    memos =  memostbl %>% 
      select(
        pidm = memo_pidm
        , desg
        , campaign
        , fisc_code
        , amt = memo_amt
        , dt = gift_date
      )
    
    if(!onedollar) {
      gifts %<>% filter(amt > 1)
      memos %<>% filter(amt > 1)
    }
    
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



#' @rdname giftsandmemos
#' 
#' @export
commitsandmemos  <-  function( onedollar = F, collectdata = F) {
  
  if( !exists("committbl" )) {
    initcommitsdb()
  }
  
  
  commits  = committbl %>% 
    select(
      pidm 
      , desg
      , campaign
      , fisc_code
      , amt = commit_amt
      , dt  = commit_date
    )
  
  memos =  memostbl %>% 
    select(
      pidm = memo_pidm
      , desg
      , campaign
      , fisc_code
      , amt = memo_amt
      , dt = gift_date
    )
  
  
  if(!onedollar) {
    commits  %<>% filter(amt > 1)
    memos %<>% filter(amt > 1)
  }
  
  if( collectdata ) {
    bind_rows(
        commit = commits %>% collect() 
      , memo   = memos %>% collect() 
      , .id    = 'type'
    )
  } else {
    commits %<>% mutate(type = 'commit')
    memos %<>% mutate(type = 'memo')
    dplyr::union_all(commits,memos)
  }
  
  
  
  
}
