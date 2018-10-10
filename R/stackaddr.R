#' Stack Names Based on Mailing Address
#'
#' @param df a data frame of a mailing list
#' @param segment an optional segment to segment by
#'
#' @details segment is currently mandatory--it should be eventually be made optional. df must include at least the following columns: \itemize{
#'    \item pidm
#'    \item addr1
#'    \item addr2
#'    \item addr3
#'    \item city
#'    \item st
#'    \item zip
#'    \item keyline1
#'    \item keyline2
#'    \item primdnrind
#'    \item prefname
#'    \item cmname
#'    \item frms
#'    \item frmp
#'    \item nckp
#'    \item preffirstname
#'    
#' }
#' @return a data frame with rows that share an address and a segement stacked, reducing mailing numbers. 
#' 
#' @export
#'
stackaddr  <- function(df, segment) {
  
  segmentq  <- enquo(segment)
  
  primarystuff  <- df %>% select(everything(), mysegment = !!segmentq)
  spstuff  <- df %>% select(sppidm = pidm, spsegment = !!segmentq)
  
  mail3  <- primarystuff %>% 
    dupstatus( segmentcol = mysegment)  %>%
    left_join(spstuff, by = 'sppidm') %>% 
    mutate(
      segmentmatch = mysegment == spsegment
      , primdnrind   = yesno.as.logical(primdnrind) 
      , row_num = row_number()
    )
  
  
  
  # prep for stacking --------------------------------------------------------
  
  # all the single people
  singles  <- mail3 %>% 
    filter(
        dupstatus == "single"      |
        dupstatus == "bulk inbox" 
    ) %>% 
    mutate(
      envelopesal = prefname
      , lettersalformal = frms
      , lettersalinformal = preffirstname
    )
  
  
  # all the married folks
  spouse  <- mail3 %>% 
    filter( 
      str_detect(dupstatus, "spouse") 
      , (primdnrind | (!segmentmatch & !primdnrind) )
    ) %>% 
    mutate(
        envelopesal = ifelse(segmentmatch,     cmname, prefname      ) 
      , lettersalformal = ifelse(segmentmatch,   frmp, frms          )
      , lettersalinformal = ifelse(segmentmatch, nckp, preffirstname )
    )
 
  
  morethan3  <- mail3 %>% 
    filter(countcat > 3) %>% 
    mutate(
        `envelope 1` = prefname
      , `letterformal 1` = frms
      , `letterinformal 1` = preffirstname
    )
  
  
  tostack  <- mail3 %>% 
    filter(countcat <= 3) %>% 
    filter( dupstatus == 'other family' | dupstatus == 'relationships unknown')  %>% 
    mutate( catkey = row_num)
  
  
  stacked  <- tostack %>% 
    arrange(pidm) %>% 
    group_by(cataddr,mysegment) %>% 
    summarize(
      envelopenames = paste(prefname, collapse ="QQQ") 
      , letterformalnames = paste(frms, collapse = "QQQ")
      , letterinformalnames = paste(preffirstname, collapse = "QQQ")
    ) %>% 
    separate(envelopenames,into = paste("envelope",c(1,unique(tostack$countcat))), sep = "QQQ" ) %>% 
    separate(letterformalnames,into = paste("letterformal",c(1,unique(tostack$countcat))), sep = "QQQ") %>% 
    separate(letterinformalnames,into = paste("letterinformal",c(1,unique(tostack$countcat))), sep = "QQQ") %>% 
    left_join(mail3 %>% 
                group_by(cataddr) %>% 
                summarize(
                  addr1 = unique(addr1)
                  , addr2 = ifelse(all(is.na(mail3$addr2)), NA, unique(addr2))
                  , addr3 = ifelse(all(is.na(mail3$addr3)), NA, unique(addr3))
                  , city  = unique(city)
                  , st    = unique(st)
                  , zip   = unique(zip)
                  , pidm  = min(pidm)
                )
              , by = 'cataddr'
    ) %>% 
    left_join(mail3 %>% select(pidm, keyline1, keyline2) %>% distinct, by = 'pidm')
  
  
  # pull together -----------------------------------------------------------
  
  bind_rows( singles, spouse, stacked, morethan3, .id = 'type') %>% 
    mutate(
        `envelope 1` = coalesce(envelopesal, `envelope 1`)
      , `letterformal 1` = coalesce(lettersalformal, `letterformal 1`)
      , `letterinformal 1` = coalesce(lettersalinformal, `letterinformal 1`)
    ) %>% 
    select(
      pidm
      , finalsegment = mysegment
      , contains("envelope")
      , contains("letter")
      , addr1:keyline2
      # , type
      , -envelopesal
      , -lettersalformal
      , -lettersalinformal
    )
  
}