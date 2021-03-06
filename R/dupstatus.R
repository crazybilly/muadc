

# helper/mutate functions -----------------------------

# this function is largely useless - just use paste
# #' @title CatAdd
# #'
# #' @description Creates a concatenated address. Primarily for use in dplyr::mutate().
# #' @param addr a referece to the address column
# #' @param city a reference to the city column
# #' @param st   a reference to the state column
# #' @param zip  a reference to the zip code column
# #' @param sep  a string to be used to seperate the columns
# #' @return returns the 4 columns pasted together
# #' @export
# 
# catadd <- function( addr = addr1, city = city, st = st, zip = zip, sep = ' ' ) {
#   addq  <- enquo(addr)
#   cityq <- enquo(city)
#   stq   <- enquo(st)
#   zipq  <- enquo(zip)
#   
#   paste(!!addq, !!cityq, !!stq, !!zipq, sep = sep)
# }
# 
# 


#' @title Countif
#' 
#' @description returns a vector of counts for a vector. Uses table() to calculate counts. Primarily for use in dplyr::mutate().
#' @param col a referece to the column to be counted. 
#' @return a integer vector of counts for each value in col.
#' @export

countif  <- function( col  ) {
  table(col)[as.character(col)]  %>% as.integer()
}





# buildlongxref ---------------------------------------------------------------


#' @title buildlongxref
#' @description build a table of all available xrefs from existing csvs (called from XAXREF). Also pulls all spouse xrefs from hallp. Used by getrelationships().
#' @param ... as many file locations as you have available. All csvs will be read in and bad rows dumped.
#' @param calculateCH a logical value determing whether or not CH relationships should be calculated from PA codes. 
#' @return a data frame with pidm, xref_pidm and xref_code columns in long format.
buildlongxref  <- function( ... , calculateCH = T ) {
  
  args  <- list(...)
  
  # union any called files
  allfiles  <- lapply( args , function(x) {
    
    filemodtime  <- file.info(x)[['mtime']]
    timesincemod  <- filemodtime - ymd_hms(Sys.time())
    
    if( timesincemod > as.difftime(1, units='weeks') ) {
      warning(paste('XAXREF for',x,'is older than 1 week'))
    }
    
    
    xref  <- read.tidy(x)  %>% 
      filter(!is.na(as.integer(xref_pidm)), xref_code == xref_code,!grepl("DUP",xref_code), !grepl("DUP",xref_pidm))   %>%  # remove bad rows
      mutate(pidm = as.integer(pidm), xref_pidm = as.integer(xref_pidm))  %>% 
      select(pidm,xref_pidm,xref_code) 
  })  %>% 
    rbind_all()
  
  
  # if you don't have access to the database, initialize it
  if( !exists('hallptbl' )) {
    initcommitsdb()
  }
  
  # add in all spouse pidms
  spouses  <- hallptbl  %>%
    filter(!is.na(sppidm))  %>% 
    select(pidm,xref_pidm = sppidm)  %>% 
    mutate (xref_code = 'SP')  %>% 
    collect
  
  
  allfiles  <- rbind_list(allfiles,spouses)  %>% 
    distinct 
  
  chxrefs  <- allfiles  %>% 
    filter(xref_code == 'PA')  %>% 
    select(pidm = xref_pidm, xref_pidm = pidm )  %>% 
    mutate( xref_code = 'CH') 
  
  allfiles  %>% 
    rbind_list(chxrefs)
  
  
}


# getrelationships --------------------------------------------------------

#' @title GetRelationships
#' @description builds a table showing what relationships each individual in a data frame has with others in the same data frame. Used by dupstatus().
#' @param df a data frame to work on with one line per constituent with one pidm column.
#' @param idcol a string naming the pidm column
#' @param xrefslong an object reference to a data frame built by buildlongxref() on the same data. If NULL, this is generated from data/xaxref-pa.csv and data/xaxref-si.csv.
#' @return a data frame with one row per person who has any relationship to someone else in the database. Constituents who are not related to anyone else are removed.

getrelationships  <- function(df, idcol = 'pidm', xrefslong = NULL ) {
  
  
  if( is.null(xrefslong) ) {
    xrefslong  <- buildlongxref('G:/ALUMNI/Jake T/datawarehouse/data/xaxref-pa.csv','G:/ALUMNI/Jake T/datawarehouse/data/xaxref-si.csv')
  }
  
  
  prepped  <- df  %>% 
    mutate(   cataddr = paste(addr1,city,st,zip)
              , countcat = countif(cataddr)
    )   
  
  prepped  <- prepped %>% 
    left_join(prepped  %>% select(duppidm = pidm, cataddr), by = 'cataddr')  %>% 
    mutate(duppidm = ifelse(pidm == duppidm, NA, duppidm)   ) %>% 
    left_join( xrefslong, by = c('pidm','duppidm' = 'xref_pidm') ) %>% 
    filter( countcat ==1 | (countcat > 1 & !is.na(duppidm) ))  %>% 
    filter( !is.na(xref_code) )  %>% 
    select(pidm, xref_code)  %>% 
    distinct() %>% 
    mutate(x= T)  %>% 
    spread(xref_code,x)
  
  prepped[is.na(prepped)]  <- F
  
  prepped
  
}


# dupstatus ---------------------------------------------------------------


#' @title Duplicate Status
#' 
#' @description calculate dup status
#' 
#' @param df a data frame to work on 
#' @param dfxrefs a data frame of xrefs generated by getrelationships(). If NA, this is generated.
#' @param xrefslong a data frame in long format of all known xrefs, passed as an argument to getrelationships() if necessary. 
#'    If NA, defaults to xaxref-pa.csv and xaxref-si.csv in data/warehouse.
#' @param segmentcol an the unquoted name of a column that df is segemented by
#' 
#' @return a data frame with new columns describing status. Possible statuses for the dupstatus are: \itemize{
#'    \item dupstatus a column describing whether the row is a duplicate or not. Possible statuses include: \itemize{
#'        \item single - no relationship to anyone else in df
#'        \item bulk inbox - address is a bulk inbox (1184 W Main, PO Box 1955 or 301 SW Adams)
#'        \item relationships unknown - constituent shares an address with one or more people in df, but are not xref'd to those people
#'        \item primary spouse - constituent is married to another constituent in df and is the primary
#'        \item 2nd spouse - constituent is married to another constituent in df and is not the primary
#'        \item other family - constituent shares an address with someone else on th elist and that person's child, parent or sibling
#'        }
#'    \item cataddr - the concatenated mailing address
#'    \item countcat - a count of how many times cataddr appears in df
#'    \item bulkaddr - a logical value describing whether cataddr is a bulk mail box
#'    \item CH - whether the row is a child of someone else in df
#'    \item PA - whether the row is a parent of someone else in df
#'    \item SI - whether the row is a sibling of someone else in df
#'    \item SP - whether the row is a spouse of someone else in df
#'    }
#'  Also included are cataddr (concatenated address), countcat (a count of how often cataddr appears in df), bulkaddr a logical 
#'    
#' @export
 
dupstatus  <- function(df, dfxrefs = NA, xrefslong = NA, segmentcol = NA  ) { 
  
  if( is.na(dfxrefs) ) {
    dfxrefs  <- getrelationships(df, xrefslong)
  }
  
  bulkmailaddresses  <-  "1184 W Main|PO Box 1955|301 SW Adams"
  segmentcol  <- lazyeval::lazy(segmentcol)
  
  
  df  %>% 
    left_join(dfxrefs, by = 'pidm')  %>% 
    mutate(
        cataddr = paste(addr1,city,st,zip)
      , countcat = countif(cataddr)
      , bulkaddr = grepl(bulkmailaddresses,cataddr)
      , CH = replace(CH,is.na(CH),F)
      , PA = replace(PA,is.na(PA),F)
      , SI = replace(SI,is.na(SI),F)
      , SP = replace(SP,is.na(SP),F)
    )  %>% 
    mutate(
      dupstatus = ifelse( countcat == 1, 'single'
                          , ifelse( bulkaddr, 'bulk inbox'
                                    , ifelse( CH+PA+SI+SP == 0, 'relationships unknown'
                                              , ifelse( SP, ifelse(is.na(primdnrind), '2nd spouse', 'primary spouse')
                                                        ,'other family'
                                              ))))
    ) 
  
  
}
