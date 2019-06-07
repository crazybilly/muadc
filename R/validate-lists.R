# for formatting the html table
valformatter  <- formattable::formatter(
  "span"
  , style = ~ style( 
    font.weight = 'bold'
    , color = case_when(
        Value == 0 & str_detect(Category, "Total Number|Number of Primaries") ~ "red"
      , Value >  0 & str_detect(Category, "People With No Addresses|People with No State or Zip|International People|People With No Names|Deceased People|Organizations|Marked NOC|Marked No Mail|People With No Email|Marked No Email") ~ "red"
      , Value >  0 & str_detect(Category, "Secondaries with Spouse on List") ~ "orange"
      , T ~ "green"  
    ))
  , ~ formattable::icontext(case_when(
      Value == 0 & str_detect(Category, "Total Number|Number of Primaries") ~ "alert"
    , Value >  0 & str_detect(Category, "People With No Addresses|People with No State or Zip|International People|People With No Names|Deceased People|Organizations|Marked NOC|Marked No Mail|People With No Email|Marked No Email") ~ "alert"
    , Value >  0 & str_detect(Category, "Secondaries with Spouse on List") ~ "alert"
    , T ~ "check"
    )
    , Value
  )
)
  
  
  
  
#' Validate an Mail or Email List
#' 
#' @param df a data frame that is a mail or email list. Email lists should include at least pidm, and prefemail. Mail lists should include:
#' \enumerate{
#'   \item pidm
#'   \item addr1
#'   \item st
#'   \item zip
#' }
#' @param type a character string describing whether df is a "mail" or "email" list
#' @param solicitation a logical value describing whether or not df is a solicitation. Determines whether or not solicitation exclusion codes should be used.
#' @param h a database table or local data frame that looks like hallptbl 
#' 
#' @value a list with summary data. An html report of the data will be opened as well
#' 
#' @import dplyr
#' @import stringr
#' @import formattable 
#' 
#' @export
validate_list  <- function(df, type = 'mail', solicitation = T,  h = hallptbl ) {
  
  if(type == 'mail') {
   
    rmdlocation  <- system.file("rmd", "mailingreport.Rmd", package = 'muadc')
     
    theinfo  <- validate_mail(df, solicitation = solicitation, h = h)
    results  <- rmarkdown::render(rmdlocation, params = list(allinfo = theinfo))  
    
  } else if ( type == 'email') {
    
    rmdlocation  <- system.file("rmd", "emailreport.Rmd", package = 'muadc')
    
    theinfo  <- validate_email(df,  solicitation = solicitation, h = h)
    results  <- rmarkdown::render(rmdlocation, params = list(allinfo = theinfo)) 
    
  }
  
    rstudioapi::viewer(results) 
  
    return(theinfo)
}

# the email version
validate_email  <- function(df, solicitation = T, h = hallptbl) {
  
  if(any( !"prefemail" %in% names(df), !"pidm" %in% names(df)) ) {
    error("df is not a proper email list")
  }
  
  extracols  <-   c("deceased", "excl", "primdnrind", "primdnrc", "classyr", "solc")
  
  if( any(!extracols %in% names(df)) ){
    df  <- df %>% 
      select(-matches(extracols)) %>% 
      left_join(
        h %>% 
          select(pidm, matches(extracols) )
        , by = 'pidm', copy =  T)
  }
  
  df  <- df %>% 
    mutate(
      class2  = case_when(
        classyr < 1902 ~ na_int
        , classyr > currentFY + 10 ~ na_int
        , T ~classyr)
      , hasaddr = case_when(
        is.na(st) ~ "gray"
        , st %in% c(state.abb,"DC") ~ "blue"
        , T ~ "red"
      )
      , sponlist = sppidm %in% .$pidm
      , secondary_with_sp_on_list = sponlist & !yesno.as.logical(primdnrind)
    )
  
  
  n_overall  <- df %>% 
    nrow()
  
  n_missing_email  <- df %>% 
    filter(is.blank(prefemail)) %>% 
    nrow()
  
  n_primary  <- df %>% 
    filter(yesno.as.logical(primdnrind)) %>% 
    nrow()
  
  n_secondary_with_sp_on_list  <- df %>% 
    filter(secondary_with_sp_on_list) %>% 
    nrow()
  
  n_orgs <- df %>% 
    filter(is.na(deceased) | deceased == 'INACTIVE') %>% 
    nrow()
  
  n_deceased  <- df %>% 
    filter(deceased == 'Y') %>% 
    nrow()
  
  n_noc <- df %>% 
    filter(str_detect2(excl, "NOC")) %>% 
    nrow()
  
  no_email_regex  <- ifelse(solicitation, "NEC|NES", "NEC")
  
  n_nec <- df %>% 
    filter(str_detect2(excl, no_email_regex)) %>% 
    nrow()
  
  count_primdnrc  <- df %>% 
    count(primdnrc) 
  
  count_solc  <- df %>% 
    count(solc) 
  
  noclass_n  <- sum(is.na(df$class2))
  
  left  <- min(df$class2, na.rm = T) + 10
  
  hist_classyr  <-  df %>% 
    ggplot(aes(x = class2)) +
    geom_bar() + 
    annotate("text", label = paste("No class yr:", noclass_n), color = millikinblue, x = left, y = 0, vjust = -5, size = 5)  +
    theme_minimal()+ 
    ggtitle("# of People in Each Class Year") + 
    xlab(label = element_blank()) + 
    ylab(label = element_blank())
  
  thenumbers  <- data_frame(
    Category = c(
        "Total Number"
      , "Number of Primaries"
      , "Secondaries with Spouse on List"
      , "People With No Email"
      , "Deceased People"
      , "Organizations"
      , "Marked NOC"
      , "Marked No Email"
    )
    , Value = c(
        n_overall
      , n_primary
      , n_secondary_with_sp_on_list
      , n_missing_email
      , n_deceased
      , n_orgs
      , n_noc
      , n_nec
    )
  ) %>% 
  formattable( align = c("l", "r"), list(
    Value = valformatter
  ))
      
  
  
  list(
      thenumbers = thenumbers
    , count_primdnrc  = count_primdnrc 
    , count_solc  = count_solc 
    , hist_classyr  = hist_classyr 
  )
  
  
  
}





# the mail version
validate_mail  <- function(df, solicitation = T, namefields = c("name", "firstname", "lastname", "cmname", "frmp", "frms"), h = hallptbl) {
  
    extracols  <-   c("deceased", "excl", "primdnrind", "primdnrc", "classyr", "solc", "sppidm")
  
    if(any( !"addr1" %in% names(df), !"pidm" %in% names(df), !"st" %in% names(df)) ) {
      error("df is not a proper mailing list")
    }
    
    
    if( any(!extracols %in% names(df)) ){
      df  <- df %>% 
        select(-matches(extracols)) %>% 
        left_join(
          h %>% 
            select(pidm, matches(extracols) )
          , by = 'pidm', copy =  T)
    }
  
  
    df  <- df %>% 
      mutate(
        class2  = case_when(
          classyr < 1902 ~ na_int
          , classyr > currentFY + 10 ~ na_int
          , T ~classyr)
        , hasaddr = case_when(
          is.na(st) ~ "gray"
          , st %in% c(state.abb,"DC") ~ "blue"
          , T ~ "red"
        )
        , sponlist = sppidm %in% .$pidm
        , secondary_with_sp_on_list = sponlist & !yesno.as.logical(primdnrind)
      )
    
    
  
    n_intl  <- df %>% 
      filter(
        is.na(st) | !st %in% state.abb
      ) %>% 
      nrow()
   
    
    n_no_state_or_zip  <- df %>% 
      filter(
        is.na(st) | is.na(zip)
      ) %>% 
      nrow()
    
    n_no_addr  <- df %>% 
      filter(
        is.na(addr1)
      ) %>% 
      nrow()
    
    nameregex  <- namefields %>% paste0("^", ., "$") %>% paste0(collapse = "|")
    
    n_no_name  <- df %>% 
      filter_at(vars(matches(nameregex)), any_vars(is.blank(.)) ) %>% 
      nrow()
    
        n_deceased  <- df %>% 
      filter(deceased == 'Y') %>% 
      nrow()
        
    n_orgs <- df %>% 
      filter(is.na(deceased) | deceased == 'INACTIVE') %>% 
      nrow()
    
    n_noc <- df %>% 
      filter(str_detect2(excl, "NOC")) %>% 
      nrow()
    
    
    no_mail_regex  <- ifelse(solicitation, "NMC|NMS", "NMC")
    
    n_nmc <- df %>% 
      filter(str_detect2(excl, no_mail_regex)) %>% 
      nrow()
    
    n_primary  <- df %>% 
      filter(yesno.as.logical(primdnrind)) %>% 
      nrow()
    
    count_primdnrc  <- df %>% 
      count(primdnrc) 
    
    
    n_secondary_with_sp_on_list  <- df %>% 
      filter(secondary_with_sp_on_list) %>% 
      nrow()
    
    
    n_overall  <- df %>% 
      nrow()
    
    count_solc  <- df %>% 
      count(solc) 
    
    count_cataddr_n  <- df %>% 
      mutate(cataddr = paste(addr1, city, st, zip)) %>% 
      count(cataddr) %>% 
      count(n) %>% 
      select(repeated_addrs = n, n = nn)
    
    
    noclass_n  <- sum(is.na(df$class2))
    
    left  <- min(df$class2, na.rm = T) + 10
    
    hist_classyr  <-  df %>% 
      ggplot(aes(x = class2)) +
        geom_bar() + 
        annotate("text", label = paste("No class yr:", noclass_n), color = millikinblue, x = left, y = 0, vjust = -5, size = 5)  +
        theme_minimal()+ 
        ggtitle("# of People in Each Class Year") + 
        xlab(label = element_blank()) + 
        ylab(label = element_blank())
    
    
    hist_st  <- df %>% 
      ggplot( aes(x=fct_infreq(st), fill = hasaddr) ) + 
        geom_bar() + 
        coord_trans(y = 'log1p') +
        theme_minimal() + 
        theme(
          axis.text.x = element_text(angle = 90)
          , legend.position = 'none'
        ) +
        scale_fill_manual(values = c(millikinblue, "gray", "red")) + 
        ggtitle("# of People in Each State") + 
        xlab(label = element_blank()) + 
        ylab(label = element_blank()) 
    
  thenumbers  <- data_frame(
    Category = c(
      "Total Number"
      , "People With No Addresses"
      , "People with No State or Zip"
      , "International People"
      , "People With No Names"
      , "Deceased People"
      , "Organizations"
      , "Marked NOC"
      , "Marked No Mail"
      , "Number of Primaries"
      )
    , Value = c(
        n_overall
      , n_no_addr
      , n_no_state_or_zip
      , n_intl
      , n_no_name
      , n_deceased
      , n_orgs
      , n_noc
      , n_nmc
      , n_primary
      )
    ) %>% 
    formattable( align = c("l", "r"), list(
      Value = valformatter
    ))
    
  
  list(
        thenumbers = thenumbers
      , count_primdnrc  = count_primdnrc 
      , count_solc  = count_solc 
      , count_cataddr_n  = count_cataddr_n 
      , hist_classyr  = hist_classyr 
      , hist_st  = hist_st 
  )    
  
  
}




