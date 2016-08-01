#' Apply Email Exclusoins
#' 
#' @description Apply the standard Millikin email exclusions to a mailing list.
#'
#' @param df A data frame to be used as a mailing list. 
#' @param idcol A character string describing the name of the id column. Defaults to "pidm". If prefemail, deceased, primdnrind and excl columns do not exist in df, they will be joined in from hallptbl using this column.
#' @param primary.only A logical value indicating wither only the primary contact in a couple be included. Defaults to TRUE.
#' @param include.students A logical value indicating whether or not students should be kept in the list. By default, students are excluded.
#' @param sol A logical vlaue indicating whether or not the mailing is a solicitation. Note that if sol is TRUE, a warning will be generated if both exclude.fall and exclude.spring are FALSE.
#' @param exclude.fall A logical value indicating whether or not constituents with a NFC, No Fall Contact, exclusion code should be excluded.
#' @param exclude.spring A logical value indicating whether or not constituents with a NSC, No Spring Contact, exclusion code should be excluded.
#'
#' @return a data frame filtered for a traditional mailing. If prefemail, deceased, primdnrind or excl don't already exist in df, they wil be added
#' @export
 
apply.email.exclusions  <- function(df,  idcol = 'pidm', primary.only = F, include.students = F, sol = F, exclude.fall = F, exclude.spring = F) {
  
  
  if ( sol & exclude.fall + exclude.spring == 0) {
    warning("Solicitation mailings may need to exclude fall or spring!")
  }
  
  if ( (exclude.fall|exclude.spring) & !sol) {
    stop("Cannot exclude fall or spring for non-solicitation.")
  }
  
  
  mailexclcols      <-  c(' prefemail','deceased','primdnrind','excl')
  hasnecessarycols  <- sum( mailexclcols %in% names(df), na.rm= T) == length(mailexclcols)
  
  
  # get filtering columns
  if( !hasnecessarycols ) {
    warning("Joining to hallptbl. This might take a while...")
    options(warn=-1) # turn off warnings for mysql
    
    hallptbl          <- tbl( src_mysql('commits' ), 'hallp')
    df  <- df  %>% 
      left_join(hallptbl  %>% select( pidm,  prefemail, deceased, primdnrind, excl), by = c(idcol = 'pidm'), copy = T)
                
                options(warn=0) # turn warnings back on
  } 
  
  
  # standard mail filters
  df  <- df  %>% 
    filter(
      !is.blank(  prefemail)
      , deceased == 'N'
      , !grepl("NOC|NME",excl)
    )
  
  
  # No mail solicitation 
  if( sol ) {
    df  <- df  %>% 
      filter( !grepl('NES',excl) )
  } 
  
  # No fall contact solicitation 
  if( exclude.fall ) {
    df  <- df  %>% 
      filter( !grepl('NFC',excl))
  }
  
  # No spring contact solicitation 
  if( exclude.spring ) {
    df  <- df  %>% 
      filter( !grepl('NSC',excl))
  }
  
  
  # Primary contact only
  if( primary.only) {
    df  <- df  %>% 
      filter( primdnrind == 'Y')
  }
  
  # Exclude students by exclusion
  if( !include.students ) {
    df  <- df %>% 
      filter( 
        !grepl("SNC",excl)
      )
    
  }
  
  
  
  df
  
  
  
}
