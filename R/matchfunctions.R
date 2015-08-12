
# find location function - support function -----------------------------------------------------
#' @title Find columns 
#' @description Finds, within a data frame's columns, the first match for a series of regexp. Also send messages to the user to let them know what we matched on. Used within prepformatch().
#' @param df a data frame that you want to find the location on
#' @param matchtext a regular expression to be matched against columns names
#' 
#' @keywords internal
#' 
#' @return returns a single integer value, the index of the first column that matches matchtext

findlocation  <- function(df, matchtext ) {
   alllocations  <- unlist(
      sapply(matchtext, function(x) grep(x, names(df),ignore.case = T))
   ) 
   
   if ( sum(alllocations) == 0) {
      errortext = paste(deparse(substitute(matchtext)), 'matched no columns, using NA')
      warning(errortext)
      return(NA)
   } else {
      
      location  <- min(alllocations,na.rm=T)
      locationname  <- names(df)[location]
      matchtextname  <- deparse(substitute(matchtext))
      message(matchtextname, " matched to column named '", locationname,"'")
      location
   }
}



# prep the original data for matching function - support function ----------------------------------------------------------

#' @title Prep a data frame for matching.
#' @description Preps a data frame for matching by just grabbing the necessary columns. Currently does not handle missing columns/no matches very well. Needs to be improved.
#' @param x a data frame to be prepared
#' @param pidm a vector of regular expressions to try to match for pidm location
#' @param firstname a vector of regular expressions to try to match for firstname location 
#' @param lastname a vector of regular expressions to try to match for lastname location
#' @param addr a vector of regular expressions to try to match for address location
#' @param city a vector of regular expressions to try to match for city location
#' @param st a vector of regular expressions to try to match for state location
#' @param zip a vector of regular expressions to try to match for zip code location
#' @param email a vector of regular expressions to try to match for email location
#' @return a data frame with nrows(x) with the columns matched by the regular expressions.

prepformatch  <- function( x
                           , pidm = c('pidm','banner ID', 'bannerID')
                           , firstname = c('firstname','first name','^first$' )
                           , lastname = c('lastname','last name','^last$' )
                           , addr = c('address line 1','address','addr')
                           , city = c('city','town','hometown')
                           , st = c('state/province','state','^st$')
                           , zip = c('zip code', 'zipcode','zip')
                           , email = c('preferred email','preferred e-mail','pref email','pref e-mail','e-mail','email')) {
   
   
   pidmlocation = findlocation(x, pidm)
   firstnamelocation = findlocation(x, firstname)
   lastnamelocation = findlocation(x, lastname)
   addrlocation = findlocation(x, addr)
   citylocation = findlocation(x, city)
   stlocation = findlocation(x, st)
   ziplocation = findlocation(x, zip)
   emaillocation = findlocation(x, email)
   
   
   
   newdf  <- x[,c(pidmlocation,firstnamelocation,lastnamelocation,addrlocation,citylocation,stlocation,ziplocation,emaillocation)] %>% 
      setNames(c('pidm','firstname','lastname','addr','city','st','zip','email')) 
   
   newdf  %>% 
      mutate( id = seq_along(firstname))  %>% 
      select(id, everything())
}


# pull hallp data and prep it - support function ------------------------------------------------

#' @title Pull hallp
#' @description Pulls hallp and gets it ready for matching.
#' @param collectdata boolean value determing whether or not hallp data should be collected from the database. Defaults to TRUE.
#' @return a data frame of hallp data. Note that data is only collected, not prepped in any way.

prephallp  <- function(collectdata=T) {
   
   # need a way to do this for folks that don't have the database
   require(muadc)
   
   # should also test to see if db is available and only initialize it if it's not
   initcommitsdb()
   
   hallp  <- hallptbl  %>% 
      select(pidm, firstname, lastname, addr = `Address Line 1`, city = City, st = `State/Province`, zip = Zip, email = `PREF_E-mail`)  
   
   if(collectdata) {
      collect(hallp)
   } else{
      hallp
   }
}



# function to create matching columns - support function ----------------------------------------

#' @title Create matching strings.
#' @description Builds the columns to do pidm matching on. Requires a df built from prepformatch() or prephallp().
#' @param x a data frame processed by prepformatch() or prephallp()
#' @return a data frame with concatenated name fields which will be used for matching.

creatematchingcolumns  <- function(x) {
   mutate(x, 
            catname = gsub("[[:punct:]]|\\s", "", tolower(paste0(firstname,lastname)) )
          , lastfirstinitial = gsub("[[:punct:]]|\\s", "", paste0(lastname,substr(firstname,1,1)))
          , lastaddr = gsub("[[:punct:]]|\\s", "", tolower(paste0(lastname,substr(addr,1,nchar(addr)*.8)  )) )
   )
}




# create counts for each matching column - support function -------------------------------------

#' @title Create count columns
#' @description Creates COUNTIF style counts to determine whether 1:1 matching can be done on a particular field.
#' @param x a data frame which needs pidms
#' @param y a data frame of reference data, usually hallp
#' @return a data frame with nrows(x) with count columns appended

createcounts  <- function(x,y) {
   
   x  %>% 
      left_join(
         y  %>% 
            count(catname)  %>% 
            rename(n_catname = n)
         , by = 'catname'
      )  %>% 
      left_join(
         y  %>% 
            count(lastfirstinitial)  %>% 
            rename(n_lastfirst = n)
         , by = 'lastfirstinitial'
      )  %>% 
      left_join(
         y  %>% 
            count(lastaddr)  %>% 
            rename(n_lastaddr = n)
         , by = 'lastaddr'
      )  %>% 
      left_join(
         y  %>% 
            count(email)  %>% 
            rename(n_email = n)
         , by = 'email'
      ) 
   
}

# get pidms from email - support function -----------------------------------------------------

#' @title Get pidm based on email.
#' @description Returns a data frame with pidms based on email appended based on matching rules.
#' @param x a data frame which needs pidms
#' @param y a data frame of reference data, usually hallp
#' @return a data frame with nrows(x) with a new column, pidm_email, appended
 
getpidmsonemail <- function(x,y) {
   x  %>% 
      left_join(y  %>% 
                   filter(n_email == 1)  %>% 
                   select(pidm_email = pidm, email) 
                , by = 'email')  %>% 
      mutate(pidm_email = ifelse(n_email == 1, pidm_email , NA))
   
}

# get pidms from catname - support function -----------------------------------------------------

#' @title Get pidm based on name.
#' @description Returns a data frame with pidms based on catname appended.
#' @param x a data frame which needs pidms
#' @param y a data frame of reference data, usually hallp
#' @return a data frame with nrows(x) with a new column, pidm_catname, appended
 
getpidmsoncatname  <- function(x,y) {
   x  %>% 
      left_join(y  %>% 
                   filter(n_catname == 1)  %>% 
                   select(pidm_catname = pidm, catname) 
               , by = 'catname')  %>% 
      mutate(pidm_catname = ifelse(n_catname == 1, pidm_catname, NA))
   
}

# get pidms from lastfirst--------------------------------------------------

#' @title Get pidm based on lastname.
#' @description Returns a data frame with pidms based on lastname, first initial appended.
#' @param x a data frame which needs pidms
#' @param y a data frame of reference data, usually hallp
#' @return a data frame with nrows(x) with a new column, pidm_lastfirst, appended
 
getpidmsonlastfirst  <- function(x,y) {
   x  %>% 
      left_join(y  %>% 
                   filter(n_lastfirst == 1)  %>% 
                   select(pidm_lastfirst = pidm, lastfirstinitial)  
                , by = 'lastfirstinitial')  %>% 
      mutate(pidm_lastfirst = ifelse(n_lastfirst == 1, pidm_lastfirst, NA))
   
}

# get pidms from lastaddr --------------------------------------------------

#' @title Get pidm baesd on lastname, address.
#' @description Returns a data frame with pidms based on last nameand address appended.
#' @param x a data frame which needs pidms
#' @param y a data frame of reference data, usually hallp
#' @return a data frame with nrows(x) with a new column, pidm_lastaddr, appended
 
getpidmsonlastaddr  <- function(x,y) {
   x  %>% 
      left_join(y  %>% 
                   filter(n_lastaddr == 1)  %>% 
                   select(pidm_lastaddr = pidm, lastaddr) 
                , by = 'lastaddr')  %>% 
      mutate(pidm_lastaddr = ifelse(n_lastaddr == 1, pidm_lastaddr, NA))
   
}




# user-facing matching function ----------------------------------------------------

#' @title Get new pidms
#' @description Uses several matching functions to append new pidms to an existing data frame. Input should be a data frame with most (preferably all) of the columns you'd need to match on. Note, this is a time consuming function, as it pulls and maninpulates all constituent data. d
#' @param x a data frame with most (preferably all) the columns to match on
#' @return a data frame with nrows(x) with multiple columns appended, namely finalpidm, which has the function's best guess of what the pidms should be.
#' @details Matching is performed on the following fields, in this order:
#'    \enumerate{
#'       \item existing pidm column
#'       \item email
#'       \item lastname, firstname
#'       \item lastname, first initial
#'       \item lastname, address (only the first 70\% of the adddress is used)
#'    }
#'    In all cases, matches are only made when one match is possible. Also, all text fields are set to lowercase and punctuation is removed.
#' @export 
 
matchforpidms <- function(x) {
   
   # pull together the hallp data to match against
   hallp  <- prephallp()    %>% 
      creatematchingcolumns()   
   
   hallp  <- hallp  %>% 
      createcounts( hallp) 
   
   
   x  %>% 
      prepformatch()  %>% 
      creatematchingcolumns()  %>% 
      createcounts(hallp)   %>% 
      getpidmsonemail    ( hallp )  %>% 
      getpidmsoncatname  ( hallp )  %>% 
      getpidmsonlastfirst( hallp )  %>% 
      getpidmsonlastaddr ( hallp )  %>% 
      mutate(finalpidm = coalesce(pidm,pidm_email,pidm_catname,pidm_lastfirst,pidm_lastaddr) )
   
}
 



