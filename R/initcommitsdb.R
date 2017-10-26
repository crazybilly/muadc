#' Initialize my db connection
#' 
#' a convinence function to connect to the commits database on localhost. 
#' @param db a string of the name of the database name with which you should connect. Defaults to the 'commits' db.
#' @param host the hostname or ip address for the database server
#' @param user the database username 
#' @param password the database password
#' @param ... further arguments to pass to DBI::dbConnect
#' 
#' @return Assigns a database connection, plus connections to the hallp, dnrc, desgs, commits, gifts, pledges and memos tables in the global environment.
#' @import pool
#' @import dplyr
#' @import DBI
#' @importFrom RMySQL MySQL
#' @export

initcommitsdb  <- function(db = 'commits', host = '10.40.9.145', user = 'adc', password = 'goBigBlue', ...) {
 
    # connect to the database
    commitsdb  <- dbPool(RMySQL::MySQL(), dbname = db, host = host, user = user, password = password, ... )
    
    # connect to the tables in the db
    committbl  <- commitsdb  %>% dplyr::tbl("commit")
    giftstbl   <- commitsdb  %>% dplyr::tbl("gifts")
    pledgestbl <- commitsdb  %>% dplyr::tbl("pledges")
    memostbl   <- commitsdb  %>% dplyr::tbl("memos")
    hallptbl   <- commitsdb  %>% dplyr::tbl("hallp")
    dnrctbl    <- commitsdb  %>% dplyr::tbl("dnrc") %>% 
                  left_join(tbl(commitsdb, "dnr_catg"), by = "dnrc")
    
    desgstbl   <- commitsdb  %>% dplyr::tbl("desgs") %>% 
                  left_join(tbl(commitsdb, "rfcclubs") %>% 
                              select(
                                  desg
                                , rfc_college = college
                                , rfc_college_name = college_name
                                , rfc_type = desg_type
                                , rfc = rfc_club
                              )
                    , by = "desg"
                  )
    
    actstbl  <- commitsdb %>% dplyr::tbl("acts") %>% 
                  left_join(tbl(commitsdb, "acts_catg"), by = 'act')
       
    # assign the tables (and the database connection) 
    #  in the global environment
    assign("commitsdb",commitsdb,  env=globalenv())
    
    assign("committbl", committbl, env=globalenv())
    assign("giftstbl",  giftstbl,  env=globalenv())
    assign("pledgestbl",pledgestbl,env=globalenv())
    assign("memostbl",  memostbl,  env=globalenv())
    assign("hallptbl",  hallptbl,  env=globalenv())
    assign("dnrctbl",   dnrctbl,   env=globalenv())
    assign("desgstbl",  desgstbl,  env=globalenv())
  
}
