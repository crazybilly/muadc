#' Initialize my db connection
#' 
#' a convinence function to connect to the commits database on localhost. 
#' @param use what kind of database connection to use. Currently valid options include: `RMariaDB`, `odbc`, `mysql` and `pool`. RMariaDB is used by default. The pool option uses RMariaDB, but makes the connection with `pool::dbPool()` rather than `DBI::dbConnect()`.
#' @param odbc a string of an odbc connection to use. If NULL, database connects with credentials in mysql* arguments.
#' @param mysqldb a string of the name of the database name with which you should connect. Defaults to the 'commits' db.
#' @param mysqlhost the hostname or ip address for the database server
#' @param mysqluser the database username 
#' @param mysqlpassword the database password
#' @param ... further arguments to pass to DBI::dbConnect
#' 
#' @return Assigns a database connection, plus connections to the hallp, dnrc, desgs, commits, gifts, pledges and memos tables in the global environment.
#' @import dplyr
#' @import DBI
#' @import odbc
#' @import pool
#' @import RMySQL
#' @import RMariaDB
#' @export

initcommitsdb  <- function(use = 'RMariaDB', odbc = 'warehouse510c', mysqldb = 'commits', mysqlhost = '10.40.9.145', mysqluser = 'adc', mysqlpassword = 'goBigBlue', ...) {
 
  # connect to the database
  use  <-  str_to_lower(use)
  
  if(use == 'rmariadb') {
    commitsdb  <- DBI::dbConnect(RMariaDB::MariaDB(), dbname = mysqldb, host = mysqlhost, user = mysqluser, password = mysqlpassword, bigint = 'numeric', ... )
  } else if(use == 'odbc') {
    commitsdb  <- DBI::dbConnect(odbc::odbc(), odbc, timeout = 10, bigint = 'numeric')
  } else if(use == 'pool') {
    commitsdb  <- pool::dbPool(RMariaDB::MariaDB(), dbname = mysqldb, host = mysqlhost, user = mysqluser, password = mysqlpassword, bigint = 'numeric', ... )
  } else if(use == 'mysql') {
    commitsdb  <- DBI:: dbConnect(RMySQL::MySQL(), dbname = mysqldb, host = mysqlhost, user = mysqluser, password = mysqlpassword, bigint = 'numeric', ... )
  } else {
    error('Specify which connection type should be used in the `use` argument. Valid options include: `RMariaDB`, `odbc`, `mysql` and `pool`.')
  }
    
    # connect to the tables in the db
    committbl  <- commitsdb  %>% dplyr::tbl("commit")
    giftstbl   <- commitsdb  %>% dplyr::tbl("gifts")
    pledgestbl <- commitsdb  %>% dplyr::tbl("pledges")
    memostbl   <- commitsdb  %>% dplyr::tbl("memos")
    hallptbl   <- commitsdb  %>% dplyr::tbl("hallp")
    dnrctbl    <- tryCatch({
                    commitsdb  %>% dplyr::tbl("dnrc") %>% 
                     left_join(tbl(commitsdb, "dnr_catg"), by = "dnrc")
                  },  error = function(e) {
                    commitsdb  %>% dplyr::tbl("dnrc")  %>% 
                     left_join(tbl(commitsdb, "dnr_catg"), by = c("dnrc" = "donor_code"))
                  })
    
    desgstbl   <- tryCatch({
                    commitsdb  %>% dplyr::tbl("desgs") %>% 
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
                  }, error = function(e) {
                     commitsdb %>% dplyr::tbl("clubs")   
                  })
    
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
    assign("actstbl" ,  actstbl ,  env=globalenv())
  
}
