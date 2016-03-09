#' Initialize my db connection
#' 
#' a convinence function to connect to the commits database on localhost. 
#' @param db a string of the name of the database name with which you should connect. Defaults to the 'commits' db.
#' @param host the hostname or ip address for the database server
#' @param user the database username 
#' @param password the database password
#' @param ... further arguments to pass to dplyr::src_mysql
#' 
#' @return Assigns a database connection, plus connections to the hallp commits, gifts, pledges and memos tables in the global environment.
#' @importFrom dplyr src_mysql
#' @importFrom magrittr %>% 
#' @importFrom dplyr tbl
#' @export

initcommitsdb  <- function(db = 'commits', host = '10.40.9.144', user = 'adc', password = 'goBigBlue', ...) {
 
    # connect to the database
    commitsdb  <- dplyr::src_mysql(db, host = host, user = user, password = password, ... )
    
    # connect to the tables in the db
    committbl  <- commitsdb  %>% dplyr::tbl("commit")
    giftstbl   <- commitsdb  %>% dplyr::tbl("gifts")
    pledgestbl <- commitsdb  %>% dplyr::tbl("pledges")
    memostbl   <- commitsdb  %>% dplyr::tbl("memos")
    hallptbl   <- commitsdb  %>% dplyr::tbl("hallp")
    
    # assign the tables (and the database connection) 
    #  in the global environment
    assign("commitsdb",commitsdb,env=globalenv())
    
    assign("committbl", committbl, env=globalenv())
    assign("giftstbl",  giftstbl,  env=globalenv())
    assign("pledgestbl",pledgestbl,env=globalenv())
    assign("memostbl",  memostbl,  env=globalenv())
    assign("hallptbl",  hallptbl,  env=globalenv())
    
  
}