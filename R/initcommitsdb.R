#' initcommitsdb
#' 
#' a convinence function to connect to the commits database on localhost. 
#' 
#' @return Assigns a database connection, plus connections to the hallp commits, gifts, pledges and memos tables in the global environment.
#' @export
require(dplyr)
initcommitsdb  <- function() {
  
    #connect to the database
    commitsdb  <- src_mysql("commits")
    
    # connect to the tables in the db
    committbl  <- commitsdb  %>% tbl("commit")
    giftstbl   <- commitsdb  %>% tbl("gifts")
    pledgestbl <- commitsdb  %>% tbl("pledges")
    memostbl   <- commitsdb  %>% tbl("memos")
    hallp      <- commitsdb  %>% tbl("hallp")
    
    # assign the tables (and the database connection) 
    #  in the global environment
    assign("commitsdb",commitsdb,env=globalenv())
    
    assign("committbl", committbl, env=globalenv())
    assign("giftstbl",  giftstbl,  env=globalenv())
    assign("pledgestbl",pledgestbl,env=globalenv())
    assign("memostbl",  memostbl,  env=globalenv())
    assign("hallptbl",  hallp,     env=globalenv())
    
  
}