require(dplyr)

#' initcommitsdb
#' 
#' a convinence function to connect to the commits database on localhost. 
#' 
#' @return Assigns a database connection, plus connections to the commit, gifts, pledges and memos tables in the global environment.
initcommitsdb  <- function() {
  
    #connect to the database
    commitsdb  <- src_mysql("commits")
    
    # connect to the tables in the db
    committbl  <- commitsdb  %>% tbl("commit")
    giftstbl   <- commitsdb  %>% tbl("gifts")
    pledgestbl   <- commitsdb  %>% tbl("pledges")
    memostbl   <- commitsdb  %>% tbl("memos")
    
    # assign the tables (and the database connection) 
    #  in the global environment
    assign("commitsdb",commitsdb,envir=globalenv())
    assign("committbl", committbl, env=globalenv())
    assign("giftstbl",  giftstbl,  env=globalenv())
    assign("pledgestbl",pledgestbl,env=globalenv())
    assign("memostbl",  memostbl,  env=globalenv())
    
  
}