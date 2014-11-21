#' Initialize my db connection
#' 
#' a convinence function to connect to the commits database on localhost. 
#' @return Assigns a database connection, plus connections to the hallp commits, gifts, pledges and memos tables in the global environment.
#' @importFrom dplyr src_mysql
#' @importFrom dplyr tbl
#' @export
initcommitsdb  <- function() {
  
    #connect to the database
    commitsdb  <- dplyr::src_mysql("commits")
    
    # connect to the tables in the db
    committbl  <- commitsdb  %>% dplyr::tbl("commit")
    giftstbl   <- commitsdb  %>% dplyr::tbl("gifts")
    pledgestbl <- commitsdb  %>% dplyr::tbl("pledges")
    memostbl   <- commitsdb  %>% dplyr::tbl("memos")
    hallp      <- commitsdb  %>% dplyr::tbl("hallp")
    
    # assign the tables (and the database connection) 
    #  in the global environment
    assign("commitsdb",commitsdb,env=globalenv())
    
    assign("committbl", committbl, env=globalenv())
    assign("giftstbl",  giftstbl,  env=globalenv())
    assign("pledgestbl",pledgestbl,env=globalenv())
    assign("memostbl",  memostbl,  env=globalenv())
    assign("hallptbl",  hallp,     env=globalenv())
    
  
}