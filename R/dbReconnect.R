
is_db_tbl  <- function(tblname) {
  
  get(tblname) %>% 
    class() %>% 
    str_detect("tbl_sql") %>% 
    any()
}


dbMakeTableObj  <- function(tblname, con = cdw) {
  
  x  <- tbl(con, in_schema("CDW", str_to_upper(tblname)) )
  
  assign(tblname, x, env = rlang::global_env())
  
}


# should eventually add some sort of way to specify which connection to reconnect
#   but is dependent on figuring out how to connect tbl_sql with their parent connection

#' dbReconnect
#' 
#' @description reconnect to a database and reinitialize all the tables
#' 
#' @details basically just works with cdw connections at this point
#'
#' @return returns TRUE invisibly 
#' 
#' @export
dbReconnect  <- function() {
  
  all_objs_str  <- ls(envir = rlang::global_env())
  
  all_objs_class  <- map(all_objs_str, ~get(.x) %>% class)
  
  tablesi  <- map_lgl(all_objs_class, ~ any(str_detect(.x, "tbl_sql")) )
  dbtables <- all_objs_str[tablesi]
  
  connectionsi <- map_lgl(all_objs_class, ~ any(str_detect(.x, "^Oracle$")))
  dbconnections  <- all_objs_str[connectionsi]
  
  if(length(dbconnections == 1) & dbconnections[[1]] == 'cdw') {
    cdwonly  <- T
    wipeall  <- T
  } else {
    cdwonly  <- F
    wipeall  <- rstudioapi::showQuestion("You have more than just a CDW connection active. Are you sure you want to wipe out all database connetions?")
  }
  
  
  if(cdwonly | wipeall) {
    
    map(dbconnections, ~DBI::dbDisconnect(get(.x)))
    
  }
  
  if(cdwonly) {
    
    cdw <- DBI::dbConnect(odbc::odbc(), "CDW2", uid ='jaketolbert', pwd = Sys.getenv('CDW_PWD'), timeout = 10)
    assign("cdw", cdw, env = rlang::global_env())
    
    walk(dbtables, dbMakeTableObj )
  }
  
  
  # need to figure out how to match tables up with db connections when you hve more than one connection
  
  invisible(TRUE)
  
}


