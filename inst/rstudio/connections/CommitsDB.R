library(odbc)
commitsdb2  <- DBI::dbConnect(odbc::odbc(), "warehouse510c", timeout = 10, dbname = 'commits')