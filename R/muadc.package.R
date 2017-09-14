muadc.package  <- function(path, ... ) {
  
  dir.create(path, recursive = T, showWarnings = T)
  
  dots  <- list(...)
  
  
  text <- lapply(seq_along(dots), function(i) {
    key <- names(dots)[[i]]
    val <- dots[[i]]
    paste0(key, ": ", val)
  })
  
  
  
  if(dots$create_skel ) {
    skel(
        createbasescript = dots$createbasescript
      , basescriptname   = dots$basescriptname
      , path = path
    ) 
  }
  
  text
  
  
}