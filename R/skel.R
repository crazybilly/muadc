
#' Create Rproject Skeleton
#' 
#' @description Creates the folder structure and optional basescript for an R project. Creates two directories, data and output, assuming they don't already exist as well as the base script.
#'
#' @param createbasescript a logical value indicating whether or not a base script should be created. The base script will source all R scripts in the working directory
#' @param basescriptname a character string describing the file name of the base script. Defaults to '00-source-all-files.R'
#'
#' @return Does not return a value, but has side effects, namely creating directories and the base script.
#' @export
#'
skel  <- function( createbasescript = F, basescriptname = '00-source-all-files.R', path = NULL) {
  
  if(!is.null(path) ) {
    path  <- getwd()
  }
  
  
  if( !dir.exists('output') ) {
    dir.create(paste0(path,'/output'))
  }
  
  if( !dir.exists('data') ) {
    dir.create(paste0(path,'/data'))
  }
  
  
  if( createbasescript ) {
    
    allRscripts   <- dir(pattern = '\\.R$')
    allRscripts  <- allRscripts[allRscripts != basescriptname]
    
    scripttext  <- paste(
      sub("(.*)", "source('\\1', echo = T)", allRscripts)
      , collapse = "\n"
    )
    
    if( file.exists(basescriptname) ) {
      prompt <- readline(paste0("Do you want to overwrite ",basescriptname,"? [Y]"))
    } else {
      prompt  <- NA
    }
    
    if( is.blank(prompt) | grepl("y",prompt, ignore.case = T) ) {
      writeLines(paste0(scripttext,"\n\nbeepr::beep()"), basescriptname)
    }
    
  }
  
}
    