
# Various and asundry aliases
  # ideas from http://stackoverflow.com/questions/1189759/expert-r-users-whats-in-your-rprofile

muadcEnv  <- new.env()

assign('v', View, envir = muadcEnv)
assign('h', head, envir = muadcEnv)
assign('cd', setwd, envir = muadcEnv)
assign('s', summary, envir = muadcEnv)
assign('librarY', library, envir = muadcEnv)
