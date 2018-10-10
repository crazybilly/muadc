# don't use - use fundRaising::cutgiving() instead
givingfactor <- function(x) {
  
  
    # get some reasonable bins
    #   unfortunately, we have log transform the data to get useful bins
    #   which makes the results of pretty() not so pretty pretty when we transform it back
    rawbins <- pretty(
      range(asinh(x))
      , n = nclass.Sturges(asinh(x))
      , min.n = 1, na.rm = T
    ) %>%
      sinh() 
    
    
    # turn the raw bins into something more human readable and what we expect
    refinedbins  <- case_when(
      rawbins   <= 1   ~ signif(rawbins, 1)
      , rawbins <= 10  ~ rawbins - (rawbins %% 5)
      , rawbins <= 100 ~ rawbins - (rawbins %% 25)
      , rawbins <= 1000 ~ rawbins - (rawbins %% 250)
      , rawbins <= 10000 ~ rawbins - (rawbins %% 2500)
      , rawbins <= 100000 ~ rawbins - (rawbins %% 10000)
      , rawbins <  1000000 ~ rawbins - (rawbins %% 100000)
      , rawbins >= 1000000 ~ rawbins - (rawbins %% 1000000)
    ) %>% 
      unique()
    
    # create labels for the bins
    refinedlabels  <- paste0(abbrdollars(refinedbins), " - ", abbrdollars(lead(refinedbins, default = Inf)) ) %>% 
      str_replace(" - Inf", "+")
    
    # actually cut x by the newly created bins
    cut(x, breaks = c(refinedbins,Inf), labels = refinedlabels)
    
  

}