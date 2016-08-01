#' #' Set a Millikin style theme for ggplots
#' #'
#' #' @description Creates a ggthemr theme based on the Millikin University pallette and applies it to all ggplots.
#' #' @export
#' 
#' ggmutheme  <- function() {
#'   
#'   
#'   # set colors 
#'   mucolors  <- 
#'     structure(list(
#'       color = c("MU Dark Blue", "MU Blue", "Silver", 
#'                 "Red", "Yellow", "Green", "Lucite", "Slate", "Fuschia", "Warm Gray"
#'       )
#'       , hex = c("#003865", "#015A89", "#B4B7B9", "#FF366B", "#F2DE05", 
#'                 "#94E62B", "#86FFB0", "#89C5B4", "#BA08F5", "#7A6E67")) 
#'       
#'       
#'       
#'       
#'       
#'       , .Names = c("color","hex")
#'       , row.names = c(NA, -10L)
#'       , class = c("data.frame")
#'     )
#'   
#'   gridlinegray  <- '#DDDDDD'
#'   
#'   
#'   
#'   
#'   mutheme  <- define_palette(
#'     
#'     
#'     # define the theme 
#'     
#'     swatch = mucolors$hex
#'     
#'     , gradient = c(
#'       low  = mucolors[[5,2]]
#'       , high = mucolors[[2,2]]
#'     )
#'     
#'     , text = c(
#'       inner =  mucolors[[10,2]]
#'       ,  outer = mucolors[[3,2]]
#'     ) 
#'     
#'     , line = c(
#'       mucolors[[3,2]]
#'       , outer = mucolors[[10,2]]
#'     )
#'     
#'     , gridline = gridlinegray
#'     
#'   )
#'   
#'   ggthemr(mutheme)
#'   
#' }