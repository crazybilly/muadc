#' Set plot options to Millikin defaults
#' 
#' Sets a variety of plot options so plots come out looking consistent with Millikin University branding. Sets options via par().
#' 
#' @keywords plot
#' @keywords par
#' @export
 
muplot <- function() {
  par(
    bty = "L", # set the plot border shape
    fg  = "#827C78", # sets the default color, set to MU silver 
    col.lab  = "#005288", #set the axis label color, set to MU blue
    col.axis = "#005288", #set the axis text color, set to MU blue
    cex.axis = .8, #shrinks the axis text size a bit
    col.main = "#005288", #set the title color to MU blue
    col.sub  = "#827C78", #set subtitle color, set to MU gray
    pch = 20
  )
}
