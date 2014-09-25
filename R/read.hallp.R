#' Read hallp.csv
#' 
#' A convience function around read.tidy specifically to read in G:/ALUMNI/Jake T/datawarehouse/data/hallp.csv.
#' @param x the filename to load. Should be a csv
#' @param ifexists should the function check if the object exists. Default = F
#' @param existsname what object should the function look for, assuming ifexists = T
#' @export

read.hallp<- function(ifexists=T,existsname) {
  read.tidy('G:/ALUMNI/Jake T/datawarehouse/data/hallp.csv',ifexists,existsname)
}