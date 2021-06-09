##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param files
Get_Data <- function(files = list(Resource_Data, Module_Data)) {

  AllData <- map(files, fromJSON)
  names(AllData) <- c("Resources", "Modules")

  return(AllData)

}
