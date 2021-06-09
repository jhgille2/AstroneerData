##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param type
Data_URLs <- function(type = "resource") {

  if(type == "resource"){
    return(paste0(here(), "/Data/resourceInfo.json"))
  }else if(type == "modules"){
    return("https://raw.githubusercontent.com/TuckerK/astroneer-recipes/master/src/moduleInfo.json")
  }else{
    return(NA)
  }

}
