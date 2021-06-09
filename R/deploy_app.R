##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param app_source
##' @param ExportNetwork
deploy_app <- function(app_file, Export_Network) {

  rsconnect::deployApp(
    appDir = paste0(here(), "/AstroneerItemNetwork/"),
    #appFiles = c(app_file, Export_Network),
    appName  = "astroneer-recipe-network",
    appTitle = "astroneer-recipe-network",
    forceUpdate = TRUE,
    account = "jhgb5",
    server = "shinyapps.io"
  )

}
