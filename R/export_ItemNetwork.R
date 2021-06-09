##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param network
##' @param dest
export_ItemNetwork <- function(network = ItemNetwork, dest =
                               "./Data/ItemNetwork.RData") {

  save(network, file = dest)
  dest

}
