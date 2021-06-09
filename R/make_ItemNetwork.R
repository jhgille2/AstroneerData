##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param ItemData
make_ItemNetwork <- function(ItemData = Clean_Data) {

  # Combine all the cleaned data into a single dataframe and then remove
  # duplicated rows
  AllData <- reduce(ItemData, bind_rows) %>%
    mutate(produced_by = tolower(produced_by)) %>%
    distinct()

  # Convert this dataframe to an igraph object
  ItemGraph <- igraph::graph_from_data_frame(AllData)

  return(ItemGraph)
}
