##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param ItemData
make_GlobalNetwork <- function(graph = Clean_Data) {

  Edges <- graph %>%
    igraph::as_data_frame() %>%
    select(from, to, produced_by) %>%
    distinct()


  Nodes <- Edges %>%
    select(from, to) %>%
    unlist() %>%
    unique()

  Nodes <- data.frame(id = Nodes) %>%
    mutate(label    = id,
           Wiki_URL = paste0("https://astroneer.fandom.com/wiki/", id),
           title = paste0("<p><a href=", Wiki_URL, ">", id,"</a></p>")) %>%
    left_join(distinct(select(Edges, to, produced_by)), by = c("id" = "to")) %>%
    rename(group = produced_by) %>%
    mutate(group = replace_na(group, "planet"))

  p <- visNetwork(Nodes, Edges) %>%
    visEdges(arrows = "to") %>%
    # visHierarchicalLayout(direction = "UD",
    #                       sortMethod = "directed",
    #                       levelSeparation = 150,
    #                       edgeMinimization = TRUE) %>%
    visGroups() %>%
    visLegend()

  return(p)
}
