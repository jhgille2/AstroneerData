#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(igraph)
library(visNetwork)
library(tidyverse)
library(tidyselect)


igraph_to_VisNetwork <- function(graph = network, item = "Buggy", MaxDepth = 50, GraphMode = "in"){


    GetLocalGraph <- function(item, graph, mode = "in"){
        make_ego_graph(graph = graph, order = MaxDepth, nodes = item, mode = mode)[[1]]
    }

    AllLocalGraphs <- map(item, GetLocalGraph, graph, GraphMode) %>%
        reduce(igraph::union)

    # LocalGraph <- make_ego_graph(graph, order = MaxDepth, item, mode = "in")
    # LocalGraph <- LocalGraph[[1]]

    Edges <- AllLocalGraphs %>%
        igraph::as_data_frame() %>%
        mutate(produced_by = coalesce(!!! syms(vars_select(names(.), starts_with("produced_by"))))) %>%
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
        visHierarchicalLayout(direction = "UD",
                              sortMethod = "directed",
                              levelSeparation = 150,
                              edgeMinimization = TRUE) %>%
        visGroups() %>%
        visLegend()

    return(p)
}

load("./ItemNetwork.RData")

AllVertices <- V(network) %>%
    names() %>%
    sort()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(title = h1("Astroneer recipe data explorer", h3("Hover over circles in the graph to go to the corresponding wiki entry")), windowTitle = "Astroneer recipe viewer"),


    fluidRow(
        column(2,
               selectizeInput("RecipeSelection",
                              label    = "Item",
                              choices  = NULL,
                              selected = "Buggy",
                              multiple = TRUE),
               radioButtons("GraphMode",
                            label   = "Select item precursors or dependents",
                            choices = list("Incoming" = "in", "Outgoing" = "out")),
               conditionalPanel(
                   condition = "input.GraphMode == 'out'",
                   sliderInput("GraphDepth",
                               label = "Maximum Depth",
                               min = 1,
                               max = 5,
                               value = 1)
               )),

        column(10,
               visNetworkOutput("network", height = "700px"))
    )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    updateSelectizeInput(session,
                         "RecipeSelection",
                         choices = AllVertices,
                         server = TRUE,
                         selected = "Buggy")

    NetworkData <- reactive({
        igraph_to_VisNetwork(graph     = network,
                             item      = as.character(input$RecipeSelection),
                             MaxDepth  = ifelse(input$GraphMode == "in", 50, input$GraphDepth),
                             GraphMode = as.character(input$GraphMode))
    })

    output$network <- renderVisNetwork({

        NetworkData()

    })

}

# Run the application
shinyApp(ui = ui, server = server)
