#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
library(igraph)
library(visNetwork)
library(tidyverse)
library(tidyselect)
library(RcppGreedySetCover)
library(WebGestaltR)


get_ShoppingList <- function(item, graph, mode = "in"){

    # The names of all the planets
    AllPlanets <-c("Sylva",
                   "Desolo",
                   "Calidor",
                   "Vesania",
                   "Novus",
                   "Glacio",
                   "Atrox")

    # A function to get the local graph that leads to a given item. Essentially,
    # this is pulling the crafting recipe for this item. The diameter of the full graph is
    # 5 so orders larger than this (should) return the full crafting recipe.
    GetLocalGraph <- function(item, graph, mode = "in"){
        make_ego_graph(graph = graph, order = 50, nodes = item, mode = mode)[[1]]
    }

    # Get the crafting recipe for each item in the user-supplied vector, and
    # return the union of these graphs
    AllLocalGraphs <- map(item, GetLocalGraph, graph, mode)
    AllLocalGraphs <- do.call(igraph::union, AllLocalGraphs) # purrr::reduce may be dropping vertex attributes
                                                             # still not sure about the exact cause of the bug
                                                             # but switching to do.call(...) seemed to help

    # What ingredients are produced directly from the planets
    PlanetIngredients <- AllLocalGraphs %>%
        igraph::as_data_frame() %>%
        mutate(produced_by = coalesce(!!! syms(vars_select(names(.), starts_with("produced_by"))))) %>%
        select(from, to, produced_by) %>%
        dplyr::filter(from %in% AllPlanets) %>%
        distinct()

    # On how many planets can you find each of the required ingredients?
    PlanetIngredients %<>%
        group_by(to) %>%
        count(name = "n_unique_planets") %>%
        left_join(PlanetIngredients, by = "to") %>%
        ungroup()

    # Two difficulty measurements for creating an item.
    # One is just the sum of the inverse of the number of
    # planets each ingredient can be found on to approximate the
    # "average" difficulty of finding the required items. The second
    # difficulty is based on the smallest number of planets you have
    # to visit to complete a recipe.

    # "Average" difficulty
    AvgRoute <- PlanetIngredients %>%
        select(to, n_unique_planets) %>%
        distinct()

    AvgRoute <- sum(1/AvgRoute$n_unique_planets)

    # Set coverage solver with RcppGreedySetCover

    # Sorting by planets so that Sylva always comes first in the dataframe.
    # This is important as if available, it will be selected by the greedy set
    # coverage function when deciding what planets to include which makes sense as
    # it's probably easiest to reach the starting planet.
    IngredientSets <- PlanetIngredients %>%
        select(from, to) %>%
        mutate(from = factor(from, AllPlanets)) %>%
        arrange(from) %>%
        mutate(from = as.character(from))

    # "Weights" or planet difficulties so that the starting planet is
    # selected first if possible
    PlanetWeights <- list("Sylva"   = 1,
                          "Desolo"  = 2,
                          "Calidor" = 2,
                          "Vesania" = 2,
                          "Novus"   = 2,
                          "Glacio"  = 2,
                          "Atrox"   = 2)

    # Preparing a named list of sets for the weighted set coverage
    IDs     <- split(IngredientSets$to, IngredientSets$from)
    Weights <- PlanetWeights[names(IDs)] %>% as.numeric()

    sink("file")
    WeightedCoverage <- weightedSetCover(IDs, costs = Weights, topN = 3)
    sink()

    # weightedSetCover prints to the console when it runs but I don't want it to
    # invisible(capture.output(WeightedCoverage <- weightedSetCover(IDs, costs = Weights, topN = 3)))

    # Filter the full ingredient set to just those in the top set
    WeightedRoute <- IngredientSets %>%
        dplyr::filter(from %in% WeightedCoverage$topSets) %>%
        arrange(from, to)

    sink("file") # Preventing printing, use greedySetCover for unweighted set coverage.
    OptimalRoute <- RcppGreedySetCover::greedySetCover(IngredientSets, data.table = FALSE) %>%
        as_tibble()
    sink()

    # The number of planets you must visit in the optimal route
    Optimal_nPlanets <- length(unique(OptimalRoute$from))

    # A tibble to hold the two scores
    RecipeDifficultyScores <- tibble(AvgDifficulty = AvgRoute,
                                     ShortestRoute = Optimal_nPlanets)

    # Combine all the results into a list
    Results <- list(FullPlanetList = PlanetIngredients,
                    ShortestRoute  = OptimalRoute,
                    WeightedRoute  = WeightedRoute,
                    Difficulty     = RecipeDifficultyScores)

    return(Results)
}



igraph_to_VisNetwork <- function(graph = network, item = "Buggy", MaxDepth = 50, GraphMode = "in", ExcludeNodes = NA){


    GetLocalGraph <- function(item, graph, mode = "in"){
        make_ego_graph(graph = graph, order = MaxDepth, nodes = item, mode = mode)[[1]]
    }

    AllLocalGraphs <- map(item, GetLocalGraph, graph, GraphMode)
    AllLocalGraphs <- do.call(igraph::union, AllLocalGraphs)

    Edges <- AllLocalGraphs %>%
        igraph::as_data_frame() %>%
        mutate(produced_by = coalesce(!!! syms(vars_select(names(.), starts_with("produced_by"))))) %>%
        select(from, to, produced_by) %>%
        dplyr::filter(!from %in% ExcludeNodes) %>%
        distinct()

    Nodes <- Edges %>%
        select(from, to) %>%
        unlist() %>%
        unique()

    Nodes <- Nodes[!Nodes == "NA"]

    Nodes <- data.frame(id = Nodes) %>%
        dplyr::filter(!id %in% ExcludeNodes) %>%
        mutate(label    = id,
               Wiki_URL = paste0("https://astroneer.fandom.com/wiki/",  str_replace_all(str_to_title(id), " ", "_")),
               title = paste0("<p><a href=", Wiki_URL, ">", id,"</a></p>")) %>%
        left_join(distinct(select(Edges, to, produced_by)), by = c("id" = "to")) %>%
        rename(group = produced_by)

    Edges <- Edges[Edges$from != "NA", ]

    p <- visNetwork(Nodes, Edges) %>%
        visEdges(arrows = "to") %>%
        visHierarchicalLayout(direction = "UD",
                              sortMethod = "directed",
                              levelSeparation = 150,
                              edgeMinimization = TRUE) %>%
        visGroups(groupname = "Planet", color            = "#bf812d") %>%
        visGroups(groupname = "Gas", color               = "#c7eae5") %>%
        visGroups(groupname = "Mined", color             = "gray") %>%
        visGroups(groupname = "Backpack", color          = "#dfc27d") %>%
        visGroups(groupname = "Small printer", color     = "#f6e8c3") %>%
        visGroups(groupname = "Chemistry lab", color     = "#ffffbf") %>%
        visGroups(groupname = "Small printer", color     = "#fee090") %>%
        visGroups(groupname = "Medium printer", color    = "#80cdc1") %>%
        visGroups(groupname = "Large printer", color     = "#35978f") %>%
        visGroups(groupname = "Smelting furnace", color  = "#8c510a") %>%
        visLegend()

    return(p)
}

load("./ItemNetwork.RData")

AllVertices <- V(network) %>%
    names() %>%
    sort()

# The names of all the planets
PlanetNames <-c("Sylva",
                "Desolo",
                "Calidor",
                "Vesania",
                "Novus",
                "Glacio",
                "Atrox")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(title = h1("Astroneer recipe data explorer", h3("Hover over circles in the graph to go to the corresponding wiki entry")), windowTitle = "Astroneer recipe viewer"),


    fluidRow(
        sidebarPanel(width = 2,
               selectizeInput("RecipeSelection",
                              label    = "Item",
                              choices  = NULL,
                              selected = "Auto extractor",
                              multiple = TRUE),
               checkboxInput("GraphOptimize",
                            label = "Optimize Route",
                            value = FALSE),
               bsTooltip("GraphOptimize", "Filter the graph to the smallest number of planets you can visit and stil gather the raw resources you'll need to make the chosen items.",
                         "right", options = list(container = "body"))),

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
                         selected = "Auto extractor")

    ExcludePlanets <- reactive({

        if(!input$GraphOptimize){
            return("blahblahblah")
        }

        IncludePlanets <- get_ShoppingList(input$RecipeSelection, network)$ShortestRoute$from %>% as.character() %>%unique()
        PlanetNames[!(PlanetNames %in% IncludePlanets)]

    })

    NetworkData <- reactive({
        igraph_to_VisNetwork(graph        = network,
                             item         = as.character(input$RecipeSelection),
                             MaxDepth     = 50,
                             GraphMode    = "in",
                             ExcludeNodes = ExcludePlanets())
    })

    output$network <- renderVisNetwork({

        NetworkData()

    })
#
    addTooltip(session = session,
               id = "GraphOptimize",
               title = "Filter the graph to the smallest number of planets you can visit and stil gather the raw resources you'll need to make the chosen items.")

}

# Run the application
shinyApp(ui = ui, server = server)
