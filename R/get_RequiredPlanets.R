##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param NetworkData

# A function that returns dataframes to report the planets on which primitive resources can be found,
# the fewest number of planets that can be visited to complete a recipe, and
# simple difficulty metrics based on these two data.
get_RecipeDifficulties <- function(item, graph, mode = "in"){

  # The names of all the planets
  AllPlanets <-c("Sylvia",
                 "Desolo",
                 "Calidor",
                 "Vesania",
                 "Norvus",
                 "Glacio",
                 "Atrox")

  # GetLocalGraph <- function(item, graph, mode = "in"){
  #   make_ego_graph(graph = graph, order = 50, nodes = item, mode = mode)[[1]]
  # }
  #
  # AllLocalGraphs <- map(item, GetLocalGraph, graph, mode) %>%
  #   reduce(igraph::union)
  #
  # PlanetIngredients <- AllLocalGraphs %>%
  #   igraph::as_data_frame() %>%
  #   mutate(produced_by = coalesce(!!! syms(vars_select(names(.), starts_with("produced_by"))))) %>%
  #   select(from, to, produced_by) %>%
  #   dplyr::filter(from %in% AllPlanets) %>%
  #   distinct()

  # The ingredients that have to be mined directly from a planet
  PlanetIngredients <- make_ego_graph(graph = graph, order = 50, nodes = item, mode = mode)[[1]] %>%
    igraph::as_data_frame() %>%
    dplyr::filter(from %in% AllPlanets) %>%
    arrange(to, from)

  # On how many planets can you find each of the required ingredients?
  PlanetIngredients %<>%
    group_by(to) %>%
    count(name = "n_unique_planets") %>%
    left_join(PlanetIngredients, by = "to") %>%
    ungroup() %>%
    mutate(item = item)

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

  # Sorting by planets so that Sylvia always comes first in the dataframe
  IngredientSets <- PlanetIngredients %>%
    select(from, to) %>%
    mutate(from = factor(from, AllPlanets)) %>%
    arrange(from) %>%
    mutate(from = as.character(from))

  # "Weights" or planet difficulties so that the starting planet is
  # selected first if possible
  PlanetWeights <- list("Sylvia"  = 1,
                        "Desolo"  = 2,
                        "Calidor" = 2,
                        "Vesania" = 2,
                        "Norvus"  = 2,
                        "Glacio"  = 2,
                        "Atrox"   = 2)

  # Preparing a named list of sets for the weighted set coverage
  IDs     <- split(IngredientSets$to, IngredientSets$from)
  Weights <- PlanetWeights[names(IDs)] %>% as.numeric()

  sink("file")
  WeightedCoverage <- weightedSetCover(IDs, costs = Weights, topN = 3)
  sink()

  WeightedRoute <- IngredientSets %>%
    dplyr::filter(from %in% WeightedCoverage$topSets) %>%
    mutate(item = item) %>%
    arrange(from, to)

  sink("file") # Preventing printing
  OptimalRoute <- RcppGreedySetCover::greedySetCover(IngredientSets, data.table = FALSE) %>%
    as_tibble() %>%
    mutate(item = item)
  sink()

  Optimal_nPlanets <- length(unique(OptimalRoute$from))

  # A tibble to hold the two scores
  RecipeDifficultyScores <- tibble(item          = item,
                                   AvgDifficulty = AvgRoute,
                                   ShortestRoute = Optimal_nPlanets)

  Results <- list(FullPlanetList = PlanetIngredients,
                  ShortestRoute  = OptimalRoute,
                  WeightedRoute  = WeightedRoute,
                  Difficulty     = RecipeDifficultyScores)

  return(Results)
}

# Return a list of all the planets required to make an item, and the most
#efficient way to visit planets to make an item
get_RequiredPlanets <- function(NetworkData = ItemNetwork) {

  # What categories of resources do I want recipe requirements for
  ItemCategories <- c("backpack", "small", "medium", "large")

  # FIlter the data down to these categories
  ItemEdgelist <- igraph::as_data_frame(NetworkData) %>%
    dplyr::filter(produced_by %in% ItemCategories)

  # The unique items
  UniqueItems <- unique(ItemEdgelist$to)

  # Get the required planets and difficulty metrics for each recipe
  AllPlanetReqs <- map(UniqueItems, get_RecipeDifficulties, NetworkData)

  # A function to pluck and merge each element of the list
  Pluck_and_Merge <- function(PlanetReqs_name, PlanetReqs){
    map(PlanetReqs, pluck(PlanetReqs_name)) %>%
      reduce(bind_rows)
  }

  # Apply this function to the full data and return the result
  map(names(AllPlanetReqs[[1]]), Pluck_and_Merge, AllPlanetReqs) %>%
    setNames(names(AllPlanetReqs[[1]]))
}

get_ShoppingList <- function(item, graph, mode = "in"){

  # The names of all the planets
  AllPlanets <-c("Sylvia",
                 "Desolo",
                 "Calidor",
                 "Vesania",
                 "Norvus",
                 "Glacio",
                 "Atrox")

  GetLocalGraph <- function(item, graph, mode = "in"){
    make_ego_graph(graph = graph, order = 50, nodes = item, mode = mode)[[1]]
  }

  AllLocalGraphs <- map(item, GetLocalGraph, graph, mode) %>%
    reduce(igraph::union)

  PlanetIngredients <- AllLocalGraphs %>%
    igraph::as_data_frame() %>%
    mutate(produced_by = coalesce(!!! syms(vars_select(names(.), starts_with("produced_by"))))) %>%
    select(from, to, produced_by) %>%
    dplyr::filter(from %in% AllPlanets) %>%
    distinct()

  # The ingredients that have to be mined directly from a planet
  # PlanetIngredients <- make_ego_graph(graph = graph, order = 50, nodes = item, mode = mode)[[1]] %>%
  #   igraph::as_data_frame() %>%
  #   dplyr::filter(from %in% AllPlanets) %>%
  #   arrange(to, from)

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

  # Sorting by planets so that Sylvia always comes first in the dataframe
  IngredientSets <- PlanetIngredients %>%
    select(from, to) %>%
    mutate(from = factor(from, AllPlanets)) %>%
    arrange(from) %>%
    mutate(from = as.character(from))

  # "Weights" or planet difficulties so that the starting planet is
  # selected first if possible
  PlanetWeights <- list("Sylvia"  = 1,
                        "Desolo"  = 2,
                        "Calidor" = 2,
                        "Vesania" = 2,
                        "Norvus"  = 2,
                        "Glacio"  = 2,
                        "Atrox"   = 2)

  # Preparing a named list of sets for the weighted set coverage
  IDs     <- split(IngredientSets$to, IngredientSets$from)
  Weights <- PlanetWeights[names(IDs)] %>% as.numeric()

  sink("file")
  WeightedCoverage <- weightedSetCover(IDs, costs = Weights, topN = 3)
  sink()

  WeightedRoute <- IngredientSets %>%
    dplyr::filter(from %in% WeightedCoverage$topSets) %>%
    arrange(from, to)

  sink("file") # Preventing printing
  OptimalRoute <- RcppGreedySetCover::greedySetCover(IngredientSets, data.table = FALSE) %>%
    as_tibble()
  sink()

  Optimal_nPlanets <- length(unique(OptimalRoute$from))

  # A tibble to hold the two scores
  RecipeDifficultyScores <- tibble(AvgDifficulty = AvgRoute,
                                   ShortestRoute = Optimal_nPlanets)

  Results <- list(FullPlanetList = PlanetIngredients,
                  ShortestRoute  = OptimalRoute,
                  WeightedRoute  = WeightedRoute,
                  Difficulty     = RecipeDifficultyScores)

  return(Results)
}
