##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param Data
tidy_jsonData <- function(Data = Download_Data) {

  # The names of all the planets
  AllPlanets <-c("Sylvia",
                 "Desolo",
                 "Calidor",
                 "Vesania",
                 "Norvus",
                 "Glacio",
                 "Atrox")

  # A function to replace "All Planets" with the names of all the planets
  ReplaceAllPlanets <- function(location, PlanetList = AllPlanets){
    if(location == "All Planets"){
      return(PlanetList)
    }else{
      return(location)
    }
  }

  # On which planet(s) can you find each resource
  PlanetData <- Data$Resources %>%
    select(id, name, found) %>%
    unnest(found)

  LateriteData <- tibble(id = NA, name = "Laterite", found = "All Planets")

  PlanetData <- PlanetData %>%
    bind_rows(LateriteData) %>%
    mutate(found = map(found, ReplaceAllPlanets)) %>%
    unnest(found) %>%
    mutate(produced_by = "mined") %>%
    rename(input  = found,
           output = name) %>%
    select(input, output, produced_by)

  # The buildings in which output can be made, and what input
  # each output requires
  BuildData <- Data$Resources %>%
    select(id, name, builds) %>%
    unnest(builds) %>%
    pivot_longer(!c(id, name), names_to = "building") %>%
    unnest(value) %>%
    rename(output      = value,
           produced_by = building,
           input       = name) %>%
    select(input, output, produced_by)

  # A table of the modules which has the count of the resources needed
  # to make an item and where it can be made
  Modules_unnested <- Data$Modules %>%
    unnest(resources) %>%
    group_by(name, platform, resources) %>%
    count(name = "input_qty") %>%
    rename(output      = name,
           produced_by = platform,
           input       = resources) %>%
    select(input, output, produced_by, input_qty)

  # Conversion of raw resources to processed resources
  Processed_Resources <- Data$Resources %>%
    select(name, source) %>%
    unnest(source) %>%
    rename(output = name) %>%
    pivot_longer(!output) %>%
    unnest(value) %>%
    rename(produced_by = name,
           input = value) %>%
    select(input, output, produced_by)

  # Return everything as a list
  return(list(Planets   = PlanetData,
              Buildings = BuildData,
              #Modules   = Modules_unnested,
              Processed = Processed_Resources))

}
