## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

  # The url resource data
  tar_target(Resource_Data_Url,
             "https://raw.githubusercontent.com/matco/astroneer/master/src/database.json",
             format = "url"),

  # Download the .json file
  tar_target(Download,
             jsonlite::fromJSON(Resource_Data_Url)),

  # Tidy up the data from the json
  tar_target(Munge,
             munge_resourcedata(Download)),

  # Format the cleaned data as a directed network
  tar_target(ItemNetwork,
             make_ItemNetwork(ItemData = Munge)),

  # A "global" recipe visNetwork of the full dataset
  tar_target(GlobalNetwork,
             make_GlobalNetwork(graph = ItemNetwork)),

  # Export the graph object to a file
  tar_target(Export_Network,
             export_ItemNetwork(network = ItemNetwork, dest = paste0(here(), "/AstroneerItemNetwork/ItemNetwork.RData")),
             format = "file"),

  # The source file for the shiny app used to interactively display this data
  tar_target(app_file,
             paste0(here(), "/AstroneerItemNetwork/app.R"),
             format = "file"),

  # Deploy the app
  tar_target(deploy,
             deploy_app(app_file, Export_Network)),

  # Render the writeup document
  tar_render(Writeup, "doc/Writeup.Rmd")
)
