## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

  # The urls for the resource and module .json files
  tar_target(Resource_Data,
             Data_URLs(type = "resource"),
             format = "file"),
  tar_target(Module_Data,
             Data_URLs(type = "modules"),
             format = "url"),

  ## Download these files
  tar_target(Download_Data,
             Get_Data(files = list(Resource_Data, Module_Data))),

  # Clean up the json data to tidy tibbles
  tar_target(Clean_Data,
             tidy_jsonData(Data = Download_Data)),

  # Format the cleaned data as a directed network
  tar_target(ItemNetwork,
             make_ItemNetwork(ItemData = Clean_Data)),

  # A "global" recipe visNetwork of the full dataset
  tar_target(GlobalNetwork,
             make_GlobalNetwork(graph = ItemNetwork)),

  # Export this network to a file
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
