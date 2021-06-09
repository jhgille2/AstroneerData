##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param Download
munge_resourcedata <- function(Download) {

  # Tidy up the json data for resources, objects, and planets
  Resources_clean <- Download %>%
    pluck("resources") %>%
    rename(item = id) %>%
    unnest(dependencies) %>%
    jsonlite::flatten() %>%
    rename(label        = `label.en-US`,
           produced_by  = crafted,
           input        = id,
           output       = item) %>%
    select(input, output, quantity, produced_by)

  Objects_clean <- Download %>%
    pluck("objects") %>%
    rename(item = id) %>%
    unnest(resources) %>%
    jsonlite::flatten() %>%
    rename(label       = `label.en-US`,
           produced_by = printed,
           input       = id,
           output      = item) %>%
    select(input, output, quantity, produced_by)

  Planets_clean <- Download %>%
    pluck("planets") %>%
    unnest(primary_resources) %>%
    unnest(secondary_resources) %>%
    unnest(atmospheric_resources, keep_empty = TRUE) %>%
    select(id, name, tidyselect::ends_with("_resources")) %>%
    pivot_longer(tidyselect::ends_with("_resources"), names_to = "type") %>%
    distinct() %>%
    rename(input       = name,
           output      = value) %>%
    mutate(quantity    = NA,
           produced_by = case_when(type == "atmospheric_resources" ~ "gas",
                                   type != "atmospheric_resources" ~ "mined")) %>%
    select(input, output, quantity, produced_by) %>%
    dplyr::filter(!is.na(output))

  JustPlanets <- tibble(input = NA,
                        output = c("Sylva",
                                   "Desolo",
                                   "Calidor",
                                   "Vesania",
                                   "Novus",
                                   "Glacio",
                                   "Atrox"),
                        produced_by = "planet")

  # Combine everything into one tible and return this tibble
  AllTibbles <- list(Resources_clean,
                     Objects_clean,
                     Planets_clean,
                     JustPlanets) %>%
    reduce(bind_rows) %>%
    select(input, output, produced_by, quantity) %>%
    as_tibble() %>%
    dplyr::filter(!(output == "large_platform_b" & produced_by == "large_printer")) %>%
    mutate(input       = str_to_sentence(input) %>% str_replace_all("_", " "),
           output      = str_to_sentence(output) %>% str_replace_all("_", " "),
           produced_by = str_to_sentence(produced_by) %>% str_replace_all("_", " ")) %>%
    select(input, output, produced_by)

  return(AllTibbles)
}
