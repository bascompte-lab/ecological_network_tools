#' Get network info from Web of Life
#'
#' @param interaction_type "all", "pollination", "seed disperser", "plant-ant", "plant-herbivore", "anemone-fish", "host-parasite", "food web"
#' @param data_type "binary", "weighted", or "all"
#' @return data frame with metadata for each network.
#' @seealso
#' @export
#' @examples

get_network_info <- function(interaction_type, data_type){

  # create an empty list to store the networks
  network_list <- list()

  # nested ifelse statement to turn interaction types into codes for Web of Life
  type_id <- ifelse(interaction_type == "plant-ant", "3",
                    ifelse(interaction_type == "pollination", "5",
                           ifelse(interaction_type == "seed disperser", "6",
                                  ifelse(interaction_type == "anemone-fish", "11",
                                         ifelse(interaction_type == "host-parasite", "8",
                                                ifelse(interaction_type == "plant-herbivore", "10",
                                                       ifelse(interaction_type == "food web", "7",
                                                              ifelse(interaction_type == "all", "All", NA))))))))

  data_id <- ifelse(data_type == "binary", "0",
                    ifelse(data_type == "weighted", "1",
                           ifelse(data_type == "all", "All", NA)))

  # create a json file with metadata for each network
  json_file <- paste("http://www.web-of-life.es/map_networkdetails_type.php?networkType=",
                     type_id, "&data=", data_id, sep = "")

  # turn json file into a data frame and return most relevant information
  json_network_info <- jsonlite::fromJSON(paste(readLines(json_file), collapse = "")) %>%
    transmute(name = networkName,
              interaction_type = networkTypeName,
              author = networkAuthor,
              location = locationName,
              latitude = as.numeric(locationLatitude),
              longitude = as.numeric(locationLongitude),
              data_type = ifelse(networkQ == "\001", "weighted","binary"),
              S = as.numeric(countSpecies),
              L = as.numeric(countConnections),
              L.freq = as.numeric(sumStrength),
              C = as.numeric(connectance))

  return(json_network_info)
}

