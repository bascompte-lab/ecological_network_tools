get_networks <- function(network_name, interaction_type = "all", data_type = "all", names = "yes"){

  # would you like to include the names of the species? ("yes" or "no")
  speciesName <- names

  if (missing(network_name)){


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


    # create a file (json_networks) with the names of the networks we would like to download
    json_file <- paste("http://www.web-of-life.es/networkslist.php?type=",
                       type_id, "&data=", data_id, sep = "")
    json_networks <- rjson::fromJSON(file = json_file) #rjson::fromJSON(paste(readLines(json_file), collapse = ""))

    # download the networks
    #network_list <- list()
    for(i in 1: length(json_networks)){

      # identifying the network
      #if(as.numeric(json_networks[[i]]$countSpecies) > 0) { # we get networks and subnetworks
      if(json_networks[[i]]$root == 0 & is.null(json_networks[[i]]$parentNetworkId)){ # we get networks without subnetworks
        networkName <- json_networks[[i]]$networkName
        print(networkName)

        # building the URL
        url <- paste("http://www.web-of-life.es/download/",
                     networkName,
                     "_",
                     speciesName, ".csv",
                     sep = "")

        # download the network from www.web-of-life.es
        data <- data.table::fread(url)

        # storing the networks as a data table
        assign(networkName, data)

        # storing the networks as a list
        network_list[[networkName]] <- (data)
      }
      #}
    }
    return(network_list)
  }

  else  # network_name passed as an argument
  {
    url <- paste("http://www.web-of-life.es/download/",
                 network_name,
                 "_",
                 speciesName,
                 ".csv",
                 sep = "")

    # download the requested network from www.web-of-life.es
    data <- data.table::fread(url)
    return (data)
  }
}
