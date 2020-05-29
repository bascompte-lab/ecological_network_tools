# This R script is an example of how to download all networks from
# from the www.web-of-life.es website by type of interaction
# usin the JSON api.

# Intecations types
#   http://www.web-of-life.es/interactionstypes.php
#   Code:
#     library("rjson")
#     json_file <- "http://www.web-of-life.es/interactionstypes.php"
#     json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# List of networks
#   http://www.web-of-life.es/networkslist.php?type=All&data=All
#     type:
#        All: all type of interactions N: specific type of interacions
#     data:
#       All: all type of data networks 0: qualitative 1: quantitative
#   Code:
#   library("rjson")
#   json_file <- "http://www.web-of-life.es/interactionstypes.php"
#   json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# Example: Download networks by type
library(data.table)
library("rjson")

# Download network with species neames: 'yes' or 'no'?
species_names='yes'

# Build url
json_file <- "http://www.web-of-life.es/interactionstypes.php"

# Download list of types
json_types <- fromJSON(paste(readLines(json_file), collapse=""))

for(i in 1:length(json_types)){
  # Get network type id
  type_id <- json_types[[i]]$networkTypeId
  
  # Download networks 
  json_file <- paste('http://www.web-of-life.es/networkslist.php?type=',type_id,'&data=All',sep='')
  json_networks <- fromJSON(paste(readLines(json_file), collapse=""))
  
  for(j in 1:length(json_networks)){
    # Build network name
    network_name <- json_networks[[j]]$networkName
    print(network_name)
    
    # Build URL
    url <- paste('http://www.web-of-life.es/download/',network_name,'_',species_names,'.csv',sep='')
    
    # Download data from the web of life
    data <- fread(url)
    
    # Store data in data frame
    assign(network_name,data)
    
    # Place your code here to make some analisys over the network
    # ...
    # ...
  }
}
