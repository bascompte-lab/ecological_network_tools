convert_data_to_network = function(data,covariable_to_filter_by,level_of_covariable_to_filter_by){

  #############################################
  # Subset data based on input parameters
  #############################################

  if (covariable_to_filter_by == "none" | level_of_covariable_to_filter_by == "none" |
      missing(covariable_to_filter_by) | missing(level_of_covariable_to_filter_by)){
    data = data[,1:3] # take only the first columns of our spreadsheet
  } else {
    data = subset(data,data[,covariable_to_filter_by] == level_of_covariable_to_filter_by)
    data = data[,1:3] # take only the first columns of our spreadsheet
  }


  #############################################
  # Initialize empty matrix to fill
  #############################################
  unique_partner_A = unique(data[,1]) # find unique partner A
  unique_partner_B = unique(data[,2]) # find unique partner B
  incidence_matrix = matrix(0,length(unique_partner_A),length(unique_partner_B)) # create empty matrix
  rownames(incidence_matrix) = as.character(unique_partner_A) # Set names of row species, according to unique partner A
  colnames(incidence_matrix) = as.character(unique_partner_B) # Set names of row species, according to unique partner B

  #############################################
  # Fill matrix based on data
  #############################################

  for (species in rownames(incidence_matrix)) {
    interactions_for_current_species = subset(data,data$interaction.partnerA == species)$interaction.partnerB

    for (partner in interactions_for_current_species) {
      incidence_matrix[species,partner] = incidence_matrix[species,partner] + 1
    }
  }

  net = graph_from_incidence_matrix(incidence_matrix)

  return(net)
}
