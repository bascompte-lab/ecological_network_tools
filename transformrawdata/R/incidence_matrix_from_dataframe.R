incidence_matrix_from_raw_data = function(data){
  #############################################
  # Initialize empty matrix to fill
  #############################################
  unique_partner_A = unique(data[,1]) # find unique partner A
  unique_partner_B = unique(data[,2]) # find unique partner B
  incidence_matrix = matrix(0L,length(unique_partner_A),length(unique_partner_B)) # create empty matrix
  rownames(incidence_matrix) = as.character(unique_partner_A) # Set names of row species, according to unique partner A
  colnames(incidence_matrix) = as.character(unique_partner_B) # Set names of row species, according to unique partner B

  #############################################
  # Fill matrix based on data
  #############################################

  partnerA = colnames(data)[1]
  partnerB = colnames(data)[2]
  
  for (species in rownames(incidence_matrix)) {
  
    interactions_for_current_species = subset(data,data[[partnerA]] == species)[[partnerB]]
    
    column_counter = 0
    for (partner in interactions_for_current_species) {
    column_counter = column_counter + 1 
      incidence_matrix[as.character(species),as.character(partner)] = interactions_for_current_species[column_counter,3] 
    }
  }

  return(incidence_matrix)
}
