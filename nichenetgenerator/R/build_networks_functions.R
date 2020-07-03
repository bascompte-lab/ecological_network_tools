############################################################################################

# Niche model
# As described in: Do Food Web Models Reproduce the Structure of Mutualistic Networks?
# Mathias M. Pires, Paulo I Prado and Paulo R. Guimaraes Jr.
# 2011,  https://doi.org/10.1371/journal.pone.0027280

# This function creates a network following the niche model. It requires as parameters:
# num_sp_in_A : The number of speices in group A
# num_sp_in_B : The number of species in group B
# empirical_connectance : The desired connectance value for the generated network

niche_networks = function(num_sp_in_A,num_sp_in_B,empirical_connectance){

  # Build empty matrix
  niche_matrix = matrix(NA, nrow = num_sp_in_A, ncol = num_sp_in_B)

  # Position species in group A along axis
  axis_A = runif(num_sp_in_A, min = 0, max = 1)
  # Position species in group B along axis
  axis_B = runif(num_sp_in_B, min = 0, max = 1)

  # Define Beta
  beta_value = (1/(2*empirical_connectance)) - 1
  # Define beta vector
  beta_vector = rbeta(num_sp_in_A,1,beta_value)

  # Niche range for species
  range_vector = axis_A * beta_vector

  # Niche halved
  range_vector_halved = range_vector / 2
  range_vector_halved_minus_one = 1 - (range_vector/2)

  # Determine maximum value for range
  max_vec = pmin(axis_A, range_vector_halved_minus_one)

  # Initialize vector for center values for range
  range_center = rep(0,num_sp_in_A)

  # Draw range center
  for (i in 1:num_sp_in_A) {

    # Determine the center of range for each species
    range_center[i] = runif(1,min = range_vector_halved[i],max = max_vec[i])

    # Establish interactions if niche of species in B fall in range of
    # species in A
    niche_matrix[i,] = as.numeric(axis_B > (range_center[i] - range_vector_halved[i])
                      & axis_B< (range_center[i] + range_vector_halved[i]))
  }

  return(niche_matrix)

}


