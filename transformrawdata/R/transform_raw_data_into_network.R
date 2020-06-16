transform_raw_data_into_network = function(data){
  require(igraph)
  net = trasnform_raw_data_into_incidence_matrix(data) %>% graph_from_incidence_matrix(.)
  return(net)
}
