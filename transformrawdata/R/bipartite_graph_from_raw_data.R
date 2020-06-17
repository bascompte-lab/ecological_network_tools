bipartite_graph_from_raw_data = function(data){
  require(igraph)
  net = incidence_matrix_from_raw_data(data) %>% graph_from_incidence_matrix(.)
  return(net)
}
