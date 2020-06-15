remove_plants <-function(bipartite_network, n_iter, strategy) {
  require(fastmatch)
  require(permute)
  set.seed(13)

  # Convert removal_strategy factor [enum]
  # removal_strategy <- c("RND", "MTL", "LTM") %>% factor()
  # class(removal_strategy)
  if (strategy == "RND") print(paste0("RND strtegy"))
  if (strategy == "MTL") print(paste0("MTL strtegy"))
  if (strategy == "LTM") print(paste0("LTM strtegy"))

  print(paste0("dim ", dim(bipartite_network)))

  v_plants <- row.names(bipartite_network)

  res <- c()

  plants_ext = replicate(length(v_plants),0)
  animals_ext = replicate(length(v_plants),0)

  for (iter in 1:n_iter){

    print(paste0("iter = ",iter))

    iprim_ext = 0
    isec_ext = 0
    iloop = 0
    df_old <- bipartite_network

    if (strategy == "RND") v_rnd <- v_plants[shuffle(v_plants)]
    if (strategy == "LTM") v_rnd <- order_and_shuffle_bipartite(bipartite_network, FALSE)[,1]
    if (strategy == "MTL") v_rnd <- order_and_shuffle_bipartite(bipartite_network, TRUE)[,1]

    # print(v_rnd)

    for (plant in v_rnd){

      iloop = iloop + 1 # counter

      ip = grep(plant, row.names(df_old))

      if (length(ip) != 0) df <- df_old[-ip,]

      iprim_ext = iprim_ext + length(ip)

      if(is.null(dim(df))) {
        print("achtung dim(df) is null and iterations stop!!!")
        break
      }
      if(nrow(df) == 0)break

      #
      #  # check secondary extinction
      v_extinction <- c()
      v_animals <- names(df)
      for(animal in v_animals){
        w <- df[,animal]
        if (all(w == 0)) {
          # print(paste0(animal, "  goes extinct"))
          v_extinction <- c(v_extinction, animal)
          isec_ext = isec_ext + 1
        }
      }

      if(length(v_extinction)>0){

        for (aa in v_extinction){
          ja = fmatch(aa, colnames(df))
          df <- df[ ,-ja]

        }
      }

      plants_ext[iloop] = plants_ext[iloop] + iprim_ext
      animals_ext[iloop] = animals_ext[iloop] + isec_ext

      df_old <- df

    }
  }


  res <- cbind(plants_ext/n_iter,animals_ext/n_iter)

  res_data <- data.frame(res)
  names(res_data) <-c("removed_plants","removed_animals")
  return(res_data)
}


degree_moments <- function(bipartite_network){
  degree_df <- as.data.frame(rowSums(bipartite_network !=0))
  names(degree_df) <- c( "degree")
  res <- c(mean(degree_df$degree), sd(degree_df$degree), var(degree_df$degree))
  return(res)
}


order_and_shuffle_bipartite <- function(bipartite_network,desc){

  # Compute degree for Row Species and order in descending if desc == TRUE
  degree_vector = sort(rowSums(bipartite_network),decreasing = desc)
  # Extract the Species names associated to the ordering
  names_vector = names(degree_vector)
  # Convert names and degree to data frame
  name_degree_df = data.frame(names_vector,unname(degree_vector))
  # Name the columns of the dataframe
  names(name_degree_df) = c("name","degree")
  # Initialize empty dataframe to store shuffling
  shuffled_df = data.frame(NULL)

  # Loop through unique degree values
  for (i in unique(name_degree_df$degree)) {
    # Find all Species names with i degree and store in vector
    name_by_degree = name_degree_df$name[which(name_degree_df$degree == i)]
    # Shuffle vector
    shf_name_by_degree = sample(name_by_degree)
    # Create a new dataframe for the shuffled names and current degree value
    shf_df_by_degree = data.frame(as.matrix(shf_name_by_degree,nrow=1),(as.matrix(rep(i,length(shf_name_by_degree)),nrow = 1)))
    # Name the current degree dataframe
    names(shf_df_by_degree) = c("name","degree")
    # Fill the overall dataframe with current shuffle
    shuffled_df = rbind(shuffled_df,shf_df_by_degree)
  }
  # Return overall dataframe
  return(shuffled_df)
}
